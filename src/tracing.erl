%% Trace
-module(tracing).

-export([
    trace/4,
    stop_trace/0
]).

-type trace_mod_spec() :: {module()} | 
                          {module(), function()} |
                          {module(), function(), arity()}.
                          
-type trace_time_spec() :: {minutes, pos_integer()} |
                           {seconds, pos_integer()} |
                           {miliseconds, pos_integer()}.
                           
-type node_spec() :: {node(), Cookie :: atom()}.

%% @doc trace specified modules on nodes for a total amount of messages or time whichever comes first.
%%      Example: trace([ node1@host1 ], [ {mymod, myfunc} ], 1000, {seconds, 30}).
%%               trace mymod:myfunc calls on node1 for 30 seconds or a max of 1000 trace messages.
%% @end
-spec trace([ node_spec() ], [ trace_mod_spec() ], pos_integer(), trace_time_spec()) -> ok.
trace(Nodes, TraceModSpecs, TotalMessages, TraceTime) 
        when is_list(Nodes), 
             is_list(TraceModSpecs),
             is_integer(TotalMessages),
             is_tuple(TraceTime) ->
    case whereis(net_sup) of
        undefined ->
            erlang:exit({error, distribution_not_started});
        _Pid ->
            ok
    end,
    case lists:keyfind(hidden, 1, init:get_arguments()) of
        false ->
            erlang:exit({error, node_not_hidden});
        {hidden, _} ->
            ok
    end,
    stop_trace(),
    TraceTimeMiliSec = case TraceTime of
        {minutes, X} ->
            X * 60 * 1000;
        {seconds, X} ->
            X * 1000;
        {miliseconds, X} ->
            X
    end,
    timer:apply_after(TraceTimeMiliSec, io, format, ["STOP TRACE - TIMER REACHED MAX\n", []]),
    timer:apply_after(TraceTimeMiliSec, dbg, stop_clear, []),
    {ok, _} = dbg:tracer(process, {
        fun
            (Trace, Total) when Total < TotalMessages ->
                trace_message_new_total(Total, Trace);
            (Trace, Total) when Total >= TotalMessages ->
                _NewTotal = trace_message_new_total(Total, Trace),
                % [ rpc:call(Node, dbg, stop_clear, []) || Node <- Nodes ],
                io:format("STOP TRACE - MAX MESSAGES REACHED\n", []),
                stop_trace()
        end, 0}),
    [ 
      begin
          true = erlang:set_cookie(Node, Cookie),
          true = net_kernel:connect_node(Node),
          {ok, _} = dbg:n(Node) 
      end || {Node, Cookie} <- Nodes
    ],
    {ok, _} = dbg:p(all, [call, timestamp]),
    
    % Breaks things, can't exclude local node, cause we would then have to start tracer remotely!
    % ok = dbg:cn(node()),
    
    lists:foreach(
        fun
            ({Mod}) ->
                dbg:tpl(Mod, cx);
            ({Mod, Func}) ->
                dbg:tpl(Mod, Func, cx);
            ({Mod, Func, Arr}) ->
                dbg:tpl(Mod, Func, Arr, cx)
        end, TraceModSpecs).
    
trace_message_new_total(Total, Trace) ->
    Node = node(),
    case node(element(2, Trace)) of
            Node ->
                % io:format(".\n"),
                % ignore messages from the trace node, let's say you trace something like lists, ets etc
                Total;
            RemoteNode ->
                % io:format("~p\t~p\t~p\n", [RemoteNode, Total, Trace]),
                io:format("~s\n", [format_trace_item(RemoteNode, Trace)]),
                Total + 1
    end.

stop_trace() ->
    ok = dbg:stop_clear(),
    clear_all_timers().

clear_all_timers() ->
    %% TODO: maybe find only our timers?
    case ets:info(timer_tab, size) of
        undefined ->
            ok;
        _ ->
            [ {ok, cancel} = timer:cancel(element(1, E)) || E <- ets:tab2list(timer_tab) ]
    end.

format_trace_item(Node, _Trace={trace_ts, _Pid, exception_from, Info, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~s: ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(exception_from), Info]
    );

format_trace_item(Node, _Trace={trace_ts, _Pid, return_from, Info, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~s: ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(return_from), Info]
    );

format_trace_item(Node, _Trace={trace_ts, _Pid, call, Info, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~s: ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(call), Info]
    );

format_trace_item(Node, _Trace={trace_ts, _Pid, Label, Info, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~s: ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(Label), Info]
    );

format_trace_item(Node, _Trace={trace_ts, _Pid, exception_from, Info, Extra, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~s: ~p ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(exception_from), Info, Extra]
    );

format_trace_item(Node, _Trace={trace_ts, _Pid, return_from, Info, Extra, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~s: ~p ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(return_from), Info, Extra]
    );

format_trace_item(Node, _Trace={trace_ts, _Pid, call, Info, Extra, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~s: ~p ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(call), Info, Extra]
    );

format_trace_item(Node, _Trace={trace_ts, _Pid, Label, Info, Extra, ReportedTS}) ->
    io_lib:format(
        "~s ~p ~s: ~p ~1000p\n",
        [get_time(ReportedTS), Node, get_trace_abbreviation(Label), Info, Extra]
    );

format_trace_item(Node, Trace={seq_trace, _Label, _SeqTraceInfo}) ->
   io_lib:format("~p ~p", [Node, Trace]);

format_trace_item(Node, Trace={drop, _NumberOfDroppedItems}) ->
    io_lib:format("~p ~p", [Node, Trace]);

format_trace_item(Node, Trace) ->
    io_lib:format("~p ~p", [Node, Trace]).
    
get_time({_,_,Micro} = Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(Timestamp),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0sZ",
                  [Year, Month, Day, Hour, Minute, Second, integer_to_list(Micro)])).
                  
get_trace_abbreviation('receive') ->
    "RECEIVE";
get_trace_abbreviation(send) ->
    "SEND";
get_trace_abbreviation(send_to_non_existing_process) ->
    "SEND_TO_NON_EXI_PROC";
get_trace_abbreviation(call) ->
    "CALL";
get_trace_abbreviation(return_to) ->
    "RETURN_TO";
get_trace_abbreviation(return_from) ->
    "RETURN_FROM";
get_trace_abbreviation(exception_from) ->
    "EXCEPTION_FROM";
get_trace_abbreviation(spawn) ->
    "SPAWN";
get_trace_abbreviation(exit) ->
    "EXIT";
get_trace_abbreviation(link) ->
    "LINK";
get_trace_abbreviation(unlink) ->
    "ULINK";
get_trace_abbreviation(getting_linked) ->
    "GETTING_LINKED";
get_trace_abbreviation(getting_unlinked) ->
    "GETTING_UNLINKED";
get_trace_abbreviation(register) ->
    "REGISTER";
get_trace_abbreviation(unregister) ->
    "UNREGISTER";
get_trace_abbreviation(in) ->
    "IN";
get_trace_abbreviation(out) ->
    "OUT";
get_trace_abbreviation(gc_start) ->
    "GC_START";
get_trace_abbreviation(gc_end) ->
    "GC_END".