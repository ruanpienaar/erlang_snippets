-module(consolidate_logs).
-export([ main/1,
        remove_pid/0
    ]).

-define(E,entry).
-record(?E,{
    module_line_and_message,
    severity,
    count=1,
    file_name
}).

main(Args) ->
    run(Args).
    % dbg:tracer(),
    % dbg:p(all,call),
    % dbg:tpl(?MODULE,[ {'_',[],[ {message, {return_trace}} ]} ] ),
    % dbg:tpl(re,[ {'_',[],[ {message, {return_trace}} ]} ] ),
    %% remove_pid().

run([AbsolutePath]) ->
    ?E = ets:new(?E, [set, named_table, {keypos,2}]),
    nomatch = ets:new(nomatch, [set, named_table]),
    true = ets:insert(nomatch,{count,0}),
    % Example lager config.
    %% TODO: find a nice way to compile
    % the lager config into a regexp
    % Config=
    %     {formatter_config,
    %       [date, " ", time, " [", severity, "] ",
    %        pid, " ",
    %        {module,
    %         [ "[", module, {line, [":", line], ""}, "] " ],
    %         ""},
    %        message, "\n"]},
    %%DIR="/Users/ruanpienaar/Downloads/rel/wombat/wombat/log/wombat/",
    {ok,Files} = file:list_dir(AbsolutePath),
    [ run_file(AbsolutePath++F) || F <- Files ],
    to_html(),
    nomatch_to_html(),
    try
        true = ets:delete(?E)
    catch
        _C:_E ->
            ok
    end,
    try
        true = ets:delete(nomatch)
    catch
        _C2:_E2 ->
            ok
    end.

run_file(FileName) ->
    {ok,FD} = file:open(FileName,[read,raw,binary]),
    read_lines(FileName,FD).

read_lines(FN,FD) ->
    case file:read_line(FD) of
        eof ->
            ok;
        {ok,Data} ->
            case parse_line(Data) of
                {ok,LineList} ->
                    io:format("."),
                    ok = store_line(LineList,FN),
                    read_lines(FN,FD);
                nomatch ->
                    io:format("x"),
                    store_nomatch(Data,FN),
                    read_lines(FN,FD)
            end
    end.

parse_line(Data) ->

    DateTimeRE="([0-9]+-[0-9]+-[0-9]+ [0-9]+:[0-9]+:[0-9]+.[0-9]+)",
    Severity="([\[[a-z]+])",
    Pid="(<[0-9]+.[0-9]+.[0-9]+>)",

    ErrorModLine="(\[[a-z0-9_]+:[0-9]+\])",
    AlarmHandler="([a-zA-Z0-9_]+:)",
    Behaviour="([a-zA-Z0-9|_]+)",
    SASL="([a-zA-Z0-9]+ [a-zA-Z0-9]+)",

    ErrorRE = DateTimeRE++" "++Severity++" "++Pid++" "++ErrorModLine++" (.*)",
    AlarmRE = DateTimeRE++" "++Severity++" "++Pid++" "++AlarmHandler++" (.*)",
    BehRE = DateTimeRE++" "++Severity++" "++Pid++" "++Behaviour++" (.*)",
    SaslRE = DateTimeRE++" "++Severity++" "++Pid++" "++SASL++" (.*)",

    parse_line(Data,[ErrorRE, AlarmRE, BehRE, SaslRE]).

parse_line(_Data,[]) ->
    nomatch;
parse_line(Data,[H|T]) ->
    Opts=[global, {capture,[1,2,3,4,5],list}],
    case re:run(binary_to_list(Data), H, Opts) of
        {match,[[DateTime,Severity,Pid,ModAndLineNmr,Message]]} ->
            {ok,[DateTime,Severity,Pid,ModAndLineNmr,Message]};
        nomatch ->
            parse_line(Data,T)
    end.

store_line([_DateTime,Severity,_Pid,ModAndLineNmr,Message],FN) ->
    NoPidMsg = remove_pid(Message),
    case ets:lookup(?E,{ModAndLineNmr,NoPidMsg}) of
        [] ->
            insert_new(ModAndLineNmr,NoPidMsg,Severity,FN),
            ok;
        [Rec] ->
            case Rec#?E.module_line_and_message == {ModAndLineNmr,NoPidMsg} of
                true ->
                    _NewCount =
                        ets:update_counter(
                            ?E, {ModAndLineNmr,NoPidMsg}, {_Pos=4,_Incr=1}),
                    ok;
                false ->
                    insert_new(ModAndLineNmr,NoPidMsg,Severity,FN)
            end
    end.

insert_new(ModAndLineNmr,NoPidMsg,Severity,FN) ->
    true = ets:insert_new(?E,#?E{
        module_line_and_message = {ModAndLineNmr,NoPidMsg},
        severity = Severity,
        file_name = FN}
    ).

store_nomatch(Data,FN) ->
    NewCount=ets:update_counter(nomatch,count,1),
    ets:insert(nomatch,{NewCount,Data,FN}).

remove_pid() ->
    remove_pid("2015-01-14 01:13:21.788 [debug] <45389.30259.3477> Supervisor {<45389.30259.3477>,wombat_plugin_proc_sup} started wombat_plugin:start_link(wombat_plugin_alarm, {wombat_ref,alarm,<45389.30258.3477>}, [{collection_interval,60000},{interval,10000},{system_checks,[{process_limit,[{minor,80},{major,...}]},...]},...]) at pid <45389.3949.3478>").

remove_pid(Msg) ->
    case re:run(Msg,"(<[0-9]+.[0-9]+.[0-9]+>)",[global]) of
        {match,[[{Start,Length},_]|_]} ->
            remove_pid(string:substr(Msg,1,Start)++
                       lists:flatten([ "_" || _X <- lists:seq(1,Length)])++
                       lists:nthtail(Start+Length, Msg)
            );
        nomatch ->
            Msg
    end.

ensure_list(M) when is_atom(M) ->
    ensure_list(atom_to_list(M));
ensure_list(M) when is_binary(M) ->
    ensure_list(binary_to_list(M));
ensure_list(M) when is_integer(M) ->
    ensure_list(integer_to_list(M));
ensure_list(M) when is_list(M) ->
    M.

to_html() ->
    Entries = ets:tab2list(?E),
    Sorted = lists:reverse(lists:keysort(4, Entries)),
    file:delete("results.html"),
    {ok,FD} = file:open("results.html",[write]),
    HTML1 = "<html><head></head><body><table>
    <th><td>Module : Line Number</td><td>Severity</td><td>Count</td>
    <td>Message</td><td>Log filename</td></th>",
    ok = file:write(FD,HTML1),
    [
        begin
            {ModuleLine,Message} = E#?E.module_line_and_message,
            Data = "<tr><td>"++ModuleLine++"</td>
                <td>"++E#?E.severity++"</td>
                <td>"++integer_to_list(E#?E.count)++"</td>
                <td>"++Message++"</td>
                <td>"++E#?E.file_name++"</td></tr>",
            ok = file:write(FD,Data)
        end || E <- Sorted
    ],
    HTML2 = "</table></body></html>",
    ok = file:write(FD,HTML2),
    ok = file:close(FD),
    os:cmd("open results.txt").

nomatch_to_html() ->
    Entries = ets:tab2list(nomatch),
    Sorted = lists:keysort(1,Entries),
    file:delete("nomatch_results.html"),
    {ok,FD} = file:open("nomatch_results.html",[write]),
    HTML1 = "<html><head></head><body><table>
    <th><td>Id</td><td>No Match Entry</td><td>Log filename</td></th>",
    ok = file:write(FD,HTML1),
    [
        begin
            Data = "<tr><td>"++ensure_list(Count)++"</td>" ++
                "<td>"++ensure_list(M)++"</td>" ++
                "<td>"++ensure_list(FN)++"</td></tr>",
            ok = file:write(FD,Data)
        end || {Count,M,FN} <- Sorted
    ],
    HTML2 = "</tr></table></body></html>",
    ok = file:write(FD,HTML2),
    ok = file:close(FD),
    os:cmd("open nomatch_results.html").