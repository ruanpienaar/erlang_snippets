-module (disklog_export).

%% Get a list of nodes that were monitored.
%% TODO - strip out node names from log entries
%% We already have info about re-occurances

% Grindr Run:
% From 2282 entries to 6 entries ( log-2015-02-10_08-20-22 )
% From 15632 entries to 6 entries ( log-2015-02-16_20-25-53 )

% EE Run:



% Props:[{alarm_msg_type,new}]
% Props:[{alarm_msg_type,repeat}]
% Props:[{alarm_msg_type,clear}]

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-define(STATE, dislog_txt_state).
-record(?STATE, { entries = [],
                  entry_count = 0,
                  status = busy,
                  push_server_pid,
                  total = 0 }).

-type id() :: {string(), binary(), binary()}.
-record(?MODULE, {id :: id(),
                  count = 0 :: integer(),
                  message :: string()
                  }).

-export([
         install/0,
         dl_to_db/0,
         csv/0]).

-record(log, {
          date          :: erlang:timestamp(),
          severity      :: binary(),
          originator    :: binary(),
          node_id       :: binary(),
          node          :: binary(),
          message       :: binary(),
          props = []    :: proplists:proplist()
         }).

% ets_tbl() ->    All = ets:all(),
%     case lists:member(notifications,All) of
%         true ->
%             true = ets:delete_all_objects(notifications);
%         false ->
%             notifications = ets:new(notifications,[named_table,set,public])
%     end.

install() ->
    stopped = mnesia:stop(),
    mnesia:create_schema([node()]),
    ok = mnesia:start().

mnesia() ->
    mnesia:start(),
    try
        [id,count,message] = mnesia:table_info(?MODULE,attributes),
        {atomic,ok} = mnesia:delete_table(?MODULE),
        table()
    catch
        exit:{aborted,{no_exists,?MODULE,attributes}} ->
            table();
        C:E ->
            throw({stop,[{c,C},{e,E},{s,erlang:get_stacktrace()}]})
    end.

table() ->
    {atomic,ok} =
    mnesia:create_table(
            ?MODULE,
            [{type,set},
             {attributes,record_info(fields, ?MODULE)}
           ]).

dl_to_db() ->
    % ets_tbl(),
    mnesia(),
    dl_to_db("logs").

dl_to_db(Dir) ->
    % RevFiles = filelib:fold_files(
    %           Dir,
    %           "log-.*",
    %           false,
    %           fun(LogFile, Acc) -> [filename:basename(LogFile) | Acc] end,
    %           []),
    % Files = lists:sort(RevFiles),
    Files = ["log-2015-02-03_11-03-58"],
    Pids = lists:map(fun(_) ->
        {ok,Pid} = start_link(pop_server),
        Pid
    end, lists:seq(1,erlang:system_info(schedulers_online))),

    ok = lists:foreach(fun(File) ->
        export_disklog_file(Dir++"/"++File,Pids)
    end, Files),

    lists:foreach(fun(Pid) ->
        done(Pid)
    end, Pids),
    ok.

csv() -> %% §
    {ok,FPid} = file:open("entries.csv", [write]),
    ok = file:write(FPid,<<"Count§App§Node§Message\n">>),
    csv(FPid,first()).

csv(FPid,'$end_of_table') ->
    ok = file:close(FPid);
csv(FPid,Id) ->
    [Rec] = lookup(Id),
    {_Str,O,N} = Rec#?MODULE.id,
    Data = list_to_binary(io_lib:format("~p§\"~p\"§~p§~p\n",
        [Rec#?MODULE.count,O,N,Rec#?MODULE.message])),
    ok = file:write(FPid,Data),
    csv(FPid,next(Id)).

export_disklog_file(F,Pids) ->
    export_disklog_file(F,Pids,1).

export_disklog_file(F,Pids,PidNumber) ->
    io:format("File : ~p\n",[F]),
    %%io:format(".f."),
    case disk_log:open([{name, F}, {file, F}, {mode, read_write}]) of
        {error, E} ->
            io:format("Error : ~p\n",[E]);
        _ ->
            export_disklog_file(F,disk_log:chunk(F, start),Pids,PidNumber)
    end.

export_disklog_file(F,eof,_Pids,_PidNumber) ->
    disk_log:close(F);
export_disklog_file(F,{NewCont, Entries},Pids,PidNumber) ->
    % io:format("C."),
    Pid = lists:nth(PidNumber,Pids),
    case push(Pid,Entries) of
        EntryCount when EntryCount >= 500 ->
            timer:sleep(1000),
            go_on(PidNumber,F,NewCont,Pids);
        _EntryCount ->
            %% io:format("~p. ",[EntryCount]),
            go_on(PidNumber,F,NewCont,Pids)
    end.

go_on(PidNumber,F,NewCont,Pids) ->
    NewPNmr = case PidNumber of
        N when N == length(Pids) ->
            1;
        N when N > 0, N < length(Pids) ->
            N + 1
    end,
    export_disklog_file(F,disk_log:chunk(F, NewCont),Pids,NewPNmr).

%% -----------

start_link(InitArgs) ->
    gen_server:start_link(?MODULE, InitArgs, []).

push(Pid,Entries) ->
    gen_server:call(Pid, {push,Entries}, infinity).

done(Pid) ->
    gen_server:cast(Pid, done).

%% -----------

init(pop_server) ->
    _TREF = erlang:start_timer(2, self(), pop_a_few),
    {ok, #?STATE{}}.

handle_call({process, Entries}, _From, State) ->
    process_data(Entries),
    {reply,ok,State};
handle_call({push,Entries}, _From, State) ->
    NewCount = State#?STATE.entry_count + 1,
    {reply, NewCount,
        State#?STATE{ entries = [ Entries | State#?STATE.entries ],
                      entry_count = NewCount
                    }};
handle_call(_Request, _From, State) ->
    {reply, {error,     unknown_call}, State}.

handle_cast(done, State) ->
    {noreply, State#?STATE{ status = done }};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout,_TREF,pop_a_few}, #?STATE{ status = done,
                                                entries = [] } = State) ->
    {noreply,State};
handle_info({timeout,_,pop_a_few}, #?STATE{ entries = []         } = State) ->
    {noreply, State};
handle_info({timeout,_,pop_a_few}, #?STATE{ entries = [Entries|R] } = State) ->
    Total = State#?STATE.total,
    case Total rem 100 of
        0 ->
            io:format(".");
        _ ->
            ok
    end,
    ok = process_data(Entries),
    _TREF = erlang:start_timer(25, self(), pop_a_few),
    {noreply,State#?STATE{ entries = R,
                           entry_count = State#?STATE.entry_count - 1,
                           total = Total + 1
                           }}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------

process_data(Entries) when is_list(Entries) ->
    %%io:format("p.",[]),
    ok =
        %% TODO: Maybe do a first and last on date,
        %% so that you can see a period...
        lists:foreach(fun(#log{date=_D,
                               severity=_S,
                               originator=O,
                               node_id=_NI,
                               node=N,
                               message=M,
                               props=_P}) ->
            CleanStr = remove_datetime( remove_pid(binary_to_list(M)) ),
            Preped =   prepare_for_lev_s(CleanStr),
            Id = {Preped,O,N},
            case lev_s_loop_table(Id) of  %% This just bumps up a count
                not_simmilar ->
                    new_rec(#?MODULE{id = Id, message = M});
                {simmilar,LoopId} ->
                    [Rec] = lookup(LoopId),
                    inc_rec_count(Rec)
            end
        end, Entries).

remove_pid(Str) -> %% And port, etc
    case remove_re({"(<[0-9]+.[0-9]+.[0-9]+>)"},Str,"P") of
        {nomatch,FinalStr}  -> FinalStr;
        {match,CleaningStr} -> remove_pid(CleaningStr)
    end.

remove_datetime(Str) ->
    case remove_re(
    [ %% 9-Feb-2015::09:20:58
     "([0-9]-[A-Z][a-z]{2}-[0-9]{4}::[0-9]{2}:[0-9]{2}:[0-9]{2})",
      %% 01:24:52.553
     "([0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3})"],
     Str,"D") of
        {nomatch,FinalStr}  -> FinalStr;
        {match,CleaningStr} -> remove_datetime(CleaningStr)
    end.

% remove_cr(Str) ->
%     case remove_re({"\n"},Str,"N") of
%         {nomatch,FinalStr} -> FinalStr;
%         {match,CleaningStr} -> remove_cr(CleaningStr)
%     end.

remove_re(Re,Str,Char) when is_list(Re) ->
    F = fun(R,_) ->
        remove_re({R},Str,Char)
    end,
    lists:foldl(F,{nomatch,Str},Re);
remove_re({Re},Str,Char) ->
    case re:run(Str,Re,[global]) of
        {match,Match} ->
            [{Start,Length}|_] = lists:flatten(Match),
            do_clean(Str,Char,Start,Length);
        nomatch ->
            {nomatch,Str}
    end.

do_clean(Str,Char,Start,Length) ->
    {match,string:substr(Str,1,Start)++Char++lists:nthtail(Start+Length, Str)}.


%%------------------------------------------------------------------------------
%% @spec levenshtein(StringA, StringB) -> integer()
%%      StringA = StringB = string()
%% @doc Calculates the Levenshtein distance between two strings
%% @end
%%------------------------------------------------------------------------------

levenshtein(Samestring, Samestring) -> 0;
levenshtein(String, []) -> length(String);
levenshtein([], String) -> length(String);
levenshtein(Source, Target) ->
    levenshtein_rec(Source, Target, lists:seq(0, length(Target)), 1).

%% Recurses over every character in the source string and calculates a list of distances
levenshtein_rec([SrcHead|SrcTail], Target, DistList, Step) ->
    levenshtein_rec(SrcTail, Target, levenshtein_distlist(Target, DistList, SrcHead, [Step], Step), Step + 1);
levenshtein_rec([], _, DistList, _) ->
    lists:last(DistList).

%% Generates a distance list with distance values for every character in the target string
levenshtein_distlist([TargetHead|TargetTail], [DLH|DLT], SourceChar, NewDistList, LastDist) when length(DLT) > 0 ->
    Min = lists:min([LastDist + 1, hd(DLT) + 1, DLH + dif(TargetHead, SourceChar)]),
    levenshtein_distlist(TargetTail, DLT, SourceChar, NewDistList ++ [Min], Min);
levenshtein_distlist([], _, _, NewDistList, _) ->
    NewDistList.

% Calculates the difference between two characters or other values
dif(C, C) -> 0;
dif(_, _) -> 1.

% Acceptance rate is 75% simmilarity
% Rate(%) = levenshtein Count / String length
lev_s_loop_table(Id={_,_,_}) ->
    loop_tbl(first(), Id).

loop_tbl('$end_of_table',_Id={_,_,_}) ->
    not_simmilar;
loop_tbl(LoopId={CleanStr,_,_},
            _Id={CleanStr,_,_}) ->
    %% ok = io:format(".d.",[]), % Drop
    {simmilar,LoopId};
loop_tbl(LoopId={LoopCleanStr,_,_},
             Id={CleanStr,    _,_}) ->
    LevCount = levenshtein(CleanStr, LoopCleanStr),
    case (LevCount/length(CleanStr)) > 0.75 of
        true ->
            %% ok = io:format(".d.",[]),% Drop
            {simmilar,LoopId};
        false ->
            loop_tbl(next(LoopId),Id)
    end.

prepare_for_lev_s(CleanStr) ->
    NoSpaces = re:replace(CleanStr, "(\\s+)", "", [global,{return,list}]),
    UP=string:to_upper(NoSpaces),
    Limit = 250,
    L=case length( UP ) > Limit of
        true ->
             {List2, _List3} = lists:split(Limit, UP),
            List2;
        false ->
            UP
    end,
     _NoNumbers = re:replace(L, "([0-9])", "", [global,{return,list}]).

lookup(Id) ->
    mnesia:dirty_read(?MODULE, Id).

inc_rec_count(Rec) ->
    UpdateRec = Rec#?MODULE{ count = Rec#?MODULE.count + 1 },
    ok = mnesia:dirty_write(UpdateRec).

new_rec(NewR) ->
    %% TODO: do we need some duplication check?
    ok = mnesia:async_dirty(fun() ->
        mnesia:dirty_write(NewR)
    end).

first() ->
    mnesia:dirty_first(?MODULE).

next(Id) ->
    mnesia:dirty_next(?MODULE, Id).
