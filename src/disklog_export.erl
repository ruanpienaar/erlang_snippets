-module (disklog_export).

%% Get a list of nodes that were monitored.
%% TODO - strip out node names from log entries
%% We already have info about re-occurances

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
                  push_server_pid }).

-export([dl_to_ets/0,
         dl_to_ets_g/0,
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

ets_tbl() ->    All = ets:all(),
    case lists:member(notifications,All) of
        true ->
            true = ets:delete_all_objects(notifications);
        false ->
            notifications = ets:new(notifications,[named_table,set,public])
    end.

dl_to_ets() ->
    ets_tbl(),
    dl_to_ets("/Users/ruanpienaar/code/wombat_ee/rel/wombat/wombat/data/wombat@vsiswebwb01/core_mm").

dl_to_ets_g() ->
    ets_tbl(),
    % dl_to_ets("/Users/ruanpienaar/code/wombat_ee/rel/wombat/wombat/data/wombat@vsiswebwb01/core_mm/grindr").
    dl_to_ets("/Users/ruanpienaar/code/erlang/erlang_snippets/test").


dl_to_ets(Dir) ->
    % RevFiles = filelib:fold_files(
    %           Dir,
    %           "log-.*",
    %           false,
    %           fun(LogFile, Acc) -> [filename:basename(LogFile) | Acc] end,
    %           []),
    % Files = lists:sort(RevFiles),
    Files = ["log-2015-02-18_05-27-07"],
    Pids = lists:map(fun(_) ->
        {ok,Pid} = start_link(pop_server),
        Pid
    end, lists:seq(1,8)),

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
    csv(FPid,ets:first(notifications)).

csv(FPid,'$end_of_table') ->
    ok = file:close(FPid);
csv(FPid,Id) ->
    [{{Str,O,N},Cnt}] = ets:lookup(notifications,Id),
    Data = list_to_binary(io_lib:format("~p§\"~p\"§~p§~p\n",[Cnt,O,N,Str])),
    ok = file:write(FPid,Data),
    csv(FPid,ets:next(notifications,Id)).

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
    io:format("C."),
    Pid = lists:nth(PidNumber,Pids),
    case push(Pid,Entries) of
        EntryCount when EntryCount >= 1000 ->
            timer:sleep(500),
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
    _TREF = erlang:start_timer(50, self(), pop_a_few),
    {ok, #?STATE{}}.

handle_call({process, Entries}, _From, State) ->
    process_data(Entries),
    {reply,ok,State};
handle_call({push,Entries}, _From, State) ->
    NewCount = State#?STATE.entry_count + 1,
    {reply, NewCount,
        State#?STATE{ entries = [ Entries | State#?STATE.entries ],
                      entry_count = NewCount }};
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
    ok = process_data(Entries),
    _TREF = erlang:start_timer(4, self(), pop_a_few),
    {noreply,State#?STATE{ entries = R,
                           entry_count = State#?STATE.entry_count - 1 }}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------

process_data(Entries) when is_list(Entries) ->
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
            Id = {CleanStr,O,N},

            ok = loop_table({CleanStr,O,N}),

            case ets:lookup(notifications, Id) of
                []         -> true=ets:insert_new(notifications,{Id,0});
                [{Id,Cnt}] -> true=ets:insert(notifications,{Id,Cnt+1})
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



% Acceptance rate is 85% simmilarity
% Rate(%) = levenshteinCount/length
loop_table({CleanStr,_O,_N}) ->
    loop_tbl(ets:first(notifications),CleanStr).

loop_tbl('$end_of_table',_CleanStr) ->
    ok;
loop_tbl(Id,CleanStr) ->
    [{{Str,_O,_N},Cnt}] = ets:lookup(notifications,Id),
    LevCount = levenshtein(CleanStr, Str),
    case (LevCount/length(Str)) > 0.85 of
        true ->
            true=ets:insert(notifications,{Id,Cnt+1}),
            ok = io:format(".d.",[]);
        false ->
            loop_tbl(ets:next(notifications,Id),CleanStr)
    end.
