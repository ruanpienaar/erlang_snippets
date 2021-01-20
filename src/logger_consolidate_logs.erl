-module(logger_consolidate_logs).
-export([
    main/1,
    levenshtein/2
]).

% -define(E,entry).
% -record(?E,{
%     module_line_and_message,
%     severity,
%     count=1,
%     file_name
% }).

main(Args) ->
    run(Args).
    % dbg:tracer(),
    % dbg:p(all,call),
    % dbg:tpl(?MODULE,[ {'_',[],[ {message, {return_trace}} ]} ] ),
    % dbg:tpl(re,[ {'_',[],[ {message, {return_trace}} ]} ] ),
    %% remove_pid().

run([AbsolutePath]) ->
    % ?E = ets:new(?E, [set, named_table, {keypos, 2}]),
    % nomatch = ets:new(nomatch, [set, named_table]),
    % true = ets:insert(nomatch, {count, 0}),
    {ok, Files} = file:list_dir(AbsolutePath),
    erlang:display({absolute_path_files, Files}),
    try
        ets:delete(tbl)
    catch
        _:_:_ ->
          ok
    end,
    %% TODO: filename join...
    [ run_file(AbsolutePath ++ F) || F <- Files ],
    % to_html(),
    % nomatch_to_html(),
    % try
    %     true = ets:delete(?E)
    % catch
    %     _C:_E ->
    %         ok
    % end,
    % try
    %     true = ets:delete(nomatch)
    % catch
    %     _C2:_E2 ->
    %         ok
    % end.
    ok.

run_file(FileName) ->

    BaseFilename = filename:basename(FileName),

    case exclusion(BaseFilename) of
        nomatch ->
            case re:run(BaseFilename, "([a-zA-Z_]{1,})\.", [{capture, all, list}]) of
                {match, [_, TblNameString]} ->
                    io:format("Processing file ~p\n", [BaseFilename]),
                    TblName = list_to_atom(TblNameString),
                    case ets:info(TblName, size) of
                        undefined ->
                            TblName = ets:new(TblName, [set, named_table, {write_concurrency, true}, {read_concurrency, true}]);
                        _ ->
                            ok
                    end,
                    % erlang:display({filename, BaseFilename}),
                    {ok,FD} = file:open(FileName, [read, raw, binary]),
                    read_lines(TblName, BaseFilename, FD);
                _ ->
                    io:format("NOT processing file ~p\n", [BaseFilename])
            end;
        _ ->
            io:format("NOT processing file ~p\n", [BaseFilename])
    end.

exclusion(BaseFilename) ->
    ExcludeList = [
        "monitor",
        "historical",
        "memento",
        "monty",
        "suggestions_sugrom"
    ],
    exclusion(BaseFilename, ExcludeList).

exclusion(_BaseFilename, []) ->
    nomatch;
exclusion(BaseFilename, [E|T]) ->
    case string:find(BaseFilename, E) of
        nomatch ->
            exclusion(BaseFilename, T);
        _ ->
            match
    end.

read_lines(TblName, FN, FD) ->
    % io:format("rl "),
    case file:read_line(FD) of
        eof ->
            ok;
        {ok, Line} ->
            % io:format("l "),
            ok = start_collect_entry_lines(TblName, FN, FD, Line),
            read_lines(TblName, FN, FD)
    end.

start_collect_entry_lines(TblName, FN, FD, Line) ->
    % io:format("sc "),
    case parse_line(Line) of
        ignore_log_heading -> %% After the heading was ignored, will ignore the preceeding log lines too below
            % io:format("IH "),
            %% Drop, until we reach the first parse-able line
            ok;
        {log_line, _LogLine} ->
            %% Drop, until we reach the first parse-able line
            ok;
        {log_heading, LogHeading} ->
            collect_log_entry_lines(TblName, FN, FD, LogHeading)
    end.

collect_log_entry_lines(TblName, FN, FD, Acc) ->
    % io:format("cle "),
    case file:read_line(FD) of
        eof ->
            store_or_increment(TblName, lists:reverse(Acc));
        {ok, Line} ->
            % io:format("l "),
            case parse_line(Line) of
                {log_heading, LogHeading} ->
                    % io:format("H "),
                    store_or_increment(TblName, lists:reverse(Acc)),
                    collect_log_entry_lines(TblName, FN, FD, LogHeading);
                ignore_log_heading ->
                    % io:format("IH "),
                    % store_or_increment(lists:reverse(Acc)),
                    start_collect_entry_lines(TblName, FN, FD, Line);
                {log_line, LogLine} ->
                    collect_log_entry_lines(TblName, FN, FD, [LogLine|Acc]) %% not the most efficient
            end
    end.

% hd() == Heading list
% [Severity, Day, Month, Year, TimeWithMs, Module, Function, FuncArity, LineNumber]
% tl() == rest of log lines
% "string "
store_or_increment(Tbl, [Heading|Lines]) ->
    % io:format("s ~p\n", [Tbl]),
    [_Severity, _Day, _Month, _Year, _TimeWithMs, Module, Function, FuncArity, _LineNumber] = Heading,
    SingleLine = lists:foldl(fun(Line, A) -> Line++A end, "", Lines),
    PreparedString = prepare_for_lev_s(SingleLine),

    case ets:match_object(Tbl, { {'_', '_', '_', '_', '_', Module, Function, FuncArity, '_'}, '_', '_', '_' }) of
        [] ->
            true = ets:insert(
                Tbl,
                {
                    list_to_tuple(Heading),
                    PreparedString,
                    [SingleLine],
                    1
                }),
            ok;
        ExistingEntries ->
            % erlang:display({found, length(ExistingEntries), entries}),
            lists:foreach(
                fun({Key, ExistingPreparedString, ExistingLines, Count}) when ExistingPreparedString =:= PreparedString ->
                        % io:format("new "),
                        ets:insert(
                            Tbl,
                            {Key, ExistingPreparedString, ExistingLines, Count+1}
                        );
                   ({Key, ExistingPreparedString, ExistingLines, Count}) ->
                        % io:format("exist "),
                        LevCount = levenshtein(ExistingPreparedString, PreparedString),
                        LevPercentage = LevCount / length(PreparedString),
                        case LevPercentage =< 0.25 of
                            true -> %% 25% or less different
                                % erlang:display({not_that_different, LevCount}),
                                ets:insert(
                                    Tbl,
                                    {Key, ExistingPreparedString, [SingleLine|ExistingLines], Count+1}
                                );
                            false -> %% more than 25% different
                                % erlang:display({too_different, LevCount}),
                                ets:insert(
                                    Tbl,
                                    {list_to_tuple(Heading), SingleLine, 1}
                                )
                        end
                end,
                ExistingEntries
            )
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

    % ets:insert(tbl, {
    %     list_to_tuple(Heading), SingleLine
    % }),

    % ok.

% read_lines(FN, FD, Acc) ->
%     case fie:read_line(FD) of
%         eof ->
%             Acc;
%         {ok, Line} ->
%             case parse_line(Line) of
%                 {continue, LogEntry} ->
%                     read_lines(FN, FD, [LogEntry|Acc]);
%                 % {complete, CompleteLogEntry, LogEntry} ->
%             end

%     end.

parse_line(Line) ->
    % {continue, LogEntry} ->
    % {complete, CompleteLogEntry, LogEntry} ->

% 59> re:run(
%     "=WARNING REPORT==== 7-Jun-2020::00:06:00.196162 === <0.17802.1> spodds_market_descriptions_leexer:yyaction_16/1:39",
%     "^=([A-Z]{1,}) REPORT==== ([0-9]{1,2})-([a-zA-Z]{3})-([0-9]{4})::([0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{6}) === <[0-9]{1,6}.[0-9]{1,6}.[0-9]{1,6}> ([a-z_]{1,}):([a-z_'$0-9]{1,})\/([0-9]{1,3}):([-0-9]{1,10})",
%     [global, {capture, all, list}]
% ).
% {match,[["=WARNING REPORT==== 7-Jun-2020::00:06:00.196162 === <0.17802.1> spodds_market_descriptions_leexer:yyaction_16/1:39",
%          "WARNING","7","Jun","2020","00:06:00.196162",
%          "spodds_market_descriptions_leexer","yyaction_16","1",
%          "39"]]}
% 60>
    RegExp = "^=([A-Z]{1,}) REPORT==== ([0-9]{1,2})-([a-zA-Z]{3})-([0-9]{4})::([0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{6}) === <[0-9]{1,6}.[0-9]{1,6}.[0-9]{1,6}> ([a-z_]{1,}):([a-z_'$0-9]{1,})\/([0-9]{1,3}):([-0-9]{1,10})",
    ReResult = re:run(Line, RegExp, [global, {capture, all, list}]),
    case ReResult of
        nomatch ->
            [X] = io_lib:format("~s", [Line]),
            {log_line, X};
        {match, [[_LineNoEnding, Severity, Day, Month, Year, TimeWithMs, Module, Function, FuncArity, LineNumber]]} ->
            case Severity of
                Severity when
                        Severity == "DEBUG" orelse
                        Severity == "INFO" orelse
                        Severity == "SUPERVISOR" orelse
                        Severity == "PROGRESS"
                         ->
                    ignore_log_heading;
                _ ->
                    {log_heading, [[Severity, Day, Month, Year, TimeWithMs, Module, Function, FuncArity, LineNumber]]}
            end
    end.




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













% parse_line(Data) ->

%     DateTimeRE="([0-9]+-[0-9]+-[0-9]+ [0-9]+:[0-9]+:[0-9]+.[0-9]+)",
%     Severity="([\[[a-z]+])",
%     Pid="(<[0-9]+.[0-9]+.[0-9]+>)",

%     ErrorModLine="(\[[a-z0-9_]+:[0-9]+\])",
%     AlarmHandler="([a-zA-Z0-9_]+:)",
%     Behaviour="([a-zA-Z0-9|_]+)",
%     SASL="([a-zA-Z0-9]+ [a-zA-Z0-9]+)",

%     ErrorRE = DateTimeRE++" "++Severity++" "++Pid++" "++ErrorModLine++" (.*)",
%     AlarmRE = DateTimeRE++" "++Severity++" "++Pid++" "++AlarmHandler++" (.*)",
%     BehRE = DateTimeRE++" "++Severity++" "++Pid++" "++Behaviour++" (.*)",
%     SaslRE = DateTimeRE++" "++Severity++" "++Pid++" "++SASL++" (.*)",

%     parse_line(Data,[ErrorRE, AlarmRE, BehRE, SaslRE]).

% parse_line(_Data,[]) ->
%     nomatch;
% parse_line(Data,[H|T]) ->
%     Opts=[global, {capture,[1,2,3,4,5],list}],
%     case re:run(binary_to_list(Data), H, Opts) of
%         {match,[[DateTime,Severity,Pid,ModAndLineNmr,Message]]} ->
%             {ok,[DateTime,Severity,Pid,ModAndLineNmr,Message]};
%         nomatch ->
%             parse_line(Data,T)
%     end.

% store_line([_DateTime, Severity, _Pid, ModAndLineNmr, Message], FN) ->
%     NoPidMsg = remove_pid(Message),
%     case ets:lookup(?E,{ModAndLineNmr,NoPidMsg}) of
%         [] ->
%             insert_new(ModAndLineNmr,NoPidMsg,Severity,FN),
%             ok;
%         [Rec] ->
%             case Rec#?E.module_line_and_message == {ModAndLineNmr,NoPidMsg} of
%                 true ->
%                     _NewCount =
%                         ets:update_counter(
%                             ?E, {ModAndLineNmr,NoPidMsg}, {_Pos=4,_Incr=1}),
%                     ok;
%                 false ->
%                     insert_new(ModAndLineNmr,NoPidMsg,Severity,FN)
%             end
%     end.

% insert_new(ModAndLineNmr,NoPidMsg,Severity,FN) ->
%     true = ets:insert_new(?E,#?E{
%         module_line_and_message = {ModAndLineNmr,NoPidMsg},
%         severity = Severity,
%         file_name = FN}
%     ).

% store_nomatch(Data,FN) ->
%     NewCount=ets:update_counter(nomatch,count,1),
%     ets:insert(nomatch,{NewCount,Data,FN}).

% remove_pid(Msg) ->
%     case re:run(Msg,"(<[0-9]+.[0-9]+.[0-9]+>)",[global]) of
%         {match,[[{Start,Length},_]|_]} ->
%             remove_pid(string:substr(Msg,1,Start)++
%                        lists:flatten([ "_" || _X <- lists:seq(1,Length)])++
%                        lists:nthtail(Start+Length, Msg)
%             );
%         nomatch ->
%             Msg
%     end.

% ensure_list(M) when is_atom(M) ->
%     ensure_list(atom_to_list(M));
% ensure_list(M) when is_binary(M) ->
%     ensure_list(binary_to_list(M));
% ensure_list(M) when is_integer(M) ->
%     ensure_list(integer_to_list(M));
% ensure_list(M) when is_list(M) ->
%     M.

% to_html() ->
%     Entries = ets:tab2list(?E),
%     Sorted = lists:reverse(lists:keysort(4, Entries)),
%     file:delete("results.html"),
%     {ok,FD} = file:open("results.html",[write]),
%     HTML1 = "<html><head></head><body><table>
%     <th><td>Module : Line Number</td><td>Severity</td><td>Count</td>
%     <td>Message</td><td>Log filename</td></th>",
%     ok = file:write(FD,HTML1),
%     [
%         begin
%             {ModuleLine,Message} = E#?E.module_line_and_message,
%             Data = "<tr><td>"++ModuleLine++"</td>
%                 <td>"++E#?E.severity++"</td>
%                 <td>"++integer_to_list(E#?E.count)++"</td>
%                 <td>"++Message++"</td>
%                 <td>"++E#?E.file_name++"</td></tr>",
%             ok = file:write(FD,Data)
%         end || E <- Sorted
%     ],
%     HTML2 = "</table></body></html>",
%     ok = file:write(FD,HTML2),
%     ok = file:close(FD),
%     os:cmd("open results.txt").

% nomatch_to_html() ->
%     Entries = ets:tab2list(nomatch),
%     Sorted = lists:keysort(1,Entries),
%     file:delete("nomatch_results.html"),
%     {ok,FD} = file:open("nomatch_results.html",[write]),
%     HTML1 = "<html><head></head><body><table>
%     <th><td>Id</td><td>No Match Entry</td><td>Log filename</td></th>",
%     ok = file:write(FD,HTML1),
%     [
%         begin
%             Data = "<tr><td>"++ensure_list(Count)++"</td>" ++
%                 "<td>"++ensure_list(M)++"</td>" ++
%                 "<td>"++ensure_list(FN)++"</td></tr>",
%             ok = file:write(FD,Data)
%         end || {Count,M,FN} <- Sorted
%     ],
%     HTML2 = "</tr></table></body></html>",
%     ok = file:write(FD,HTML2),
%     ok = file:close(FD),
%     os:cmd("open nomatch_results.html").
