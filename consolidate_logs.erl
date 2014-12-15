-module(consolidate_logs).
-export([
    run/0,
    run/1
    ]).

-define(E,entry).
-record(?E,{
    module_line,
    severity,
    count=1,
    message,
    file_name
}).

run() ->
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
    end,

    ?E = ets:new(?E, [set, named_table, {keypos,2}]),

    nomatch = ets:new(nomatch, [set, named_table]),
    true = ets:insert(nomatch,{count,0}),

    % Wombat's lager config.
    % Config=
    %     {formatter_config,
    %       [date, " ", time, " [", severity, "] ",
    %        pid, " ",
    %        {module,
    %         [ "[", module, {line, [":", line], ""}, "] " ],
    %         ""},
    %        message, "\n"]},
    DIR="/Users/ruanpienaar/Downloads/rel/wombat/wombat/log/wombat/",
    {ok,Files}=
        file:list_dir(DIR),
    [ run(DIR++F) || F <- Files ],

    to_html().

run(FileName) ->
    {ok,FD} = file:open(FileName,[read,raw,binary]),
    read_lines(FileName,FD).

read_lines(FN,FD) ->
    case file:read_line(FD) of
        eof ->
            ok;
        {ok,Data} ->
            case parse_line(Data) of
                {ok,LineList} ->
                    ok = store_line(LineList,FN),
                    read_lines(FN,FD);
                nomatch ->
                    store_nomatch(Data,FN),
                    read_lines(FN,FD)
            end
    end.


parse_line(Data) ->
    %% io:format("Parse Data:~p\n",[Data]),

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
    %% Squash data out of message, that won't be unique like, pid's
    %% timestamps, etc

    %% Make a skeleton of the message...
    %% so we can compare it.

    case ets:lookup(?E,ModAndLineNmr) of
        [] ->
            Rec = #?E{module_line = ModAndLineNmr,
                      severity = Severity,
                      message = Message,
                      file_name = FN},
            true = ets:insert_new(?E,Rec),
            ok;
        [_Rec] ->

            %% Also compare the skeleton message here

            _NewCount=
                ets:update_counter(?E, ModAndLineNmr,
                    {_Pos=4,_Incr=1}),
            ok
            %% store duplicate message
    end.

store_nomatch(Data,FN) ->
    NewCount=ets:update_counter(nomatch,count,1),
    ets:insert(nomatch,{NewCount,Data,FN}).


to_html() ->
    Entries = ets:tab2list(?E),
    Sorted = lists:reverse(lists:keysort(4, Entries)),
    file:delete("results.html"),
    {ok,FD} = file:open("results.html",[write]),
    HTML1 = "<html><head></head><body><table>",
    ok = file:write(FD,HTML1),
    [
        begin
            Data = "<tr><td>"++E#?E.module_line++"</td>
                <td>"++E#?E.severity++"
                <td>"++integer_to_list(E#?E.count)++"
                <td>"++E#?E.message++"
                <td>"++E#?E.file_name++"</td></tr>",
            ok = file:write(FD,Data)
        end || E <- Sorted
    ],
    HTML2 = "</table></body></html>",
    ok = file:write(FD,HTML2),
    ok = file:close(FD),
    timer:sleep(50),
    os:cmd("open results,html").


