#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

-export([main/1]).

-mode(compile).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

main(_) ->
    {ok, Path} = file:get_cwd(),
    true = code:add_path(Path ++ "/ebin"),
    do_analyze().

do_analyze() ->
    io:format("Performing cover analysis...\n", []),
    %% figure out what coverdata we have
    %%CoverDir = cover_dir(),
    CoverFiles = ["ct.coverdata", "eunit.coverdata"], % get_all_coverdata(CoverDir),
    %% start the cover server if necessary
    {ok, CoverPid} = start_cover(),
    %% redirect cover output
    true = redirect_cover_output(CoverPid),
    %% analyze!
    case analyze(CoverFiles) of
        [] -> 
            ok;
        Analysis ->
            % print_analysis(Analysis, verbose()),
            write_index(Analysis)
            % maybe_fail_coverage(Analysis)
    end.

analyze([]) ->
    io:format("No coverdata found", []),
    [];
analyze(CoverFiles) ->
    %% reset any existing cover data
    ok = cover:reset(),
    %% import all coverdata files
    ok = lists:foreach(fun(M) -> import(M) end, CoverFiles),
    [{"aggregate", CoverFiles, analysis("aggregate")}] ++ analyze(CoverFiles, []).

analyze([], Acc) -> 
    lists:reverse(Acc);
analyze([F|Rest], Acc) ->
    %% reset any existing cover data
    ok = cover:reset(),
    %% extract taskname from the CoverData file
    Task = filename:basename(F, ".coverdata"),
    %% import task cover data and process it
    ok = import(F),
    analyze(Rest, [{Task, [F], analysis(Task)}] ++ Acc).

import(CoverData) ->
    case cover:import(CoverData) of
        {error, {cant_open_file, F, _Reason}} ->
            io:format(standard_io, "Can't import cover data from ~ts.\n", [F]),
            error;
        ok -> ok
    end.

analysis(Task) ->
    %% OldPath = code:get_path(),
    ok = restore_cover_paths(),
    Mods = cover:imported_modules(),
    Analysis = lists:map(fun(Mod) ->
                  {ok, Answer} = cover:analyze(Mod, coverage, line),
                  {ok, File} = analyze_to_file(Mod, Task),
                  {Mod, process(Answer), File}
              end,
              Mods),
    % true = rebar_utils:cleanup_code_path(OldPath),
    Analysis.

restore_cover_paths() ->
    ok.

analyze_to_file(Mod, Task) ->
    CoverDir = cover_dir(),
    TaskDir = filename:join([CoverDir, Task]),
    ok = filelib:ensure_dir(filename:join([TaskDir, "dummy.html"])),
    case code:ensure_loaded(Mod) of
        {module, _} ->
            write_file(Mod, mod_to_filename(TaskDir, Mod));
        {error, _}  ->
            io:format("Can't load module ~ts\n.", [Mod]),
            {ok, []}
    end.

write_file(Mod, FileName) ->
    case cover:analyze_to_file(Mod, FileName, [html]) of
        {ok, File} -> {ok, File};
        {error, Reason} ->
            io:format("Couldn't write annotated file for module ~p for reason ~p", [Mod, Reason]),
            {ok, []}
    end.

mod_to_filename(TaskDir, M) ->
    filename:join([TaskDir, atom_to_list(M) ++ ".html"]).

process(Coverage) -> process(Coverage, {0, 0}).

process([], Acc) -> Acc;
%% line 0 is a line added by eunit and never executed so ignore it
process([{{_, 0}, _}|Rest], Acc) -> process(Rest, Acc);
process([{_, {Cov, Not}}|Rest], {Covered, NotCovered}) ->
    process(Rest, {Covered + Cov, NotCovered + Not}).

calculate_total_string(Stats) ->
    integer_to_list(calculate_total(Stats))++"%".

calculate_total(Stats) ->
    percentage(lists:foldl(
        fun({_Mod, {Cov, Not}, _File}, {CovAcc, NotAcc}) ->
            {CovAcc + Cov, NotAcc + Not}
        end,
        {0, 0},
        Stats
    )).

percentage_string(Data) -> integer_to_list(percentage(Data))++"%".

percentage({_, 0}) -> 100;
percentage({Cov, Not}) -> trunc((Cov / (Cov + Not)) * 100).

write_index(Coverage) ->
    CoverDir = cover_dir(),
    FileName = filename:join([CoverDir, "index.html"]),
    {ok, F} = file:open(FileName, [write]),
    ok = file:write(F, "<!DOCTYPE HTML><html>\n"
                    "<head><meta charset=\"utf-8\">"
                    "<title>Coverage Summary</title></head>\n"
                    "<body>\n"),
    {Aggregate, Rest} = lists:partition(fun({"aggregate", _, _}) -> true; (_) -> false end,
                                        Coverage),
    ok = write_index_section(F, Aggregate),
    ok = write_index_section(F, Rest),
    ok = file:write(F, "</body></html>"),
    ok = file:close(F),
    io:format("  cover summary written to: ~ts~n", [filename:absname(FileName)]).

write_index_section(_F, []) -> ok;
write_index_section(F, [{Section, DataFile, Mods}|Rest]) ->
    %% Write the report
    ok = file:write(F, ?FMT("<h1>~ts summary</h1>\n", [Section])),
    ok = file:write(F, "coverage calculated from:\n<ul>"),
    ok = lists:foreach(fun(D) -> ok = file:write(F, io_lib:format("<li>~ts</li>", [D])) end,
                       DataFile),
    ok = file:write(F, "</ul>\n"),
    ok = file:write(F, "<table><tr><th>module</th><th>coverage %</th></tr>\n"),
    FmtLink =
        fun({Mod, Cov, Report}) ->
                ?FMT("<tr><td><a href='~ts'>~ts</a></td><td>~ts</td>\n",
                     [strip_coverdir(Report), Mod, percentage_string(Cov)])
        end,
    lists:foreach(fun(M) -> ok = file:write(F, FmtLink(M)) end, Mods),
    ok = file:write(F, ?FMT("<tr><td><strong>Total</strong></td><td>~ts</td>\n",
                     [calculate_total_string(Mods)])),
    ok = file:write(F, "</table>\n"),
    write_index_section(F, Rest).

%% fix for r15b which doesn't put the correct path in the `source` section
%%  of `module_info(compile)`
strip_coverdir([]) -> 
    "";
strip_coverdir(File) ->
    filename:join(lists:reverse(lists:sublist(lists:reverse(filename:split(File)), 2))).

start_cover() ->
    case cover:start() of
        {ok, Pid}                       -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
    end.

redirect_cover_output(CoverPid) ->
    %% redirect cover console output to file
    DataDir = cover_dir(),
    ok = filelib:ensure_dir(filename:join([DataDir, "dummy.log"])),
    {ok, F} = file:open(filename:join([DataDir, "cover.log"]),
                        [append]),
    group_leader(F, CoverPid).

cover_dir() ->
    {ok, Path} = file:get_cwd(),
    string:join([Path, "cover"], "/").

