#!/usr/bin/env escript
%%! -smp +pc unicode
-mode(compile).

main(_) ->
    do_read([]).

do_read(StdIoList) ->
  %% 1) case io:fread(standard_io, "", "~s") of
  case io:get_line(standard_io, "") of
      eof ->
          io:format("Done!\n", []);
      %% 1) {ok, [Data]} ->
      Data ->
          Data2 = lists:append(StdIoList, Data),
	  % Data2Dot = Data2 ++ ".",
          Data2Dot = Data2 ++ [46], % 46 == "."
          io:format("~p\n", [Data2Dot]),
          try
            % io:format("Data   ~p\n", [Data2]),
            {ok, Tokens, _} = erl_scan:string(Data2Dot),
            % io:format("tokens ~p\n", [Tokens]),
            {ok, Parsed} = erl_parse:parse_exprs(Tokens),
            {value, Result, _} = erl_eval:exprs(Parsed, []),
            TermResult = binary_to_term(Result),
            io:format("parsed ~p\n", [Result]),
            io:format("term   ~p\n", [TermResult])
          catch
            C:E:Stack ->
              io:format("~p ~p ~p", [C, E, Stack]),
              io:format("~p ~p\n", [C, E]),
              do_read(Data2)
          end
      %%E ->
      %%    io:format("Data ~p\n", [E])
  end.