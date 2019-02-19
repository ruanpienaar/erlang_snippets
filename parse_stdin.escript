#!/usr/bin/env escript
%%! -smp +pc unicode
-mode(compile).

main(_) ->
  case io:fread(standard_io, "", "~s") of
      {ok, [Data]} ->
          io:format("Data   ~p\n", [Data]),
          {ok, Tokens, _} = erl_scan:string(Data ++ "."),
          io:format("tokens ~p\n", [Tokens]),
          {ok, Parsed} = erl_parse:parse_exprs(Tokens),
          {value, Result, _} = erl_eval:exprs(Parsed, []),
          TermResult = binary_to_term(Result),
          io:format("parsed ~p\n", [Result]),
          io:format("term   ~p\n", [TermResult]);
      E ->
          io:format("Data ~p\n", [E])
  end.
  