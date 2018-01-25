-module(erlang_decompile).
-export([
    transform/2
]).

transform(BeamFName, ErlFName) ->
  case beam_lib:chunks(BeamFName, [abstract_code]) of
    {ok, {_, [{abstract_code, {raw_abstract_v1,Forms}}]}} ->
      Src = erl_prettypr:format(erl_syntax:form_list(tl(Forms))),
      {ok, Fd} = file:open(ErlFName, [write]),
      io:fwrite(Fd, "~s~n", [Src]),
      file:close(Fd);
    Error ->
      Error
  end.