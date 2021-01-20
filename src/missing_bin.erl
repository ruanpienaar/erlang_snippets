-module(missing_bin).

-export([
    a/1
]).

a(X) ->
   <<"test", X/binary>>.