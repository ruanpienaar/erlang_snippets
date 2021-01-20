-module(list_play).

-export([
    bench_split/1
]).

%%
%% this tests time taken to split the list on the last element
%%

bench_split(C) ->
    L = lists:seq(1, C),
    lists:foldl(fun(I, Acc) ->
        LongerList = list_entry(I),
        Len = length(LongerList),
        {T, _} = timer:tc(fun() ->
            lists:split(Len - 1, LongerList)
        end),
        [{I, T}|Acc]
    end, 0, L).

list_entry(Times) ->
    list_entry(Times, []).

list_entry(Times, R) when Times =< 0 ->
    R;
list_entry(Times, R) ->
    list_entry(Times-1, ["string"|R]).
