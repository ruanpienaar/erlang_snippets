-module(ets_select_perf).

-export([
    get_q1/1,
    get_q1/2
]).

get_q1(Partition) ->
    Rows = ets:tab2list(kafork_ets:shard(batch_q, Partition)),
    [Msg || {_TimeStamp, Msg} <- Rows].

get_q2(Partition) ->
    ets:select(
        kafork_ets:shard(batch_q, Partition),
        ets:fun2ms(fun({_, Msg}) -> Msg end)
    ).
