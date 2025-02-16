-module(ets_select_delete).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([do/0]).

do() ->
    tbl = ets:new(tbl, [named_table, public]),
    true = ets:insert(tbl, {1, #{ a => 1, b => 2, c => 3 }}),
    ets:select_delete(
        tbl,
        ets:fun2ms(
            fun({_, #{ a => X }}) when X > 0 ->
                true
            end
        )
    ).