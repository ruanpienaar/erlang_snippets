-module(mnesia_frag_walk).

%% erl -sname mnesia_node -setcookie mnesia_node -mnesia dir '"Mnesia"'
%% l(mnesia_frag_walk).
%% mnesia_frag_walk:start().
%% mnesia:info().
%% dbg:tracer().
%% dbg:p(all, call).
%% dbg:tpl(mnesia_frag, cx).
%% read_all().
%% dbg:stop_clear().

-export([
	start/0,
	read_all/0
]).

-record(?MODULE, {
	key,
	value
}).

start() ->
    stopped = mnesia:stop(),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {atomic, ok} = create_table(),
    F = fun() ->
    	[ ok = mnesia:write(#?MODULE{ key = X, value = ok}) || X <- lists:seq(1, 32) ]
    end,
    mnesia:activity(transaction, F, _Args=[], mnesia_frag).

create_table() ->
	mnesia:create_table(
		?MODULE, 
		[{type, ordered_set},
		 {record_name, ?MODULE},
		 {attributes, record_info(fields, ?MODULE)},
		 {frag_properties, 
			[{node_pool, [node()]}, {n_fragments, 10}]
		 }
		]
	).

read_all() ->
    F = fun() ->
        mnesia:first(?MODULE)
    end,
    read_all(
         mnesia:activity(transaction, F, _Args=[], mnesia_frag)
    ).

read_all('$end_of_table') ->
    ok;
read_all(Key) ->
    io:format("~p~n", [Key]),
    F = fun() ->
        mnesia:next(?MODULE, Key)
    end,
    read_all(
         mnesia:activity(transaction, F, _Args=[], mnesia_frag)
    ).
