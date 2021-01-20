-module(ignore_sup).

-export([
    start_link/0,
    add_child/0
]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

%% @private
init({}) ->
    Child1 =
        {ignore_proc,
            {ignore_proc, start_link, []},
            permanent, 5000, worker,
            [ignore_proc]},
    Children = [Child1],
    {ok,
        {
            {simple_one_for_one, 5, 10},
            Children
        }
    }.

add_child() ->
    supervisor:start_child(?MODULE, []).
