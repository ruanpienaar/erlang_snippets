-module(sup_example).

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

%% @private
init({}) ->
    Child1 =
        {gen_server_example1,
            {gen_server_example, start_link, []},
            permanent, 5000, worker,
            [gen_server_example]},
    Child2 =
        {gen_server_example2,
            {gen_server_example, start_link, []},
            transient, 5000, worker,
            [gen_server_example]},
    Child3 =
        {gen_server_example3,
            {gen_server_example, start_link, []},
            temporary, 5000, worker,
            [gen_server_example]},

    Children = [Child1, Child2, Child3],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.