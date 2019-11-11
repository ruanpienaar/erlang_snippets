-module(transient_worker_example_sup).

-export([start_link/0]).

-behaviour(supervisor).
-export([
    init/1,
    start_child/1,
    stop_child/1
]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

init({}) ->
    Child1 =
        #{
        id => transient_worker_example,
        start => {transient_worker_example, start_link, []},
        shutdown => 5000,
        restart => transient,
        type => worker
    },
    Children = [Child1],
    RestartStrategy = {simple_one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.

start_child(Name) ->
    supervisor:start_child(?MODULE, [Name]).

%% @doc ...
%%  ...If the supervisor is simple_one_for_one, Id must be the pid() of the child process. ...
%%  Also, if simple_one_for_one, then we don't need to delete_child, seems that being done for us...
%% @end
stop_child(Pid) when is_pid(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).