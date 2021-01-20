-module(supervisor_child_names).

%%
%% copied from erlang/21.3.8/lib/stdlib-3.8.2/src/supervisor.erl
%% This is defnied within supervisor.erl
%%

-record(child, {
    pid,
    id,
    mfargs,
    restart_type,
    shutdown,
    child_type,
    modules
}).

-record(state, {
    name,
    strategy,
    children,
    dynamics,
    intensity,
    period,
    restarts = [],
    dynamic_restarts = 0,
    module,
    args
}).
%%
%% end
%%

-export([
    start_link/0,
    add_child/1,
    get_pid_from_id/1
]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

%% @private
init({}) ->
    {ok,
        {
            % Restart strategy
            #{
                strategy  => one_for_one,
                intensity => 1,
                period    => 10
            },
            []
        }
    }.

add_child(Id) ->
    {ok, _} = supervisor:start_child(
        ?MODULE,
        #{
            id => Id,
            start => {supervised_child, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [supervised_child]
        }
    ).

% record definition in supervisor.erl :(
get_pid_from_id(Id) ->
    #state{ children = Children } = sys:get_state(?MODULE),
    {_, ChildrenMap} = Children,
    case maps:get(Id, ChildrenMap, undefined) of
        undefined ->
            undefined;
        #child{ pid = Pid } ->
            Pid
    end.
