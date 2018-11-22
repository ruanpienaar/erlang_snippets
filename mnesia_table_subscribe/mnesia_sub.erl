-module(mnesia_sub).
-export([
    start_system_subs/2,
    start_activity_subs/2,
    start_tables_subs/3,
    stop/0,
    worker_loop/2
]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_system_subs(RegName, RemoteNode) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {system, {RegName, RemoteNode}}, []).

start_activity_subs(RegName, RemoteNode) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {activity, {RegName, RemoteNode}}, []).

start_tables_subs(RegName, RemoteNode, Tables) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {tables, {RegName, RemoteNode, Tables}}, []).

stop() ->
    erlang:exit(whereis(?MODULE), kill).

init({system, {RegName, RemoteNode}}) ->
    P = spawn_link(fun() ->
        worker({system, {RegName, RemoteNode}})
    end),
    {ok, #{
        reg_name => RegName,
        remote_node => RemoteNode,
        workers => P
    }};
init({activity, {RegName, RemoteNode}}) ->
    P = spawn_link(fun() ->
        worker({activity, {RegName, RemoteNode}})
    end),
    {ok, #{
        reg_name => RegName,
        remote_node => RemoteNode,
        workers => P
    }};
init({tables, {RegName, RemoteNode, Tables}}) ->
    Workers = 
        lists:map(fun(Tbl) ->
            spawn_link(fun() ->
                worker({table, {RegName, RemoteNode, Tbl}})
            end)
        end, Tables),
    {ok, #{
        reg_name => RegName,
        remote_node => RemoteNode,
        tables => Tables,
        workers => Workers
    }}.

handle_call(Request, _From, #{ reg_name := RegName, remote_node := RemoteNode } = State) ->
    {RegName, RemoteNode} ! {node(), ?MODULE, handle_call, Request},
    {reply, ok, State}.

handle_cast(Msg, #{ reg_name := RegName, remote_node := RemoteNode } = State) ->
    {RegName, RemoteNode} ! {node(), ?MODULE, handle_cast, Msg},
    {noreply, State}.

handle_info(Info, #{ reg_name := RegName, remote_node := RemoteNode } = State) ->
    {RegName, RemoteNode} ! {node(), ?MODULE, handle_info, Info},
    {noreply, State}.

%% TODO: unsubscribe
terminate(_Reason, #{ reg_name := RegName, remote_node := RemoteNode } = _State) ->
    {RegName, RemoteNode} ! {node(), ?MODULE, terminate},
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

worker({system, {RegName, RemoteNode}}) ->
    {ok, _} = mnesia:subscribe(system),
    worker_loop(RegName, RemoteNode);
worker({activity, {RegName, RemoteNode}}) ->
    {ok, _} = mnesia:subscribe(activity),
    worker_loop(RegName, RemoteNode);
worker({table, {RegName, RemoteNode, Tbl}}) ->
    {ok, _} = mnesia:subscribe({table, Tbl, detailed}),
    worker_loop(RegName, RemoteNode).

worker_loop(RegName, RemoteNode) ->
    receive
        X ->
            {RegName, RemoteNode} ! {node(), X},
            worker_loop(RegName, RemoteNode)
    end.
