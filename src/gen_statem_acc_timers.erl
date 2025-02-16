-module(gen_statem_acc_timers).

-export([
    start_link/0,
    add_timer/1,
    state_timer/1
]).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_statem).

-export([
    callback_mode/0,
    init/1,
    terminate/3,
    code_change/4
]).

-export([
    handle_event/4
]).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, {}, []).

add_timer(X) ->
    gen_statem:cast(?MODULE, {?FUNCTION_NAME, X}).

state_timer(X) ->
    gen_statem:cast(?MODULE, {?FUNCTION_NAME, X}).

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [handle_event_function].

init({}) ->
    {ok, initial_state, _Data=#{}}.

handle_event(T=timeout, Msg, State, Data) ->
    ?LOG_WARNING("Unhandled ~p ~p", [T, Msg]),
    {next_state, State, Data, []};
handle_event(T=internal, Msg, State, Data) ->
    ?LOG_WARNING("Unhandled ~p ~p", [T, Msg]),
    {next_state, State, Data, []};
handle_event({call, From}, _Msg, State, Data) ->
    {next_state, State, Data, [{reply,From,ok}]};
handle_event(cast, {add_timer, _} = Msg, State, Data) ->
    {
        keep_state,
        Data,
        [
            {{timeout, Msg}, timer:seconds(10), suspend_all}
        ]
    };
handle_event(cast, {state_timer, _} = Msg, State, Data) ->
    {
        keep_state,
        Data,
        [
            {state_timeout, timer:seconds(10), Msg}
        ]
    };
handle_event(info, _Msg, State, Data) ->
    {next_state, State, Data, []};
handle_event({timeout, X}, suspend_all, initial_state, _Data) ->
    io:format("S. ~p\n", [X]),
    keep_state_and_data;
handle_event(state_timeout, Msg, initial_state, _Data) ->
    io:format("S2 ~p\n", [Msg]),
    keep_state_and_data.

terminate(_Reason, _StateName, _State) ->
    void.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.