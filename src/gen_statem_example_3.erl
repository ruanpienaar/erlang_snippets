-module(gen_statem_example_3).

-export([
    start_link/0,
    transition/0
]).

-behaviour(gen_statem).
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, {}, []).

transition() ->
    gen_statem:call(?MODULE, ?FUNCTION_NAME).

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [handle_event_function, state_enter].

init({}) ->
    {ok, initial_state, #{}}.

handle_event(enter, initial_state, _, Data) ->
    io:format("Enter into initial_state\n", []),
    {keep_state, Data, {state_timeout, 10000, check_something}};
handle_event(state_timeout, check_something, initial_state, Data) ->
    io:format("Checking something\n", []),
    {keep_state, Data, {state_timeout, 5000, check_something}};
    % {next_state, initial_state, Data};
    % {keep_state, Data};
handle_event({call, From}, transition, initial_state, Data) ->
    {keep_state, Data, [{reply, From, ok}]}.

terminate(_Reason, _StateName, _State) ->
    void.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
