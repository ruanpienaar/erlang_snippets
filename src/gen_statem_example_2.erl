-module(gen_statem_example_2).

-export([
    callback_mode/0,
    start_link/0,
    init/1,
    state1/3
]).

callback_mode() ->
    [state_functions, state_enter].

start_link() ->
    gen_statem:start_link(?MODULE, {}, []).

init({}) ->
    process_flag(trap_exit, true),
    {ok, state1, #{}}.

state1(A, B, State) ->
    io:format("~p ~p\n", [A, B]),
    {keep_state_and_data,
        {state_timeout, 5000, process}
    }.

% state1(state_timeout, process, _State) ->
