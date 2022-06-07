-module(statem_send_nei_before_transtition).

-export([
    start_link/0
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
    initial_state/3,
    other_state/3
]).

-spec start_link() ->
            {ok, Pid :: pid()} |
            ignore |
            {error, Error :: term()}.
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, {}, []).

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_functions, state_enter].

-spec init(Args :: term()) ->
          gen_statem:init_result(term()).
init({}) ->
    {ok, initial_state, _Data=#{}}.

initial_state(enter, _, _) ->
    keep_state_and_data;
initial_state(info, _Msg, State) ->
    {next_state, other_state, State, [{next_event, internal, xxx}]}.

other_state(internal, xxx, _) ->
    io:format("next event internal\n", []),
    keep_state_and_data;
other_state(enter, _, _Data) ->
    io:format("Enter state\n", []),
    keep_state_and_data.

terminate(_Reason, _StateName, _State) ->
    void.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
