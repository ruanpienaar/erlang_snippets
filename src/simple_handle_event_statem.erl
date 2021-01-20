-module(simple_handle_event_statem).

-export([start_link/0]).

-behaviour(gen_statem).
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    handle_event_function.

init({}) ->
    {ok, initial_state, #{}}.

handle_event(info, go_to_next_state, initial_state, Data) ->
    {next_state, other_state, Data}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
