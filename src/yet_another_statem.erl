-module(yet_another_statem).

-export([
    start_link/0,
    nudge/0,
    move/0
]).

-behaviour(gen_statem).
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, {}, []).

nudge() ->
    gen_statem:cast(?MODULE, ?FUNCTION_NAME).

move() ->
    gen_statem:cast(?MODULE, ?FUNCTION_NAME).

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    handle_event_function.

%% @private
init({}) ->
    {ok, initial_state, undefined}.

handle_event({call, From}, _Msg, State, Data) ->
    {next_state, State, Data, [{reply,From,ok}]};

%% So 2 timeuots cause only the last one to actually send the timeout to the gen_statem
%% This:
%%
%% {timeout, 9000, has_to_happen},
%% {state_timeout, 10000, un_nudged_again}
%%
%% OR
%% {timeout, 9000, has_to_happen},
%% {timeout, 10000, un_nudged_again}
%%
%% Both will only showup the last un_nudged_again

handle_event(cast, nudge, _State, _Data) ->
    {keep_state_and_data, [
        {timeout, 9000, has_to_happen},
        {{timeout, nudge_timeout}, 10000, un_nudged_again}
    ]};
handle_event(cast, move, _State, Data) ->
    {next_state, moved, Data};
handle_event(cast, _Msg, State, Data) ->
    {next_state, State, Data, []};
handle_event(info, _Msg, State, Data) ->
    {next_state, State, Data, []};
handle_event(timeout, has_to_happen, _State, Data) ->
    erlang:display("Had to happen"),
    keep_state_and_data;
handle_event({timeout, nudge_timeout}, un_nudged_again, _State, Data) ->
    erlang:display("Nudge me again"),
    keep_state_and_data.

%% @private
terminate(_Reason, _StateName, _State) ->
    void.

%% @private
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
