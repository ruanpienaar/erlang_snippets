-module(blank_postpone).

-export([
    start_link/0,
    transition_to_postpone_state/0,
    send_something/1,
    transition_to_recv_state/0
]).

-behaviour(gen_statem).

-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, {}, []).

transition_to_postpone_state() ->
    gen_statem:call(?MODULE, ?FUNCTION_NAME).

send_something(Something) ->
    gen_statem:call(?MODULE, Something).

transition_to_recv_state() ->
    gen_statem:call(?MODULE, ?FUNCTION_NAME).

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    handle_event_function.

init({}) ->
    {ok, recv_state, #{}, 0}.

handle_event({call, From}, transition_to_postpone_state, recv_state, Data) ->
    {next_state, postpone_state, Data, [{reply, From, ok}, 5000]};
%%
%% return back to recv state
%%
handle_event({call, From}, transition_to_recv_state, postpone_state, Data) ->
    {next_state, recv_state, Data, [{reply, From, ok}]};
%%
%% Postpone all calls when in postpost_state
%%
handle_event({call, From}, Msg, postpone_state, _Data) ->
    erlang:display({call, postponed, Msg}),
    {keep_state_and_data, [
        postpone,
        {reply, From, ok}
    ]};
handle_event({call, From}, Msg, State, Data) ->
    erlang:display({unknown_call, Msg}),
    {next_state, State, Data, [{reply, From, ok}]};
handle_event(cast, Msg, State, Data) ->
    erlang:display({unknown_cast, Msg}),
    {next_state, State, Data, []};
handle_event(info, Msg, State, Data) ->
    erlang:display({unknown_info, Msg}),
    {next_state, State, Data, []};
handle_event(timeout, _, _State, _Data) ->
    {keep_state_and_data, [
        {next_event, internal, dowork}
    ]};
handle_event(internal, _Msg, recv_state, _Data) ->
    erlang:display({doing_the_work}),
    {keep_state_and_data, [5000]};
%%
%% postpone all work requests when in postpone_state
%%
handle_event(internal, Msg, postpone_state, _Data) ->
    erlang:display({internal, postponed, Msg}),
    {keep_state_and_data, [
        postpone,
        5000
    ]};
handle_event(E, Msg, State, Data) ->
    erlang:display({E, Msg, State, Data}),
    keep_state_and_data.

terminate(_Reason, _StateName, _State) ->
    void.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
