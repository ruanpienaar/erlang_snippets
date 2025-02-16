-module(gen_statem_timers).

-export([
    start_link/0,
    state_timeout/0,
    state_timeout_cleared_after_transition/0,
    new_state_then_timeout/0,
    new_timeout_then_state_change/0,
    new_gen_timeout_then_state_change/0,
    stop/0
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

%% 1
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, {}, []).

%% 2 ( state timeout example )
state_timeout() ->
    ok = gen_statem:call(?MODULE, {state_timeout}).

%% 3 ( example of state_timeout removed when transitioning to new state )
state_timeout_cleared_after_transition() ->
    ok = gen_statem:call(?MODULE, {state_timeout}),
    ok = gen_statem:call(?MODULE, {new_state}).

%% 4 ( example of how a timer still ticks even when transitioning to new states )
new_state_then_timeout() ->
    ok = gen_statem:call(?MODULE, {new_state}),
    ok = gen_statem:call(?MODULE, {new_timeout}).

%% 5 ( timeout DOES NOT survive after state transition ) ! NON generic timeout
%%   you'll notice this timeout never shows up
new_timeout_then_state_change() ->
    ok = gen_statem:call(?MODULE, {new_timeout}),
    ok = gen_statem:call(?MODULE, {new_state}).

%% 6 ( timeout DOES survive after state transition )
new_gen_timeout_then_state_change() ->
    ok = gen_statem:call(?MODULE, {gen_new_timeout}),
    ok = gen_statem:call(?MODULE, {new_state}).

stop() ->
    erlang:exit(whereis(?MODULE), kill).

callback_mode() ->
    [
        handle_event_function,
        state_enter
    ].

init({}) ->
    {ok, initial_state, _Data=#{}, [{state_timeout, timer:seconds(60), event_content}]}.

%% @doc
%% 1) When starting up ( old and new states both initial_state )
%% @end
handle_event(enter, initial_state, initial_state, _Data) ->
    keep_state_and_data;
%% @doc
%%  (3) When moving into new_state
%% @end
handle_event(enter, initial_state, new_state, _Data) ->
    ?LOG_NOTICE(#{
        info => transitioned_to_new_state,
        line => ?LINE,
        prev_state => initial_state,
        state => new_state
    }),
    keep_state_and_data;
%% @doc
%%  (2) state_timeout Example (Start timer)
%% @end
handle_event({call, From}, {state_timeout}, State, Data) ->
    {
        next_state,
        State,
        Data,
        [
            {state_timeout, timer:seconds(60), event_content},
            {reply, From, ok}
        ]
    };
%% @doc
%%  (3)(5) state timeout Example ( what happens to the timer when transitioning states )
%% @end
handle_event({call, From}, {same_state}, _State, Data) ->
    {keep_state, Data, [{reply, From, ok}]};
handle_event({call, From}, {new_state}, _State, Data) ->
    {next_state, new_state, Data, [{reply, From, ok}]};
%% @doc
%%  (4)(5) setting a timer within the statem.
%% @end
handle_event({call, From}, {new_timeout}, State, _Data) ->
    ?LOG_NOTICE(#{
        event => set_new_timeout,
        state => State
    }),
    {keep_state_and_data, [
        {reply, From, ok},
        {timeout, 5000, event_content}
    ]};
%% @doc
%%  (6) Setting of generic timeout
%% @end
handle_event({call, From}, {gen_new_timeout}, State, _Data) ->
    ?LOG_NOTICE(#{
        event => set_new_named_generic_timeout,
        state => State
    }),
    {keep_state_and_data, [
        {reply, From, ok},
        {{timeout, generic_timeout}, 5000, event_content}
    ]};
%% @doc
%%  (2) state_timeout Example (timer ran out)
%% @end
handle_event(M = state_timeout, Ec = event_content, S, _Data) ->
    ?LOG_NOTICE(#{
        info => {state_timeout, Ec, S},
        line => ?LINE,
        m => M,
        ec => Ec,
        state => S
    }),
    keep_state_and_data;
%% @doc
%%  (4) Receiving the timer timeout ( even after changing states )
%% @end
handle_event(timeout, event_content, State, _Data) ->
    ?LOG_NOTICE(#{
        event => timeout,
        state => State
    }),
    keep_state_and_data;
%% @doc
%% (6) timer still triggered
%% @end
handle_event({timeout, generic_timeout}, event_content, State, Data) ->
    ?LOG_NOTICE(#{
        event => generic_named_timeout,
        state => State
    }),
    keep_state_and_data;
handle_event(T = timeout, Msg, State, Data) ->
    ?LOG_WARNING("LINE ~p, Unhandled ~p ~p", [?LINE, T, Msg]),
    {next_state, State, Data, []};
handle_event(T = internal, Msg, State, Data) ->
    ?LOG_WARNING("LINE ~p, Unhandled ~p ~p", [?LINE, T, Msg]),
    {next_state, State, Data, []};
handle_event(T = cast, Msg, State, Data) ->
    ?LOG_WARNING("LINE ~p, Unhandled ~p ~p", [?LINE, T, Msg]),
    {next_state, State, Data, []};
handle_event(T = info, Msg, State, Data) ->
    ?LOG_WARNING("LINE ~p, Unhandled ~p ~p", [?LINE, T, Msg]),
    {next_state, State, Data, []}.

terminate(_Reason, _StateName, _State) ->
    void.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
