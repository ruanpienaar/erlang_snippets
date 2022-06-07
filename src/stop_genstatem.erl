-module(stop_genstatem).

-include_lib("kernel/include/logger.hrl").

-export([
    start_link/0,
    do_cast_stop/0,
    do_call_stop/0,
    do_call_stop_state_timeout/0
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

do_cast_stop() ->
    gen_statem:cast(?MODULE, ?FUNCTION_NAME).

do_call_stop() ->
    gen_statem:call(?MODULE, ?FUNCTION_NAME).

do_call_stop_state_timeout() ->
    try
        gen_statem:call(?MODULE, {?FUNCTION_NAME, erlang:system_time()})
    catch
        exit: {normal, {gen_statem, call, [stop_genstatem, {do_call_stop_state_timeout, _}, infinity]}}: _ ->
            ?LOG_NOTICE(#{ action => handled_statem_stopped_during_call })
    end.

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [handle_event_function].

init({}) ->
    {ok, initial_state, _Data=#{ started => erlang:system_time() }}.

handle_event(cast, do_cast_stop, initial_state, _Data) ->
    {stop, normal, #{}};
handle_event({call, _From}, do_call_stop, initial_state, _Data) ->
    {stop, normal, #{}};
handle_event({call, _From}, {do_call_stop_state_timeout, _SystemTime}, initial_state, Data) ->
    {keep_state, Data, [{state_timeout, 25, boom}]};
handle_event(state_timeout, boom, initial_state, _Data) ->
    stop.

terminate(Reason, StateName, Data) ->
    ?LOG_NOTICE(#{
        reason => Reason,
        state => StateName,
        data => Data
    }),
    void.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.