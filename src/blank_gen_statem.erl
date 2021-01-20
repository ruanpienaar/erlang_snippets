-module(blank_gen_statem).

-export([start_link/0]).

-behaviour(gen_statem).
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([
    handle_event/4,
    stop/0,
    stop/1
]).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, {}, []).

stop() ->
    whereis(?MODULE) ! stop.

stop(Reason) ->
    whereis(?MODULE) ! {stop, Reason}.

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    handle_event_function.

init({}) ->
    {ok, initial_state, undefined}.

handle_event({call, From}, _Msg, State, Data) ->
    {next_state, State, Data, [{reply, From, ok}]};
handle_event(cast, _Msg, State, Data) ->
    {next_state, State, Data, []};
handle_event(info, stop, _State, _Data) ->
    stop;
handle_event(info, {stop, Reason}, _State, _Data) ->
    {stop, Reason};
handle_event(info, _Msg, State, Data) ->
    {next_state, State, Data, []}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
