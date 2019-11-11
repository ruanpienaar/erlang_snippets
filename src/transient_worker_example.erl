-module(transient_worker_example).

-export([
    start_link/1,
    stop/1
]).

-behaviour(gen_statem).

%% gen_statem callbacks
-export([
    callback_mode/0,
    init/1,
    terminate/3,
    code_change/4
]).

%% State functions
-export([
    check/3,
    work/3
]).

-define(SERVER, ?MODULE).

%% ======================================================================
%% API

-spec start_link(Name :: atom()) ->
            {ok, Pid :: pid()} |
            ignore |
            {error, Error :: term()}.
start_link(Name) ->
    gen_statem:start_link({local, Name}, ?MODULE, {}, []).

-spec stop(atom()) -> ok.
stop(Name) ->
    gen_statem:cast(Name, stop).

%% ======================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    state_functions.

-spec init(Args :: term()) ->
          gen_statem:init_result(term()).
init({}) ->
    {ok, check, _State = #{}, [{next_event, internal, get_ready}]}.

-spec check('enter',
         OldState :: atom(),
         Data :: term()) ->
            gen_statem:state_enter_result('check');
        (gen_statem:event_type(),
         Msg :: term(),
         Data :: term()) ->
            gen_statem:event_handler_result(atom()).
check(internal, get_ready, State) ->
    can_start_work(State);
check({timeout, try_again}, retry, State) ->
    can_start_work(State);
check({call, From}, _Msg, State) ->
    {next_state, check, State, [{reply, From, ok}]};
check(cast, _Msg, State) ->
    {next_state, check, State};
check(info, _Msg, State) ->
    {next_state, check, State}.

-spec work('enter',
         OldState :: atom(),
         Data :: term()) ->
            gen_statem:state_enter_result('work');
        (gen_statem:event_type(),
         Msg :: term(),
         Data :: term()) ->
            gen_statem:event_handler_result(atom()).
work({call, From}, _Msg, State) ->
    {next_state, work, State, [{reply, From, ok}]};
work(cast, _Msg, State) ->
    {next_state, work, State};
work(info, _Msg, State) ->
    {next_state, work, State}.

terminate(_Reason, _StateName, _State) ->
    void.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ======================================================================
%% Internal

%% @doc some arbitrary function to show that it won't ALWAYS work
%% @end
can_start_work(State) ->
    io:format("CHECKING ~p!\n", [self()]),
    case calendar:datetime_to_gregorian_seconds( calendar:universal_time() ) rem 2 of
        0 ->
            {next_state, work, State};
        _ ->
            {keep_state_and_data, [{{timeout, try_again}, 900, retry}]}
    end.