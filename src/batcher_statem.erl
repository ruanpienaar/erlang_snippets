%% @doc
%%
%% 2 states ( batching / processing )
%%
%% Queue data, and process queue items when either MaxSize reached,
%% or when in queue longer than allowed max_timer_linger_ms
%%
%% Considerations:
%% - would never want to lose Queue contents
%%   ( some other proc has to create+own ETS table)
%%
%% @end


%% TODO
%% start timer somewhere
%%

%% batcher_statem:start_link({local, test}, #{ queue_ets_table => queue, max_queue_size_trigger => 10, max_timer_linger_ms => 5000, processing_fn => fun(_L) -> ok end, terminate_fn => fun(_L) -> ok end }).

-module(batcher_statem).

-export([
    start_link/2,
    put/2
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
    batching/3,
    processing/3
]).

-type via() :: {'via', Module :: module(), Name :: term()}.
%% Copied as not exported. (OTP/stdlib/gen.erl)
-type emgr_name()  :: {'local', atom()}
                    | {'global', term()}
                    | via().
-type opts() :: #{
    queue_ets_table := atom(),
    max_queue_size_trigger := pos_integer(),
    batch_sizes := pos_integer(),
    max_timer_linger_ms := pos_integer(),
    processing_fn := fun(), %% should return ok.
    terminate_fn := fun() %% Should return ok.
}.

%%-----------------------------------------------------------------------------

-spec start_link(emgr_name(), opts()) ->
            {ok, Pid :: pid()} |
            ignore |
            {error, Error :: term()}.
start_link(ServerName, Opts) ->

    %% TODO: check for table named and public and ordered_set
    process_flag(trap_exit, true),
    gen_statem:start_link(ServerName, ?MODULE, {Opts}, []).

-spec put(atom() | via() | term(), term()) -> ok.
put(ServerName, Data) ->
    gen_statem:cast(ServerName, {?FUNCTION_NAME, Data}).

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_functions, state_enter].

%%-----------------------------------------------------------------------------

-spec init(Args :: term()) ->
          gen_statem:init_result(term()).
init({Opts}) ->
    {ok, batching, Opts}.

batching(enter, _PrevState, #{ max_timer_linger_ms := Ms }) ->
    {keep_state_and_data, {state_timeout, Ms, process_queue}};
batching(state_timeout, process_queue,
    #{
        queue_ets_table := Table,
        max_timer_linger_ms := Ms
    } = Data) ->
    case ets:info(Table, size) of
        0 ->
            {keep_state_and_data, {state_timeout, Ms, process_queue}};
        _ ->
            {next_state, processing, Data}
    end;
batching(cast, {put, PutData},
    #{
        queue_ets_table := Table,
        max_queue_size_trigger := MaxSize,
        max_timer_linger_ms := Ms
    } = Data) ->
    case put_and_check_queue_filled_up(Table, PutData, MaxSize) of
        true ->
            {next_state, processing, Data};
        false ->
            {keep_state_and_data, {state_timeout, Ms, process_queue}}
    end.

processing(enter, _, Data) ->
    {keep_state, Data, {timeout, 0, process_queue_event}};
processing(timeout, process_queue_event,
    #{
        queue_ets_table := Table,
        processing_fn := Fn
    }) ->
    SmPid = self(),
    _Pid = erlang:spawn_link(fun() ->
        Queue = ets:tab2list(Table),
        ok = Fn(Queue), % process queue
        ok = clear_queue(Table, Queue),
        % io:format("processing complete...\n"),
        SmPid ! xxx
    end),
    keep_state_and_data;
processing(info, _,
    #{
        queue_ets_table := Table,
        max_queue_size_trigger := MaxSize
    } = Data) ->
    case check_queue_filled_up(Table, MaxSize) of
        true ->
            repeat_state_and_data;
        false ->
            {next_state, batching, Data}
    end;
processing(cast, {put, PutData}, #{ queue_ets_table := Table } = _Data) ->
    true = put_in_queue(Table, PutData),
    keep_state_and_data.

terminate(_Reason, _StateName,
    #{
        queue_ets_table := Table,
        terminate_fn := TFun,
        batch_sizes := BatchSize
    }) ->
    ok = TFun(get_batch_items(Table, BatchSize)),
    void.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

put_and_check_queue_filled_up(Table, Data, MaxSize) ->
    true = put_in_queue(Table, Data),
    check_queue_filled_up(Table, MaxSize).

put_in_queue(Table, Data) ->
    true = ets:insert(Table, {erlang:unique_integer([monotonic]), Data}).

check_queue_filled_up(Table, MaxSize) ->
    ets:info(Table, size) >= MaxSize.

get_batch_items(T, BatchSize) ->
    ets:tab2list(T).

clear_queue(T, Queue) ->
    lists:foreach(fun(E) -> ets:delete(T, get_key(E)) end, Queue).

get_key(E) ->
    element(1, E).
