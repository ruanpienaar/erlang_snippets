-module(toaster_fsm).

%% Events API
-export([
    start_link/0,
    plug_in/0,
    plug_in_sync/0,
    toast/0,
    toast_sync/0,
    stop_toasting/0,
    stop_toasting_sync/0
]).
%% Any State API
-export([
    plug_out_cable/0,
    add_bread/0, add_bread/1,
    remove_bread/0
]).

-behaviour(gen_fsm).
-export([init/1, handle_info/3, terminate/3, code_change/4, handle_event/3, handle_sync_event/4]).

%% States
-export([
    initial_state/2, initial_state/3,
    powered/2, powered/3,
    toasting/2, toasting/3
]).

%%------------------------------------------------------------

%% initial state
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, #{ untoasted_slices => 0, toasted_slices => 0 }, []).
    
%% powered 
plug_in() ->
    gen_fsm:send_event(?MODULE, plug_in).

plug_in_sync() ->
    gen_fsm:sync_send_event(?MODULE, plug_in).
   
%% toasting
toast() ->
    gen_fsm:send_event(?MODULE, toast).

toast_sync() ->
    gen_fsm:sync_send_event(?MODULE, toast).
    
%% powered 
stop_toasting() ->
    gen_fsm:send_event(?MODULE, stop_toasting).

stop_toasting_sync() ->
    gen_fsm:sync_send_event(?MODULE, stop_toasting).

%% Remove the power cable
plug_out_cable() ->
    gen_fsm:send_all_state_event(?MODULE, plug_out).

%% insert slices
add_bread() ->
    add_bread(2).
    
add_bread(X) ->
    gen_fsm:send_all_state_event(?MODULE, {add_bread, X}).

%% Remove the bread slices
remove_bread() ->
    gen_fsm:send_all_state_event(?MODULE, remove_bread).

%%------------------------------------------------------------

init(StateMap) ->
    {ok, initial_state, StateMap}.

%% Async events
initial_state(plug_in, State) ->
    {next_state, powered, State};
initial_state(toast, State) ->
    io:format("S: ~p Uhm, NOT plugged in...~n",[initial_state]),
    {next_state, initial_state, State};
initial_state(stop_toasting, State) ->
    io:format("S: ~p Uhm, NOT plugged in...~n",[initial_state]),
    {next_state, initial_state, State}.

powered(plug_in, State) ->
    io:format("S: ~p Uhm, already plugged in...~n",[powered]),
    {next_state, powered, State};
powered(toast, State) ->
    io:format("Going to toast...~n"),
    {next_state, toasting, State, 5000};
powered(stop_toasting, State) ->
    io:format("S: ~p Uhm, not toasting...~n",[toasting]),
    {next_state, powered, State}.

toasting(plug_in, State) ->
    io:format("S: ~p Uhm, already plugged in...~n",[toasting]),
    {next_state, toasting, State};
toasting(toast, State) ->
    io:format("S: ~p Uhm, already toasting...~n",[toasting]),
    {next_state, toasting, State};
toasting(stop_toasting, State) ->
    io:format("Stopped toasting...~n"),
    {next_state, powered, State};
toasting(timeout, State) ->
    io:format("Finished toasting...~n"),
    {next_state, powered, State}.

%% Sync Events
initial_state(plug_in, _From, State) ->
    {reply, ok, powered, State};
initial_state(toast, _From, State) ->
    io:format("S: ~p Uhm, NOT plugged in...~n",[initial_state]),
    {reply, ok, initial_state, State};
initial_state(stop_toasting, _From, State) ->
    io:format("S: ~p Uhm, NOT plugged in...~n",[initial_state]),
    {reply, ok, initial_state, State}.
    
powered(plug_in, _From, State) ->
    io:format("S: ~p Uhm, already plugged in...~n",[powered]),
    {reply, ok, powered, State};
powered(toast, _From, State) ->
    io:format("Going to toast...~n"),
    {reply, ok, toasting, State, 5000};
powered(stop_toasting, _From, State) ->
    io:format("S: ~p Uhm, not toasting...~n",[toasting]),
    {reply, ok, powered, State}.

toasting(plug_in, _From, State) ->
    io:format("S: ~p Uhm, already plugged in...~n",[powered]),
    {reply, ok, toasting, State};
toasting(toast, _From, State) ->
    io:format("S: ~p Uhm, already toasting...~n",[toasting]),
    {reply, ok, toasting, State};
toasting(stop_toasting, _From, State) ->
    {reply, ok, powered, State}.
    
%% cancel the toast timer
handle_event(plug_out, StateName, State) ->
    io:format("cable plugged out in ~p state~n", [StateName]),
    {next_state, initial_state, State}.

handle_sync_event(plug_out, _From, StateName, State) ->
    io:format("cable plugged out in ~p state~n", [StateName]),
    {reply, ok, initial_state, State}.

handle_info(Info, StateName, State) ->
    io:format("~p handle_info ~p in ~p ~n", [?MODULE, Info, StateName]),
    {next_state, StateName, State}.

terminate(Reason, StateName, _State) ->
    io:format("~p Terminate in ~p with Reason: ~p~n", [StateName, Reason]),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.