-module(toaster_fsm).

%% i left a lot of the extra functions catching wrong state 
%% transitions, just to illustrate the code footprint differences 
%% between gen_fsm, gen_statem(state-functions) and 
%% gen_statem(handle-event-functions)

%% And also, let's assume that the person will not try to add 
%% more bread than what's allowed in the toaster...

%% Events API
-export([
    start_link/0,
    plug_in/0, plug_in_sync/0,
    toast/0, toast_sync/0,
    stop_toasting/0, stop_toasting_sync/0
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
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, #{ slices => 0 }, []).
    
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

%% insert slices
add_bread(X) ->
    gen_fsm:sync_send_event(?MODULE, {add_bread, X}).

%% Remove the bread slices
remove_bread() ->
    gen_fsm:sync_send_event(?MODULE, remove_bread).

%%------------------------------------------------------------

init(State) ->
    {ok, initial_state, State}.

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
    io:format("S: ~p Uhm, not toasting...~n",[powered]),
    {next_state, powered, State}.

%% Chose not to use erlang:start_timer for the wrong state calls below, 
%% but rather just reseting the timer.
toasting(plug_in, State) ->
    io:format("S: ~p Uhm, already plugged in...~n",[toasting]),
    {next_state, toasting, State, 5000};
toasting(toast, State) ->
    io:format("S: ~p Uhm, already toasting...~n",[toasting]),
    {next_state, toasting, State, 5000};
toasting(stop_toasting, State) ->
    io:format("Stopped toasting...~n"),
    {next_state, powered, State};
toasting(timeout, #{ slices := TS } = State) ->
    io:format("Finished toasting ~p slices...~n", [TS]),
    {next_state, powered, State}.

%% Sync Events
initial_state(plug_in, _From, State) ->
    {reply, ok, powered, State};
initial_state({add_bread, X}, _From, State) ->
    {reply, ok, initial_state, State#{ slices => X }};
initial_state(remove_bread, _From, State) ->
    {reply, ok, initial_state,  State#{ slices := 0 }};
initial_state(toast, _From, State) ->
    io:format("S: ~p Uhm, NOT plugged in...~n",[initial_state]),
    {reply, ok, initial_state, State};
initial_state(stop_toasting, _From, State) ->
    io:format("S: ~p Uhm, NOT plugged in...~n",[initial_state]),
    {reply, ok, initial_state, State}.
    
powered(plug_in, _From, State) ->
    io:format("S: ~p Uhm, already plugged in...~n",[powered]),
    {reply, ok, powered, State};
powered({add_bread, X}, _From, State) ->
    {reply, ok, powered, State#{ slices => X }};
powered(remove_bread, _From, State) ->
    {reply, ok, powered, State#{ slices := 0 }};
powered(toast, _From, State) ->
    io:format("Going to toast...~n"),
    {reply, ok, toasting, State, 5000};
powered(stop_toasting, _From, State) ->
    io:format("S: ~p Uhm, not toasting...~n",[powered]),
    {reply, ok, powered, State}.

%% Chose not to use erlang:start_timer for the wrong state calls below, 
%% but rather just reseting the timer.
toasting(plug_in, _From, State) ->
    io:format("S: ~p Uhm, already plugged in...~n",[toasting]),
    {reply, ok, toasting, State, 5000};
toasting({add_bread, _X}, _From, State) ->
    io:format("S: ~p Uhm, already toasting...~n",[toasting]),
    {reply, ok, toasting, State, 5000};
toasting(remove_bread, _From, State) ->
    io:format("S: ~p Uhm, already toasting...~n",[toasting]),
    {reply, ok, toasting, State, 5000};
toasting(toast, _From, State) ->
    io:format("S: ~p Uhm, already toasting...~n",[toasting]),
    {reply, ok, toasting, State, 5000};
toasting(stop_toasting, _From, State) ->
    {reply, ok, powered, State}.
    
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
    io:format("Terminate in ~p with Reason: ~p~n", [StateName, Reason]),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.