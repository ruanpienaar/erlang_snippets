-module(toaster_statem_handle_eventfunc).

%% API
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

%% State M exports
-export([
    init/1, callback_mode/0, terminate/3
]).
%% State Machine 'handle_event_function'
-export([handle_event/4]).

%% optional_callbacks:
% format_status/2 -> Has got a default implementation
% terminate/3,    -> Has got a default implementation
% code_change/4,  -> Only needed by advanced soft upgrade
% state_name/3,   -> Example for callback_mode() =:= state_functions:
%                 there has to be a StateName/3 callback function
%                 for every StateName in your state machine but the state name
%                 'state_name' does of course not have to be used.
% handle_event/4 -> For callback_mode() =:= handle_event_function

-define(REG_NAME, {local, ?MODULE}).

%% -----------------------------------
%% API
%% Added some Sync/Call's as examples...

start_link() ->
    gen_statem:start_link(?REG_NAME, ?MODULE, #{ untoasted_slices => 0, toasted_slices => 0 }, []).

%% powered 
plug_in() ->
    gen_statem:cast(?MODULE, plug_in).

plug_in_sync() ->
    gen_statem:call(?MODULE, plug_in).
   
%% toasting
toast() ->
    gen_statem:cast(?MODULE, toast).

toast_sync() ->
    gen_statem:call(?MODULE, toast).
    
%% powered 
stop_toasting() ->
    gen_statem:cast(?MODULE, stop_toasting).

stop_toasting_sync() ->
    gen_statem:call(?MODULE, stop_toasting).

%% Remove the power cable
plug_out_cable() ->
    gen_statem:cast(?MODULE, plug_out).

%% insert slices
add_bread() ->
    add_bread(2).
    
add_bread(X) ->
    gen_statem:cast(?MODULE, {add_bread, X}).

%% Remove the bread slices
remove_bread() ->
    gen_statem:cast(?MODULE, remove_bread).

%% -----------------------------------

init(Data) ->
    {ok, initial_state, Data}.

%% 'state_functions' | 'handle_event_function'
callback_mode() ->
    'handle_event_function'.

%% {next_state, NextState, NewData} % {next_state,NextState,NewData,[]} |
%% {next_state, NextState, NewData, ActionsList}
%% {keep_state, NewData} % {keep_state, NewData, []} |
%% {keep_state, NewData, ActionsList} |
%% keep_state_and_data % {keep_state_and_data,[]} |
%% {keep_state_and_data, ActionsList} |
%% {repeat_state, NewData} % {repeat_state,NewData,[]} |
%% {repeat_state, NewData, ActionsList} |
%% repeat_state_and_data % {repeat_state_and_data,[]} |
%% {repeat_state_and_data, ActionsList} |
%% stop % {stop,normal} |
%% {stop, Reason} |
%% {stop, Reason, NewData} |
%% {stop_and_reply, Reason, Replies} |
%% {stop_and_reply, Reason, Replies, NewData}.

%% kept the redundant 'wrong' state check functions, so that i 
%% i use 'keep_state_and_data'.

handle_event(cast, plug_out, initial_state, _Data) ->
    'keep_state_and_data';
handle_event({call, From}, plug_out, initial_state, _Data) ->
    ok = gen_statem:reply({reply, From, ok}),
    'keep_state_and_data';
handle_event(cast, plug_out, _OtherState, Data) ->
    {next_state, initial_state, Data};
handle_event({call, From}, plug_out, _OtherState, Data) ->
    ok = gen_statem:reply({reply, From, ok}),
    {next_state, initial_state, Data};

handle_event(cast, plug_in, initial_state, Data) ->
    {next_state, powered, Data};
handle_event({call, From}, plug_in, initial_state, Data) ->
    ok = gen_statem:reply({reply, From, ok}),
    {next_state, powered, Data};
handle_event(cast, plug_in, OtherState, _Data) ->
    io:format("S: ~p Uhm, already plugged in...~n",[OtherState]),
    keep_state_and_data;
handle_event({call, From}, plug_in, OtherState, _Data) ->
    io:format("S: ~p Uhm, already plugged in...~n",[OtherState]),
    ok = gen_statem:reply({reply, From, ok}),
    keep_state_and_data;

handle_event(cast, toast, powered, Data) ->
    {next_state, toasting, Data, [{state_timeout, 5000, done_toasting}]};
handle_event({call, From}, toast, powered, Data) ->
    ok = gen_statem:reply({reply, From, ok}),
    {next_state, toasting, Data, [{state_timeout, 5000, done_toasting}]};
handle_event(cast, toast, OtherState, _Data) ->
    io:format("S: ~p Uhm, cannot toast...~n",[OtherState]),
    keep_state_and_data;
handle_event({call, From}, toast, OtherState, _Data) ->
    io:format("S: ~p Uhm, cannot toast...~n",[OtherState]),
    ok = gen_statem:reply({reply, From, ok}),
    keep_state_and_data;

handle_event(cast, stop_toasting, toasting, Data) ->
    io:format("Stopped toasting...~n"),
    {next_state, powered, Data};
handle_event({call, From}, stop_toasting, toasting, Data) ->
    io:format("Stopped toasting...~n"),
    ok = gen_statem:reply({reply, From, ok}),
    {next_state, powered, Data};
handle_event(cast, stop_toasting, OtherState, _Data) ->
    io:format("S: ~p Uhm, cannot stop toasting...~n",[OtherState]),
    keep_state_and_data;
handle_event({call, From}, stop_toasting, OtherState, _Data) ->
    io:format("S: ~p Uhm, cannot stop toasting...~n",[OtherState]),
    ok = gen_statem:reply({reply, From, ok}),
    keep_state_and_data;

handle_event(cast, {add_bread, X}, OtherState, Data) 
        when OtherState == 'initial_state' orelse
             OtherState == 'powered' ->
    {keep_state, Data#{untoasted_slices => X}};
handle_event({call, From}, {add_bread, X}, OtherState, Data)
        when OtherState == 'initial_state' orelse
             OtherState == 'powered' ->
    NewData=Data#{untoasted_slices => X},
    ok = gen_statem:reply({reply, From, ok}),
    {keep_state, NewData};
handle_event(cast, {add_bread, _X}, OtherState, _Data) ->
    io:format("S: ~p Uhm, cannot add bread...~n",[OtherState]),
    'keep_state_and_data';
handle_event({call, From}, {add_bread, _X}, OtherState, _Data) ->
    io:format("S: ~p Uhm, cannot add bread...~n",[OtherState]),
    ok = gen_statem:reply({reply, From, ok}),
    'keep_state_and_data';

handle_event(cast, remove_bread, OtherState, Data) 
        when OtherState == 'initial_state' orelse
             OtherState == 'powered' ->
    {keep_state, Data#{untoasted_slices => 0}};
handle_event({call, From}, remove_bread, OtherState, Data)
        when OtherState == 'initial_state' orelse
             OtherState == 'powered' ->
    NewData=Data#{untoasted_slices => 0},
    ok = gen_statem:reply({reply, From, ok}),
    {keep_state, NewData};
handle_event(cast, remove_bread, OtherState, _Data) ->
    io:format("S: ~p Uhm, cannot remove bread...~n",[OtherState]),
    'keep_state_and_data';
handle_event({call, From}, remove_bread, OtherState, _Data) ->
    io:format("S: ~p Uhm, cannot remove bread...~n",[OtherState]),
    ok = gen_statem:reply({reply, From, ok}),
    'keep_state_and_data'.

terminate(Reason, State, Data) ->
    io:format("~p terminate(~p, ~p, ~p)~n",
        [?MODULE, Reason, State, Data]),
    ok.