-module(toaster_statem_statefunc).

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
%% State Machine states
-export([
    initial_state/3,
    powered/3,
    toasting/3
]).
% gen_statem module            Callback module
% -----------------            ---------------
% gen_statem:start
% gen_statem:start_link -----> Module:init/1

% Server start or code change
%                       -----> Module:callback_mode/0

% gen_statem:stop       -----> Module:terminate/3

% gen_statem:call
% gen_statem:cast
% erlang:send
% erlang:'!'            -----> Module:StateName/3
%                              Module:handle_event/4

% -                     -----> Module:terminate/3

% -                     -----> Module:code_change/4

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
%% Added some Sync/Call's as an example...
start_link() ->
    gen_statem:start_link(?REG_NAME, ?MODULE, {}, []).

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

init({}) ->
    {ok, initial_state, _Data = #{}}.

%% state_functions | handle_event_function
callback_mode() ->
    'state_functions'.

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

%% Actions:
% https://github.com/erlang/otp/blob/master/lib/stdlib/src/gen_statem.erl#L113

initial_state(cast, plug_in, Data) ->
    {next_state, powered, Data};
initial_state({call, From}, plug_in, Data) ->
    ok = gen_statem:reply({reply, From, ok}),
    {next_state, powered, Data};
initial_state(cast, toast, Data) ->
    io:format("S: ~p Uhm, NOT plugged in...~n",[initial_state]),
    {next_state, initial_state, Data};
initial_state({call, From}, toast, Data) ->
    io:format("S: ~p Uhm, NOT plugged in...~n",[initial_state]),
    ok = gen_statem:reply({reply, From, ok}),
    {next_state, initial_state, Data};
initial_state(cast, stop_toasting, Data) ->
    io:format("S: ~p Uhm, NOT plugged in...~n",[initial_state]),
    {next_state, initial_state, Data};
initial_state({call, From}, stop_toasting, Data) ->
    io:format("S: ~p Uhm, NOT plugged in...~n",[initial_state]),
    ok = gen_statem:reply({reply, From, ok}),
    {next_state, initial_state, Data};
%% Leaving this here to explore...
initial_state(EventType, CurentState, Data) ->
    io:format("~p initial_state(~p, ~p, ~p)~n", 
        [?MODULE, EventType, CurentState, Data]),
    {next_state, initial_state, _NewData=Data}.

powered(cast, plug_in, Data) ->
    io:format("S: ~p Uhm, already plugged in...~n",[powered]),
    {next_state, powered, Data};
powered({call, From}, plug_in, Data) ->
    io:format("S: ~p Uhm, already plugged in...~n",[powered]),
    ok = gen_statem:reply({reply, From, ok}),
    {next_state, powered, Data};
powered(cast, toast, Data) ->
    {next_state, toasting, Data, [{state_timeout, 5000, done_toasting}]};
powered({call, From}, toast, Data) ->
    ok = gen_statem:reply({reply, From, ok}),
    {next_state, toasting, Data, [{state_timeout, 5000, done_toasting}]};
powered(cast, stop_toasting, Data) ->
    {next_state, powered, Data};
powered({call, From}, stop_toasting, Data) ->
    ok = gen_statem:reply({reply, From, ok}),
    {next_state, powered, Data};
%% Leaving this here to explore...
powered(EventType, CurentState, Data) ->
    io:format("~p powered(~p, ~p, ~p)~n", 
        [?MODULE, EventType, CurentState, Data]),
    {next_state, powered, _NewData=Data}.

toasting(cast, plug_in, Data) ->
    io:format("S: ~p Uhm, already plugged in...~n",[toasting]),
    {next_state, toasting, Data};
toasting({call, From}, plug_in, Data) ->
    io:format("S: ~p Uhm, already plugged in...~n",[toasting]),
    ok = gen_statem:reply({reply, From, ok}),
    {next_state, toasting, Data};
toasting(cast, toast, Data) ->
    io:format("S: ~p Uhm, already toasting...~n",[toasting]),
    {next_state, toasting, Data};
toasting({call, From}, toast, Data) ->
    io:format("S: ~p Uhm, already toasting...~n",[toasting]),
    ok = gen_statem:reply({reply, From, ok}),
    {next_state, toasting, Data};
toasting(cast, stop_toasting, Data) ->
    {next_state, powered, Data};
toasting({call, From}, stop_toasting, Data) ->
    ok = gen_statem:reply({reply, From, ok}),
    {next_state, powered, Data};
toasting(state_timeout, done_toasting, Data) ->
    io:format("Finished toasting...~n"),
    {next_state, powered, Data};
%% Leaving this here to explore...
toasting(EventType, CurentState, Data) ->
    io:format("~p toasting(~p, ~p, ~p)~n", 
        [?MODULE, EventType, CurentState, Data]),
    {next_state, toasting, _NewData=Data}.

%% Leaving this here to explore...
terminate(Reason, State, Data) ->
    io:format("~p terminate(~p, ~p, ~p)~n",
        [?MODULE, Reason, State, Data]),
    ok.