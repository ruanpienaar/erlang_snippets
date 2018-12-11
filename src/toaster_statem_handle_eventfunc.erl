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
    plug_out_cable_sync/0,
    add_bread/0, add_bread/1,
    remove_bread/0
]).

%% Experiments:
-export([
    red_button/0,
    red_button_sync/0,
    start_link_annoyer/0
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

plug_out_cable_sync() ->
    gen_statem:call(?MODULE, plug_out).

%% insert slices
add_bread() ->
    add_bread(2).
    
add_bread(X) ->
    gen_statem:cast(?MODULE, {add_bread, X}).

%% Remove the bread slices
remove_bread() ->
    gen_statem:cast(?MODULE, remove_bread).

%% @doc Check what happens with the ActionList when using keep_state_and_data.
%% TODO : Postpone all events when in broken state
%% 
%% @end

red_button() ->
    gen_statem:cast(?MODULE, red_button).

red_button_sync() ->
    gen_statem:call(?MODULE, red_button).

%% @doc Start annoyer - the really annoying process that keeps sending you 
%%      messages.
%% @end
start_link_annoyer() ->
    gen_statem:cast(?MODULE, start_link_annoyer).

%% -----------------------------------

init(Data) ->
    process_flag(trap_exit, true),
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
%% i use keep_state_and_data.

%%------------------------------------------------------------------------ 
%% plug out
handle_event(cast, plug_out, initial_state, _Data) ->
    io:format("S: ~p Uhm, not plugged in...~n",[initial_state]),
    keep_state_and_data;
handle_event({call, From}, plug_out, initial_state, Data) ->
    {keep_state_and_data, [{reply, From, ok}]};
handle_event(cast, plug_out, _OtherState, Data) ->
    {next_state, initial_state, Data};
handle_event({call, From}, plug_out, _OtherState, Data) ->
    % Rather than doing :
    % ok = gen_statem:reply({reply, From, ok}),
    % {next_state, initial_state, Data};
    % Changed to:
    {next_state, initial_state, Data, [{reply, From, ok}]};

%%------------------------------------------------------------------------
%% plug_in

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

%%------------------------------------------------------------------------
%% stop toasting

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

%%------------------------------------------------------------------------
%% Add bread

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
    keep_state_and_data;
handle_event({call, From}, {add_bread, _X}, OtherState, _Data) ->
    io:format("S: ~p Uhm, cannot add bread...~n",[OtherState]),
    ok = gen_statem:reply({reply, From, ok}),
    keep_state_and_data;

%%------------------------------------------------------------------------
%% Remove bread

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
    keep_state_and_data;
handle_event({call, From}, remove_bread, OtherState, _Data) ->
    io:format("S: ~p Uhm, cannot remove bread...~n",[OtherState]),
    ok = gen_statem:reply({reply, From, ok}),
    keep_state_and_data;

%%------------------------------------------------------------------------
%% experiments

% red button
handle_event(cast, red_button, OtherState, Data) ->
    io:format("~p S: ~p Uhm, RED BUTTON PRESSED !!!!!!...~n",[get_time(), OtherState]),
    timer:sleep(5000),
    {keep_state_and_data, [
        {next_event, internal, {going_to_break_toaster}},
        {next_event, internal, {break_toaster}}
    ]};
handle_event({call, From}, red_button, OtherState, Data) ->
    {keep_state_and_data, [
        {next_event, internal, {going_to_break_toaster}},
        {next_event, internal, {break_toaster}},
        {reply, From, broken}
    ]};
handle_event(internal, {going_to_break_toaster}, OtherState, Data) ->
    timer:sleep(10000),
    io:format("~p S: ~p Uhm, going to break toaster...~n",[get_time(), OtherState]),
    keep_state_and_data;
handle_event(internal, {break_toaster}, OtherState, Data) ->
    timer:sleep(10000),
    io:format("~p S: ~p Uhm, toaster broken...~n",[get_time(), OtherState]),
    {next_state, broken, Data};

% annoyer
handle_event(cast, start_link_annoyer, OtherState, Data) ->    
    Pid = spawn_link(fun annoyer/0),
    true = erlang:register(annoyer, Pid),
    keep_state_and_data; %% Strange, could not reply Pid as value 
% handle_event({call, From}, stop_annoyer, OtherState, #{ annoyer_pid := Pid } = Data) ->
%     {keep_state, Data#{ annoyer_pid => undefined }};
handle_event(info, poke, OtherState, Data) ->
    {keep_state_and_data, [
        {next_event, internal, {cringe_at_poke}}
    ]};
handle_event(internal, {cringe_at_poke}, OtherState, Data) ->

    %% OLD: was just a 1000 sleep ( results: caused a backlog of messages in the queue, not even 
    % handling gen_call's  )
    %% effectively cause a queue, send them at 250ms, and handle them at 1000ms....
    %% This is to test, whether internal next_events jump the queue
    timer:sleep(1000),

    io:format("~p S: ~p Uhm, Cringe - don't poke me...~n",[get_time(), OtherState]),
    keep_state_and_data;

%% {'EXIT', Pid ,killed}
handle_event(info, {'EXIT', Pid, Reason}, OtherState, Data) ->
    io:format("~p S: ~p Uhm, process ~p Died ~p...~n",[get_time(), OtherState, Pid, Reason]),
    keep_state_and_data.

%%------------------------------------------------------------------------
%% Terminate/3

terminate(Reason, State, Data) ->
    io:format("~p terminate(~p, ~p, ~p)~n",
        [?MODULE, Reason, State, Data]),
    ok.

%%------------------------------------------------------------------------
%% The annoyer !

annoyer() ->
    receive
        A ->
            annoyer()
    after 
        250 ->
            ?MODULE ! poke, %% TODO: prob add some timestamp, to indicate order in observer
            annoyer()
    end.

get_time() ->
    Now = {_, _, Micro} = os:timestamp(),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(Now),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~p",
                  [Year, Month, Day, Hour, Minute, Second, Micro])).

        % % observe
        % observer:start().

        % % start gen_statem 
        % {ok, _} = toaster_statem_handle_eventfunc:start_link().

        % % start the sending of messages to toaster
        % toaster_statem_handle_eventfunc:start_link_annoyer().

        % % Check msg queue in observer

        % % place the cast call in the message_q
        % ok = toaster_statem_handle_eventfunc:red_button().

        % wait a bit

        % % kill anoying pid
        % erlang:exit(whereis(annoyer), kill).

        % check msg queue in observer

        % notice the below Ex:

% "2018-12-11T17:25:23.659978" S: initial_state Uhm, Cringe - don't poke me...
% "2018-12-11T17:25:24.661035" S: initial_state Uhm, Cringe - don't poke me...
% "2018-12-11T17:25:25.662241" S: initial_state Uhm, Cringe - don't poke me...
% "2018-12-11T17:25:26.663015" S: initial_state Uhm, Cringe - don't poke me...
% "2018-12-11T17:25:27.664080" S: initial_state Uhm, Cringe - don't poke me...
% "2018-12-11T17:25:28.665032" S: initial_state Uhm, Cringe - don't poke me...
% "2018-12-11T17:25:28.665235" S: initial_state Uhm, RED BUTTON PRESSED !!!!!!...
% "2018-12-11T17:25:43.667151" S: initial_state Uhm, going to break toaster...
% "2018-12-11T17:25:53.668254" S: initial_state Uhm, toaster broken...
% "2018-12-11T17:25:54.669087" S: broken Uhm, Cringe - don't poke me...
% "2018-12-11T17:25:55.670116" S: broken Uhm, Cringe - don't poke me...
% "2018-12-11T17:25:56.670985" S: broken Uhm, Cringe - don't poke me...
% "2018-12-11T17:25:57.672050" S: broken Uhm, Cringe - don't poke me...
% "2018-12-11T17:25:58.673100" S: broken Uhm, Cringe - don't poke me...
% "2018-12-11T17:25:59.674163" S: broken Uhm, Cringe - don't poke me...
% "2018-12-11T17:26:00.674955" S: broken Uhm, Cringe - don't poke me...
% "2018-12-11T17:26:01.676044" S: broken Uhm, Cringe - don't poke me...




