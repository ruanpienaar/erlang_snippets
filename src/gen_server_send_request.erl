-module(gen_server_send_request).

-export([
    start_link/0,
    send_request/0
]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

send_request() ->
    gen_server:send_request(?MODULE, do_work).

init({}) ->
    {ok, undefined}.

handle_call(_Request, _From, State) ->
    timer:sleep(2000),
    erlang:exit(1),
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.