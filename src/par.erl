-module(par).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {from}).

start_link() ->
    gen_server:start_link({local, par1}, ?MODULE, {}, []),
    gen_server:start_link({local, par2}, ?MODULE, {}, []).

init({}) ->
    {ok, #state{}}.

handle_call(reply, From, State) ->
    gen_server:reply(From, {reply,ok,State}),
    {reply,ok,State};
handle_call(noreply, _From, State) ->
    {noreply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.