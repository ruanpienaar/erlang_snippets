-module(exit_in_catch).

-export([test/0]).

-export([start_link/0]).

-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

test() ->
    gen_server:cast(?MODULE, bla).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
    {ok, undefined}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    try
        doit()
    catch
        C:E:S ->
            {C,E,S}
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

doit() ->
    spawn_link(fun() -> bla end).