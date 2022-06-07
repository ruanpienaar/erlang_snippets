%% @doc
%%   Super simple client ( that may crash at multiple points ) for sending data on a loop
%% @end
-module(tcpip_v4_sender).

-export([
    run_loop/0
]).

run_loop() ->
    run_loop("localhost", 8800).

run_loop(Host, Port) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [inet, binary, {packet, 2}]),
    run_loop(Host, Port, Sock).
    
run_loop(Host, Port, Sock) ->
    timer:sleep(500),
    io:format("."),
    ok = gen_tcp:send(Sock, integer_to_binary(erlang:unique_integer())),
    run_loop(Host, Port, Sock).