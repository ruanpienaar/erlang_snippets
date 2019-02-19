#!/usr/bin/env escript 

main([]) ->
    io:format(standard_io, "echo", []);
main(["binary_term", String]) ->
    io:format(standard_io, "~p\n", [term_to_binary(String)]).