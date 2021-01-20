-module(splitting).

-export([
    bench_binary_split/0,
    bench_function_split/0,
    binary_split/1,
    function_split/1,

    bench_parsed_messages_split/0,
    bench_parsed_messages_other/0,
    parse_messages_split/1,
    parse_messages_other/1
]).

-define(TIMES, 1000000). % Million

%% Seperator being "#"

bench_binary_split() ->
    L = lists:map(fun(_) -> <<"string#other">> end, lists:seq(1, ?TIMES)),
    timer:tc(fun() -> do_bench_binary_split(L) end).

do_bench_binary_split([]) ->
    ok;
do_bench_binary_split([H|T]) ->
    _ = binary_split(H),
    do_bench_binary_split(T).

bench_function_split() ->
    L = lists:map(fun(_) -> <<"string#other">> end, lists:seq(1, ?TIMES)),
    timer:tc(fun() -> do_bench_function_split(L) end).

do_bench_function_split([]) ->
    ok;
do_bench_function_split([H|T]) ->
    _ = function_split(H),
    do_bench_function_split(T).

binary_split(BinStr) ->
    [_|[Rest]] = binary:split(BinStr, <<"#">>),
    Rest.

function_split(<<"string#", Rest/binary>>) ->
    Rest.



bench_parsed_messages_split() ->
    Bs = <<"string\r\n\r\n">>,
    FullString = lists:foldl(fun(_, Acc) ->
        <<Acc/binary, Bs/binary>>
    end, <<>>, lists:seq(1, ?TIMES)),
    timer:tc(fun() ->
        parse_messages_split(FullString)
    end).

bench_parsed_messages_other() ->
    Bs = <<"string\r\n\r\n">>,
    FullString = lists:foldl(fun(_, Acc) ->
        <<Acc/binary, Bs/binary>>
    end, <<>>, lists:seq(1, ?TIMES)),
    timer:tc(fun() ->
        parse_messages_other(FullString)
    end).

parse_messages_split(Data) ->
    case binary:split(Data, <<"\r\n\r\n">>, [global]) of
        Partial when is_binary(Partial) ->
            Partial;
        Msgs when is_list(Msgs) ->
            Msgs
    end.

parse_messages_other(Msgs) ->
    case byte_size(Msgs) >= 4 of
        true ->
            case keep_reading(Msgs, <<>>) of
                {<<>>, Rest} ->
                    [<<>>, Rest];
                {Msg, Rest} ->
                    [ Msg | parse_messages_other(Rest) ]
            end;
        false ->
            [Msgs]
    end.

keep_reading(<<>>, Return) ->
    Return;
keep_reading(<<H:1/binary, T/binary>>, Return) ->
    case {H, binary:part(T, {0, 3})} of
        {<<13>>, <<10, 13, 10>>} ->
            {Return, binary:part(T, {3, byte_size(T)-3})};
        _ ->
            keep_reading(T, <<Return/binary, H/binary>>)
    end.
