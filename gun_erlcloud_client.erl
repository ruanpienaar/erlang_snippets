-module(gun_erlcloud_client).

-include_lib("kernel/include/logger.hrl").

-export([
    request/6
    % ,
    % test/0
]).

-ignore_xref([{request, 6}]).

request(URL, Method, Headers, Body, Timeout, _Config) ->
    % ?LOG_NOTICE(#{
    %     url => URL,
    %     method => Method,
    %     headers => Headers,
    %     % body => Body,
    %     timeout => Timeout,
    %     config => Config
    % }),
    % ?LOG_CRITICAL(#{ going_to_call_model => {timeout, Timeout}}),
    {response, {Status, RespHeaders, Data, TimesMap}} =
            holster:simple_proc_req_timed(
                Method,
                URL,
                #{ transport => tls },
                Headers,
                Timeout,
                Body
            ),
    %% TODO: should be metrics,
    %%       see if we can pass this outside of erlcloud_lambda
    #{
        before_opening_micsec := A1,
        after_opening_micsec := B1,
        recv_req_micsec := A2,
        recv_final_response := B2
    } = TimesMap,
    ?LOG_CRITICAL(#{
        opening => B1-A1,
        recv_resp => B2-A2
    }),
    RespHeadersString = header_str(RespHeaders),
    {ok, {{Status, undefined}, RespHeadersString, Data}}.

    % {ok, {{Status, undefined}, HdrsStr, Body}}
    % {error, {hackney_error, Reason}};

header_str(Headers) ->
    [{string:to_lower(to_list_string(K)), to_list_string(V)} || {K, V} <- Headers].

to_list_string(Val) when erlang:is_binary(Val) ->
    erlang:binary_to_list(Val);
to_list_string(Val) when erlang:is_list(Val) ->
    Val.