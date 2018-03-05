FROM alpine:latest
RUN apk --update add make git wget erlang erlang-ssl erlang-public-key erlang-asn1 erlang-crypto erlang-debugger erlang-dev erlang-dialyzer erlang-edoc erlang-erl-docgen erlang-erl-interface erlang-erts erlang-et erlang-eunit erlang-hipe erlang-ic erlang-inets erlang-runtime-tools erlang-sasl erlang-syntax-tools erlang-tools erlang-wx && rm -rf /var/cache/apk/*
CMD epmd -daemon && \
    sleep 1 && \
    epmd -names && \
    erl -sname node -setcookie node
EXPOSE 4369
