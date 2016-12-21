FROM alpine:latest

RUN apk --no-cache --repository http://dl-3.alpinelinux.org/alpine/edge/testing/ add \
        git \
        g++ \
        openssl \
        erlang \
        erlang-asn1 \
        erlang-crypto \
        erlang-compiler \
        erlang-erl-interface \
        erlang-ssl \
        erlang-syntax-tools \
        erlang-kernel \
        erlang-parsetools \
        erlang-dev \
        erlang-sasl \
        erlang-public-key \
    ;

COPY bin/* /usr/bin/

WORKDIR /source

RUN mkdir cert \
    && openssl genrsa -out cert/server.key 2048 \
    && openssl req -new -key cert/server.key -out cert/server.csr -subj "/CN=rdpsso" \
    && openssl x509 -req -sha256 -days 365 -in cert/server.csr -signkey cert/server.key -out cert/server.crt \
    ;

COPY rebar.config ./

RUN rebar get-deps compile

COPY run.es ./
COPY src ./src

RUN rebar compile

COPY rdpsso.config ./

EXPOSE 3389

ENTRYPOINT ["dumb-init", "escript", "run.es"]
