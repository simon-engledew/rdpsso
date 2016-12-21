#!/usr/bin/env escript
%% -*- erlang -*-
%%! -config rdpsso.config -env ERL_LIBS .:./deps

main(_) ->
    [{ok, ok} = {application:load(X), application:start(X)} || X <- [sasl, crypto, asn1, public_key, ssl, syntax_tools, compiler, goldrush, lager, rdp_proto]],
    application:start(rdpsso),
    receive
        stop ->
            done
    end.
