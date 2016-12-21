%%
%% rdpproxy
%% remote desktop proxy
%%
%% Copyright 2012-2015 Alex Wilson <alex@uq.edu.au>
%% The University of Queensland
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%% IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
%% NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%

-module(backend).
-behaviour(gen_fsm).

-include_lib("rdp_proto/include/rdp_server.hrl").

-export([start_link/3, probe/2]).
-export([initiation/2, proxy_intercept/2, proxy/2]).
-export([init/1, handle_info/3, terminate/3, code_change/4]).

-spec start_link(Frontend :: pid(), Address :: inet:ip_address() | inet:hostname(), Port :: inet:port_number()) -> {ok, pid()}.
start_link(Frontend, Address, Port) ->
    gen_fsm:start_link(?MODULE, [Frontend, Address, Port], []).

probe(Address, Port) ->
    case (catch gen_fsm:start(?MODULE, [self(), Address, Port], [{timeout, 5000}])) of
        {ok, Pid} ->
            MonRef = monitor(process, Pid),
            probe_rx(Pid, MonRef, {error, bad_host});
        {'EXIT', Reason} -> {error, Reason};
        Err -> Err
    end.

probe_rx(Pid, MonRef, RetVal) ->
    receive
        {backend_ready, Pid} ->
            gen_fsm:send_event(Pid, close),
            probe_rx(Pid, MonRef, ok);
        {'DOWN', MonRef, process, Pid, no_ssl} ->
            {error, no_ssl};
        {'DOWN', MonRef, process, Pid, _} ->
            RetVal
    after 10000 ->
        exit(Pid, kill),
        probe_rx(Pid, MonRef, {error, timeout})
    end.

-record(data, {addr, port, sock, sslsock=none, themref=0, usref=0, unused, server, origcr}).

%% @private
init([Pid, Address, Port]) when is_pid(Pid) ->
    init([Pid, Address, Port, #x224_cr{
        class = 0, dst = 0, src = crypto:rand_uniform(2000,9999)
        }]);

init([Srv = {P, _}, Address, Port]) when is_pid(P) ->
    #x224_state{cr = OrigCr} = rdp_server:x224_state(Srv),
    init([Srv, Address, Port, OrigCr]);

init([Srv, Address, Port, OrigCr]) ->
    error_logger:info_msg("connecting to ~p", [Address]),
    random:seed(os:timestamp()),
    #x224_cr{src = Us} = OrigCr,
    case gen_tcp:connect(Address, Port, [binary, {active, once}, {packet, raw}, {nodelay, true}], 2000) of
        {ok, Sock} ->
            error_logger:info_msg("connected to ~p", [Address]),
            Cr = OrigCr#x224_cr{rdp_protocols = [ssl]},
            {ok, CrData} = x224:encode(Cr),
            {ok, Packet} = tpkt:encode(CrData),
            ok = gen_tcp:send(Sock, Packet),
            {ok, initiation, #data{addr = Address, port = Port, server = Srv, sock = Sock, usref = Us, origcr = OrigCr}};

        {error, Reason} ->
            error_logger:error_msg("failed to connect to ~p: ~p", [Address, Reason]),

            {stop, Reason}
    end.

initiation({pdu, #x224_cc{class = 0, dst = UsRef, rdp_status = error, rdp_error = ssl_not_allowed}},
        #data{addr = _Address, usref = UsRef} = Data) ->
    {stop, no_ssl, Data};

initiation({pdu, #x224_cc{class = 0, dst = UsRef, rdp_status = ok} = Pkt}, #data{usref = UsRef, sock = Sock, server = Srv, addr = Address} = Data) ->
    #x224_cc{src = ThemRef, rdp_selected = Selected} = Pkt,

    HasSsl = lists:member(ssl, Selected),

    if HasSsl ->
        inet:setopts(Sock, [{packet, raw}]),
        {ok, SslSock} = ssl:connect(Sock, [{verify, verify_none}]),
        ok = ssl:setopts(SslSock, [binary, {active, true}, {nodelay, true}]),

        error_logger:info_msg("Establishing SSL connection to ~p", [Address]),

        case Srv of
            P when is_pid(P) ->
                P ! {backend_ready, self()};
            {P,_} when is_pid(P) ->
                rdp_server:start_tls(Srv, [{certfile, "cert/server.crt"}, {keyfile, "cert/server.key"}], Pkt)
        end,

        {next_state, proxy_intercept, Data#data{sslsock = SslSock, themref = ThemRef}};
    true ->
        error_logger:warning_msg("Disconnecting from ~p as SSL is not enabled", [Address]),
        gen_tcp:close(Sock),
        {stop, no_ssl, Data}
    end.

proxy_intercept({data, Bin}, #data{server = Srv, origcr = OrigCr} = Data) ->
    case rdpp:decode_connseq(Bin) of
        {ok, {mcs_pdu, Cr = #mcs_cr{data = TsudsBin0}}, Rem} ->
            {ok, Tsuds0} = tsud:decode(TsudsBin0),
            TsudSvrCore0 = lists:keyfind(tsud_svr_core, 1, Tsuds0),
            TsudSvrCore1 = TsudSvrCore0#tsud_svr_core{requested = OrigCr#x224_cr.rdp_protocols},
            Tsuds1 = lists:keyreplace(tsud_svr_core, 1, Tsuds0, TsudSvrCore1),
            TsudsBin1 = lists:foldl(fun(Tsud, SoFar) ->
                {ok, TsudBin} = tsud:encode(Tsud),
                <<SoFar/binary, TsudBin/binary>>
            end, <<>>, Tsuds1),
            {ok, OutCrData} = mcsgcc:encode_cr(Cr#mcs_cr{data = TsudsBin1}),
            {ok, OutDtData} = x224:encode(#x224_dt{data = OutCrData}),
            {ok, OutPkt} = tpkt:encode(OutDtData),
            rdp_server:send_raw(Srv, <<OutPkt/binary, Rem/binary>>),
            {next_state, proxy, Data};
        _ ->
            rdp_server:send_raw(Srv, Bin),
            {next_state, proxy_intercept, Data}
    end;

proxy_intercept({frontend_data, Bin}, #data{sock = Sock, sslsock = SslSock} = Data) ->
    if SslSock =:= none ->
        ok = gen_tcp:send(Sock, Bin);
    true ->
        ok = ssl:send(SslSock, Bin)
    end,
    {next_state, proxy_intercept, Data};

proxy_intercept(close, Data) ->
    {stop, normal, Data}.

proxy({data, Bin}, #data{server = Srv} = Data) ->
    rdp_server:send_raw(Srv, Bin),
    {next_state, proxy, Data};

proxy({frontend_data, Bin}, #data{sock = Sock, sslsock = SslSock} = Data) ->
    if SslSock =:= none ->
        ok = gen_tcp:send(Sock, Bin);
    true ->
        ok = ssl:send(SslSock, Bin)
    end,
    {next_state, proxy, Data};

proxy(close, Data) ->
    {stop, normal, Data}.

%% @private
handle_info({tcp, Sock, Bin}, State, #data{sslsock = SslSock, sock = Sock} = Data) ->
    case rdpp:decode_connseq(Bin) of
        {ok, {x224_pdu, Pdu}, Rem} ->
            case byte_size(Rem) of
                N when N > 0 -> self() ! {ssl, SslSock, Rem};
                _ -> ok
            end,
            ?MODULE:State({pdu, Pdu}, Data);
        _Other ->
            {next_state, State, Data}
    end;

handle_info({ssl, SslSock, Bin}, State, #data{sslsock = SslSock} = Data)
        when (State =:= proxy) orelse (State =:= proxy_intercept) ->
    ?MODULE:State({data, Bin}, Data);
handle_info({ssl, SslSock, Bin}, State, #data{sock = Sock, sslsock = SslSock} = Data) ->
    handle_info({tcp, Sock, Bin}, State, Data);

handle_info({ssl_closed, SslSock}, _State, #data{sslsock = SslSock} = Data) ->
    {stop, normal, Data};

handle_info({tcp_closed, Sock}, _State, #data{sock = Sock} = Data) ->
    {stop, normal, Data};

handle_info(_Msg, State, Data) ->
    {next_state, State, Data}.

%% @private
terminate(_Reason, _State, #data{sslsock = none, sock = Sock}) ->
    gen_tcp:close(Sock);
terminate(_Reason, _State, #data{sslsock = SslSock, sock = Sock}) ->
    ssl:close(SslSock),
    gen_tcp:close(Sock).

%% @private
% default handler
code_change(_OldVsn, State, _Data, _Extra) ->
    {ok, State}.
