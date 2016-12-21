-module(frontend).

-include_lib("rdp_proto/include/rdp_server.hrl").

-behaviour(rdp_server).

-include("session.hrl").

-export([
    init/1,
    handle_connect/4,
    init_ui/2,
    handle_event/3,
    handle_raw_data/3,
    terminate/2
]).

-record(state, {
    peer :: term(),
    backend :: pid(),
    intercept = true :: boolean(),
    session :: #session{} | undefined
  }).

%% @arg Peer  the peer address (IPv4 or IPv6) connecting
init(Peer) ->
    {ok, #state{peer = Peer}}.

handle_connect(_Cookie, _Protocols, Srv, S = #state{}) ->
    {credentials, {Address, Domain, Username, Password}} = lists:keyfind(credentials, 1, application:get_all_env(rdpsso)),

    error_logger:info_msg("new connection ~p\\~p@~p", [Domain, Username, Address]),

    Sess = #session{ host = Address, port = 3389, domain = Domain, user = Username, password = Password},
    {ok, Backend} = backend:start_link(Srv, binary_to_list(Sess#session.host), Sess#session.port),
    ok = rdp_server:watch_child(Srv, Backend),
    {accept_raw, S#state{session = Sess, backend = Backend}}.

init_ui(_Srv, S = #state{}) ->
    {ok, S}.

handle_event(_, _Srv, S = #state{}) ->
    {ok, S}.

handle_raw_data(Data, _Srv, S = #state{intercept = false, backend = B}) ->
    gen_fsm:send_event(B, {frontend_data, Data}),
    {ok, S};

handle_raw_data(Bin, _Srv, S = #state{intercept = true, backend = B}) ->
    case rdpp:decode_server(Bin) of
        {ok, {mcs_pdu, McsData = #mcs_data{data = RdpData0}}, Rem} ->
            case rdpp:decode_basic(RdpData0) of
                {ok, TsInfo0 = #ts_info{secflags = []}} ->
                    #state{session = #session{user = User, password = Password, domain = Domain}} = S,

                    error_logger:info_msg("new connection Username(~ts) Password(~ts)", [TsInfo0#ts_info.username, TsInfo0#ts_info.password]),

                    TsInfo1 = TsInfo0#ts_info{flags = [autologon, unicode | TsInfo0#ts_info.flags]},
                    Unicode = lists:member(unicode, TsInfo1#ts_info.flags),
                    TsInfo2 = TsInfo1#ts_info{
                        domain = if
                            Unicode -> unicode:characters_to_binary(<<Domain/binary,0>>, latin1, {utf16, little});
                            true -> <<Domain/binary, 0>> end,
                        username = if
                            Unicode -> unicode:characters_to_binary(<<User/binary,0>>, latin1, {utf16, little});
                            true -> <<User/binary, 0>> end,
                        password = if
                            Unicode -> unicode:characters_to_binary(<<Password/binary,0>>, latin1, {utf16, little});
                            true -> <<Password/binary, 0>> end
                        },

                    {ok, RdpData1} = rdpp:encode_basic(TsInfo2),
                    {ok, McsOutBin} = mcsgcc:encode_dpdu(McsData#mcs_data{data = RdpData1}),
                    {ok, X224OutBin} = x224:encode(#x224_dt{data = McsOutBin}),
                    {ok, OutBin} = tpkt:encode(X224OutBin),
                    gen_fsm:send_event(B, {frontend_data, <<OutBin/binary, Rem/binary>>}),
                    {ok, S#state{intercept = false}};

                 _ ->
                    gen_fsm:send_event(B, {frontend_data, Bin}),
                    {ok, S}

            end;

        _ ->
          gen_fsm:send_event(B, {frontend_data, Bin}),
          {ok, S}
    end.

terminate(_Reason, _) ->
    % any cleanup you need to do at exit
    ok.
