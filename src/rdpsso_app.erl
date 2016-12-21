-module(rdpsso_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

ensure_file(Path) ->
    case filelib:is_regular(Path) of
        false -> erlang:error({notfound, Path});
        _ -> {ok}
    end.

start(_StartType, _StartArgs) ->
    ensure_file("cert/server.key"),
    ensure_file("cert/server.crt"),
    rdp_server_sup:start_link(3389, frontend).

stop(_State) ->
    ok.
