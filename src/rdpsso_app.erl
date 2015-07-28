-module(rdpsso_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rdp_server_sup:start_link(3000, frontend).

stop(_State) ->
    ok.
