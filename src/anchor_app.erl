-module(anchor_app).

-export([
    start/0
]).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

%% public
start() ->
    ensure:all_started(anchor).

%% application callbacks
start(_StartType, _StartArgs) ->
    anchor_sup:start_link().

stop(_State) ->
    ok.
