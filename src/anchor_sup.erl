-module(anchor_sup).
-include("anchor.hrl").

-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).

%% public
-spec start_link() -> {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init([]) ->
    anchor_backlog:init(),
    anchor_queue:init(),

    {ok, {{one_for_one, 5, 10},
        anchor_utils:child_specs()
    }}.
