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
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(anchor)
    ]}}.
