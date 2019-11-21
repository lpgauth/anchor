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
-spec init([]) -> {ok, {{one_for_one, 5, 10}, []}}.

init([]) ->
    case ?GET_ENV(pools) of
        undefined ->
            BacklogSize = ?GET_ENV(backlog_size,
                ?DEFAULT_BACKLOG_SIZE),
            Ip = ?GET_ENV(ip, ?DEFAULT_IP),
            PoolSize = ?GET_ENV(pool_size,
                ?DEFAULT_POOL_SIZE),
            PoolStrategy = ?GET_ENV(pool_strategy,
                ?DEFAULT_POOL_STRATEGY),
            Port = ?GET_ENV(port, ?DEFAULT_PORT),
            Reconnect = ?GET_ENV(reconnect,
                ?DEFAULT_RECONNECT),
            ReconnectTimeMax = ?GET_ENV(reconnect_time_max,
                ?DEFAULT_RECONNECT_MAX),
            ReconnectTimeMin = ?GET_ENV(reconnect_time_min,
                ?DEFAULT_RECONNECT_MIN),
            SocketOptions = ?GET_ENV(socket_options,
                ?DEFAULT_SOCKET_OPTIONS),

            ok = shackle_pool:start(?APP, ?CLIENT, [
                {ip, Ip},
                {port, Port},
                {reconnect, Reconnect},
                {reconnect_time_max, ReconnectTimeMax},
                {reconnect_time_min, ReconnectTimeMin},
                {socket_options, SocketOptions}
            ], [
                {backlog_size, BacklogSize},
                {pool_size, PoolSize},
                {pool_strategy, PoolStrategy}
            ]);
        Pools ->
            lists:foreach(fun ({Name, PoolOpts}) ->
                BacklogSize = lookup(backlog_size, PoolOpts,
                    ?DEFAULT_BACKLOG_SIZE),
                Ip = lookup(ip, PoolOpts, ?DEFAULT_IP),
                PoolSize = lookup(pool_size, PoolOpts,
                    ?DEFAULT_POOL_SIZE),
                PoolStrategy = lookup(pool_strategy, PoolOpts,
                    ?DEFAULT_POOL_STRATEGY),
                Port = lookup(port, PoolOpts, ?DEFAULT_PORT),
                Reconnect = lookup(reconnect, PoolOpts,
                    ?DEFAULT_RECONNECT),
                ReconnectTimeMax = lookup(reconnect_time_max, PoolOpts,
                    ?DEFAULT_RECONNECT_MAX),
                ReconnectTimeMin = lookup(reconnect_time_min, PoolOpts,
                    ?DEFAULT_RECONNECT_MIN),
                SocketOptions = lookup(socket_options, PoolOpts,
                    ?DEFAULT_SOCKET_OPTIONS),

                ok = shackle_pool:start(Name, ?CLIENT, [
                    {ip, Ip},
                    {port, Port},
                    {reconnect, Reconnect},
                    {reconnect_time_max, ReconnectTimeMax},
                    {reconnect_time_min, ReconnectTimeMin},
                    {socket_options, SocketOptions}
                ], [
                    {backlog_size, BacklogSize},
                    {pool_size, PoolSize},
                    {pool_strategy, PoolStrategy}
                ])
            end, Pools)
    end,

    {ok, {{one_for_one, 5, 10}, []}}.

%% private
lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false ->
            Default;
        {_, Value} ->
            Value
    end.
