-module(anchor_sup_tests).
-include_lib("anchor/include/anchor.hrl").
-include_lib("eunit/include/eunit.hrl").

anchor_sup_creates_all_pools_test_() ->
    {setup,
     fun() ->
             application:set_env(anchor, pools, [{foo, []}, {bar, []}]),
             application:ensure_all_started(shackle),
             anchor_sup:start_link(),
             ok
     end,
     fun(_) ->
             shackle_pool:stop(foo),
             shackle_pool:stop(bar),
             ok
     end,
     [?_assertEqual(2 * ?DEFAULT_POOL_SIZE,
                           length(supervisor:which_children(shackle_sup)))]}.

anchor_uses_config_from_env_test_() ->
    {setup,
     fun() ->
             meck:new(shackle_pool),
             meck:expect(shackle_pool, start, fun(_, _, _, _) -> ok end),
             ok
     end,
     fun(_) ->
             meck:unload(shackle_pool)
     end,
     [fun() ->
              MemcachedConfig = [{ip, 1},
                                 {port, 4},
                                 {reconnect, 5},
                                 {reconnect_time_max, 6},
                                 {reconnect_time_min, 7},
                                 {socket_options, 8}],
              PoolConfig = [{backlog_size, 0},
                            {pool_size, 2},
                            {pool_strategy, 3}],
              application:set_env(anchor,
                                  pools,
                                  [{foo, [{backlog_size, 0},
                                          {ip, 1},
                                          {pool_size, 2},
                                          {pool_strategy, 3},
                                          {port, 4},
                                          {reconnect, 5},
                                          {reconnect_time_max, 6},
                                          {reconnect_time_min, 7},
                                          {socket_options, 8}]}]),
              anchor_sup:init([]),
              StartArgs = [foo, anchor_client, MemcachedConfig, PoolConfig],
              ?assert(meck:called(shackle_pool,
                                  start,
                                  StartArgs))
      end]}.
