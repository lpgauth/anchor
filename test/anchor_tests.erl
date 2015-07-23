-module(anchor_tests).
-include_lib("anchor/include/anchor.hrl").
-include_lib("eunit/include/eunit.hrl").

%% runners
anchor_test_() ->
    {setup,
        fun () -> setup() end,
        fun (_) -> cleanup() end,
    [
        fun add_subtest/0,
        fun delete_subtest/0,
        fun flush_subtest/0,
        fun increment_decrement_subtest/0,
        fun noop_subtest/0,
        fun replace_subtest/0,
        fun set_get_subtest/0,
        fun version_subtest/0
    ]}.

anchor_quit_test_() ->
    {setup,
        fun () -> setup([{pool_size, 1}]) end,
        fun (_) -> cleanup() end,
    [fun quit_subtest/0]}.

%% tests
add_subtest() ->
    Key = random(),
    Value = random(),
    ok = anchor:add(Key, Value),
    {error, key_exists} = anchor:add(Key, Value).

delete_subtest() ->
    Key = random(),
    Value = random(),
    ok = anchor:set(Key, Value),
    ok = anchor:delete(Key).

flush_subtest() ->
    ok = anchor:flush().

increment_decrement_subtest() ->
    Key = random(),
    {ok, 0} = anchor:increment(Key),
    {ok, 1} = anchor:increment(Key),
    {ok, 2} = anchor:increment(Key),
    {ok, 1} = anchor:decrement(Key),
    {ok, 0} = anchor:decrement(Key).

noop_subtest() ->
    ok = anchor:noop().

quit_subtest() ->
    anchor:quit(),
    {error, _} = anchor:get(random()).

replace_subtest() ->
    Key = random(),
    Value = random(),
    {error, key_not_found} = anchor:replace(Key, Value),
    ok = anchor:add(Key, random()),
    ok = anchor:replace(Key, Value),
    {ok, Value} = anchor:get(Key).

set_get_subtest() ->
    Key = random(),
    Value = random(),
    ok = anchor:set(Key, Value),
    {ok, Value} = anchor:get(Key).

version_subtest() ->
    {ok, _} = anchor:version().

%% utils
cleanup() ->
    anchor_app:stop().

random() ->
    crypto:rand_bytes(24).

setup() ->
    setup([]).

setup(KeyVals) ->
    error_logger:tty(false),
    application:load(?APP),
    set_env(KeyVals),
    anchor_app:start().

set_env([]) ->
    ok;
set_env([{K, V} | T]) ->
    application:set_env(?APP, K, V),
    set_env(T).
