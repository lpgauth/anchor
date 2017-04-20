-module(anchor_tests).
-include_lib("anchor/include/anchor.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 1000).

%% runners
anchor_test_() ->
    {setup,
        fun () -> setup() end,
        fun (_) -> cleanup() end,
    [
        fun async_add_subtest/0,
        fun async_delete_subtest/0,
        fun async_flush_subtest/0,
        fun async_increment_decrement_subtest/0,
        fun async_noop_subtest/0,
        fun async_replace_subtest/0,
        fun async_set_get_subtest/0,
        fun async_version_subtest/0,
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

anchor_async_quit_test_() ->
    {setup,
        fun () -> setup([{pool_size, 1}]) end,
        fun (_) -> cleanup() end,
    [fun async_quit_subtest/0]}.

%% tests
async_add_subtest() ->
    Key = random(),
    Value = random(),
    {ok, Ref} = anchor:async_add(Key, Value),
    ok = anchor:receive_response(Ref, ?TIMEOUT),
    {ok, Ref2} = anchor:async_add(Key, Value),
    {error, key_exists} = anchor:receive_response(Ref2, ?TIMEOUT).

async_delete_subtest() ->
    Key = random(),
    Value = random(),
    {ok, Ref} = anchor:async_set(Key, Value),
    ok = anchor:receive_response(Ref, ?TIMEOUT),
    {ok, Ref2} = anchor:async_delete(Key),
    ok = anchor:receive_response(Ref2, ?TIMEOUT).

async_flush_subtest() ->
    {ok, Ref} = anchor:async_flush(),
    ok = anchor:receive_response(Ref, ?TIMEOUT).

async_increment_decrement_subtest() ->
    Key = random(),
    {ok, Ref} = anchor:async_increment(Key),
    {ok, 0} = anchor:receive_response(Ref, ?TIMEOUT),
    {ok, Ref2} = anchor:async_increment(Key),
    {ok, 1} = anchor:receive_response(Ref2, ?TIMEOUT),
    {ok, Ref3} = anchor:async_increment(Key),
    {ok, 2} = anchor:receive_response(Ref3, ?TIMEOUT),
    {ok, Ref4} = anchor:async_decrement(Key),
    {ok, 1} = anchor:receive_response(Ref4, ?TIMEOUT),
    {ok, Ref5} = anchor:async_decrement(Key),
    {ok, 0} = anchor:receive_response(Ref5, ?TIMEOUT).

async_noop_subtest() ->
    {ok, Ref} = anchor:async_noop(),
    ok = anchor:receive_response(Ref, ?TIMEOUT).

async_quit_subtest() ->
    anchor:async_quit(),
    {ok, Ref} = anchor:async_get(random()),
    {error, _} = anchor:receive_response(Ref, ?TIMEOUT).

async_replace_subtest() ->
    Key = random(),
    Value = random(),
    {ok, Ref} = anchor:async_replace(Key, Value),
    {error, key_not_found} = anchor:receive_response(Ref, ?TIMEOUT),
    {ok, Ref2} = anchor:async_add(Key, random()),
    ok = anchor:receive_response(Ref2, ?TIMEOUT),
    {ok, Ref3} = anchor:async_replace(Key, Value),
    ok = anchor:receive_response(Ref3, ?TIMEOUT),
    {ok, Ref4} = anchor:async_get(Key),
    {ok, Value} = anchor:receive_response(Ref4, ?TIMEOUT).

async_set_get_subtest() ->
    Key = random(),
    Value = random(),
    {ok, Ref} = anchor:async_set(Key, Value),
    ok = anchor:receive_response(Ref, ?TIMEOUT),
    {ok, Ref2} = anchor:async_get(Key),
    {ok, Value} = anchor:receive_response(Ref2, ?TIMEOUT).

async_version_subtest() ->
    {ok, Ref} = anchor:async_version(),
    {ok, _} = anchor:receive_response(Ref, ?TIMEOUT).

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
