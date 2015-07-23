-module(anchor_tests).
-include_lib("anchor/include/anchor.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(T, fun (Test) -> test(Test) end).

%% runners
anchor_test_() ->
    {setup,
        fun () -> setup() end,
        fun (_) -> cleanup() end,
    [
        ?T(test_add),
        ?T(test_delete),
        ?T(test_flush),
        ?T(test_increment_decrement),
        ?T(test_noop),
        ?T(test_replace),
        ?T(test_set_get),
        ?T(test_version)
    ]}.

anchor_quit_test_() ->
    {setup,
        fun () -> setup([{pool_size, 1}]) end,
        fun (_) -> cleanup() end,
    [?T(test_quit)]}.

%% tests
test_add() ->
    Key = random(),
    Value = random(),
    ok = anchor:add(Key, Value),
    {error, key_exists} = anchor:add(Key, Value).

test_delete() ->
    Key = random(),
    Value = random(),
    ok = anchor:set(Key, Value),
    ok = anchor:delete(Key).

test_flush() ->
    ok = anchor:flush().

test_increment_decrement() ->
    Key = random(),
    {ok, 0} = anchor:increment(Key),
    {ok, 1} = anchor:increment(Key),
    {ok, 2} = anchor:increment(Key),
    {ok, 1} = anchor:decrement(Key),
    {ok, 0} = anchor:decrement(Key).

test_noop() ->
    ok = anchor:noop().

test_no_socket() ->
    {error, no_socket} = anchor:noop().

test_quit() ->
    anchor:quit(),
    {error, _} = anchor:get(random()).

test_replace() ->
    Key = random(),
    Value = random(),
    {error, key_not_found} = anchor:replace(Key, Value),
    ok = anchor:add(Key, random()),
    ok = anchor:replace(Key, Value),
    {ok, Value} = anchor:get(Key).

test_set_get() ->
    Key = random(),
    Value = random(),
    ok = anchor:set(Key, Value),
    {ok, Value} = anchor:get(Key).

test_version() ->
    {ok, _} = anchor:version().

%% utils
cleanup() ->
    anchor_app:stop().

random() ->
    crypto:rand_bytes(24).

receive_loop(0) -> [];
receive_loop(N) ->
    receive
        {response, X} ->
            [X | receive_loop(N - 1)]
    end.

setup() ->
    setup([]).

setup(KeyVals) ->
    error_logger:tty(false),
    application:load(?APP),
    set_env(KeyVals),
    anchor_app:start(),
    application:stop(sasl).

set_env([]) ->
    ok;
set_env([{K, V} | T]) ->
    application:set_env(?APP, K, V),
    set_env(T).

test(Test) ->
    {atom_to_list(Test), ?MODULE, Test}.
