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
        ?T(test_quit),
        ?T(test_replace),
        ?T(test_set_get),
        ?T(test_version)
    ]}.

anchor_backlog_test_() ->
    {setup,
        fun () -> setup([{backlog_size, 1}]) end,
        fun (_) -> cleanup() end,
    [
        ?T(test_backlogfull_async),
        ?T(test_backlogfull_sync)
    ]}.

anchor_connection_error_test_() ->
    {setup,
        fun () -> setup([{port, 11212}]) end,
        fun (_) -> cleanup() end,
    [
        ?T(test_no_socket)
    ]}.

%% tests
test_add() ->
    Key = random(),
    Value = random(),
    ok = anchor:add(Key, Value),
    {error, key_exists} = anchor:add(Key, Value).

test_backlogfull_async() ->
    Key = random(),
    Value = random(),
    ok = anchor:set(Key, Value),

    Responses = [anchor:get(Key, 1000, [{async, self()}]) || _ <- lists:seq(1,100)],
    ?assert(lists:any(fun
        ({error, backlog_full}) -> true;
        (_) -> false
    end, Responses)).

test_backlogfull_sync() ->
    Key = random(),
    Value = random(),
    ok = anchor:set(Key, Value),
    Pid = self(),

    [spawn(fun () -> Pid ! {response, anchor:get(Key)} end) || _ <- lists:seq(1,20)],
    ?assert(lists:any(fun
        ({error, backlog_full}) -> true;
        (_) -> false
    end, receive_loop(20))).

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
    ok = anchor:quit().

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
    error_logger:tty(false),
    application:stop(?APP),
    error_logger:tty(true).

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
    [application:set_env(?APP, K, V) || {K, V} <- KeyVals],
    anchor_app:start(),
    error_logger:tty(true).

test(Test) ->
    {atom_to_list(Test), ?MODULE, Test}.
