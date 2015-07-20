-module(anchor_utils).
-include("anchor.hrl").

-export([
    child_name/1,
    child_specs/0,
    error_msg/2,
    warning_msg/2
]).

-ifdef(TEST).
-define(IF_DEF_TEST, fun (_F) -> ok end).
-else.
-define(IF_DEF_TEST, fun (F) -> F() end).
-endif.

%% public
child_name(N) ->
    list_to_atom(?SERVER_BASE_NAME ++ integer_to_list(N)).

child_specs() ->
    PoolSize = application:get_env(?APP, pool_size, ?DEFAULT_POOL_SIZE),
    [?CHILD(child_name(N), ?SERVER) || N <- lists:seq(1, PoolSize)].

error_msg(Format, Data) ->
    ?IF_DEF_TEST(fun () -> error_logger:error_msg("[anchor] " ++ Format, Data) end).

warning_msg(Format, Data) ->
    ?IF_DEF_TEST(fun () -> error_logger:warning_msg("[anchor] " ++ Format, Data) end).
