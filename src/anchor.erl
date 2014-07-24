-module(anchor).
-include("anchor.hrl").

-export([
    add/2,
    add/3,
    add/4,
    delete/1,
    delete/2,
    get/1,
    get/2,
    replace/2,
    replace/3,
    replace/4,
    set/2,
    set/3,
    set/4
]).

%% public
-spec add(binary(), binary()) -> ok | {error, atom()}.
add(Key, Value) ->
    add(Key, Value, ?DEFAULT_TTL).

-spec add(binary(), binary(), non_neg_integer()) -> ok | {error, atom()}.
add(Key, Value, TTL) ->
    add(Key, Value, TTL, ?DEFAULT_TIMEOUT).

-spec add(binary(), binary(), non_neg_integer(), pos_integer()) -> ok | {error, atom()}.
add(Key, Value, TTL, Timeout) ->
    call({add, Key, Value, TTL}, Timeout).

-spec delete(binary()) -> ok | {error, atom()}.
delete(Key) ->
    delete(Key, ?DEFAULT_TIMEOUT).

-spec delete(binary(), pos_integer()) -> ok | {error, atom()}.
delete(Key, Timeout) ->
    call({delete, Key}, Timeout).

-spec get(binary()) -> {ok, binary()} | {error, atom()}.
get(Key) ->
    get(Key, ?DEFAULT_TIMEOUT).

-spec get(binary(), pos_integer()) -> {ok, binary()} | {error, atom()}.
get(Key, Timeout) ->
    call({get, Key}, Timeout).

-spec replace(binary(), binary()) -> ok | {error, atom()}.
replace(Key, Value) ->
    replace(Key, Value, ?DEFAULT_TTL).

-spec replace(binary(), binary(), non_neg_integer()) -> ok | {error, atom()}.
replace(Key, Value, TTL) ->
    replace(Key, Value, TTL, ?DEFAULT_TIMEOUT).

-spec replace(binary(), binary(), non_neg_integer(), pos_integer()) -> ok | {error, atom()}.
replace(Key, Value, TTL, Timeout) ->
    call({replace, Key, Value, TTL}, Timeout).

-spec set(binary(), binary()) -> ok | {error, atom()}.
set(Key, Value) ->
    set(Key, Value, ?DEFAULT_TTL).

-spec set(binary(), binary(), non_neg_integer()) -> ok | {error, atom()}.
set(Key, Value, TTL) ->
    set(Key, Value, TTL, ?DEFAULT_TIMEOUT).

-spec set(binary(), binary(), non_neg_integer(), pos_integer()) -> ok | {error, atom()}.
set(Key, Value, TTL, Timeout) ->
    call({set, Key, Value, TTL}, Timeout).

%% private
call(Msg, Timeout) ->
    try gen_server:call(?SERVER, Msg, Timeout) of
        {ok, Response} ->
            response(Response);
        {error, Reason} ->
            {error, Reason}
    catch
        exit:{noproc, _} ->
            {error, not_started};
        exit:{timeout, _} ->
            {error, timeout}
    end.

ok(?OP_ADD, _Response) -> ok;
ok(?OP_DELETE, _Response) -> ok;
ok(?OP_GET, #response {value = Value}) -> {ok, Value};
ok(?OP_REPLACE, _Response) -> ok;
ok(?OP_SET, _Response) -> ok.

response(#response {
        op_code = OpCode,
        status = Status
    } = Response) ->

    case status(Status) of
        ok ->
            ok(OpCode, Response);
        Reason ->
            {error, Reason}
    end.

status(?STAT_AUTH_CONTINUE) -> auth_continue;
status(?STAT_AUTH_ERROR) -> auth_error;
status(?STAT_BUSY) -> busy;
status(?STAT_INCR_NON_NUMERIC) -> incr_non_numeric;
status(?STAT_INTERNAL_ERROR) -> internal_error;
status(?STAT_INVALID_ARGS) -> invalid_args;
status(?STAT_ITEM_NOT_STORED) -> item_not_stored;
status(?STAT_KEY_EXISTS) -> key_exists;
status(?STAT_KEY_NOT_FOUND) -> key_not_found;
status(?STAT_NOT_SUPPORTED) -> not_supported;
status(?STAT_OK) -> ok;
status(?STAT_OUT_OF_MEMORY) -> out_of_memory;
status(?STAT_TEMP_FAILURE) -> temp_failure;
status(?STAT_UNKNOWN_COMMAND) -> unknown_command;
status(?STAT_VALUE_TOO_LARGE) -> value_too_large;
status(?STAT_VBUCKET_ERROR) -> vbucket_error.
