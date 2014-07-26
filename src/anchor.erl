-module(anchor).
-include("anchor.hrl").

-export([
    add/2,
    add/3,
    add/4,
    decrement/1,
    decrement/2,
    decrement/3,
    decrement/4,
    decrement/5,
    delete/1,
    delete/2,
    flush/0,
    flush/1,
    flush/2,
    get/1,
    get/2,
    increment/1,
    increment/2,
    increment/3,
    increment/4,
    increment/5,
    noop/0,
    noop/1,
    quit/0,
    quit/1,
    replace/2,
    replace/3,
    replace/4,
    set/2,
    set/3,
    set/4,
    version/0,
    version/1
]).

%% public
-spec add(binary(), binary()) -> ok | error().
add(Key, Value) ->
    add(Key, Value, ?DEFAULT_TTL).

-spec add(binary(), binary(), non_neg_integer()) -> ok | error().
add(Key, Value, TTL) ->
    add(Key, Value, TTL, ?DEFAULT_TIMEOUT).

-spec add(binary(), binary(), non_neg_integer(), pos_integer()) -> ok | error().
add(Key, Value, TTL, Timeout) ->
    call({add, Key, Value, TTL}, Timeout).

-spec decrement(binary()) -> {ok, integer()} | error().
decrement(Key) ->
    decrement(Key, ?DEFAULT_INCREMENT).

-spec decrement(binary(), integer()) -> {ok, integer()} | error().
decrement(Key, Amount) ->
    decrement(Key, Amount, ?DEFAULT_INITIAL_VALUE).

-spec decrement(binary(), integer(), integer()) -> {ok, integer()} | error().
decrement(Key, Amount, InitialValue) ->
    decrement(Key, Amount, InitialValue, ?DEFAULT_TTL).

-spec decrement(binary(), integer(), integer(), non_neg_integer()) -> {ok, integer()} | error().
decrement(Key, Amount, InitialValue, TTL) ->
    decrement(Key, Amount, InitialValue, TTL, ?DEFAULT_TIMEOUT).

-spec decrement(binary(), integer(), integer(), non_neg_integer(), pos_integer()) -> {ok, integer()} | error().
decrement(Key, Amount, InitialValue, TTL, Timeout) ->
    call({decrement, Key, Amount, InitialValue, TTL}, Timeout).

-spec delete(binary()) -> ok | error().
delete(Key) ->
    delete(Key, ?DEFAULT_TIMEOUT).

-spec delete(binary(), pos_integer()) -> ok | error().
delete(Key, Timeout) ->
    call({delete, Key}, Timeout).

-spec flush() -> ok | error().
flush() ->
    flush(?DEFAULT_TTL).

-spec flush(non_neg_integer()) -> ok | error().
flush(TTL) ->
    flush(TTL, ?DEFAULT_TIMEOUT).

-spec flush(non_neg_integer(), pos_integer()) -> ok | error().
flush(TTL, Timeout) ->
    call({flush, TTL}, Timeout).

-spec quit() -> ok | error().
quit() ->
    quit(?DEFAULT_TIMEOUT).

-spec quit(pos_integer()) -> ok | error().
quit(Timeout) ->
    call(quit, Timeout).

-spec get(binary()) -> {ok, binary()} | error().
get(Key) ->
    get(Key, ?DEFAULT_TIMEOUT).

-spec get(binary(), pos_integer()) -> {ok, binary()} | error().
get(Key, Timeout) ->
    call({get, Key}, Timeout).

-spec increment(binary()) -> {ok, integer()} | error().
increment(Key) ->
    increment(Key, ?DEFAULT_INCREMENT).

-spec increment(binary(), integer()) -> {ok, integer()} | error().
increment(Key, Amount) ->
    increment(Key, Amount, ?DEFAULT_INITIAL_VALUE).

-spec increment(binary(), integer(), integer()) -> {ok, integer()} | error().
increment(Key, Amount, InitialValue) ->
    increment(Key, Amount, InitialValue, ?DEFAULT_TTL).

-spec increment(binary(), integer(), integer(), non_neg_integer()) -> {ok, integer()} | error().
increment(Key, Amount, InitialValue, TTL) ->
    increment(Key, Amount, InitialValue, TTL, ?DEFAULT_TIMEOUT).

-spec increment(binary(), integer(), integer(), non_neg_integer(), pos_integer()) -> {ok, integer()} | error().
increment(Key, Amount, InitialValue, TTL, Timeout) ->
    call({increment, Key, Amount, InitialValue, TTL}, Timeout).

-spec noop() -> ok | error().
noop() ->
    noop(?DEFAULT_TIMEOUT).

-spec noop(pos_integer()) -> ok | error().
noop(Timeout) ->
    call(noop, Timeout).

-spec replace(binary(), binary()) -> ok | error().
replace(Key, Value) ->
    replace(Key, Value, ?DEFAULT_TTL).

-spec replace(binary(), binary(), non_neg_integer()) -> ok | error().
replace(Key, Value, TTL) ->
    replace(Key, Value, TTL, ?DEFAULT_TIMEOUT).

-spec replace(binary(), binary(), non_neg_integer(), pos_integer()) -> ok | error().
replace(Key, Value, TTL, Timeout) ->
    call({replace, Key, Value, TTL}, Timeout).

-spec set(binary(), binary()) -> ok | error().
set(Key, Value) ->
    set(Key, Value, ?DEFAULT_TTL).

-spec set(binary(), binary(), non_neg_integer()) -> ok | error().
set(Key, Value, TTL) ->
    set(Key, Value, TTL, ?DEFAULT_TIMEOUT).

-spec set(binary(), binary(), non_neg_integer(), pos_integer()) -> ok | error().
set(Key, Value, TTL, Timeout) ->
    call({set, Key, Value, TTL}, Timeout).

-spec version() -> {ok, binary()} | error().
version() ->
    version(?DEFAULT_TIMEOUT).

-spec version(pos_integer()) -> {ok, binary()} | error().
version(Timeout) ->
    call(version, Timeout).

%% private
call(Msg, Timeout) ->
    try gen_server:call(?SERVER, Msg, Timeout) of
        {ok, Response} ->
            reply(Response);
        {error, Reason} ->
            {error, Reason}
    catch
        exit:{noproc, _} ->
            {error, not_started};
        exit:{timeout, _} ->
            {error, timeout}
    end.

reply(#response {status = Status} = Response) ->
    case status(Status) of
        ok ->
            return(Response);
        Reason ->
            {error, Reason}
    end.

return(#response {op_code = ?OP_ADD}) -> ok;
return(#response {op_code = ?OP_DECREMENT, value = Value}) -> {ok, binary:decode_unsigned(Value)};
return(#response {op_code = ?OP_DELETE}) -> ok;
return(#response {op_code = ?OP_FLUSH}) -> ok;
return(#response {op_code = ?OP_GET, value = Value}) -> {ok, Value};
return(#response {op_code = ?OP_INCREMENT, value = Value}) -> {ok, binary:decode_unsigned(Value)};
return(#response {op_code = ?OP_NOOP}) -> ok;
return(#response {op_code = ?OP_QUIT}) -> ok;
return(#response {op_code = ?OP_REPLACE}) -> ok;
return(#response {op_code = ?OP_SET}) -> ok;
return(#response {op_code = ?OP_VERSION, value = Value}) -> {ok, Value}.

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
