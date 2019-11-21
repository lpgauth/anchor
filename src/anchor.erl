-module(anchor).
-include("anchor.hrl").

-export([
    async_add/2,
    async_add/3,
    async_add/4,
    async_add/5,
    async_add/6,
    async_decrement/1,
    async_decrement/2,
    async_decrement/3,
    async_decrement/4,
    async_decrement/5,
    async_decrement/6,
    async_decrement/7,
    async_delete/1,
    async_delete/2,
    async_delete/3,
    async_delete/4,
    async_flush/0,
    async_flush/1,
    async_flush/2,
    async_flush/3,
    async_flush/4,
    async_get/1,
    async_get/2,
    async_get/3,
    async_get/4,
    async_increment/1,
    async_increment/2,
    async_increment/3,
    async_increment/4,
    async_increment/5,
    async_increment/6,
    async_increment/7,
    async_noop/0,
    async_noop/1,
    async_noop/2,
    async_noop/3,
    async_quit/0,
    async_quit/1,
    async_quit/2,
    async_quit/3,
    async_replace/2,
    async_replace/3,
    async_replace/4,
    async_replace/5,
    async_replace/6,
    async_set/2,
    async_set/3,
    async_set/4,
    async_set/5,
    async_set/6,
    async_version/0,
    async_version/1,
    async_version/2,
    async_version/3,
    add/2,
    add/3,
    add/4,
    add/5,
    decrement/1,
    decrement/2,
    decrement/3,
    decrement/4,
    decrement/5,
    decrement/6,
    delete/1,
    delete/2,
    delete/3,
    flush/0,
    flush/1,
    flush/2,
    flush/3,
    get/1,
    get/2,
    get/3,
    increment/1,
    increment/2,
    increment/3,
    increment/4,
    increment/5,
    increment/6,
    noop/0,
    noop/1,
    noop/2,
    quit/0,
    quit/1,
    quit/2,
    receive_response/1,
    replace/2,
    replace/3,
    replace/4,
    replace/5,
    response/1,
    set/2,
    set/3,
    set/4,
    set/5,
    version/0,
    version/1,
    version/2
]).

%% public
-spec async_add(binary(), binary()) ->
    {ok, shackle:request_id()} | error().

async_add(Key, Value) ->
    async_add(Key, Value, ?DEFAULT_TTL).

-spec async_add(binary(), binary(), non_neg_integer()) ->
    {ok, shackle:request_id()} | error().

async_add(Key, Value, TTL) ->
    async_add(Key, Value, TTL, ?DEFAULT_PID).

-spec async_add(binary(), binary(), non_neg_integer(), pid()) ->
    {ok, shackle:request_id()} | error().

async_add(Key, Value, TTL, Pid) ->
    async_add(Key, Value, TTL, Pid, ?DEFAULT_TIMEOUT).

-spec async_add(binary(), binary(), non_neg_integer(), pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_add(Key, Value, TTL, Pid, Timeout) ->
    async_add(?APP, Key, Value, TTL, Pid, Timeout).

-spec async_add(pool_name(), binary(), binary(), non_neg_integer(), pid(),
                timeout()) ->
    {ok, shackle:request_id()} | error().

async_add(PoolName, Key, Value, TTL, Pid, Timeout) ->
    cast(PoolName, {add, Key, Value, TTL}, Pid, Timeout).

-spec async_decrement(binary()) ->
    {ok, shackle:request_id()} | error().

async_decrement(Key) ->
    async_decrement(Key, ?DEFAULT_INCREMENT).

-spec async_decrement(binary(), integer()) ->
    {ok, shackle:request_id()} | error().

async_decrement(Key, Amount) ->
    async_decrement(Key, Amount, ?DEFAULT_INITIAL_VALUE).

-spec async_decrement(binary(), integer(), integer()) ->
    {ok, shackle:request_id()} | error().

async_decrement(Key, Amount, InitialValue) ->
    async_decrement(Key, Amount, InitialValue, ?DEFAULT_TTL).

-spec async_decrement(binary(), integer(), integer(), non_neg_integer()) ->
    {ok, shackle:request_id()} | error().

async_decrement(Key, Amount, InitialValue, TTL) ->
    async_decrement(Key, Amount, InitialValue, TTL, ?DEFAULT_PID).

-spec async_decrement(binary(), integer(), integer(), non_neg_integer(),
        pid()) -> {ok, shackle:request_id()} | error().

async_decrement(Key, Amount, InitialValue, TTL, Pid) ->
    async_decrement(Key, Amount, InitialValue, TTL, Pid, ?DEFAULT_TIMEOUT).

-spec async_decrement(binary(), integer(), integer(), non_neg_integer(),
        pid(), timeout()) -> {ok, shackle:request_id()} | error().

async_decrement(Key, Amount, InitialValue, TTL, Pid, Timeout) ->
    async_decrement(?APP, Key, Amount, InitialValue, TTL, Pid, Timeout).

async_decrement(PoolName, Key, Amount, InitialValue, TTL, Pid, Timeout) ->
    cast(PoolName, {decrement, Key, Amount, InitialValue, TTL}, Pid, Timeout).

-spec async_delete(binary()) ->
    {ok, shackle:request_id()} | error().

async_delete(Key) ->
    async_delete(Key, ?DEFAULT_PID).

-spec async_delete(binary(), pid()) ->
    {ok, shackle:request_id()} | error().

async_delete(Key, Pid) ->
    async_delete(Key, Pid, ?DEFAULT_TIMEOUT).

-spec async_delete(binary(), pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_delete(Key, Pid, Timeout) ->
    async_delete(?APP, Key, Pid, Timeout).

async_delete(PoolName, Key, Pid, Timeout) ->
    cast(PoolName, {delete, Key}, Pid, Timeout).

-spec async_flush() ->
    {ok, shackle:request_id()} | error().

async_flush() ->
    async_flush(?DEFAULT_TTL).

-spec async_flush(non_neg_integer()) ->
    {ok, shackle:request_id()} | error().

async_flush(TTL) ->
    async_flush(TTL, ?DEFAULT_PID).

-spec async_flush(non_neg_integer(), pid()) ->
    {ok, shackle:request_id()} | error().

async_flush(TTL, Pid) ->
    async_flush(TTL, Pid, ?DEFAULT_TIMEOUT).

-spec async_flush(non_neg_integer(), pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_flush(TTL, Pid, Timeout) ->
    async_flush(?APP, TTL, Pid, Timeout).

-spec async_flush(pool_name(), non_neg_integer(), pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_flush(PoolName, TTL, Pid, Timeout) ->
    cast(PoolName, {flush, TTL}, Pid, Timeout).

-spec async_get(binary()) ->
    {ok, shackle:request_id()} | error().

async_get(Key) ->
    async_get(Key, ?DEFAULT_PID).

-spec async_get(binary(), pid()) ->
    {ok, shackle:request_id()} | error().

async_get(Key, Pid) ->
    async_get(Key, Pid, ?DEFAULT_TIMEOUT).

-spec async_get(binary(), pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_get(Key, Pid, Timeout) ->
    async_get(?APP, Key, Pid, Timeout).

-spec async_get(pool_name(), binary(), pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_get(PoolName, Key, Pid, Timeout) ->
    cast(PoolName, {get, Key}, Pid, Timeout).

-spec async_increment(binary()) ->
    {ok, shackle:request_id()} | error().

async_increment(Key) ->
    async_increment(Key, ?DEFAULT_INCREMENT).

-spec async_increment(binary(), integer()) ->
    {ok, shackle:request_id()} | error().

async_increment(Key, Amount) ->
    async_increment(Key, Amount, ?DEFAULT_INITIAL_VALUE).

-spec async_increment(binary(), integer(), integer()) ->
    {ok, shackle:request_id()} | error().

async_increment(Key, Amount, InitialValue) ->
    async_increment(Key, Amount, InitialValue, ?DEFAULT_TTL).

-spec async_increment(binary(), integer(), integer(), non_neg_integer()) ->
    {ok, shackle:request_id()} | error().

async_increment(Key, Amount, InitialValue, TTL) ->
    async_increment(Key, Amount, InitialValue, TTL, ?DEFAULT_PID).

-spec async_increment(binary(), integer(), integer(), non_neg_integer(),
        pid()) -> {ok, shackle:request_id()} | error().

async_increment(Key, Amount, InitialValue, TTL, Pid) ->
    async_increment(Key, Amount, InitialValue, TTL, Pid, ?DEFAULT_TIMEOUT).

-spec async_increment(binary(), integer(), integer(), non_neg_integer(),
        pid(), timeout()) -> {ok, shackle:request_id()} | error().

async_increment(Key, Amount, InitialValue, TTL, Pid, Timeout) ->
    async_increment(?APP, Key, Amount, InitialValue, TTL, Pid, Timeout).

-spec async_increment(pool_name(), binary(), integer(), integer(),
                      non_neg_integer(), pid(),
                      timeout()) -> {ok, shackle:request_id()} | error().

async_increment(PoolName, Key, Amount, InitialValue, TTL, Pid, Timeout) ->
    cast(PoolName, {increment, Key, Amount, InitialValue, TTL}, Pid, Timeout).

-spec async_noop() ->
    {ok, shackle:request_id()} | error().

async_noop() ->
    async_noop(?DEFAULT_PID).

-spec async_noop(pid()) ->
    {ok, shackle:request_id()} | error().

async_noop(Pid) ->
    async_noop(Pid, ?DEFAULT_TIMEOUT).

-spec async_noop(pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_noop(Pid, Timeout) ->
    async_noop(?APP, Pid, Timeout).

-spec async_noop(pool_name(), pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_noop(PoolName, Pid, Timeout) ->
    cast(PoolName, noop, Pid, Timeout).

-spec async_quit() ->
    {ok, shackle:request_id()} | error().

async_quit() ->
    async_quit(?DEFAULT_PID).

-spec async_quit(pid()) ->
    {ok, shackle:request_id()} | error().

async_quit(Pid) ->
    async_quit(Pid, ?DEFAULT_TIMEOUT).

-spec async_quit(pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_quit(Pid, Timeout) ->
    async_quit(?APP, Pid, Timeout).

-spec async_quit(pool_name(), pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_quit(PoolName, Pid, Timeout) ->
    cast(PoolName, quit, Pid, Timeout).

-spec async_replace(binary(), binary()) ->
    {ok, shackle:request_id()} | error().

async_replace(Key, Value) ->
    async_replace(Key, Value, ?DEFAULT_TTL).

-spec async_replace(binary(), binary(), non_neg_integer()) ->
    {ok, shackle:request_id()} | error().

async_replace(Key, Value, TTL) ->
    async_replace(Key, Value, TTL, ?DEFAULT_PID).

-spec async_replace(binary(), binary(), non_neg_integer(), pid()) ->
    {ok, shackle:request_id()} | error().

async_replace(Key, Value, TTL, Pid) ->
    async_replace(Key, Value, TTL, Pid, ?DEFAULT_TIMEOUT).

-spec async_replace(binary(), binary(), non_neg_integer(), pid(),
    timeout()) -> {ok, shackle:request_id()} | error().

async_replace(Key, Value, TTL, Pid, Timeout) ->
    async_replace(?APP, Key, Value, TTL, Pid, Timeout).

-spec async_replace(pool_name(), binary(), binary(), non_neg_integer(), pid(),
    timeout()) -> {ok, shackle:request_id()} | error().

async_replace(PoolName, Key, Value, TTL, Pid, Timeout) ->
    cast(PoolName, {replace, Key, Value, TTL}, Pid, Timeout).

-spec async_set(binary(), binary()) ->
    {ok, shackle:request_id()} | error().

async_set(Key, Value) ->
    async_set(Key, Value, ?DEFAULT_TTL).

-spec async_set(binary(), binary(), non_neg_integer()) ->
    {ok, shackle:request_id()} | error().

async_set(Key, Value, TTL) ->
    async_set(Key, Value, TTL, ?DEFAULT_PID).

-spec async_set(binary(), binary(), non_neg_integer(), pid()) ->
    {ok, shackle:request_id()} | error().

async_set(Key, Value, TTL, Pid) ->
    async_set(Key, Value, TTL, Pid, ?DEFAULT_TIMEOUT).

-spec async_set(binary(), binary(), non_neg_integer(), pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_set(Key, Value, TTL, Pid, Timeout) ->
    async_set(?APP, Key, Value, TTL, Pid, Timeout).

-spec async_set(pool_name(), binary(), binary(), non_neg_integer(), pid(),
                timeout()) ->
    {ok, shackle:request_id()} | error().

async_set(PoolName, Key, Value, TTL, Pid, Timeout) ->
    cast(PoolName, {set, Key, Value, TTL}, Pid, Timeout).

-spec async_version() ->
    {ok, shackle:request_id()} | error().

async_version() ->
    async_version(?DEFAULT_PID).

-spec async_version(pid()) ->
    {ok, shackle:request_id()} | error().

async_version(Pid) ->
    async_version(Pid, ?DEFAULT_TIMEOUT).

-spec async_version(pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_version(Pid, Timeout) ->
    async_version(?APP, Pid, Timeout).

-spec async_version(pool_name(), pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_version(PoolName, Pid, Timeout) ->
    cast(PoolName, version, Pid, Timeout).

-spec add(binary(), binary()) ->
    ok | error().

add(Key, Value) ->
    add(Key, Value, ?DEFAULT_TTL).

-spec add(binary(), binary(), non_neg_integer()) ->
    ok | error().

add(Key, Value, TTL) ->
    add(Key, Value, TTL, ?DEFAULT_TIMEOUT).

-spec add(binary(), binary(), non_neg_integer(), pos_integer()) ->
    ok | error().

add(Key, Value, TTL, Timeout) ->
    add(?APP, Key, Value, TTL, Timeout).

-spec add(pool_name(), binary(), binary(), non_neg_integer(), pos_integer()) ->
    ok | error().

add(PoolName, Key, Value, TTL, Timeout) ->
    call(PoolName, {add, Key, Value, TTL}, Timeout).

-spec decrement(binary()) ->
    {ok, integer()} | error().

decrement(Key) ->
    decrement(Key, ?DEFAULT_INCREMENT).

-spec decrement(binary(), integer()) ->
    {ok, integer()} | error().

decrement(Key, Amount) ->
    decrement(Key, Amount, ?DEFAULT_INITIAL_VALUE).

-spec decrement(binary(), integer(), integer()) ->
    {ok, integer()} | error().

decrement(Key, Amount, InitialValue) ->
    decrement(Key, Amount, InitialValue, ?DEFAULT_TTL).

-spec decrement(binary(), integer(), integer(), non_neg_integer()) ->
    {ok, integer()} | error().

decrement(Key, Amount, InitialValue, TTL) ->
    decrement(Key, Amount, InitialValue, TTL, ?DEFAULT_TIMEOUT).

-spec decrement(binary(), integer(), integer(), non_neg_integer(),
        pos_integer()) -> {ok, integer()} | error().

decrement(Key, Amount, InitialValue, TTL, Timeout) ->
    decrement(?APP, Key, Amount, InitialValue, TTL, Timeout).

-spec decrement(pool_name(), binary(), integer(), integer(), non_neg_integer(),
        pos_integer()) -> {ok, integer()} | error().

decrement(PoolName, Key, Amount, InitialValue, TTL, Timeout) ->
    call(PoolName, {decrement, Key, Amount, InitialValue, TTL}, Timeout).

-spec delete(binary()) ->
    ok | error().

delete(Key) ->
    delete(Key, ?DEFAULT_TIMEOUT).

-spec delete(binary(), pos_integer()) ->
    ok | error().

delete(Key, Timeout) ->
    delete(?APP, Key, Timeout).

-spec delete(pool_name(), binary(), pos_integer()) ->
    ok | error().

delete(PoolName, Key, Timeout) ->
    call(PoolName, {delete, Key}, Timeout).

-spec flush() ->
    ok | error().

flush() ->
    flush(?DEFAULT_TTL).

-spec flush(non_neg_integer()) ->
    ok | error().

flush(TTL) ->
    flush(TTL, ?DEFAULT_TIMEOUT).

-spec flush(non_neg_integer(), pos_integer()) ->
    ok | error().

flush(TTL, Timeout) ->
    flush(?APP, TTL, Timeout).

-spec flush(pool_name(), non_neg_integer(), pos_integer()) ->
    ok | error().

flush(PoolName, TTL, Timeout) ->
    call(PoolName, {flush, TTL}, Timeout).

-spec get(binary()) ->
    {ok, binary()} | error().

get(Key) ->
    get(Key, ?DEFAULT_TIMEOUT).

-spec get(binary(), pos_integer()) ->
    {ok, binary()} | error().

get(Key, Timeout) ->
    get(?APP, Key, Timeout).

-spec get(pool_name(), binary(), pos_integer()) ->
    {ok, binary()} | error().

get(PoolName, Key, Timeout) ->
    call(PoolName, {get, Key}, Timeout).

-spec increment(binary()) ->
    {ok, integer()} | error().

increment(Key) ->
    increment(Key, ?DEFAULT_INCREMENT).

-spec increment(binary(), integer()) ->
    {ok, integer()} | error().

increment(Key, Amount) ->
    increment(Key, Amount, ?DEFAULT_INITIAL_VALUE).

-spec increment(binary(), integer(), integer()) ->
    {ok, integer()} | error().

increment(Key, Amount, InitialValue) ->
    increment(Key, Amount, InitialValue, ?DEFAULT_TTL).

-spec increment(binary(), integer(), integer(), non_neg_integer()) ->
    {ok, integer()} | error().

increment(Key, Amount, InitialValue, TTL) ->
    increment(Key, Amount, InitialValue, TTL, ?DEFAULT_TIMEOUT).

-spec increment(binary(), integer(), integer(), non_neg_integer(),
        pos_integer()) -> {ok, integer()} | error().

increment(Key, Amount, InitialValue, TTL, Timeout) ->
    increment(?APP, Key, Amount, InitialValue, TTL, Timeout).

-spec increment(pool_name(), binary(), integer(), integer(), non_neg_integer(),
        pos_integer()) -> {ok, integer()} | error().

increment(PoolName, Key, Amount, InitialValue, TTL, Timeout) ->
    call(PoolName, {increment, Key, Amount, InitialValue, TTL}, Timeout).

-spec noop() ->
    ok | error().

noop() ->
    noop(?DEFAULT_TIMEOUT).

-spec noop(pos_integer()) ->
    ok | error().

noop(Timeout) ->
    noop(?APP, Timeout).

-spec noop(pool_name(), pos_integer()) ->
    ok | error().

noop(PoolName, Timeout) ->
    call(PoolName, noop, Timeout).

-spec quit() ->
    ok | error().

quit() ->
    quit(?DEFAULT_TIMEOUT).

-spec quit(pos_integer()) ->
    ok | error().

quit(Timeout) ->
    quit(?APP, Timeout).

-spec quit(pool_name(), pos_integer()) ->
    ok | error().

quit(PoolName, Timeout) ->
    call(PoolName, quit, Timeout).

-spec receive_response(shackle:request_id()) ->
    {ok, term()} | {error, term()}.

receive_response(RequestId) ->
    response(shackle:receive_response(RequestId)).

-spec replace(binary(), binary()) ->
    ok | error().

replace(Key, Value) ->
    replace(Key, Value, ?DEFAULT_TTL).

-spec replace(binary(), binary(), non_neg_integer()) ->
    ok | error().

replace(Key, Value, TTL) ->
    replace(Key, Value, TTL, ?DEFAULT_TIMEOUT).

-spec replace(binary(), binary(), non_neg_integer(), pos_integer()) ->
    ok | error().

replace(Key, Value, TTL, Timeout) ->
    replace(?APP, Key, Value, TTL, Timeout).

-spec replace(pool_name(), binary(), binary(), non_neg_integer(),
              pos_integer()) -> ok | error().

replace(PoolName, Key, Value, TTL, Timeout) ->
    call(PoolName, {replace, Key, Value, TTL}, Timeout).

-spec response({ok, term()} | error()) ->
    ok | {ok, term()} | error().

response({ok, Response}) ->
    anchor_response:format(Response);
response({error, Reason}) ->
    {error, Reason}.

-spec set(binary(), binary()) ->
    ok | error().

set(Key, Value) ->
    set(Key, Value, ?DEFAULT_TTL).

-spec set(binary(), binary(), non_neg_integer()) ->
    ok | error().

set(Key, Value, TTL) ->
    set(Key, Value, TTL, ?DEFAULT_TIMEOUT).

-spec set(binary(), binary(), non_neg_integer(), pos_integer()) ->
    ok | error().

set(Key, Value, TTL, Timeout) ->
    set(?APP, Key, Value, TTL, Timeout).

-spec set(pool_name(), binary(), binary(), non_neg_integer(), pos_integer()) ->
    ok | error().

set(PoolName, Key, Value, TTL, Timeout) ->
    call(PoolName, {set, Key, Value, TTL}, Timeout).

-spec version() ->
    {ok, binary()} | error().

version() ->
    version(?DEFAULT_TIMEOUT).

-spec version(pos_integer()) ->
    {ok, binary()} | error().

version(Timeout) ->
    version(?APP, Timeout).

-spec version(pool_name(), pos_integer()) ->
    {ok, binary()} | error().

version(PoolName, Timeout) ->
    call(PoolName, version, Timeout).

%% private
call(PoolName, Msg, Timeout) ->
    response(shackle:call(PoolName, Msg, Timeout)).

cast(PoolName, Msg, Pid, Timeout) ->
    shackle:cast(PoolName, Msg, Pid, Timeout).
