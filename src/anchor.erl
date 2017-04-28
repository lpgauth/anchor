-module(anchor).
-include("anchor.hrl").

-export([
    async_add/2,
    async_add/3,
    async_add/4,
    async_add/5,
    async_decrement/1,
    async_decrement/2,
    async_decrement/3,
    async_decrement/4,
    async_decrement/5,
    async_decrement/6,
    async_delete/1,
    async_delete/2,
    async_delete/3,
    async_flush/0,
    async_flush/1,
    async_flush/2,
    async_flush/3,
    async_get/1,
    async_get/2,
    async_get/3,
    async_increment/1,
    async_increment/2,
    async_increment/3,
    async_increment/4,
    async_increment/5,
    async_increment/6,
    async_noop/0,
    async_noop/1,
    async_noop/2,
    async_quit/0,
    async_quit/1,
    async_quit/2,
    async_replace/2,
    async_replace/3,
    async_replace/4,
    async_replace/5,
    async_set/2,
    async_set/3,
    async_set/4,
    async_set/5,
    async_version/0,
    async_version/1,
    async_version/2,
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
    receive_response/1,
    replace/2,
    replace/3,
    replace/4,
    response/1,
    set/2,
    set/3,
    set/4,
    version/0,
    version/1
]).

%% public
-spec async_add(binary(), binary()) ->
    {ok, reference()} | error().

async_add(Key, Value) ->
    async_add(Key, Value, ?DEFAULT_TTL).

-spec async_add(binary(), binary(), non_neg_integer()) ->
    {ok, reference()} | error().

async_add(Key, Value, TTL) ->
    async_add(Key, Value, TTL, ?DEFAULT_PID).

-spec async_add(binary(), binary(), non_neg_integer(), pid()) ->
    {ok, reference()} | error().

async_add(Key, Value, TTL, Pid) ->
    async_add(Key, Value, TTL, Pid, ?DEFAULT_TIMEOUT).

-spec async_add(binary(), binary(), non_neg_integer(), pid(), timeout()) ->
    {ok, reference()} | error().

async_add(Key, Value, TTL, Pid, Timeout) ->
    cast({add, Key, Value, TTL}, Pid, Timeout).

-spec async_decrement(binary()) ->
    {ok, reference()} | error().

async_decrement(Key) ->
    async_decrement(Key, ?DEFAULT_INCREMENT).

-spec async_decrement(binary(), integer()) ->
    {ok, reference()} | error().

async_decrement(Key, Amount) ->
    async_decrement(Key, Amount, ?DEFAULT_INITIAL_VALUE).

-spec async_decrement(binary(), integer(), integer()) ->
    {ok, reference()} | error().

async_decrement(Key, Amount, InitialValue) ->
    async_decrement(Key, Amount, InitialValue, ?DEFAULT_TTL).

-spec async_decrement(binary(), integer(), integer(), non_neg_integer()) ->
    {ok, reference()} | error().

async_decrement(Key, Amount, InitialValue, TTL) ->
    async_decrement(Key, Amount, InitialValue, TTL, ?DEFAULT_PID).

-spec async_decrement(binary(), integer(), integer(), non_neg_integer(),
        pid()) -> {ok, reference()} | error().

async_decrement(Key, Amount, InitialValue, TTL, Pid) ->
    async_decrement(Key, Amount, InitialValue, TTL, Pid, ?DEFAULT_TIMEOUT).

-spec async_decrement(binary(), integer(), integer(), non_neg_integer(),
        pid(), timeout()) -> {ok, reference()} | error().

async_decrement(Key, Amount, InitialValue, TTL, Pid, Timeout) ->
    cast({decrement, Key, Amount, InitialValue, TTL}, Pid, Timeout).

-spec async_delete(binary()) ->
    {ok, reference()} | error().

async_delete(Key) ->
    async_delete(Key, ?DEFAULT_PID).

-spec async_delete(binary(), pid()) ->
    {ok, reference()} | error().

async_delete(Key, Pid) ->
    async_delete(Key, Pid, ?DEFAULT_TIMEOUT).

-spec async_delete(binary(), pid(), timeout()) ->
    {ok, reference()} | error().

async_delete(Key, Pid, Timeout) ->
    cast({delete, Key}, Pid, Timeout).

-spec async_flush() ->
    {ok, reference()} | error().

async_flush() ->
    async_flush(?DEFAULT_TTL).

-spec async_flush(non_neg_integer()) ->
    {ok, reference()} | error().

async_flush(TTL) ->
    async_flush(TTL, ?DEFAULT_PID).

-spec async_flush(non_neg_integer(), pid()) ->
    {ok, reference()} | error().

async_flush(TTL, Pid) ->
    async_flush(TTL, Pid, ?DEFAULT_TIMEOUT).

-spec async_flush(non_neg_integer(), pid(), timeout()) ->
    {ok, reference()} | error().

async_flush(TTL, Pid, Timeout) ->
    cast({flush, TTL}, Pid, Timeout).

-spec async_get(binary()) ->
    {ok, reference()} | error().

async_get(Key) ->
    async_get(Key, ?DEFAULT_PID).

-spec async_get(binary(), pid()) ->
    {ok, reference()} | error().

async_get(Key, Pid) ->
    async_get(Key, Pid, ?DEFAULT_TIMEOUT).

-spec async_get(binary(), pid(), timeout()) ->
    {ok, reference()} | error().

async_get(Key, Pid, Timeout) ->
    cast({get, Key}, Pid, Timeout).

-spec async_increment(binary()) ->
    {ok, reference()} | error().

async_increment(Key) ->
    async_increment(Key, ?DEFAULT_INCREMENT).

-spec async_increment(binary(), integer()) ->
    {ok, reference()} | error().

async_increment(Key, Amount) ->
    async_increment(Key, Amount, ?DEFAULT_INITIAL_VALUE).

-spec async_increment(binary(), integer(), integer()) ->
    {ok, reference()} | error().

async_increment(Key, Amount, InitialValue) ->
    async_increment(Key, Amount, InitialValue, ?DEFAULT_TTL).

-spec async_increment(binary(), integer(), integer(), non_neg_integer()) ->
    {ok, reference()} | error().

async_increment(Key, Amount, InitialValue, TTL) ->
    async_increment(Key, Amount, InitialValue, TTL, ?DEFAULT_PID).

-spec async_increment(binary(), integer(), integer(), non_neg_integer(),
        pid()) -> {ok, reference()} | error().

async_increment(Key, Amount, InitialValue, TTL, Pid) ->
    async_increment(Key, Amount, InitialValue, TTL, Pid, ?DEFAULT_TIMEOUT).

-spec async_increment(binary(), integer(), integer(), non_neg_integer(),
        pid(), timeout()) -> {ok, reference()} | error().

async_increment(Key, Amount, InitialValue, TTL, Pid, Timeout) ->
    cast({increment, Key, Amount, InitialValue, TTL}, Pid, Timeout).

-spec async_noop() ->
    {ok, reference()} | error().

async_noop() ->
    async_noop(?DEFAULT_PID).

-spec async_noop(pid()) ->
    {ok, reference()} | error().

async_noop(Pid) ->
    async_noop(Pid, ?DEFAULT_TIMEOUT).

-spec async_noop(pid(), timeout()) ->
    {ok, reference()} | error().

async_noop(Pid, Timeout) ->
    cast(noop, Pid, Timeout).

-spec async_quit() ->
    {ok, reference()} | error().

async_quit() ->
    async_quit(?DEFAULT_PID).

-spec async_quit(pid()) ->
    {ok, reference()} | error().

async_quit(Pid) ->
    async_quit(Pid, ?DEFAULT_TIMEOUT).

-spec async_quit(pid(), timeout()) ->
    {ok, reference()} | error().

async_quit(Pid, Timeout) ->
    cast(quit, Pid, Timeout).

-spec async_replace(binary(), binary()) ->
    {ok, reference()} | error().

async_replace(Key, Value) ->
    async_replace(Key, Value, ?DEFAULT_TTL).

-spec async_replace(binary(), binary(), non_neg_integer()) ->
    {ok, reference()} | error().

async_replace(Key, Value, TTL) ->
    async_replace(Key, Value, TTL, ?DEFAULT_PID).

-spec async_replace(binary(), binary(), non_neg_integer(), pid()) ->
    {ok, reference()} | error().

async_replace(Key, Value, TTL, Pid) ->
    async_replace(Key, Value, TTL, Pid, ?DEFAULT_TIMEOUT).

-spec async_replace(binary(), binary(), non_neg_integer(), pid(),
    timeout()) -> {ok, reference()} | error().

async_replace(Key, Value, TTL, Pid, Timeout) ->
    cast({replace, Key, Value, TTL}, Pid, Timeout).

-spec async_set(binary(), binary()) ->
    {ok, reference()} | error().

async_set(Key, Value) ->
    async_set(Key, Value, ?DEFAULT_TTL).

-spec async_set(binary(), binary(), non_neg_integer()) ->
    {ok, reference()} | error().

async_set(Key, Value, TTL) ->
    async_set(Key, Value, TTL, ?DEFAULT_PID).

-spec async_set(binary(), binary(), non_neg_integer(), pid()) ->
    {ok, reference()} | error().

async_set(Key, Value, TTL, Pid) ->
    async_set(Key, Value, TTL, Pid, ?DEFAULT_TIMEOUT).

-spec async_set(binary(), binary(), non_neg_integer(), pid(), timeout()) ->
    {ok, reference()} | error().

async_set(Key, Value, TTL, Pid, Timeout) ->
    cast({set, Key, Value, TTL}, Pid, Timeout).

-spec async_version() ->
    {ok, reference()} | error().

async_version() ->
    async_version(?DEFAULT_PID).

-spec async_version(pid()) ->
    {ok, reference()} | error().

async_version(Pid) ->
    async_version(Pid, ?DEFAULT_TIMEOUT).

-spec async_version(pid(), timeout()) ->
    {ok, reference()} | error().

async_version(Pid, Timeout) ->
    cast(version, Pid, Timeout).

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
    call({add, Key, Value, TTL}, Timeout).

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
    call({decrement, Key, Amount, InitialValue, TTL}, Timeout).

-spec delete(binary()) ->
    ok | error().

delete(Key) ->
    delete(Key, ?DEFAULT_TIMEOUT).

-spec delete(binary(), pos_integer()) ->
    ok | error().

delete(Key, Timeout) ->
    call({delete, Key}, Timeout).

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
    call({flush, TTL}, Timeout).

-spec get(binary()) ->
    {ok, binary()} | error().

get(Key) ->
    get(Key, ?DEFAULT_TIMEOUT).

-spec get(binary(), pos_integer()) ->
    {ok, binary()} | error().

get(Key, Timeout) ->
    call({get, Key}, Timeout).

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
    call({increment, Key, Amount, InitialValue, TTL}, Timeout).

-spec noop() ->
    ok | error().

noop() ->
    noop(?DEFAULT_TIMEOUT).

-spec noop(pos_integer()) ->
    ok | error().

noop(Timeout) ->
    call(noop, Timeout).

-spec quit() ->
    ok | error().

quit() ->
    quit(?DEFAULT_TIMEOUT).

-spec quit(pos_integer()) ->
    ok | error().

quit(Timeout) ->
    call(quit, Timeout).

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
    call({replace, Key, Value, TTL}, Timeout).

-spec response({ok, term()} | error()) ->
    {ok, term()} | error().

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
    call({set, Key, Value, TTL}, Timeout).

-spec version() ->
    {ok, binary()} | error().

version() ->
    version(?DEFAULT_TIMEOUT).

-spec version(pos_integer()) ->
    {ok, binary()} | error().

version(Timeout) ->
    call(version, Timeout).

%% private
call(Msg, Timeout) ->
    response(shackle:call(?APP, Msg, Timeout)).

cast(Msg, Pid, Timeout) ->
    shackle:cast(?APP, Msg, Pid, Timeout).
