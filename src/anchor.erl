-module(anchor).
-include("anchor.hrl").

-export([
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
    replace/2,
    replace/3,
    replace/4,
    replace/5,
    set/2,
    set/3,
    set/4,
    set/5,
    version/0,
    version/1,
    version/2
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
    add(Key, Value, TTL, Timeout, []).

-spec add(binary(), binary(), non_neg_integer(), pos_integer(), options()) -> ok | error().
add(Key, Value, TTL, Timeout, Options) ->
    call({add, Key, Value, TTL}, Timeout, Options).

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
    decrement(Key, Amount, InitialValue, TTL, Timeout, []).

-spec decrement(binary(), integer(), integer(), non_neg_integer(), pos_integer(), options()) -> {ok, integer()} | error().
decrement(Key, Amount, InitialValue, TTL, Timeout, Options) ->
    call({decrement, Key, Amount, InitialValue, TTL}, Timeout, Options).

-spec delete(binary()) -> ok | error().
delete(Key) ->
    delete(Key, ?DEFAULT_TIMEOUT).

-spec delete(binary(), pos_integer()) -> ok | error().
delete(Key, Timeout) ->
    delete(Key, Timeout, []).

-spec delete(binary(), pos_integer(), options()) -> ok | error().
delete(Key, Timeout, Options) ->
    call({delete, Key}, Timeout, Options).

-spec flush() -> ok | error().
flush() ->
    flush(?DEFAULT_TTL).

-spec flush(non_neg_integer()) -> ok | error().
flush(TTL) ->
    flush(TTL, ?DEFAULT_TIMEOUT).

-spec flush(non_neg_integer(), pos_integer()) -> ok | error().
flush(TTL, Timeout) ->
    flush(TTL, Timeout, []).

-spec flush(non_neg_integer(), pos_integer(), options()) -> ok | error().
flush(TTL, Timeout, Options) ->
    call({flush, TTL}, Timeout, Options).

-spec quit() -> ok | error().
quit() ->
    quit(?DEFAULT_TIMEOUT).

-spec quit(pos_integer()) -> ok | error().
quit(Timeout) ->
    quit(Timeout, []).

-spec quit(pos_integer(), options()) -> ok | error().
quit(Timeout, Options) ->
    call(quit, Timeout, Options).

-spec get(binary()) -> {ok, binary()} | error().
get(Key) ->
    get(Key, ?DEFAULT_TIMEOUT).

-spec get(binary(), pos_integer()) -> {ok, binary()} | error().
get(Key, Timeout) ->
    get(Key, Timeout, []).

-spec get(binary(), pos_integer(), options()) -> {ok, binary()} | error().
get(Key, Timeout, Options) ->
    call({get, Key}, Timeout, Options).

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
    increment(Key, Amount, InitialValue, TTL, Timeout, []).

-spec increment(binary(), integer(), integer(), non_neg_integer(), pos_integer(), options()) -> {ok, integer()} | error().
increment(Key, Amount, InitialValue, TTL, Timeout, Options) ->
    Msg = {increment, Key, Amount, InitialValue, TTL},
    call(Msg, Timeout, Options).

-spec noop() -> ok | error().
noop() ->
    noop(?DEFAULT_TIMEOUT).

-spec noop(pos_integer()) -> ok | error().
noop(Timeout) ->
    noop(Timeout, []).

-spec noop(pos_integer(), options()) -> ok | error().
noop(Timeout, Options) ->
    call(noop, Timeout, Options).

-spec replace(binary(), binary()) -> ok | error().
replace(Key, Value) ->
    replace(Key, Value, ?DEFAULT_TTL).

-spec replace(binary(), binary(), non_neg_integer()) -> ok | error().
replace(Key, Value, TTL) ->
    replace(Key, Value, TTL, ?DEFAULT_TIMEOUT).

-spec replace(binary(), binary(), non_neg_integer(), pos_integer()) -> ok | error().
replace(Key, Value, TTL, Timeout) ->
    replace(Key, Value, TTL, Timeout, []).

-spec replace(binary(), binary(), non_neg_integer(), pos_integer(), options()) -> ok | error().
replace(Key, Value, TTL, Timeout, Options) ->
    call({replace, Key, Value, TTL}, Timeout, Options).

-spec set(binary(), binary()) -> ok | error().
set(Key, Value) ->
    set(Key, Value, ?DEFAULT_TTL).

-spec set(binary(), binary(), non_neg_integer()) -> ok | error().
set(Key, Value, TTL) ->
    set(Key, Value, TTL, ?DEFAULT_TIMEOUT).

-spec set(binary(), binary(), non_neg_integer(), pos_integer()) -> ok | error().
set(Key, Value, TTL, Timeout) ->
    set(Key, Value, TTL, Timeout, []).

-spec set(binary(), binary(), non_neg_integer(), pos_integer(), options()) -> ok | error().
set(Key, Value, TTL, Timeout, Options) ->
    call({set, Key, Value, TTL}, Timeout, Options).

-spec version() -> {ok, binary()} | error().
version() ->
    version(?DEFAULT_TIMEOUT).

-spec version(pos_integer()) -> {ok, binary()} | error().
version(Timeout) ->
    version(Timeout, []).

-spec version(pos_integer(), options()) -> {ok, binary()} | error().
version(Timeout, Options) ->
    call(version, Timeout, Options).

%% private
call(Msg, _Timeout, [{async, Pid}]) ->
    anchor_server:async_call(Msg, Pid);
call(Msg, Timeout, _Options) ->
    anchor_server:call(Msg, Timeout).
