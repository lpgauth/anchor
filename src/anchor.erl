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
    add_(Key, Value, TTL, Timeout).

-spec delete(binary()) -> ok | {error, atom()}.
delete(Key) ->
    delete(Key, ?DEFAULT_TIMEOUT).

-spec delete(binary(), pos_integer()) -> ok | {error, atom()}.
delete(Key, Timeout) ->
    delete_(Key, Timeout).

-spec get(binary()) -> {ok, binary()} | {error, atom()}.
get(Key) ->
    get(Key, ?DEFAULT_TIMEOUT).

-spec get(binary(), pos_integer()) -> {ok, binary()} | {error, atom()}.
get(Key, Timeout) ->
    get_(Key, Timeout).

-spec replace(binary(), binary()) -> ok | {error, atom()}.
replace(Key, Value) ->
    replace(Key, Value, ?DEFAULT_TTL).

-spec replace(binary(), binary(), non_neg_integer()) -> ok | {error, atom()}.
replace(Key, Value, TTL) ->
    replace(Key, Value, TTL, ?DEFAULT_TIMEOUT).

-spec replace(binary(), binary(), non_neg_integer(), pos_integer()) -> ok | {error, atom()}.
replace(Key, Value, TTL, Timeout) ->
    replace_(Key, Value, TTL, Timeout).

-spec set(binary(), binary()) -> ok | {error, atom()}.
set(Key, Value) ->
    set(Key, Value, ?DEFAULT_TTL).

-spec set(binary(), binary(), non_neg_integer()) -> ok | {error, atom()}.
set(Key, Value, TTL) ->
    set(Key, Value, TTL, ?DEFAULT_TIMEOUT).

-spec set(binary(), binary(), non_neg_integer(), pos_integer()) -> ok | {error, atom()}.
set(Key, Value, TTL, Timeout) ->
    set_(Key, Value, TTL, Timeout).

%% private
add_(Key, Value, TTL, Timeout) ->
    case call({add, Key, Value, TTL}, Timeout) of
        {ok, #response {status = Status}} ->
            case Status of
                0 ->
                    ok;
                _ ->
                    {error, key_exists}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

call(Msg, Timeout) ->
    try gen_server:call(?SERVER, Msg, Timeout) of
        Reply ->
            Reply
    catch
        exit:{noproc, _} ->
            {error, not_started};
        exit:{timeout, _} ->
            {error, timeout}
    end.

delete_(Key, Timeout) ->
    case call({delete, Key}, Timeout) of
        {ok, _Resp} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

get_(Key, Timeout) ->
    case call({get, Key}, Timeout) of
        {ok, #response {status = Status, value = Value}} ->
            case Status of
                0 ->
                    {ok, Value};
                _ ->
                    {error, key_not_found}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

replace_(Key, Value, TTL, Timeout) ->
    case call({replace, Key, Value, TTL}, Timeout) of
        {ok, #response {status = Status}} ->
            case Status of
                0 ->
                    ok;
                _ ->
                    {error, key_not_found}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

set_(Key, Value, TTL, Timeout) ->
    case call({set, Key, Value, TTL}, Timeout) of
        {ok, _Resp} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.