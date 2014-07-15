-module(anchor).
-include("anchor.hrl").

-export([
    get/1,
    get/2,
    set/2,
    set/3,
    set/4
]).

%% public
-spec get(binary()) -> {ok, binary()} | {error, atom()}.
get(Key) ->
    get(Key, ?DEFAULT_TIMEOUT).

-spec get(binary(), pos_integer()) -> {ok, binary()} | {error, atom()}.
get(Key, Timeout) ->
    case call({get, Key}, Timeout) of
        {ok, #response {status = Status, value = Value}} ->
            case Status of
                0 ->
                    {ok, Value};
                _ ->
                    {error, not_found}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec set(binary(), binary()) -> ok | {error, atom()}.
set(Key, Value) ->
    set(Key, Value, ?DEFAULT_TTL).

-spec set(binary(), binary(), non_neg_integer()) -> ok | {error, atom()}.
set(Key, Value, TTL) ->
    set(Key, Value, TTL, ?DEFAULT_TIMEOUT).

-spec set(binary(), binary(), non_neg_integer(), pos_integer()) -> ok | {error, atom()}.
set(Key, Value, TTL, Timeout) ->
    case call({set, Key, Value, TTL}, Timeout) of
        {ok, _Resp} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% private
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
