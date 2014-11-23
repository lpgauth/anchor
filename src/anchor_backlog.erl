-module(anchor_backlog).
-include("anchor.hrl").

-export([
    check/2,
    decrement/1,
    new/1
]).

-define(KEY, backlog).

%% public
-spec check(atom(), pos_integer()) -> boolean().
check(Tid, MaxBacklog) ->
    case increment(Tid, MaxBacklog) of
        [MaxBacklog, MaxBacklog] ->
            false;
        [_, Value] when Value =< MaxBacklog ->
            true;
        {error, tid_missing} ->
            false
    end.

decrement(Tid) ->
    safe_update_counter(Tid, {2, -1, 0, 0}).

-spec new(atom()) -> ok.
new(Tid) ->
    Tid = ets:new(Tid, [
        named_table,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    true = ets:insert(Tid, {?KEY, 0}),
    ok.

%% private
increment(Tid, MaxBacklog) ->
    safe_update_counter(Tid, [{2, 0}, {2, 1, MaxBacklog, MaxBacklog}]).

safe_update_counter(Tid, UpdateOp) ->
    try ets:update_counter(Tid, ?KEY, UpdateOp)
    catch
        error:badarg ->
            {error, tid_missing}
    end.
