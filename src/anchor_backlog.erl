-module(anchor_backlog).

-export([
    decrement/1,
    function/3,
    increment/2,
    new/1
]).

-define(KEY, backlog).

%% public
function(Tid, MaxBacklog, Fun) ->
    case increment(Tid, MaxBacklog) of
        Value when Value =< MaxBacklog ->
            Response = Fun(),
            decrement(Tid),
            Response;
        {error, tid_missing} ->
            {error, table_missing};
        _Value ->
            {error, queue_full}
    end.

%% private
decrement(Tid) ->
    safe_update_counter(Tid, {2, -1, 0, 0}).

increment(Tid, MaxBacklog) ->
    safe_update_counter(Tid, {2, 1, MaxBacklog + 1, MaxBacklog + 1}).

new(Tid) ->
    ets:new(Tid, [
        named_table,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    ets:insert(Tid, {?KEY, 0}).

safe_update_counter(Tid, UpdateOp) ->
    try ets:update_counter(Tid, ?KEY, UpdateOp)
    catch
        error:badarg ->
            {error, tid_missing}
    end.
