-module(anchor_backlog).

-export([
    function/3,
    new/1
]).

-define(KEY, backlog).

%% public
-spec function(atom(), pos_integer(), fun()) -> term() | {error, atom()}.
function(Tid, MaxBacklog, Fun) ->
    case increment(Tid, MaxBacklog) of
        Value when Value =< MaxBacklog ->
            Response = Fun(),
            decrement(Tid),
            Response;
        {error, Reason} ->
            {error, Reason};
        _Value ->
            {error, backlog_full}
    end.

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
decrement(Tid) ->
    safe_update_counter(Tid, {2, -1, 0, 0}).

increment(Tid, MaxBacklog) ->
    safe_update_counter(Tid, {2, 1, MaxBacklog + 1, MaxBacklog + 1}).

safe_update_counter(Tid, UpdateOp) ->
    try ets:update_counter(Tid, ?KEY, UpdateOp)
    catch
        error:badarg ->
            {error, table_missing}
    end.
