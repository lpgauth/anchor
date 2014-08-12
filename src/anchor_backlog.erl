-module(anchor_backlog).
-include("anchor.hrl").

-export([
    function/3,
    new/1
]).


%% public
-spec function(atom(), pos_integer(), fun()) -> term() | {error, atom()}.
function(TableId, MaxBacklog, Fun) ->
    Result = case increment(TableId) of
        Value when Value =< MaxBacklog ->
            Response = try Fun()
            catch
                _Error:_Reason ->
                    {error, badarg}
            end,
            decrement(TableId),
            Response;
        {error, Reason} ->
            {error, Reason};
        _Value ->
            decrement(TableId),
            {error, backlog_full}
    end,
    Result.

-spec new(atom()) -> ok.
new(TableId) ->
    TableId = ets:new(TableId, [
        named_table,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    true = ets:insert(TableId, {?BACKLOG_KEY, 0}),
    ok.

%% private
decrement(TableId) ->
    safe_update_counter(TableId, {2, -1, 0, 0}).

increment(TableId) ->
    safe_update_counter(TableId, {2, 1}).

safe_update_counter(TableId, UpdateOp) ->
    try ets:update_counter(TableId, ?BACKLOG_KEY, UpdateOp)
    catch
        error:badarg ->
            {error, table_missing}
    end.
