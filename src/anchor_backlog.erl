-module(anchor_backlog).
-include("anchor.hrl").

-export([
    apply/4,
    new/2
]).

%% public
-spec apply(atom(), reference(), pos_integer(), fun()) -> term() | {error, atom()}.
apply(TableId, Ref, MaxSize, Fun) ->
    maybe_apply(TableId, Ref, MaxSize, Fun, ?RETRY).

-spec new(atom(), pos_integer()) -> ok.
new(TableId, MaxSize) ->
    TableOpts = [named_table, public, {write_concurrency, true}],
    TableId = ets:new(TableId, TableOpts),
    [reset(TableId, Key) || Key <- lists:seq(1, MaxSize)],
    ok.

%% private
increment(TableId, Key) ->
    safe_update_counter(TableId, Key, {2, 1}).

maybe_apply(_TableId, _Ref, _MaxSize, _Fun, 0) ->
    {error, backlog_full};
maybe_apply(TableId, Ref, MaxSize, Fun, Retry) ->
    Key = random(Ref, MaxSize),
    case increment(TableId, Key) of
        {ok, 1} ->
            Response = try Fun()
            catch
                _Error:_Reason ->
                    {error, badarg}
            end,
            reset(TableId, Key),
            Response;
        {ok, _N} ->
            maybe_apply(TableId, Ref, MaxSize, Fun, Retry - 1);
        {error, Reason} ->
            {error, Reason}
    end.

random(Ref, Number) ->
    erlang:phash2(Ref, Number) + 1.

reset(TableId, Key) ->
    true = ets:insert(TableId, {Key, 0}).

safe_update_counter(TableId, Key, UpdateOp) ->
    try ets:update_counter(TableId, Key, UpdateOp) of
        N ->
            {ok, N}
    catch
        error:badarg ->
            {error, table_missing}
    end.
