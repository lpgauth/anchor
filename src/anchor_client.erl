-module(anchor_client).
-include("anchor.hrl").

-behavior(shackle_client).
-export([
    init/0,
    after_connect/2,
    handle_cast/2,
    handle_data/2,
    terminate/1
]).

-record(state, {
    buffer   = <<>>,
    item     = undefined,
    requests = 0,
    response = undefined
}).

%% shackle_server callbacks
init() ->
    Ip = application:get_env(?APP, ip, ?DEFAULT_IP),
    Port = application:get_env(?APP, port, ?DEFAULT_PORT),
    Reconnect = application:get_env(?APP, reconnect, ?DEFAULT_RECONNECT),

    {ok, [
        {ip, Ip},
        {port, Port},
        {reconnect, Reconnect},
        {state, #state {}}
    ]}.

after_connect(_Socket, State) ->
    {ok, State}.

handle_cast(Request, #state {
        requests = Requests
    } = State) ->

    RequestId = request_id(Requests),
    {ok, Data} = anchor_protocol:encode(RequestId, Request),
    {ok, RequestId, Data, State#state {
        requests = Requests + 1
    }}.

handle_data(Data, #state {
        buffer = Buffer
    } = State) ->

    Data2 = <<Buffer/binary, Data/binary>>,
    decode_data(Data2, [], State).

terminate(_State) ->
    ok.

%% private
decode_data(<<>>, Replies, State) ->
    {ok, Replies, State};
decode_data(Data, Replies, #state {
        response = Resp
    } = State) ->

    {ok, Rest, Resp2} = anchor_protocol:decode(Data, Resp),
    case Resp2#response.state of
        complete ->
            ReqId = Resp2#response.opaque,
            Reply = reply(Resp2),
            decode_data(Rest, [{ReqId, Reply} | Replies], State#state {
                buffer = <<>>,
                response = undefined
            });
        _ ->
            {ok, Replies, State#state {
                buffer = Rest,
                response = Resp2
            }}
    end.

reply(#response {status = Status} = Response) ->
    case status(Status) of
        ok ->
            return(Response);
        Reason ->
            {error, Reason}
    end.

request_id(N) ->
    (N + 1) rem ?MAX_32_BIT_INT.

return(#response {op_code = ?OP_ADD}) -> ok;
return(#response {op_code = ?OP_DECREMENT, value = Value}) ->
    {ok, binary:decode_unsigned(Value)};
return(#response {op_code = ?OP_DELETE}) -> ok;
return(#response {op_code = ?OP_FLUSH}) -> ok;
return(#response {op_code = ?OP_GET, value = Value}) ->
    {ok, Value};
return(#response {op_code = ?OP_INCREMENT, value = Value}) ->
    {ok, binary:decode_unsigned(Value)};
return(#response {op_code = ?OP_NOOP}) -> ok;
return(#response {op_code = ?OP_QUIT}) -> ok;
return(#response {op_code = ?OP_REPLACE}) -> ok;
return(#response {op_code = ?OP_SET}) -> ok;
return(#response {op_code = ?OP_VERSION, value = Value}) ->
    {ok, Value}.

status(?STAT_AUTH_CONTINUE) -> auth_continue;
status(?STAT_AUTH_ERROR) -> auth_error;
status(?STAT_BUSY) -> busy;
status(?STAT_INCR_NON_NUMERIC) -> incr_non_numeric;
status(?STAT_INTERNAL_ERROR) -> internal_error;
status(?STAT_INVALID_ARGS) -> invalid_args;
status(?STAT_ITEM_NOT_STORED) -> item_not_stored;
status(?STAT_KEY_EXISTS) -> key_exists;
status(?STAT_KEY_NOT_FOUND) -> key_not_found;
status(?STAT_NOT_SUPPORTED) -> not_supported;
status(?STAT_OK) -> ok;
status(?STAT_OUT_OF_MEMORY) -> out_of_memory;
status(?STAT_TEMP_FAILURE) -> temp_failure;
status(?STAT_UNKNOWN_COMMAND) -> unknown_command;
status(?STAT_VALUE_TOO_LARGE) -> value_too_large;
status(?STAT_VBUCKET_ERROR) -> vbucket_error.
