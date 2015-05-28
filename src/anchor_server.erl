-module(anchor_server).
-include("anchor.hrl").

-export([
    init/2,
    start_link/1
]).

-record(state, {
    buffer        = <<>>,
    connect_retry = 0,
    ip            = undefined,
    item          = undefined,
    name          = undefined,
    port          = undefined,
    reconnect     = undefined,
    requests      = 0,
    response      = undefined,
    socket        = undefined,
    timer         = undefined
}).

%% public
-spec init(pid(), atom()) -> no_return().

init(Parent, Name) ->
    register(Name, self()),
    proc_lib:init_ack(Parent, {ok, self()}),

    anchor_backlog:new(Name),
    self() ! ?CONNECT_RETRY_MSG,

    loop(#state {
        name = Name,
        ip = application:get_env(?APP, ip, ?DEFAULT_IP),
        port = application:get_env(?APP, port, ?DEFAULT_PORT),
        reconnect = application:get_env(?APP, reconnect, ?DEFAULT_RECONNECT)
    }).

-spec start_link(atom()) -> {ok, pid()}.

start_link(Name) ->
    proc_lib:start_link(?MODULE, init, [self(), Name]).

%% private
connect_retry(#state {reconnect = false} = State) ->
    {ok, State#state {
        socket = undefined
    }};
connect_retry(#state {
        connect_retry = ConnectRetry
    } = State) ->

    {ok, State#state {
        connect_retry = ConnectRetry + 1,
        socket = undefined,
        timer = erlang:send_after(timeout(State), self(), ?CONNECT_RETRY_MSG)
    }}.

decode_data(<<>>, State) ->
    {ok, State};
decode_data(Data, #state {
        item = undefined,
        name = Name
    } = State) ->

    {ok, Rest, Resp} = anchor_protocol:decode(Data),
    case Resp#response.state of
        complete ->
            ReqId = Resp#response.opaque,
            {Ref, From} = anchor_queue:out(Name, ReqId),
            Reply = reply(Resp),
            reply(Name, Ref, From, Reply),

            decode_data(Rest, State#state {
                buffer = <<>>
            });
        _ ->
            {ok, State#state {
                buffer = Rest
            }}
    end;
decode_data(Data, #state {
        name = Name,
        response = Resp
    } = State) ->

    {ok, Rest, Resp2} = anchor_protocol:decode(Data, Resp),

    case Resp2#response.state of
        complete ->
            ReqId = Resp#response.opaque,
            {Ref, From} = anchor_queue:out(Name, ReqId),
            Reply = reply(Resp2),
            reply(Name, Ref, From, Reply),

            decode_data(Rest, State#state {
                buffer = <<>>,
                response = undefined
            });
        _ ->
            {ok, State#state {
                buffer = Rest,
                response = Resp2
            }}
    end.

handle_msg(?CONNECT_RETRY_MSG, #state {
        ip = Ip,
        port = Port
    } = State) ->

    Opts = [
        binary,
        {active, true},
        {packet, raw},
        {send_timeout, ?DEFAULT_SEND_TIMEOUT},
        {send_timeout_close, true}
    ],

    case gen_tcp:connect(Ip, Port, Opts) of
        {ok, Socket} ->
            {ok, State#state {
                connect_retry = 0,
                socket = Socket
            }};
        {error, Reason} ->
            anchor_utils:warning_msg("tcp connect error: ~p", [Reason]),
            connect_retry(State)
    end;
handle_msg({call, Ref, From, _Msg}, #state {
        name = Name,
        socket = undefined
    } = State) ->

    reply(Name, Ref, From, {error, no_socket}),
    {ok, State};
handle_msg({call, Ref, From, Msg}, #state {
        name = Name,
        requests = Requests,
        socket = Socket
    } = State) ->

    ReqId = request_id(Requests),
    {ok, Packet} = anchor_protocol:encode(ReqId, Msg),
    case gen_tcp:send(Socket, Packet) of
        ok ->
            anchor_queue:in(Name, ReqId, {Ref, From}),

            {ok, State#state {
                requests = Requests + 1
            }};
        {error, Reason} ->
            anchor_utils:error_msg("tcp send error: ~p", [Reason]),
            reply(Name, Ref, From, {error, Reason}),
            gen_tcp:close(Socket),
            tcp_close(State)
    end;
handle_msg({tcp, Socket, Data}, #state {
        socket = Socket,
        buffer = Buffer
    } = State) ->

    Data2 = <<Buffer/binary, Data/binary>>,
    decode_data(Data2, State);
handle_msg({tcp_closed, Socket}, #state {
        socket = Socket
    } = State) ->

    anchor_utils:warning_msg("tcp closed", []),
    tcp_close(State);
handle_msg({tcp_error, Socket, Reason}, #state {
        socket = Socket
    } = State) ->

    anchor_utils:warning_msg("tcp error: ~p", [Reason]),
    gen_tcp:close(Socket),
    tcp_close(State).

loop(State) ->
    receive Msg ->
        {ok, State2} = handle_msg(Msg, State),
        loop(State2)
    end.

reply(#response {status = Status} = Response) ->
    case status(Status) of
        ok ->
            return(Response);
        Reason ->
            {error, Reason}
    end.

reply(Name, Ref, From, Msg) ->
    anchor_backlog:decrement(Name),
    From ! {?APP, Ref, Msg}.

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

tcp_close(#state {name = Name} = State) ->
    Msg = {error, tcp_closed},
    Items = anchor_queue:empty(Name),
    [reply(Name, Ref, From, Msg) || {Ref, From, _} <- Items],
    connect_retry(State).

timeout(#state {
        connect_retry = ConnectRetry
    }) when ConnectRetry > 10 ->

    ?DEFAULT_CONNECT_RETRY * 10;
timeout(#state {
        connect_retry = ConnectRetry
    }) ->

    ?DEFAULT_CONNECT_RETRY * ConnectRetry.
