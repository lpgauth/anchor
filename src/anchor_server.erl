-module(anchor_server).
-include("anchor.hrl").

-export([
    call/2,
    start_link/0,
    init/1
]).

-record(state, {
    ip          = undefined,
    port        = undefined,
    socket      = undefined,
    queue       = queue:new(),
    req_counter = 0,
    buffer      = <<>>,
    from        = undefined,
    response    = undefined
}).

-define(BACKLOG_MAX, 4096).
-define(BACKLOG_TID, anchor_backlog).

%% public
-spec call(term(), pos_integer()) -> {ok, term()} | {error, atom()}.
call(Msg, Timeout) ->
    Pid = self(),
    backpressure:function(?BACKLOG_TID, ?BACKLOG_MAX, fun () ->
        ?MODULE ! {call, Pid, Msg},
        receive
            Response ->
                Response
        after Timeout ->
            {error, timeout}
        end
    end).

-spec start_link() -> {ok, pid()}.
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

-spec init(pid()) -> no_return().
init(Parent) ->
    register(?MODULE, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    backpressure:new(?BACKLOG_TID),
    self() ! newsocket,

    loop(#state {
        ip = application:get_env(?APP, ip, ?DEFAULT_IP),
        port = application:get_env(?APP, port, ?DEFAULT_PORT)
    }).

%% private
loop(State) ->
    receive
        Msg ->
            {ok, State2} = handle_msg(Msg, State),
            loop(State2)
    end.

handle_msg({call, From, _Msg}, #state {
        socket = undefined
    } = State) ->

    reply(From, {error, no_socket}),
    {ok, State};
handle_msg({call, From, Msg}, #state {
        socket = Socket,
        queue = Queue,
        req_counter = ReqCounter
    } = State) ->

    ReqId = req_id(ReqCounter),
    {ok, Packet} = anchor_protocol:encode(ReqId, Msg),
    case gen_tcp:send(Socket, Packet) of
        {error, Reason} ->
            error_msg("tcp send error: ~p", [Reason]),
            gen_tcp:close(Socket),
            tcp_close(Queue),
            reply(From, {error, Reason}),

            {ok, State#state {
                socket = undefined,
                queue = queue:new(),
                buffer = <<>>,
                from = undefined,
                response = undefined
            }};
        ok ->
            {ok, State2} = queue_in({ReqId, From}, State),

            {ok, State2#state {
                req_counter = ReqCounter + 1
            }}
    end;
handle_msg(newsocket, #state {
        ip = Ip,
        port = Port
    } = State) ->

    Opts = [
        binary,
        {active, once},
        {nodelay, true},
        {packet, raw},
        {send_timeout, ?DEFAULT_SEND_TIMEOUT},
        {send_timeout_close, true}
    ],
    case gen_tcp:connect(Ip, Port, Opts) of
        {ok, Socket} ->
            {ok, State#state {
                socket = Socket
            }};
        {error, Reason} ->
            error_msg("tcp connect error: ~p", [Reason]),
            erlang:send_after(?DEFAULT_RECONNECT, self(), newsocket),
            {ok, State}
    end;
handle_msg({tcp, Socket, Data}, #state {
        socket = Socket,
        buffer = Buffer
    } = State) ->

    inet:setopts(Socket, [{active, once}]),
    decode_data(<<Buffer/binary, Data/binary>>, State);
handle_msg({tcp_closed, Socket}, #state {
        socket = Socket,
        queue = Queue
    } = State) ->

    tcp_close(Queue),
    {ok, State#state {
        socket = undefined,
        queue = queue:new(),
        buffer = <<>>,
        from = undefined,
        response = undefined
    }};
handle_msg({tcp_error, Socket, Reason}, #state {
        socket = Socket
    } = State) ->

    error_msg("tcp error: ~p", [Reason]),
    {ok, State}.

%% private
decode_data(<<>>, State) ->
    {ok, State};
decode_data(Data, #state {
        from = undefined
    } = State) ->

    case queue_out(State) of
        {ok, {ReqId, From}, State2} ->
            {ok, Rest, #response {
                state = Parsing
            } = Resp} = anchor_protocol:decode(ReqId, Data),

            case Parsing of
                complete ->
                    reply(From, {ok, Resp}),
                    decode_data(Rest, State2#state {
                        buffer = <<>>
                    });
                _ ->
                    {ok, State2#state {
                        buffer = Rest,
                        from = From,
                        response = Resp#response {
                            opaque = ReqId
                        }
                    }}
            end;
        {error, empty} ->
            warning_msg("empty queue", []),
            {ok, State}
    end;
decode_data(Data, #state {
        from = From,
        response = #response {
            opaque = ReqId
        } = Resp
    } = State) ->

    {ok, Rest, #response {
        state = Parsing
    } = Resp2} = anchor_protocol:decode(ReqId, Data, Resp),

    case Parsing of
        complete ->
            reply(From, {ok, Resp2}),
            decode_data(Rest, State#state {
                from = undefined,
                buffer = <<>>,
                response = undefined
            });
        _ ->
            {ok, State#state {
                buffer = Rest,
                response = Resp2
            }}
    end.

queue_in(Item, #state {queue = Queue} = State) ->
    {ok, State#state {
        queue = queue:in(Item, Queue)
    }}.

queue_out(#state {
        queue = Queue
    } = State) ->

    case queue:out(Queue) of
        {{value, Item}, Queue2} ->
            {ok, Item , State#state {
                queue = Queue2
            }};
        {empty, Queue} ->
            {error, emtpy}
    end.

reply(From, Msg) ->
    From ! Msg.

reply_all(Queue, Msg) ->
    [reply(From, Msg) || From <- queue:to_list(Queue)].

req_id(N) ->
    (N + 1) rem ?MAX_32_BIT_INT.

tcp_close(Queue) ->
    reply_all(Queue, {error, tcp_closed}),
    erlang:send_after(?DEFAULT_RECONNECT, self(), newsocket).

%% logging
error_msg(Format, Data) ->
    error_logger:error_msg(Format, Data).

warning_msg(Format, Data) ->
    error_logger:warning_msg(Format, Data).
