-module(anchor_server).
-include("anchor.hrl").

-export([
    start_link/0
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(state, {
    ip          = undefined,
    port        = undefined,
    socket      = undefined,
    queue       = queue:new(),
    queue_size  = 0,
    req_counter = 0,
    buffer      = <<>>,
    from        = undefined,
    response    = undefined
}).

%% public
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    self() ! newsocket,

    {ok, #state {
        ip = application:get_env(?APP, ip, ?DEFAULT_IP),
        port = application:get_env(?APP, port, ?DEFAULT_PORT)
    }}.

handle_call(_Request, _From, #state{
        socket = undefined
    } = State) ->

    Reply = {error, no_socket},
    {reply, Reply, State};
handle_call(_Request, _From, #state{
        queue_size = QueueSize
    } = State) when QueueSize > ?MAX_QUEUE_SIZE ->

    Reply = {error, queue_full},
    {reply, Reply, State};
handle_call(Request, From, #state {
        socket = Socket,
        queue = Queue,
        req_counter = ReqCounter
    } = State) ->

    ReqId = req_id(ReqCounter),
    {ok, Packet} = anchor_protocol:encode(ReqId, Request),
    case gen_tcp:send(Socket, Packet) of
        {error, Reason} ->
            error_msg("tcp send error: ~p", [Reason]),
            gen_tcp:close(Socket),
            tcp_close(Queue),
            Reply = {error, Reason},
            {reply, Reply, State#state {
                socket = undefined,
                queue = queue:new(),
                buffer = <<>>,
                from = undefined,
                response = undefined
            }};
        ok ->
            {ok, State2} = queue_in({ReqId, From}, State),
            {noreply, State2#state {
                req_counter = ReqCounter + 1
            }}
    end;
handle_call(Call, _From, State) ->
    warning_msg("unexpected call: ~p~n", [Call]),
    {noreply, State}.

handle_cast(Cast, State) ->
    warning_msg("unexpected cast: ~p~n", [Cast]),
    {noreply, State}.

handle_info(newsocket, #state {
        ip = Ip,
        port = Port
    } = State) ->

    Opts = [binary, {active, once}, {nodelay, true}, {packet, raw}],
    case gen_tcp:connect(Ip, Port, Opts) of
        {ok, Socket} ->
            {noreply, State#state {
                socket = Socket
            }};
        {error, Reason} ->
            error_msg("tcp connect error: ~p", [Reason]),
            erlang:send_after(?DEFAULT_RECONNECT, self(), newsocket),
            {noreply, State}
    end;
handle_info({tcp, Socket, Data}, #state {
        socket = Socket,
        buffer = Buffer
    } = State) ->

    inet:setopts(Socket, [{active, once}]),
    decode_data(<<Buffer/binary, Data/binary>>, State);
handle_info({tcp_closed, Socket}, #state {
        socket = Socket,
        queue = Queue
    } = State) ->

    tcp_close(Queue),
    {noreply, State#state {
        socket = undefined,
        queue = queue:new(),
        buffer = <<>>,
        from = undefined,
        response = undefined
    }};
handle_info({tcp_error, Socket, Reason}, #state {
        socket = Socket
    } = State) ->

    error_msg("tcp error: ~p", [Reason]),
    {noreply, State};
handle_info(Info, State) ->
    warning_msg("unexpected info: ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% private
decode_data(<<>>, State) ->
    {noreply, State};
decode_data(Data, #state {
        queue = Queue,
        from = undefined
    } = State) ->

    case queue_out(Queue) of
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
                    {noreply, State2#state {
                        buffer = Rest,
                        from = From,
                        response = Resp#response {
                            opaque = ReqId
                        }
                    }}
            end;
        {error, empty} ->
            warning_msg("empty queue", []),
            {noreply, State}
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
            {noreply, State#state {
                buffer = Rest,
                response = Resp2
            }}
    end.

queue_in(Item, #state {
        queue = Queue,
        queue_size = QueueSize
    } = State) ->

    {ok, State#state {
        queue = queue:in(Item, Queue),
        queue_size = QueueSize + 1
    }}.

queue_out(#state {
        queue = Queue,
        queue_size = QueueSize
    } = State) ->

    case queue:out(Queue) of
        {{value, Item}, Queue2} ->
            {ok, Item , State#state {
                queue = Queue2,
                queue_size = QueueSize - 1
            }};
        {empty, Queue} ->
            {error, emtpy}
    end.

reply(From, Msg) ->
    gen_server:reply(From, Msg).

reply_all(Queue, Msg) ->
    [gen_server:reply(From, Msg) || From <- queue:to_list(Queue)].

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
