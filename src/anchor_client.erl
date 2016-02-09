-module(anchor_client).
-include("anchor.hrl").

-compile(inline).
-compile({inline_size, 512}).

-behavior(shackle_client).
-export([
    options/0,
    init/0,
    setup/2,
    handle_request/2,
    handle_data/2,
    terminate/1
]).

-record(state, {
    buffer   = <<>>      :: binary(),
    requests = 0         :: non_neg_integer(),
    response = undefined :: response() | undefined
}).

-type state() :: #state {}.

%% shackle_server callbacks
-spec options() ->
    {ok, shackle:client_options()}.

options() ->
    Ip = ?GET_ENV(ip, ?DEFAULT_IP),
    Port = ?GET_ENV(port, ?DEFAULT_PORT),
    Reconnect = ?GET_ENV(reconnect, ?DEFAULT_RECONNECT),
    ReconnectTimeMax = ?GET_ENV(reconnect_time_max, ?DEFAULT_RECONNECT_MAX),
    ReconnectTimeMin = ?GET_ENV(reconnect_time_min, ?DEFAULT_RECONNECT_MIN),

    {ok, [
        {ip, Ip},
        {port, Port},
        {reconnect, Reconnect},
        {reconnect_time_max, ReconnectTimeMax},
        {reconnect_time_min, ReconnectTimeMin},
        {socket_options, [
            binary,
            {packet, raw},
            {send_timeout, 50},
            {send_timeout_close, true}
        ]}
    ]}.

-spec init() ->
    {ok, state()}.

init() ->
    {ok, #state {}}.

-spec setup(inet:socket(), state()) ->
    {ok, state()}.

setup(_Socket, State) ->
    {ok, State}.

-spec handle_request(term(), state()) ->
    {ok, pos_integer(), binary(), state()}.

handle_request(Request, #state {
        requests = Requests
    } = State) ->

    RequestId = request_id(Requests),
    {ok, Data} = anchor_protocol:encode(RequestId, Request),

    {ok, RequestId, Data, State#state {
        requests = Requests + 1
    }}.

-spec handle_data(binary(), state()) ->
    {ok, [{pos_integer(), term()}], state()}.

handle_data(Data, #state {
        buffer = Buffer
    } = State) ->

    Data2 = <<Buffer/binary, Data/binary>>,
    decode_data(Data2, [], State).

-spec terminate(state()) -> ok.

terminate(_State) ->
    ok.

%% private
decode_data(<<>>, Replies, State) ->
    {ok, Replies, State};
decode_data(Data, Replies, #state {
        response = Response
    } = State) ->

    {ok, Rest, Response2} = anchor_protocol:decode(Data, Response),
    case Response2#response.state of
        complete ->
            ReqId = Response2#response.opaque,
            Response3 = anchor_response:format(Response2),
            decode_data(Rest, [{ReqId, Response3} | Replies], State#state {
                buffer = <<>>,
                response = undefined
            });
        _ ->
            {ok, Replies, State#state {
                buffer = Rest,
                response = Response2
            }}
    end.

request_id(N) ->
    (N + 1) rem ?MAX_32_BIT_INT.
