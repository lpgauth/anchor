-module(anchor_client).
-include("anchor.hrl").

-behavior(shackle_client).
-export([
    after_connect/2,
    handle_cast/2,
    handle_data/2,
    options/0,
    process_timings/1,
    terminate/1
]).

-record(state, {
    buffer   = <<>>,
    item     = undefined,
    requests = 0,
    response = undefined
}).

%% shackle_server callbacks
-spec after_connect(inet:socket(), #state {}) -> {ok, #state {}}.

after_connect(_Socket, State) ->
    {ok, State}.

-spec handle_cast(term(), #state {}) ->
    {ok, pos_integer(), binary(), #state {}}.

handle_cast(Request, #state {
        requests = Requests
    } = State) ->

    RequestId = request_id(Requests),
    {ok, Data} = anchor_protocol:encode(RequestId, Request),
    {ok, RequestId, Data, State#state {
        requests = Requests + 1
    }}.

-spec handle_data(binary(), #state {}) ->
    {ok, [{pos_integer(), term()}], #state {}}.

handle_data(Data, #state {
        buffer = Buffer
    } = State) ->

    Data2 = <<Buffer/binary, Data/binary>>,
    decode_data(Data2, [], State).

-spec options() -> {ok, [
    {ip, inet:ip_address() | inet:hostname()} |
    {port, inet:port_number()} |
    {reconnect, boolean()} |
    {state, #state {}}
]}.

options() ->
    Ip = application:get_env(?APP, ip, ?DEFAULT_IP),
    Port = application:get_env(?APP, port, ?DEFAULT_PORT),
    Reconnect = application:get_env(?APP, reconnect, ?DEFAULT_RECONNECT),

    {ok, [
        {ip, Ip},
        {port, Port},
        {reconnect, Reconnect},
        {state, #state {}}
    ]}.

-spec process_timings([non_neg_integer()]) -> ok.

process_timings(_Timings) ->
    ok.

-spec terminate(#state {}) -> ok.

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
