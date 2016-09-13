-module(anchor_client).
-include("anchor.hrl").

-compile(inline).
-compile({inline_size, 512}).

-behavior(shackle_client).
-export([
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
            Reply = {Response2#response.opaque, {ok, Response2}},
            decode_data(Rest, [Reply | Replies], State#state {
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
