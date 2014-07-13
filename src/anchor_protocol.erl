% https://code.google.com/p/memcached/wiki/MemcacheBinaryProtocol

-module(anchor_protocol).
-include("anchor.hrl").

-export([
    generate_request/2,
    parse_response_data/3
]).

%% public
generate_request(ReqId, {get, Key}) ->
    {ok, encode_request(#request {
        op_code = ?OP_GET,
        opaque = ReqId,
        key = Key
    })};
generate_request(ReqId, {set, Key, Value, TTL}) ->
    {ok, encode_request(#request {
        op_code = ?OP_SET,
        opaque = ReqId,
        extras = <<16#deadbeef:32, TTL:32>>,
        key = Key,
        value = Value
    })}.

parse_response_data(ReqId, Data, #response {
        op_code = undefined
    } = Response) when size(Data) >= ?HEADER_LENGTH ->

    <<Header:?HEADER_LENGTH/binary, Rest/binary>> = Data,

    {ok, #response {
        body_length = BodyLength
    } = Response2} = parse_response_header(ReqId, Header, Response),

    case size(Rest) of
        RestLength when RestLength >= BodyLength ->
            parse_response_body(Rest, Response2);
        _RestLength ->
            {ok, Rest, Response2}
    end;
parse_response_data(_ReqId, Data, #response {
        body_length = BodyLength
    } = Response) when size(Data) >= BodyLength ->

    parse_response_body(Data, Response);
parse_response_data(_ReqId, Data, Response) ->
    {ok, Data, Response}.

%% private
encode_request(#request {
        op_code = OpCode,
        data_type = DataType,
        opaque = Opaque,
        cas = CAS,
        extras = Extras,
        key = Key,
        value = Value
    })->

    KeyLength = size(Key),
    ExtraLength = size(Extras),
    Body = <<Extras/binary, Key/binary, Value/binary>>,
    BodyLength = size(Body),

    <<?MAGIC_REQUEST:8, OpCode:8, KeyLength:16, ExtraLength:8, DataType:8,
        ?RESERVED:16, BodyLength:32, Opaque:32, CAS:64, Body/binary>>.

parse_response_header(ReqId, Data, Response) ->
    <<?MAGIC_RESPONSE:8, OpCode:8, KeyLength:16, ExtrasLength:8,
        DataType:8, Status:16, BodyLength:32, ReqId:32, CAS:64>> = Data,

    {ok, Response#response {
        op_code = OpCode,
        key_length = KeyLength,
        extras_length = ExtrasLength,
        data_type = DataType,
        status = Status,
        body_length = BodyLength,
        opaque = ReqId,
        cas = CAS
    }}.

parse_response_body(Data, #response {
        extras_length = ExtraLength,
        key_length = KeyLength,
        body_length = BodyLength
    } = Response) ->

    <<Body:BodyLength/binary, Rest/binary>> = Data,
    <<Extras:ExtraLength/binary, Key:KeyLength/binary, Value/binary>> = Body,

    {ok, Rest, Response#response {
        extras = Extras,
        key = Key,
        value = Value
    }}.
