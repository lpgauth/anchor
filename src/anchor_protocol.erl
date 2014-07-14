% https://code.google.com/p/memcached/wiki/MemcacheBinaryProtocol

-module(anchor_protocol).
-include("anchor.hrl").

-export([
    generate/2,
    parse/3
]).

%% public
generate(ReqId, {get, Key}) ->
    {ok, encode_request(#request {
        op_code = ?OP_GET,
        opaque = ReqId,
        key = Key
    })};
generate(ReqId, {set, Key, Value, TTL}) ->
    {ok, encode_request(#request {
        op_code = ?OP_SET,
        opaque = ReqId,
        extras = <<16#deadbeef:32, TTL:32>>,
        key = Key,
        value = Value
    })}.

parse(ReqId, Data, #response {
        op_code = undefined
    } = Resp) when size(Data) >= ?HEADER_LENGTH ->

    <<Header:?HEADER_LENGTH/binary, Rest/binary>> = Data,

    {ok, #response {
        body_length = BodyLength
    } = Resp2} = parse_header(ReqId, Header, Resp),

    case size(Rest) of
        RestLength when RestLength >= BodyLength ->
            parse_body(Rest, Resp2);
        _RestLength ->
            {ok, Rest, Resp2}
    end;
parse(_ReqId, Data, #response {
        body_length = BodyLength
    } = Resp) when size(Data) >= BodyLength ->

    parse_body(Data, Resp);
parse(_ReqId, Data, Resp) ->
    {ok, Data, Resp}.

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

parse_header(ReqId, Data, Resp) ->
    <<?MAGIC_RESPONSE:8, OpCode:8, KeyLength:16, ExtrasLength:8,
        DataType:8, Status:16, BodyLength:32, ReqId:32, CAS:64>> = Data,

    {ok, Resp#response {
        op_code = OpCode,
        key_length = KeyLength,
        extras_length = ExtrasLength,
        data_type = DataType,
        status = Status,
        body_length = BodyLength,
        opaque = ReqId,
        cas = CAS
    }}.

parse_body(Data, #response {
        extras_length = ExtraLength,
        key_length = KeyLength,
        body_length = BodyLength
    } = Resp) ->

    <<Body:BodyLength/binary, Rest/binary>> = Data,
    <<Extras:ExtraLength/binary, Key:KeyLength/binary, Value/binary>> = Body,

    {ok, Rest, Resp#response {
        extras = Extras,
        key = Key,
        value = Value
    }}.
