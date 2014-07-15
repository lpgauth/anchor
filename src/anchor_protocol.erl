% https://code.google.com/p/memcached/wiki/BinaryProtocolRevamped

-module(anchor_protocol).
-include("anchor.hrl").

-export([
    generate/2,
    parse/3
]).

%% public
generate(ReqId, {add, Key, Value, TTL}) ->
    encode_request(#request {
        op_code = ?OP_ADD,
        opaque  = ReqId,
        extras  = <<16#deadbeef:32, TTL:32>>,
        key     = Key,
        value   = Value
    });
generate(ReqId, {delete, Key}) ->
    encode_request(#request {
        op_code = ?OP_DELETE,
        opaque  = ReqId,
        key     = Key
    });
generate(ReqId, {get, Key}) ->
    encode_request(#request {
        op_code = ?OP_GET,
        opaque  = ReqId,
        key     = Key
    });
generate(ReqId, {replace, Key, Value, TTL}) ->
    encode_request(#request {
        op_code = ?OP_REPLACE,
        opaque  = ReqId,
        extras  = <<16#deadbeef:32, TTL:32>>,
        key     = Key,
        value   = Value
    });
generate(ReqId, {set, Key, Value, TTL}) ->
    encode_request(#request {
        op_code = ?OP_SET,
        opaque  = ReqId,
        extras  = <<16#deadbeef:32, TTL:32>>,
        key     = Key,
        value   = Value
    }).

parse(ReqId, Data, #response {
        parsing = header
    } = Resp) when size(Data) >= ?HEADER_LENGTH ->

    {ok, Rest, Resp2} = parse_header(ReqId, Data, Resp),
    parse(ReqId, Rest, Resp2);
parse(_ReqId, Data, #response {
        parsing = body,
        body_length = BodyLength
    } = Resp) when size(Data) >= BodyLength ->

    parse_body(Data, Resp);
parse(_ReqId, Data, Resp) ->
    {ok, Data, Resp}.

%% private
encode_request(#request {
        op_code = OpCode,
        data_type = DataType,
        vbucket = VBucket,
        opaque = Opaque,
        cas = CAS,
        extras = Extras,
        key = Key,
        value = Value
    })->

    KeyLength = size(Key),
    ExtrasLength = size(Extras),
    Body = <<Extras/binary, Key/binary, Value/binary>>,
    BodyLength = size(Body),

    {ok, <<?MAGIC_REQUEST:8, OpCode:8, KeyLength:16, ExtrasLength:8, DataType:8,
        VBucket:16, BodyLength:32, Opaque:32, CAS:64, Body/binary>>}.

parse_header(ReqId, Data, Resp) ->
    <<Header:?HEADER_LENGTH/binary, Rest/binary>> = Data,
    <<?MAGIC_RESPONSE:8, OpCode:8, KeyLength:16, ExtrasLength:8,
        DataType:8, Status:16, BodyLength:32, ReqId:32, CAS:64>> = Header,

    {ok, Rest, Resp#response {
        parsing = body,
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
        extras_length = ExtrasLength,
        key_length = KeyLength,
        body_length = BodyLength
    } = Resp) ->

    <<Body:BodyLength/binary, Rest/binary>> = Data,
    <<Extras:ExtrasLength/binary, Key:KeyLength/binary, Value/binary>> = Body,

    {ok, Rest, Resp#response {
        parsing = complete,
        extras = Extras,
        key = Key,
        value = Value
    }}.
