% https://code.google.com/p/memcached/wiki/BinaryProtocolRevamped

-module(anchor_protocol).
-include("anchor.hrl").

-export([
    encode/2,
    decode/2,
    decode/3
]).

%% public
encode(ReqId, {add, Key, Value, TTL}) ->
    encode_request(#request {
        op_code = ?OP_ADD,
        opaque  = ReqId,
        extras  = <<16#deadbeef:32, TTL:32>>,
        key     = Key,
        value   = Value
    });
encode(ReqId, {delete, Key}) ->
    encode_request(#request {
        op_code = ?OP_DELETE,
        opaque  = ReqId,
        key     = Key
    });
encode(ReqId, {get, Key}) ->
    encode_request(#request {
        op_code = ?OP_GET,
        opaque  = ReqId,
        key     = Key
    });
encode(ReqId, {replace, Key, Value, TTL}) ->
    encode_request(#request {
        op_code = ?OP_REPLACE,
        opaque  = ReqId,
        extras  = <<16#deadbeef:32, TTL:32>>,
        key     = Key,
        value   = Value
    });
encode(ReqId, {set, Key, Value, TTL}) ->
    encode_request(#request {
        op_code = ?OP_SET,
        opaque  = ReqId,
        extras  = <<16#deadbeef:32, TTL:32>>,
        key     = Key,
        value   = Value
    }).

decode(ReqId, Data) ->
    decode(ReqId, Data, #response {
        parsing = header
    }).

decode(ReqId, Data, #response {
        parsing = header
    } = Resp) when size(Data) >= ?HEADER_LENGTH ->

    {ok, Rest, Resp2} = decode_header(ReqId, Data, Resp),
    decode(ReqId, Rest, Resp2);
decode(_ReqId, Data, #response {
        parsing = body,
        body_length = BodyLength
    } = Resp) when size(Data) >= BodyLength ->

    decode_body(Data, Resp);
decode(_ReqId, Data, Resp) ->
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

decode_header(ReqId, Data, Resp) ->
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

decode_body(Data, #response {
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
