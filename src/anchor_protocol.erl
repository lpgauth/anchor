-module(anchor_protocol).
-include("anchor.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    encode/2,
    decode/1
]).

%% public
-spec encode(pos_integer(), atom() | tuple()) -> {ok, binary()}.

encode(ReqId, {add, Key, Value, TTL}) ->
    encode_request(#request {
        op_code = ?OP_ADD,
        opaque  = ReqId,
        extras  = <<16#deadbeef:32, TTL:32>>,
        key     = Key,
        value   = Value
    });
encode(ReqId, {decrement, Key, Amount, InitialValue, TTL}) ->
    encode_request(#request {
        op_code = ?OP_DECREMENT,
        opaque  = ReqId,
        key     = Key,
        extras  = <<Amount:64, InitialValue:64, TTL:32>>
    });
encode(ReqId, {delete, Key}) ->
    encode_request(#request {
        op_code = ?OP_DELETE,
        opaque  = ReqId,
        key     = Key
    });
encode(ReqId, {flush, TTL}) ->
    encode_request(#request {
        op_code = ?OP_FLUSH,
        opaque  = ReqId,
        extras  = <<TTL:32>>
    });
encode(ReqId, {get, Key}) ->
    encode_request(#request {
        op_code = ?OP_GET,
        opaque  = ReqId,
        key     = Key
    });
encode(ReqId, {increment, Key, Amount, InitialValue, TTL}) ->
    encode_request(#request {
        op_code = ?OP_INCREMENT,
        opaque  = ReqId,
        key     = Key,
        extras  = <<Amount:64, InitialValue:64, TTL:32>>
    });
encode(ReqId, noop) ->
    encode_request(#request {
        op_code = ?OP_NOOP,
        opaque  = ReqId
    });
encode(ReqId, quit) ->
    encode_request(#request {
        op_code = ?OP_QUIT,
        opaque  = ReqId
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
    });
encode(ReqId, version) ->
    encode_request(#request {
        op_code = ?OP_VERSION,
        opaque  = ReqId
    }).

-spec decode(binary()) ->
    {ok, binary(), response()} | {error, not_enough_data}.

decode(<<Header:?HEADER_LENGTH/binary, Rest/binary>>) ->
    <<?MAGIC_RESPONSE:8, OpCode:8, KeyLength:16, ExtrasLength:8,
        DataType:8, Status:16, BodyLength:32, ReqId:32, CAS:64>> = Header,

    case size(Rest) >= BodyLength of
        true ->
            <<Body:BodyLength/binary, Rest2/binary>> = Rest,
            <<Extras:ExtrasLength/binary, Key:KeyLength/binary,
                Value/binary>> = Body,

            {ok, Rest2, #response {
                op_code = OpCode,
                key_length = KeyLength,
                extras_length = ExtrasLength,
                data_type = DataType,
                status = Status,
                body_length = BodyLength,
                opaque = ReqId,
                cas = CAS,
                extras = Extras,
                key = Key,
                value = Value
            }};
        false ->
            {error, not_enough_data}
    end;
decode(_Data) ->
    {error, not_enough_data}.

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
