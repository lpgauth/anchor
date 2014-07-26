-module(anchor_protocol_tests).
-include_lib("anchor/include/anchor.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(REQ_ID, 123).

decode_test_() ->
    % add
    [?_assertEqual(decode(<<129,2,0,0,0,0,0,0,0,0,0,0,0,0,0,123,0,0,0,0,0,0,0,1>>), #response {
        state = complete,
        op_code = ?OP_ADD,
        key_length = 0,
        extras_length = 0,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 0,
        opaque = ?REQ_ID,
        cas = 1,
        extras = <<>>,
        key = <<>>,
        value = <<>>
    }),
    % decrement
    ?_assertEqual(decode(<<129,6,0,0,0,0,0,0,0,0,0,8,0,0,0,123,0,0,0,0,0,0,0,15,0,0,0,0,0,0,0,0>>), #response {
        state = complete,
        op_code = ?OP_DECREMENT,
        key_length = 0,
        extras_length = 0,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 8,
        opaque = ?REQ_ID,
        cas = 15,
        extras = <<>>,
        key = <<>>,
        value = <<0,0,0,0,0,0,0,0>>
    }),
    % delete
    ?_assertEqual(decode(<<129,4,0,0,0,0,0,0,0,0,0,0,0,0,0,123,0,0,0,0,0,0,0,0>>), #response {
        state = complete,
        op_code = ?OP_DELETE,
        key_length = 0,
        extras_length = 0,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 0,
        opaque = ?REQ_ID,
        cas = 0,
        extras = <<>>,
        key = <<>>,
        value = <<>>
    }),
    % flush
    ?_assertEqual(decode(<<129,8,0,0,0,0,0,0,0,0,0,0,0,0,0,123,0,0,0,0,0,0,0,0>>),  #response {
        state = complete,
        op_code = ?OP_FLUSH,
        key_length = 0,
        extras_length = 0,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 0,
        opaque = ?REQ_ID,
        cas = 0,
        extras = <<>>,
        key = <<>>,
        value = <<>>
    }),
    % get
    ?_assertEqual(decode(<<129,0,0,0,4,0,0,0,0,0,0,7,0,0,0,123,0,0,0,0,0,0,0,2,222,173,190,239,98,97,114>>), #response {
        state = complete,
        op_code = ?OP_GET,
        key_length = 0,
        extras_length = 4,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 7,
        opaque = ?REQ_ID,
        cas = 2,
        extras = <<222,173,190,239>>,
        key = <<>>,
        value = <<"bar">>
    }),
    % increment
    % {response,complete,5,0,0,0,0,8,123,16,<<>>,<<>>,<<0,0,0,0,0,0,0,1>>}
    ?_assertEqual(decode(<<129,5,0,0,0,0,0,0,0,0,0,8,0,0,0,123,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,1>>),  #response {
        state = complete,
        op_code = ?OP_INCREMENT,
        key_length = 0,
        extras_length = 0,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 8,
        opaque = ?REQ_ID,
        cas = 16,
        extras = <<>>,
        key = <<>>,
        value = <<0,0,0,0,0,0,0,1>>
    }),
    % noop
    % {response,complete,10,0,0,0,0,0,123,0,<<>>,<<>>,<<>>}
    ?_assertEqual(decode(<<129,10,0,0,0,0,0,0,0,0,0,0,0,0,0,123,0,0,0,0,0,0,0,0>>),  #response {
        state = complete,
        op_code = ?OP_NOOP,
        key_length = 0,
        extras_length = 0,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 0,
        opaque = ?REQ_ID,
        cas = 0,
        extras = <<>>,
        key = <<>>,
        value = <<>>
    }),
    % quit
    % {response,complete,7,0,0,0,0,0,123,0,<<>>,<<>>,<<>>}
    ?_assertEqual(decode(<<129,7,0,0,0,0,0,0,0,0,0,0,0,0,0,123,0,0,0,0,0,0,0,0>>),  #response {
        state = complete,
        op_code = ?OP_QUIT,
        key_length = 0,
        extras_length = 0,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 0,
        opaque = ?REQ_ID,
        cas = 0,
        extras = <<>>,
        key = <<>>,
        value = <<>>
    }),
    % replace
    ?_assertEqual(decode(<<129,3,0,0,0,0,0,0,0,0,0,0,0,0,0,123,0,0,0,0,0,0,0,3>>), #response {
        state = complete,
        op_code = ?OP_REPLACE,
        key_length = 0,
        extras_length = 0,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 0,
        opaque = ?REQ_ID,
        cas = 3,
        extras = <<>>,
        key = <<>>,
        value = <<>>
    }),
    % set
    ?_assertEqual(decode(<<129,1,0,0,0,0,0,0,0,0,0,0,0,0,0,123,0,0,0,0,0,0,0,5>>), #response {
        state = complete,
        op_code = ?OP_SET,
        key_length = 0,
        extras_length = 0,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 0,
        opaque = ?REQ_ID,
        cas = 5,
        extras = <<>>,
        key = <<>>,
        value = <<>>
    }),
    % version
    ?_assertEqual(decode(<<129,11,0,0,0,0,0,0,0,0,0,6,0,0,0,123,0,0,0,0,0,0,0,0,49,46,52,46,50,48>>),  #response {
        state = complete,
        op_code = ?OP_VERSION,
        key_length = 0,
        extras_length = 0,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 6,
        opaque = ?REQ_ID,
        cas = 0,
        extras = <<>>,
        key = <<>>,
        value = <<"1.4.20">>
    })].

encode_test_() ->
    % add
    [?_assertEqual(encode({add, <<"foo">>, <<"bar">>, 3600}),
        <<128,2,0,3,8,0,0,0,0,0,0,14,0,0,0,123,0,0,0,0,0,0,0,0,222,173,190,239,0,0,14,16,102,111,111,98,97,114>>),
    % decrement
    ?_assertEqual(encode({decrement, <<"foo">>, 1, 0, 3600}),
        <<128,6,0,3,20,0,0,0,0,0,0,23,0,0,0,123,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,14,16,102,111,111>>),
    % delete
    ?_assertEqual(encode({delete, <<"foo">>}),
        <<128,4,0,3,0,0,0,0,0,0,0,3,0,0,0,123,0,0,0,0,0,0,0,0,102,111,111>>),
    % flush
    ?_assertEqual(encode({flush, 3600}),
        <<128,8,0,0,4,0,0,0,0,0,0,4,0,0,0,123,0,0,0,0,0,0,0,0,0,0,14,16>>),
    % get
    ?_assertEqual(encode({get, <<"foo">>}),
        <<128,0,0,3,0,0,0,0,0,0,0,3,0,0,0,123,0,0,0,0,0,0,0,0,102,111,111>>),
    % increment
    ?_assertEqual(encode({increment, <<"foo">>, 1, 0, 3600}),
        <<128,5,0,3,20,0,0,0,0,0,0,23,0,0,0,123,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,14,16,102,111,111>>),
    % noop
    ?_assertEqual(encode(noop),
        <<128,10,0,0,0,0,0,0,0,0,0,0,0,0,0,123,0,0,0,0,0,0,0,0>>),
    % quit
    ?_assertEqual(encode(quit),
        <<128,7,0,0,0,0,0,0,0,0,0,0,0,0,0,123,0,0,0,0,0,0,0,0>>),
    % replace
    ?_assertEqual(encode({replace, <<"foo">>, <<"bar">>, 3600}),
        <<128,3,0,3,8,0,0,0,0,0,0,14,0,0,0,123,0,0,0,0,0,0,0,0,222,173,190,239,0,0,14,16,102,111,111,98,97,114>>),
    % set
    ?_assertEqual(encode({set, <<"foo">>, <<"bar">>, 3600}),
        <<128,1,0,3,8,0,0,0,0,0,0,14,0,0,0,123,0,0,0,0,0,0,0,0,222,173,190,239,0,0,14,16,102,111,111,98,97,114>>),
    % version
    ?_assertEqual(encode(version),
        <<128,11,0,0,0,0,0,0,0,0,0,0,0,0,0,123,0,0,0,0,0,0,0,0>>)].

% helpers
decode(Reponse) ->
    {ok, <<>>, DecodedResponse} = anchor_protocol:decode(?REQ_ID, Reponse),
    DecodedResponse.

encode(Request) ->
    {ok, EncodedRequest} = anchor_protocol:encode(?REQ_ID, Request),
    EncodedRequest.
