-module(anchor_protocol_tests).
-include_lib("anchor/include/anchor.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(REQ_ID, 123).

decode_test_() ->
    % add
    [?_assertEqual(decode(<<129,2,0,0,0,0,0,0,0,0,0,0,0,0,0,123,0,0,0,0,0,0,0,1>>), #response {
        op_code = 2,
        key_length = 0,
        extras_length = 0,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 0,
        opaque = ?REQ_ID,
        cas = 1,
        extras = <<>>,
        key = <<>>,
        value = <<>>,
        parsing = complete
    }),
    % delete
    ?_assertEqual(decode(<<129,4,0,0,0,0,0,0,0,0,0,0,0,0,0,123,0,0,0,0,0,0,0,0>>), #response {
        op_code = 4,
        key_length = 0,
        extras_length = 0,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 0,
        opaque = ?REQ_ID,
        cas = 0,
        extras = <<>>,
        key = <<>>,
        value = <<>>,
        parsing = complete
    }),
    % get
    ?_assertEqual(decode(<<129,0,0,0,4,0,0,0,0,0,0,7,0,0,0,123,0,0,0,0,0,0,0,2,222,173,190,239,98,97,114>>), #response {
        op_code = 0,
        key_length = 0,
        extras_length = 4,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 7,
        opaque = ?REQ_ID,
        cas = 2,
        extras = <<222,173,190,239>>,
        key = <<>>,
        value = <<"bar">>,
        parsing = complete
    }),
    % replace
    ?_assertEqual(decode(<<129,3,0,0,0,0,0,0,0,0,0,0,0,0,0,123,0,0,0,0,0,0,0,3>>), #response {
        op_code = 3,
        key_length = 0,
        extras_length = 0,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 0,
        opaque = ?REQ_ID,
        cas = 3,
        extras = <<>>,
        key = <<>>,
        value = <<>>,
        parsing = complete
    }),
    % set
    ?_assertEqual(decode(<<129,1,0,0,0,0,0,0,0,0,0,0,0,0,0,123,0,0,0,0,0,0,0,5>>), #response {
        op_code = 1,
        key_length = 0,
        extras_length = 0,
        data_type = ?DATA_TYPE,
        status = 0,
        body_length = 0,
        opaque = ?REQ_ID,
        cas = 5,
        extras = <<>>,
        key = <<>>,
        value = <<>>,
        parsing = complete
    })].

encode_test_() ->
    % add
    [?_assertEqual(encode({add, <<"foo">>, <<"bar">>, 3600}),
        <<128,2,0,3,8,0,0,0,0,0,0,14,0,0,0,123,0,0,0,0,0,0,0,0,222,173,190,239,0,0,14,16,102,111,111,98,97,114>>),
    % delete
    ?_assertEqual(encode({delete, <<"foo">>}),
        <<128,4,0,3,0,0,0,0,0,0,0,3,0,0,0,123,0,0,0,0,0,0,0,0,102,111,111>>),
    % delete
    ?_assertEqual(encode({get, <<"foo">>}),
        <<128,0,0,3,0,0,0,0,0,0,0,3,0,0,0,123,0,0,0,0,0,0,0,0,102,111,111>>),
    % replace
    ?_assertEqual(encode({replace, <<"foo">>, <<"bar">>, 3600}),
        <<128,3,0,3,8,0,0,0,0,0,0,14,0,0,0,123,0,0,0,0,0,0,0,0,222,173,190,239,0,0,14,16,102,111,111,98,97,114>>),
    % set
    ?_assertEqual(encode({set, <<"foo">>, <<"bar">>, 3600}),
        <<128,1,0,3,8,0,0,0,0,0,0,14,0,0,0,123,0,0,0,0,0,0,0,0,222,173,190,239,0,0,14,16,102,111,111,98,97,114>>)].

% helpers
decode(Reponse) ->
    {ok, <<>>, DecodedRequest} = anchor_protocol:decode(?REQ_ID, Reponse),
    DecodedRequest.

encode(Request) ->
    {ok, EncodedRequest} = anchor_protocol:encode(?REQ_ID, Request),
    EncodedRequest.
