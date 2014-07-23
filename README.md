# anchor [![Build Status](https://travis-ci.org/lpgauth/anchor.svg?branch=master)](https://travis-ci.org/lpgauth/anchor)

Non-blocking Erlang Memcached client.

Features:
 * Performance optimized
 * Binary protocol
 * Pipelining

Examples:

    1> application:start(anchor).
    ok
    2> anchor:get(<<"foo">>).
    {error,key_not_found}
    3> anchor:set(<<"foo">>, <<"bar">>, 3600).
    ok
    4> anchor:get(<<"foo">>).
    {ok,<<"bar">>}
    5> anchor:delete(<<"foo">>).
    ok
    
Commands:

 * add(Key::binary(), Value::binary()) -> ok | {error, Reason::atom()}.
 * add(Key::binary(), Value::binary(), TTL::non_neg_integer()) -> ok | {error, Reason::atom()}.
 * add(Key::binary(), Value::binary(), TTL::non_neg_integer(), Timeout::pos_integer()) -> ok | {error, Reason::atom()}.
 * delete(Key::binary()) -> ok | {error, Reason::atom()}.
 * delete(Key::binary(), Timeout::pos_integer()) -> ok | {error, Reason::atom()}.
 * get(Key::binary()) -> {ok, Value::binary()} | {error, Reason::atom()}.
 * get(Key::binary(), Timeout::pos_integer()) -> {ok, binary()} | {Reason::error, atom()}.
 * replace(Key::binary(), Value::binary()) -> ok | {error, Reason::atom()}.
 * replace(Key::binary(), Value::binary(), TTL::non_neg_integer()) -> ok | {error, Reason::atom()}.
 * replace(Key::binary(), Value::binary(), TTL::non_neg_integer(), Timeout::pos_integer()) -> ok | {error, Reason::atom()}.
 * set(Key::binary(), Value::binary()) -> ok | {error, Reason::atom()}.
 * set(Key::binary(), Value::binary(), TTL::non_neg_integer()) -> ok | {error, Reason::atom()}.
 * set(Key::binary(), Value::binary(), TTL::non_neg_integer(), Timeout::pos_integer()) -> ok | {error, Reason::atom()}.
