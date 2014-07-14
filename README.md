# anchor

Non-blocking Erlang Memcached client.

Features:
 * Performance optimized
 * Binary protocol
 * Pipelining

Commands:

 * get(Key::binary()) -> {ok, Value::binary()} | {error, Reason::atom()}.
 * get(Key::binary(), Timeout::pos_integer()) -> {ok, binary()} | {Reaons::error, atom()}.
 * set(Key::binary(), Value::binary()) -> ok | {error, Reason::atom()}.
 * set(Key::binary(), Value::binary(), TTL::non_neg_integer()) -> ok | {error, Reason::atom()}.
 * set(Key::binary(), Value::binary(), TTL::non_neg_integer(), Timeout::pos_integer()) -> ok | {error, Reason::atom()}.
