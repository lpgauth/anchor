% application
-define(APP, anchor).
-define(BACKLOG_MAX_SIZE, 256).
-define(BACKLOG_TABLE_ID, anchor_backlog).
-define(CHILD(Mod), {Mod, {Mod, start_link, []}, permanent, 5000, worker, [Mod]}).
-define(MAX_32_BIT_INT, 4294967296).
-define(MAX_QUEUE_SIZE, 4096).
-define(SERVER, anchor_server).

-define(DEFAULT_INCREMENT, 1).
-define(DEFAULT_INITIAL_VALUE, 0).
-define(DEFAULT_IP, "127.0.0.1").
-define(DEFAULT_PORT, 11211).
-define(DEFAULT_RECONNECT, 5000).
-define(DEFAULT_SEND_TIMEOUT, 20).
-define(DEFAULT_TIMEOUT, 1000).
-define(DEFAULT_TTL, 0).

% protocol
-define(CAS, 16#00).
-define(DATA_TYPE, 16#00).
-define(HEADER_LENGTH, 24).
-define(MAGIC_REQUEST, 16#80).
-define(MAGIC_RESPONSE, 16#81).
-define(VBUCKET, 16#00).

-define(OP_ADD, 16#02).
-define(OP_DECREMENT, 16#06).
-define(OP_DELETE, 16#04).
-define(OP_FLUSH, 16#08).
-define(OP_GET, 16#00).
-define(OP_INCREMENT, 16#05).
-define(OP_NOOP, 16#0a).
-define(OP_QUIT, 16#07).
-define(OP_REPLACE, 16#03).
-define(OP_SET, 16#01).
-define(OP_VERSION, 16#0b).

-define(STAT_AUTH_CONTINUE, 16#09).
-define(STAT_AUTH_ERROR, 16#08).
-define(STAT_BUSY, 16#85).
-define(STAT_INCR_NON_NUMERIC, 16#06).
-define(STAT_INTERNAL_ERROR, 16#84).
-define(STAT_INVALID_ARGS, 16#04).
-define(STAT_ITEM_NOT_STORED, 16#05).
-define(STAT_KEY_EXISTS, 16#02).
-define(STAT_KEY_NOT_FOUND, 16#01).
-define(STAT_NOT_SUPPORTED, 16#83).
-define(STAT_OK, 16#00).
-define(STAT_OUT_OF_MEMORY, 16#82).
-define(STAT_TEMP_FAILURE, 16#86).
-define(STAT_UNKNOWN_COMMAND, 16#81).
-define(STAT_VALUE_TOO_LARGE, 16#03).
-define(STAT_VBUCKET_ERROR, 16#07).

% records
-record(request, {
    op_code   = undefined,
    data_type = ?DATA_TYPE,
    vbucket   = ?VBUCKET,
    opaque    = <<>>,
    cas       = ?CAS,
    extras    = <<>>,
    key       = <<>>,
    value     = <<>>
}).

-record(response, {
    state :: parsing_header | parsing_body | complete,
    op_code,
    key_length,
    extras_length,
    data_type,
    status,
    body_length,
    opaque,
    cas,
    extras,
    key,
    value
}).

% types
-type error () :: {error, atom()}.
-type response() :: #response {}.

-export_type([
    error/0,
    response/0
]).
