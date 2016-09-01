%% macros
-define(APP, anchor).
-define(CLIENT, anchor_client).
-define(GET_ENV(Key, Default), application:get_env(?APP, Key, Default)).
-define(MAX_32_BIT_INT, 4294967296).

%% defaults
-define(DEFAULT_BACKLOG_SIZE, 1024).
-define(DEFAULT_INCREMENT, 1).
-define(DEFAULT_INITIAL_VALUE, 0).
-define(DEFAULT_IP, "127.0.0.1").
-define(DEFAULT_POOL_SIZE, 8).
-define(DEFAULT_POOL_STRATEGY, round_robin).
-define(DEFAULT_PORT, 11211).
-define(DEFAULT_RECONNECT, true).
-define(DEFAULT_RECONNECT_MAX, timer:minutes(2)).
-define(DEFAULT_RECONNECT_MIN, timer:seconds(1)).
-define(DEFAULT_SOCKET_OPTIONS, [
    binary,
    {packet, raw},
    {send_timeout, 50},
    {send_timeout_close, true}
]).
-define(DEFAULT_TIMEOUT, 1000).
-define(DEFAULT_TTL, 0).

%% protocol
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

%% records
-record(request, {
    op_code   = undefined  :: non_neg_integer(),
    data_type = ?DATA_TYPE :: non_neg_integer(),
    vbucket   = ?VBUCKET   :: non_neg_integer(),
    opaque    = <<>>       :: non_neg_integer(),
    cas       = ?CAS       :: non_neg_integer(),
    extras    = <<>>       :: binary(),
    key       = <<>>       :: binary(),
    value     = <<>>       :: binary()
}).

-record(response, {
    state         :: parsing_header | parsing_body | complete,
    op_code       :: non_neg_integer() | undefined,
    key_length    :: non_neg_integer() | undefined,
    extras_length :: non_neg_integer() | undefined,
    data_type     :: non_neg_integer() | undefined,
    status        :: non_neg_integer() | undefined,
    body_length   :: non_neg_integer() | undefined,
    opaque        :: non_neg_integer() | undefined,
    cas           :: non_neg_integer() | undefined,
    extras        :: binary() | undefined,
    key           :: binary() | undefined,
    value         :: binary() | undefined
}).

%% types
-type error () :: {error, atom()}.
-type option() :: {async, pid()}.
-type options() :: [option()].
-type response() :: #response {}.

-export_type([
    error/0,
    options/0,
    response/0
]).
