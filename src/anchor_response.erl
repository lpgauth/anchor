-module(anchor_response).
-include("anchor.hrl").

-export([
    format/1
]).

%% public
-spec format(#response {}) -> ok | {ok, term()} | {error, term()}.

format(#response {status = ?STAT_OK} = Response) ->
    return(Response);
format(#response {status = Status}) ->
    {error, status(Status)}.

%% private
return(#response {op_code = ?OP_ADD}) -> ok;
return(#response {op_code = ?OP_DECREMENT, value = Value}) ->
    {ok, binary:decode_unsigned(Value)};
return(#response {op_code = ?OP_DELETE}) -> ok;
return(#response {op_code = ?OP_FLUSH}) -> ok;
return(#response {op_code = ?OP_GET, value = Value}) ->
    {ok, Value};
return(#response {op_code = ?OP_INCREMENT, value = Value}) ->
    {ok, binary:decode_unsigned(Value)};
return(#response {op_code = ?OP_NOOP}) -> ok;
return(#response {op_code = ?OP_QUIT}) -> ok;
return(#response {op_code = ?OP_REPLACE}) -> ok;
return(#response {op_code = ?OP_SET}) -> ok;
return(#response {op_code = ?OP_VERSION, value = Value}) ->
    {ok, Value}.

status(?STAT_AUTH_CONTINUE) -> auth_continue;
status(?STAT_AUTH_ERROR) -> auth_error;
status(?STAT_BUSY) -> busy;
status(?STAT_INCR_NON_NUMERIC) -> incr_non_numeric;
status(?STAT_INTERNAL_ERROR) -> internal_error;
status(?STAT_INVALID_ARGS) -> invalid_args;
status(?STAT_ITEM_NOT_STORED) -> item_not_stored;
status(?STAT_KEY_EXISTS) -> key_exists;
status(?STAT_KEY_NOT_FOUND) -> key_not_found;
status(?STAT_NOT_SUPPORTED) -> not_supported;
status(?STAT_OK) -> ok;
status(?STAT_OUT_OF_MEMORY) -> out_of_memory;
status(?STAT_TEMP_FAILURE) -> temp_failure;
status(?STAT_UNKNOWN_COMMAND) -> unknown_command;
status(?STAT_VALUE_TOO_LARGE) -> value_too_large;
status(?STAT_VBUCKET_ERROR) -> vbucket_error.