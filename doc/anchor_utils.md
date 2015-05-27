

# Module anchor_utils #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-error">error()</a> ###



<pre><code>
error() = {error, atom()}
</code></pre>





### <a name="type-option">option()</a> ###



<pre><code>
option() = {async, pid()}
</code></pre>





### <a name="type-options">options()</a> ###



<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>





### <a name="type-response">response()</a> ###



<pre><code>
response() = #response{state = undefined | parsing_header | parsing_body | complete, op_code = any(), key_length = any(), extras_length = any(), data_type = any(), status = any(), body_length = any(), opaque = any(), cas = any(), extras = any(), key = any(), value = any()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#child_name-1">child_name/1</a></td><td></td></tr><tr><td valign="top"><a href="#child_specs-0">child_specs/0</a></td><td></td></tr><tr><td valign="top"><a href="#error_msg-2">error_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#warning_msg-2">warning_msg/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="child_name-1"></a>

### child_name/1 ###

`child_name(N) -> any()`


<a name="child_specs-0"></a>

### child_specs/0 ###

`child_specs() -> any()`


<a name="error_msg-2"></a>

### error_msg/2 ###

`error_msg(Format, Data) -> any()`


<a name="warning_msg-2"></a>

### warning_msg/2 ###

`warning_msg(Format, Data) -> any()`


