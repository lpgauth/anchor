

# Module anchor_response #
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
response() = #response{state = parsing_header | parsing_body | complete, op_code = non_neg_integer() | undefined, key_length = non_neg_integer() | undefined, extras_length = non_neg_integer() | undefined, data_type = non_neg_integer() | undefined, status = non_neg_integer() | undefined, body_length = non_neg_integer() | undefined, opaque = non_neg_integer() | undefined, cas = non_neg_integer() | undefined, extras = binary() | undefined, key = binary() | undefined, value = binary() | undefined}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-1">format/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="format-1"></a>

### format/1 ###

<pre><code>
format(Response::<a href="#type-response">response()</a>) -&gt; ok | {ok, term()} | {error, term()}
</code></pre>
<br />

