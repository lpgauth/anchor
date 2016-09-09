

# Module anchor_protocol #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(Data::binary()) -&gt; {ok, binary(), <a href="#type-response">response()</a>}
</code></pre>
<br />

<a name="decode-2"></a>

### decode/2 ###

<pre><code>
decode(Data::binary(), Response::<a href="#type-response">response()</a> | undefined) -&gt; {ok, binary(), <a href="#type-response">response()</a>}
</code></pre>
<br />

<a name="encode-2"></a>

### encode/2 ###

<pre><code>
encode(ReqId::pos_integer(), X2::atom() | tuple()) -&gt; {ok, binary()}
</code></pre>
<br />

