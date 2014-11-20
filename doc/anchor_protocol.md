

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
response() = #response{state = undefined | parsing_header | parsing_body | complete, op_code = any(), key_length = any(), extras_length = any(), data_type = any(), status = any(), body_length = any(), opaque = any(), cas = any(), extras = any(), key = any(), value = any()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode-3">decode/3</a></td><td></td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-2"></a>

### decode/2 ###


<pre><code>
decode(ReqId::pos_integer(), Data::binary()) -&gt; {ok, binary(), <a href="#type-response">response()</a>}
</code></pre>
<br />


<a name="decode-3"></a>

### decode/3 ###


<pre><code>
decode(ReqId::pos_integer(), Data::binary(), Response::<a href="#type-response">response()</a>) -&gt; {ok, binary(), <a href="#type-response">response()</a>}
</code></pre>
<br />


<a name="encode-2"></a>

### encode/2 ###


<pre><code>
encode(ReqId::pos_integer(), X2::atom() | tuple()) -&gt; {ok, binary()}
</code></pre>
<br />


