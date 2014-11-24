

# Module anchor_backlog #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#check-2">check/2</a></td><td></td></tr><tr><td valign="top"><a href="#decrement-1">decrement/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="check-2"></a>

### check/2 ###


<pre><code>
check(Tid::atom(), MaxBacklog::pos_integer()) -&gt; boolean()
</code></pre>
<br />


<a name="decrement-1"></a>

### decrement/1 ###

`decrement(Tid) -> any()`


<a name="new-1"></a>

### new/1 ###


<pre><code>
new(Tid::atom()) -&gt; ok
</code></pre>
<br />


