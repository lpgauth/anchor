

# Module anchor_queue #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#empty-1">empty/1</a></td><td></td></tr><tr><td valign="top"><a href="#in-3">in/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#out-2">out/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="empty-1"></a>

### empty/1 ###


<pre><code>
empty(ServerName::atom()) -&gt; [term()]
</code></pre>
<br />


<a name="in-3"></a>

### in/3 ###


<pre><code>
in(ServerName::atom(), Stream::non_neg_integer(), Item::term()) -&gt; true
</code></pre>
<br />


<a name="init-0"></a>

### init/0 ###


<pre><code>
init() -&gt; anchor_queue
</code></pre>
<br />


<a name="out-2"></a>

### out/2 ###


<pre><code>
out(ServerName::atom(), Stream::non_neg_integer()) -&gt; term()
</code></pre>
<br />


