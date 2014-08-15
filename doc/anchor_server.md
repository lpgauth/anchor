

# Module anchor_server #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-error">error()</a> ###



<pre><code>
error() = {error, atom()}
</code></pre>





### <a name="type-response">response()</a> ###



<pre><code>
response() = #response{state = undefined | parsing_header | parsing_body | complete, op_code = any(), key_length = any(), extras_length = any(), data_type = any(), status = any(), body_length = any(), opaque = any(), cas = any(), extras = any(), key = any(), value = any()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-2">call/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#queue_size-0">queue_size/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-2"></a>

### call/2 ###


<pre><code>
call(Msg::term(), Timeout::pos_integer()) -&gt; {ok, term()} | {error, atom()}
</code></pre>
<br />


<a name="init-1"></a>

### init/1 ###


<pre><code>
init(Parent::pid()) -&gt; no_return()
</code></pre>
<br />


<a name="queue_size-0"></a>

### queue_size/0 ###


<pre><code>
queue_size() -&gt; non_neg_integer()
</code></pre>
<br />


<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, pid()}
</code></pre>
<br />


