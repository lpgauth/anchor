

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-2">init/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-2"></a>

### init/2 ###

<pre><code>
init(Parent::pid(), Name::atom()) -&gt; no_return()
</code></pre>
<br />

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Name::atom()) -&gt; {ok, pid()}
</code></pre>
<br />

