

# Module anchor_app #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`application`](application.md).

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start-0"></a>

### start/0 ###

<pre><code>
start() -&gt; {ok, [atom()]}
</code></pre>
<br />

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(StartType::<a href="application.md#type-start_type">application:start_type()</a>, StartArgs::term()) -&gt; {ok, pid()}
</code></pre>
<br />

<a name="stop-0"></a>

### stop/0 ###

<pre><code>
stop() -&gt; ok | {error, {not_started, anchor}}
</code></pre>
<br />

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(State::term()) -&gt; ok
</code></pre>
<br />

