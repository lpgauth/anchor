

# Module anchor_client #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#after_connect-2">after_connect/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_data-2">handle_data/2</a></td><td></td></tr><tr><td valign="top"><a href="#options-0">options/0</a></td><td></td></tr><tr><td valign="top"><a href="#process_timings-2">process_timings/2</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-1">terminate/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="after_connect-2"></a>

### after_connect/2 ###

<pre><code>
after_connect(Socket::<a href="inet.md#type-socket">inet:socket()</a>, State::#state{}) -&gt; {ok, #state{}}
</code></pre>
<br />

<a name="handle_cast-2"></a>

### handle_cast/2 ###

<pre><code>
handle_cast(Request::term(), State::#state{}) -&gt; {ok, pos_integer(), binary(), #state{}}
</code></pre>
<br />

<a name="handle_data-2"></a>

### handle_data/2 ###

<pre><code>
handle_data(Data::binary(), State::#state{}) -&gt; {ok, [{pos_integer(), term()}], #state{}}
</code></pre>
<br />

<a name="options-0"></a>

### options/0 ###

<pre><code>
options() -&gt; {ok, [{ip, <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>} | {port, <a href="inet.md#type-port_number">inet:port_number()</a>} | {reconnect, boolean()} | {state, #state{}}]}
</code></pre>
<br />

<a name="process_timings-2"></a>

### process_timings/2 ###

<pre><code>
process_timings(Cast::term(), Timings::[non_neg_integer()]) -&gt; ok
</code></pre>
<br />

<a name="terminate-1"></a>

### terminate/1 ###

<pre><code>
terminate(State::#state{}) -&gt; ok
</code></pre>
<br />

