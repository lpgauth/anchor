

# Module anchor #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-2">add/2</a></td><td></td></tr><tr><td valign="top"><a href="#add-3">add/3</a></td><td></td></tr><tr><td valign="top"><a href="#add-4">add/4</a></td><td></td></tr><tr><td valign="top"><a href="#decrement-1">decrement/1</a></td><td></td></tr><tr><td valign="top"><a href="#decrement-2">decrement/2</a></td><td></td></tr><tr><td valign="top"><a href="#decrement-3">decrement/3</a></td><td></td></tr><tr><td valign="top"><a href="#decrement-4">decrement/4</a></td><td></td></tr><tr><td valign="top"><a href="#decrement-5">decrement/5</a></td><td></td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td></td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td></td></tr><tr><td valign="top"><a href="#flush-0">flush/0</a></td><td></td></tr><tr><td valign="top"><a href="#flush-1">flush/1</a></td><td></td></tr><tr><td valign="top"><a href="#flush-2">flush/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#increment-1">increment/1</a></td><td></td></tr><tr><td valign="top"><a href="#increment-2">increment/2</a></td><td></td></tr><tr><td valign="top"><a href="#increment-3">increment/3</a></td><td></td></tr><tr><td valign="top"><a href="#increment-4">increment/4</a></td><td></td></tr><tr><td valign="top"><a href="#increment-5">increment/5</a></td><td></td></tr><tr><td valign="top"><a href="#noop-0">noop/0</a></td><td></td></tr><tr><td valign="top"><a href="#noop-1">noop/1</a></td><td></td></tr><tr><td valign="top"><a href="#quit-0">quit/0</a></td><td></td></tr><tr><td valign="top"><a href="#quit-1">quit/1</a></td><td></td></tr><tr><td valign="top"><a href="#replace-2">replace/2</a></td><td></td></tr><tr><td valign="top"><a href="#replace-3">replace/3</a></td><td></td></tr><tr><td valign="top"><a href="#replace-4">replace/4</a></td><td></td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td></td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td></td></tr><tr><td valign="top"><a href="#set-4">set/4</a></td><td></td></tr><tr><td valign="top"><a href="#version-0">version/0</a></td><td></td></tr><tr><td valign="top"><a href="#version-1">version/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-2"></a>

### add/2 ###


<pre><code>
add(Key::binary(), Value::binary()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="add-3"></a>

### add/3 ###


<pre><code>
add(Key::binary(), Value::binary(), TTL::non_neg_integer()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="add-4"></a>

### add/4 ###


<pre><code>
add(Key::binary(), Value::binary(), TTL::non_neg_integer(), Timeout::pos_integer()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="decrement-1"></a>

### decrement/1 ###


<pre><code>
decrement(Key::binary()) -&gt; {ok, integer()} | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="decrement-2"></a>

### decrement/2 ###


<pre><code>
decrement(Key::binary(), Amount::integer()) -&gt; {ok, integer()} | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="decrement-3"></a>

### decrement/3 ###


<pre><code>
decrement(Key::binary(), Amount::integer(), InitialValue::integer()) -&gt; {ok, integer()} | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="decrement-4"></a>

### decrement/4 ###


<pre><code>
decrement(Key::binary(), Amount::integer(), InitialValue::integer(), TTL::non_neg_integer()) -&gt; {ok, integer()} | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="decrement-5"></a>

### decrement/5 ###


<pre><code>
decrement(Key::binary(), Amount::integer(), InitialValue::integer(), TTL::non_neg_integer(), Timeout::pos_integer()) -&gt; {ok, integer()} | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Key::binary()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="delete-2"></a>

### delete/2 ###


<pre><code>
delete(Key::binary(), Timeout::pos_integer()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="flush-0"></a>

### flush/0 ###


<pre><code>
flush() -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="flush-1"></a>

### flush/1 ###


<pre><code>
flush(TTL::non_neg_integer()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="flush-2"></a>

### flush/2 ###


<pre><code>
flush(TTL::non_neg_integer(), Timeout::pos_integer()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="get-1"></a>

### get/1 ###


<pre><code>
get(Key::binary()) -&gt; {ok, binary()} | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="get-2"></a>

### get/2 ###


<pre><code>
get(Key::binary(), Timeout::pos_integer()) -&gt; {ok, binary()} | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="increment-1"></a>

### increment/1 ###


<pre><code>
increment(Key::binary()) -&gt; {ok, integer()} | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="increment-2"></a>

### increment/2 ###


<pre><code>
increment(Key::binary(), Amount::integer()) -&gt; {ok, integer()} | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="increment-3"></a>

### increment/3 ###


<pre><code>
increment(Key::binary(), Amount::integer(), InitialValue::integer()) -&gt; {ok, integer()} | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="increment-4"></a>

### increment/4 ###


<pre><code>
increment(Key::binary(), Amount::integer(), InitialValue::integer(), TTL::non_neg_integer()) -&gt; {ok, integer()} | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="increment-5"></a>

### increment/5 ###


<pre><code>
increment(Key::binary(), Amount::integer(), InitialValue::integer(), TTL::non_neg_integer(), Timeout::pos_integer()) -&gt; {ok, integer()} | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="noop-0"></a>

### noop/0 ###


<pre><code>
noop() -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="noop-1"></a>

### noop/1 ###


<pre><code>
noop(Timeout::pos_integer()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="quit-0"></a>

### quit/0 ###


<pre><code>
quit() -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="quit-1"></a>

### quit/1 ###


<pre><code>
quit(Timeout::pos_integer()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="replace-2"></a>

### replace/2 ###


<pre><code>
replace(Key::binary(), Value::binary()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="replace-3"></a>

### replace/3 ###


<pre><code>
replace(Key::binary(), Value::binary(), TTL::non_neg_integer()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="replace-4"></a>

### replace/4 ###


<pre><code>
replace(Key::binary(), Value::binary(), TTL::non_neg_integer(), Timeout::pos_integer()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="set-2"></a>

### set/2 ###


<pre><code>
set(Key::binary(), Value::binary()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="set-3"></a>

### set/3 ###


<pre><code>
set(Key::binary(), Value::binary(), TTL::non_neg_integer()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="set-4"></a>

### set/4 ###


<pre><code>
set(Key::binary(), Value::binary(), TTL::non_neg_integer(), Timeout::pos_integer()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="version-0"></a>

### version/0 ###


<pre><code>
version() -&gt; {ok, binary()} | <a href="#type-error">error()</a>
</code></pre>
<br />


<a name="version-1"></a>

### version/1 ###


<pre><code>
version(Timeout::pos_integer()) -&gt; {ok, binary()} | <a href="#type-error">error()</a>
</code></pre>
<br />


