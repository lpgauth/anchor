# anchor

High Performance Erlang Memcached Client

[![Build Status](https://travis-ci.org/lpgauth/anchor.svg?branch=master)](https://travis-ci.org/lpgauth/anchor)
[![Coverage Status](https://coveralls.io/repos/github/lpgauth/anchor/badge.svg?branch=master)](https://coveralls.io/github/lpgauth/anchor?branch=master)
#### Requirements

* Memcached
* Erlang 16.0 +

#### Features

* Backpressure via backlog (OOM protection)
* Binary protocol
* Fast pool implementation (random | round_robin)
* Performance optimized
* Request pipelining

## API
<a href="http://github.com/lpgauth/anchor/blob/master/doc/anchor.md#index" class="module">Function Index</a>

#### Environment variables

<table width="100%">
  <theader>
    <th>Name</th>
    <th>Type</th>
    <th>Default</th>
    <th>Description</th>
  </theader>
  <tr>
    <td>backlog_size</td>
    <td>pos_integer()</td>
    <td>1024</td>
    <td>maximum number of concurrent requests per connection</td>
  </tr>
  <tr>
    <td>ip</td>
    <td>list()</td>
    <td>"127.0.0.1"</td>
    <td>server ip</td>
  </tr>
  <tr>
    <td>pool_size</td>
    <td>pos_integer()</td>
    <td>16</td>
    <td>number of connections</td>
  </tr>
  <tr>
    <td>pool_strategy</td>
    <td>random | round_robin</td>
    <td>random</td>
    <td>connection selection strategy</td>
  </tr>
  <tr>
    <td>port</td>
    <td>pos_integer()</td>
    <td>9042</td>
    <td>server port</td>
  </tr>
  <tr>
    <td>reconnect</td>
    <td>boolean()</td>
    <td>true</td>
    <td>reconnect closed connections</td>
  </tr>
  <tr>
    <td>reconnect_time_max</td>
    <td>boolean()</td>
    <td>true</td>
    <td>reconnect maximum time</td>
  </tr>
  <tr>
    <td>reconnect_time_min</td>
    <td>boolean()</td>
    <td>true</td>
    <td>reconnect minimum time</td>
  </tr>
  <tr>
    <td>socket_options</td>
    <td>[gen_tcp:connect_option()]</td>
    <td>
        [binary,
        {buffer, 65535},
        {nodelay, true},
        {packet, raw},
        {send_timeout, 50},
        {send_timeout_close, true}]
    </td>
    <td>options passed to the socket</td>
  </tr>
</table>

## Examples

```erlang
1> anchor_app:start().
{ok,[shackle,anchor]}

2> anchor:get(<<"foo">>).
{error,key_not_found}

3> anchor:set(<<"foo">>, <<"bar">>, 3600).
ok

4> anchor:get(<<"foo">>).
{ok,<<"bar">>}

5> anchor:delete(<<"foo">>).
ok

6> {ok, Ref} = anchor:async_get(<<"foo">>).
{ok,{anchor_6,#Ref<0.0.1.1363>}}

7> anchor:receive_response(Ref, 1000).
{error,key_not_found}
```

## Tests

```makefile
make dialyzer
make elvis
make eunit
make xref
```

## License

```license
The MIT License (MIT)

Copyright (c) 2014-2017 Louis-Philippe Gauthier

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
