# anchor

__Authors:__ Louis-Philippe Gauthier.

Non-blocking Erlang Memcached client

[![Build Status](https://travis-ci.org/lpgauth/anchor.svg?branch=master)](https://travis-ci.org/lpgauth/anchor)

### Requirements

* Memcached
* Erlang 16.0 +

### Features

* Binary protocol
* Performance optimized
* Asynchronous mode
* Request pipelining
* Backpressure via backlog (OOM protection)

### Environment variables

<table width="100%">
  <theader>
    <th>Name</th>
    <th>Type</th>
    <th>Default</th>
    <th>Description</th>
  </theader>
  <tr>
    <td>ip</td>
    <td>list()</td>
    <td>"127.0.0.1"</td>
    <td>server ip</td>
  </tr>
  <tr>
    <td>port</td>
    <td>pos_integer()</td>
    <td>9042</td>
    <td>server port</td>
  </tr>
  <tr>
    <td>pool_size</td>
    <td>pos_integer()</td>
    <td>16</td>
    <td>number of connections</td>
  </tr>
  <tr>
    <td>backlog_size</td>
    <td>pos_integer()</td>
    <td>1024</td>

    <td>maximum number of concurrent requests per connection</td>
  </tr>
  <tr>
    <td>reconnect</td>
    <td>boolean()</td>
    <td>true</td>
    <td>reconnect closed connections</td>
  </tr>
</table>

## Examples

```erlang
1> application:start(anchor).
ok
2> anchor:get(<<"foo">>).
{error,key_not_found}
3> anchor:set(<<"foo">>, <<"bar">>, 3600).
ok
4> anchor:get(<<"foo">>).
{ok,<<"bar">>}
5> anchor:delete(<<"foo">>).
ok
6> anchor:get(<<"foo">>, 1000, [{async, self()}]).
{ok,#Ref<0.0.0.23623>}
7> flush().
Shell got {anchor,#Ref<0.0.0.23623>,{error,key_not_found}}
ok
```

## Modules

<table width="100%" border="0" summary="list of modules">
  <tr>
    <td><a href="http://github.com/lpgauth/anchor/blob/master/doc/anchor.md" class="module">anchor</a></td>
  </tr>
  <tr>
    <td><a href="http://github.com/lpgauth/anchor/blob/master/doc/anchor_app.md" class="module">anchor_app</a></td>
  </tr>
  <tr>
    <td><a href="http://github.com/lpgauth/anchor/blob/master/doc/anchor_backlog.md" class="module">anchor_backlog</a></td>
  </tr>
  <tr>
    <td><a href="http://github.com/lpgauth/anchor/blob/master/doc/anchor_protocol.md" class="module">anchor_protocol</a></td>
  </tr>
  <tr>
    <td><a href="http://github.com/lpgauth/anchor/blob/master/doc/anchor_queue.md" class="module">anchor_queue</a></td>
  </tr>
  <tr>
    <td><a href="http://github.com/lpgauth/anchor/blob/master/doc/anchor_server.md" class="module">anchor_server</a></td>
  </tr>
  <tr>
    <td><a href="http://github.com/lpgauth/anchor/blob/master/doc/anchor_sup.md" class="module">anchor_sup</a></td>
  </tr>
  <tr><td>
    <a href="http://github.com/lpgauth/anchor/blob/master/doc/anchor_utils.md" class="module">anchor_utils</a></td>
  </tr>
</table>

## Tests

```makefile
make eunit
make build-plt && make dialyze
make xref
```

## License

```license
The MIT License (MIT)

Copyright (c) 2015 Louis-Philippe Gauthier

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
