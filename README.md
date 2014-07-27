# anchor [![Build Status](https://travis-ci.org/lpgauth/anchor.svg?branch=master)](https://travis-ci.org/lpgauth/anchor)

Non-blocking Erlang Memcached client.

### Features ###
 * Performance optimized
 * Binary protocol
 * Pipelining

###Examples ###

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
    
### Modules ###

<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/lpgauth/anchor/blob/master/doc/anchor.md" class="module">anchor</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/anchor/blob/master/doc/anchor_app.md" class="module">anchor_app</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/anchor/blob/master/doc/anchor_protocol.md" class="module">anchor_protocol</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/anchor/blob/master/doc/anchor_server.md" class="module">anchor_server</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/anchor/blob/master/doc/anchor_sup.md" class="module">anchor_sup</a></td></tr></table>
