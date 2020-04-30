# Exercises for Chapter 14

**1. Start two nodes on the same host. Perform some RPC calls between the two nodes.**

The manual pages for `rpc` can be viewed here: http://erlang.org/doc/man/rpc.html

In one shell session:

```
$ erl -sname watson
% After both shells are running
1> rpc:call('holmes@my-laptop', erlang, nodes, []).
```

In another shell session:

```
$ erl -sname holmes
1> rpc:call('watson@my-laptop', erlang, get_cookie, []).
% After watson node is stopped
2> rpc:call('watson@my-laptop', erlang, nodes, []).
{badrpc,nodedown}
```

**2. Repeat the first exercise with two nodes on a LAN.**

**3. Repeat the first exercise with two nodes on separate networks.**

**4. Write a simple file server using the libraries in lib_chan.**
