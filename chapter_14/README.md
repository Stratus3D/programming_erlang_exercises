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

Same as above, but with a common cookie specified on both computers, and node names in the rpc calls containing the full hostname to the server or IP address.

**3. Repeat the first exercise with two nodes on separate networks.**

Make sure port 4369 is open on both computers, and then choose a range of ports to open for communication between individuals nodes. Then run the same command as for exercise #2, but specify the range of open ports with the `kernel inet_dist_listen_min <min port in the range> inet_dist_listen_max <max port in the range>`.

**4. Write a simple file server using the libraries in lib_chan.**

Solution is in the `exercise_4/` directory. My implementation is a very minimal file server. Compile the lib_chan and yaf server code:

```
cd exercise_4/
erlc *.erl
erlc lib_chan/*.erl
```

Then start the server in an `erl` shell:

```
erl -pa lib_chan -pa .
1> yaf_server:start().
lib_chan starting:"lib_chan.conf"
ConfigData=[{port,1234},
            {service,yaf_server,password,"123files",mfa,yaf_server,
                     start_file_server_loop,
                     [{location,"shared_files"}]}]
```

In another shell make the client calls:

```
erl -pa lib_chan -pa .
1> {ok, Pid} = lib_chan:connect("localhost", 1234, "123files", yaf_server, []).
% List files
2> lib_chan:rpc(Pid, {ls, "/"}).
{ok,["afile.txt"]}
% Read a file
3> lib_chan:rpc(Pid, {read, "/afile.txt"}).
{ok,<<"You read a file from the server!\n">>}
% Write a new file
4>lib_chan:rpc(Pid, {write, "another_file.txt", "New file!!!"}).
ok
```
