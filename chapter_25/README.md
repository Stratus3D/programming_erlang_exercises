# Exercises for Chapter 25

**1. Sign up for an account on github.com and go through the steps in this chapter to create your own project.**

[Already done](https://github.com/Stratus3D).

**2. The second cowboy example may be unsafe. A user could request execution of any Erlang module through the CGI call interface. Redesign the interface to only allow calls to a set of predefined modules.**

Solution in the [exercise_2](exercise_2/) directory.

Start the server:

```
rebar3 shell
===> Verifying dependencies...
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> cgi_web_server:start().
```

Then send a request via curl:

```
curl 'http://localhost:12345/cgi?mod=ping_pong&func=ping' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8'
```

**3. Do a security audit of the cowboy example code. There are a number of security problems with this code.**

I identified security issues with the code for the exercise:

* As the question in the book points out, the requested file path is unchecked, so it is possible to access files outside of the web server directory.
* Query string parameters for module name and function name are converted to atoms using `list_to_atom/1` which an attacker could use to fill up the atom table and crash the server.
* Prior to the changes for exercise 2, any function could be invoked.
* When a file cannot be read the filename is not properly escaped when rendering the error page, which means the client can inject HTML and JavaScript into the page.

Improved code in the [exercise_3](exercise_3/) directory.

Start the server:

```
rebar3 shell
===> Verifying dependencies...
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> cgi_web_server:start().
```

Then send a request via curl:

```
curl 'http://localhost:12345/cgi?mod=ping_pong&func=ping' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8'
```

**4. Any host can connect to the server. Change the code so as to only allow connections from hosts with known IP addresses. Store these hosts a persistent database. Keep track of how many times connections are made fom a specific host. Blacklist hosts that connect too frequently with a given time period**

Solution in the [exercise_4](exercise_4/) directory. I chose not to implement the IP blacklisting feature as I was short on time. Blacklisting IPs that make too many connections would not be difficult but would require looking up information on previous connections when handling a new request in order to determine if the rate limit has been exceeded for that IP. Once the rate limit has been exceeded the IP would be written to a new blacklist table in the Mnesia database, which would also have to be checked before handling each request.

Start the server:

```
rebar3 shell
===> Verifying dependencies...
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> cgi_web_server:start().
```

Then send a request via curl:

```
curl 'http://localhost:12345/cgi?mod=ping_pong&func=ping' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8'
```

**5. Change the web server to allow dynamic recompilation of the modules that are invoked by the CGI interface. When a module is invoked by the CGI interface read the timestamp on the beam file and compare it with the timestamp on the corresponding `.erl` file. Then recompile and load the Erlang code if needed**

Solution in the [exercise_5](exercise_5/) directory.

Start the server:

```
rebar3 shell
===> Verifying dependencies...
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> cgi_web_server:start().
```

Then send a request via curl:

```
curl 'http://localhost:12345/cgi?mod=ping_pong&func=ping' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8'
```

Then modify the `ping_pong` module (edit it so the return value is changed from `pong` to some other string). Then send another request via curl:

```
curl 'http://localhost:12345/cgi?mod=ping_pong&func=ping' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8'
```

**6. Rebar is an excellent example of an Erlang program that is distributed as a "self contained" binary. Copy the rebar binary to a scratch directory and rename it to `rebar.zip`. Rebar is actually a zip file. UNzip it and examine the contents. Make your own self contained binary out of the cowboy example.**

While rebar is [self-contained Erlang script](https://github.com/rebar/rebar/blob/master/bootstrap), rebar3 does not appear to be a zip archive that can be unpacked by the `unzip` command. Creating a self contained binary with an Erlang application inside it can be done in a couple steps:

* Compile the Erlang files much like you would with a typical Erlang release
* Create an archive containing the compiled beam files
* Prepend `#! /usr/bin/env escript` and `%%!` lines to the beginning of the archive to make it an escript executable. `%%! -escript main exercise_5 -pz exercise_5/exercise_5/ebin`

See http://erlang.org/doc/man/escript.html#exports for the `escript` module functions that perform these steps.

With `rebar3` things are even easier. There is a `rebar escriptize` command that automates everything. See the solution in the [exercise_6](exercise_6/) directory.
