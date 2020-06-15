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
