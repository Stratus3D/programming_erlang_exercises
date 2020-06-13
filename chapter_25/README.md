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
