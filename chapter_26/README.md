# Exercises for Chapter 26

**1. On page 264, we wrote a program that fetched a web page. Change this program to issue a `HEAD` request instead of a `GET` request. We can issue an HTTP `HEAD` request to measure the response time of a website. Write a function called `web_profiler:ping(URL, Timeout)` that measures the response time to a website address URL. This should return `{time, T}` or `timeout`.**

Solution in the [exercise_1](exercise_1/) directory.

Compile the module:

```
erlc web_profiler.erl
```

Call the `ping` function:

```
erl
1> web_profiler:ping("http://stratus3d.com", 10000).
{time,64703}
```

**2. Make a long list of websites called `L`. Time how long `lists:map(fun(URL) -> web_profiler:ping(Url, Timeout) end, L)` takes. This might take quite a long time; the worst-case time is `Timeout x length(L)`.**

Solution in the [exercise_2](exercise_2/) directory.

Compile the module:

```
erlc web_profiler.erl
```

Call the `run` function:

```
erl
1> web_profiler:run(10000).
454728
```

**3. Repeat the last measurement, using `pmap` instead of map. Now all the `HEAD` requests should go out in parallel. So the worst-case response time is just `Timeout`.**

Solution in the [exercise_3](exercise_3/) directory.

Compile the module:

```
erlc web_profiler.erl
```

Call the `run` function:

```
erl
1> web_profiler:run(10000).
182777
```

**4. Store the results in a database and make a web interface to query the database. You can base your code for this on the database and web server code developed in Chapter 24.**

**5. If you call `pmap` with an extremely long list of elements, you might create too many processes. Write a function called `pmap(F, L, Max)` that computes the list `[F(I) || I <- L]` in parallel but is subject to the restriction that no more than `Max` parallel processes run simultaneously.**

Solution in the [exercise_5](exercise_5/) directory.

Compile the module:

```
erlc web_profiler.erl
```

Call the `run` function:

```
erl
1> web_profiler:run(10000).
182777
```

See https://codereview.stackexchange.com/questions/186662/erlang-pmap-with-max-number-of-processes?newreg=7f2114910c6e41fd82e89c179cf32789 for a different approach to this solution.

**6. Write a version of `pmap` that works with distributed Erlang and distributes the work over several Erlang nodes.**


Solution in the [exercise_6](exercise_6/) directory.

Compile the module:

```
erlc web_profiler.erl
```

Start two nodes in two shells to handle the work:

```
erl -sname watson
```

```
erl -sname holmes
```


In a third shell call the `run` function and pass in the list of nodes:

```
erl
1> web_profiler:run(['watson@trevor-ThinkPad-E550', 'holmes@trevor-ThinkPad-E550'], 10000).
182777
```

**7. Write a version of `pmap` that works with distributed Erlang, distributes the work over several Erlang nodes, and load balances the work between the nodes.**

I chose not to implement load balancing as the round-robin solution I implemented for exercise 6 works well enough in most cases. In scenarios where there are a lot of profiling requests and the nodes are inundated with work, a more complicated load balancing system will be needed. In such cases the solution should take into account the load of each node and send each profiling request to the node with the lowest load.
