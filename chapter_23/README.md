# Exercise 23

**1. Make a `gen_server` called `prime_tester_server` that tsets if a given number is prime. Use the `is_prime/2` function in `lib_primes` for this. Add this to the supervisor tree in `sellaprime_supervisor`.**

Solution in the [exercise_1/](exercise_1) directory.

```
erl
1> sellaprime_supervisor:start_in_shell_for_testing().
true
2> prime_tester_server:is_prime(7).
{ok,true}
3>
```

**2. Make a pool of ten prime tester servers. Make a queue server that queues requests until one of the prime tester servers becomes free. When a prime tester server is free send it a request.**

Solution in the [exercise_2/](exercise_2/) directory.

Followed much of the advice from https://learnyousomeerlang.com/building-applications-with-otp. Also see https://github.com/esumbar/programming-erlang-chapter-23 for another example. The exercises for this chapter were the most difficult for me to complete, and I ended up doing them after all the other exercises.

```
rebar3 shell
1> sellaprime_supervisor:start_in_shell_for_testing().
true
2> prime_tester_server:is_prime(7).
{ok,true}
3>
```

I also wrote a simple test module that can be run:

```
rebar3 shell
1> prime_tester_server_test:run().
ok
```

**3. Change the code in the prime server testers so that each prime server tester maintains its own queue of requests. Remove the queue server. Write a load balancer that keeps track of the work being done and the requests to be done by the prime tester servers. Requests to test new primes should now be sent to the load balancer. Arrange things so that the load balancer sends requests to the least loaded server.**

Solution in the [exercise_3/](exercise_3/) directory.

Since the prime server testers are the gen servers doing the work they aren't able to handle messages for queuing new prime test requests. And therefore they wouldn't be good at maintaining their own queues of work. With gen_servers it is possible to queue requests using calls or casts, but this behavior is built into OTP and I don't think it is what Joe had in mind for this question. I decided to go with a load balancer that behaves similarly to the server I wrote for exercise 3. The difference is that all workers are spawned and wait for work from the load balancer, and the load balancer sends work to workers using a round robin algorithm.

```
rebar3 shell
1> sellaprime_supervisor:start_in_shell_for_testing().
true
2> prime_tester_load_balancer:is_prime(7).
{ok,true}
3>
```

I also wrote a simple test module that can be run:

```
rebar3 shell
1> prime_tester_server_test:run().
ok
```

**4. Implement a supervisor heirarchy so that if any prime number tester server crashes, it should be restarted. If the load balancer crashes, crash all the prime tester servers and restart everything.**

Solved in exercise 3.

**5. Keep all the data necessary to restart everything replicated on two machines.**

Solution in the [exercise_5/](exercise_5/) directory.

As with exercise 2, I followed the advice I found on https://github.com/esumbar/programming-erlang-chapter-23 and used Mnesia for replicating state data between nodes. While I did implement some basic replication, I don't really think replication makes a lot of sense for any application like this. Requests are short lived and are only processed so that we can send a response back to the process. Once the whole node goes down there is a good chance that original process that made the request no longer exists, so even if we restarted everything on another node the state would be worthless of the processes that sent all the requests were gone. And even if those calling processes got restarted they would have different pids, so we would have no way of figuring out where to send results even if we did resume processing of queued requests on the other node.

```
rebar3 shell --sname holmes@localhost
1> sellaprime_supervisor:start_in_shell_for_testing([node(), watson@localhost]).
true
2> prime_tester_load_balancer:is_prime(7).
{ok,true}
3>
```

And in another shell (and in other directory) run the other node:

```
mkdir watson; cd watson
rebar3 shell --sname watson@localhost
1>
```

I also wrote a simple test module that can be run (make sure you are running the watson node as well as shown above):

```
rebar3 shell
1> prime_tester_server_test:run().
ok
```

**6. Implement a restart to restart everything if an entire machine crashes.**

Solved in exercise 5.
