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

Followed much of the advice from https://learnyousomeerlang.com/building-applications-with-otp. Also see https://github.com/esumbar/programming-erlang-chapter-23 for another example.

```

```
