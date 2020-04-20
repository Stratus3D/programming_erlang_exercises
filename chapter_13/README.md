# Exercises for Chapter 13

**1. Write a function named `my_spawn/3` that spawns a process and prints a message when it dies.**

Solution in [exercise_1/](exercise_1/).

**2. Solve exercise 1 with the `on_exit` function shown in chapter 13.**

Solution in [exercise_2/](exercise_2/).

**3. Write a function named `my_spawn/4` that spawns a process and kills it after it runs for more than the given number of seconds.**

Solution in [exercise_3/](exercise_3/).

**4. Write a function that creates a registered process that prints "I'm still running" every 5 seconds. Create another function that monitors this function and restarts it if it dies.**

Solution in [exercise_4/](exercise_4).

**5. Write a function that spawns and monitors several processes. If any monitored processes dies, restart the monitored process.**

Solution in [exercise_5/](exercise_5). The code is in `workers.erl`

Example usage:

```
erlc workers.erl
erl
1> workers:spawn_and_restart_workers(workers:definition()).
[<0.40.0>,<0.42.0>]
I'm still running
I'm still running
```

**6. Write a function that spawns and monitors several processes. If any monitored processes dies, kill all the processes and restart them.**

Solution in [exercise_6/](exercise_6).
