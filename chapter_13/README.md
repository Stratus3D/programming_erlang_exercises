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

Solution in [exercise_5/](exercise_5). The code is in `worker_supervisor.erl`

Example usage:

```
erlc worker_supervisors.erl
erl
% Start the supervisor with a list of children
1> SupervisorPid = worker_supervisor:start(worker_supervisor:definition()).
I'm still running
I'm still running
% Get one of the child processes
2> WorkerData = worker_supervisor:get_workers(SupervisorPid).
[{<0.41.0>,#Ref<0.0.4.148>,-576460742360537501,{worker_supervisor,test,[]}},
 {<0.42.0>,#Ref<0.0.4.149>,-576460742360529319,{worker_supervisor,test,[]}}]
3> [{Pid, _, _, _}|_] = WorkerData.
% And kill it to verify it got restarted
4> exit(Pid, test).
```

**6. Write a function that spawns and monitors several processes. If any monitored processes dies, kill all the processes and restart them.**

Solution in [exercise_6/](exercise_6). The code is in `worker_supervisor.erl`

```
erlc worker_supervisors.erl
erl
% Start the supervisor with a list of children
1> SupervisorPid = worker_supervisor:start(worker_supervisor:definition()).
I'm still running
I'm still running
% Get one of the child processes
2> WorkerData = worker_supervisor:get_workers(SupervisorPid).
[{<0.41.0>,#Ref<0.0.4.148>,-576460742360537501,{worker_supervisor,test,[]}},
 {<0.42.0>,#Ref<0.0.4.149>,-576460742360529319,{worker_supervisor,test,[]}}]
3> [{Pid, _, _, _}|_] = WorkerData.
% And kill it to verify it got restarted
4> exit(Pid, test).
```
