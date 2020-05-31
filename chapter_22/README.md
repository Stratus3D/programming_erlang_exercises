# Exercises 22

**1. Implement basic job center functionality with the following interface:**

* `job_centre:start_link() -> true.`
* `job_centre:add_job(F) -> JobNumber.`
* `job_centre:work_wanted() -> {JobNumber, F} | no.`
* `job_centre:job_done(JobNumber)`

Solution in the [exercise_1/](exercise_1/) directory.

```
erl
1> job_centre:start_link().
{ok,<0.67.0>}
2> job_centre:add_job(fun() -> io:format("working on job") end).
1
3> {JobID, JobFun} = job_centre:work_wanted().
{1,#Fun<erl_eval.20.99386804>}
4> JobFun().
working on jobok
5> job_centre:job_done(JobID).
ok
```

**2. Add function named `job_centre:statistics/0` that reports the status of the jobs in the queue.**

Solution in the [exercise_2/](exercise_2/) directory.

```
erl
1> job_centre:start_link().
{ok,<0.67.0>}
2> job_centre:add_job(fun() -> io:format("Working on job 1") end).
1
3> job_centre:add_job(fun() -> io:format("Working on job 2") end).
2
4> {JobID, JobFun} = job_centre:work_wanted().
{1,#Fun<erl_eval.20.99386804>}
5> JobFun().
Working on job 1ok
6> job_centre:job_done(JobID).
not_found
7> job_centre:statistics().
[{1,in_progress},{2,pending}]
```

**3. Add code to monitor the workers. If a worker dies makesure the jobs it was doing are returned to the queue of waiting jobs.**

Solution in the [exercise_3/](exercise_3/) directory.

```
erl
1> job_centre:start_link().
{ok,<0.67.0>}
2> job_centre:add_job(fun() -> io:format("Working on job 1") end).
1
3> job_centre:add_job(fun() -> io:format("Working on job 2") end).
2
4> spawn(fun() ->
    {JobID, JobFun} = job_centre:work_wanted(),
    JobFun(),
    exit(self(), test_failure)
    end).
Working on job 1<0.71.0>
Worker process <0.71.0> crashed with reason test_failureDOWN message from unknown process
5> {JobID, JobFun} = job_centre:work_wanted().
{1,#Fun<erl_eval.20.99386804>}
6> JobFun().
Working on job 1ok
```

**4. Check for lazy workers. Change the work wanted function to return `{JobNumber, JobTime, F}` where JobTime is the number of seconds that the worker as to complete the job by. At `JobTime - 1`, the server should send a `hurry_up` message to the worker if it has not finished the job. And at time `JobTime + 1`, it should kill the worker process with an `exit(Pid, youre_fired)` call.**

**5. Implement a trade union server to monitor the workers. Check that they are not fired without being sent a warning.**

Not going to implement this as it's not a design pattern I've seen regularly used in OTP applications.
