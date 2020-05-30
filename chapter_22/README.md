# Exercises 22

**1. Implement basic job center functionality with the following interface:**

* `job_centre:start_link() -> true.`
* `job_centre:add_job(F) -> JobNumber.`
* `job_centre:work_wanted() -> {JobNumber, F} | no.`
* `job_centre:job_done(JobNumber)`

**2. Add function named `job_centre:statistics/0` that reports the status of the jobs in the queue.**

**3. Add code to monitor the workers. If a worker dies makesure the jobs it was doing are returned to the queue of waiting jobs.**

**4. Check for lazy workers. Change the work wanted function to return `{JobNumber, JobTime, F}` where JobTime is the number of seconds that the worker as to complete the job by. At `JobTime - 1`, the server should send a `hurry_up` message to the worker if it has not finished the job. And at time `JobTime + 1`, it should kill the worker process with an `exit(Pid, youre_fired)` call.**

**5. Implement a trade union server to monitor the workers. Check that they are not fired without being sent a warning.**

Not going to implement this as it's not a design pattern I've seen regularly used in OTP applications.
