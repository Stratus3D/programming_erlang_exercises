# Exercises for Chapter 15

**1. Download the code for the port driver given earlier and test it.**

Nothing needed for this exercise.

**2. Download code for a linked in driver and test it on your system**

The book says to download drivers from git://github.com/erlang/linked_in_drivers.git but this repository no longer exists.

**3. Find an operating system command to discover what kind of CPU your computer has. Write a function that returns your CPU type using the `os:cmd/1`**

Solution in the `exercise_3/` directory.

```
erlc cpu_info.erl
erl
1> cpu_info:get().
[{"Architecture","x86_64"},
 {"CPU op-mode","32-bit, 64-bit"},
 {"Model name","Intel(R) Core(TM) i7-5500U CPU @ 2.40GHz"},
 ...
2> cpu_info:get("Model name").
"Intel(R) Core(TM) i7-5500U CPU @ 2.40GHz"
```
