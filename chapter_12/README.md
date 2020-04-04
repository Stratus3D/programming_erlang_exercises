# Exercises for Chapter 12

**1. Write a function `start(AnAtom, Fun)` to register `AnAtom` as `spawn(Fun)`. Make sure the program works correctly in the case when two parallel processes simultaneously evalutate `start/2`. In this case ensure one process fails and the other succeeds.**

In `exercise_1/` there is a module named `spawn_registered_fun` that contains a `start/2` function.

Example usage:

```
erlc spawn_registered_fun.erl
erl
1> spawn_registered_fun:start(foo, fun() -> receive _ -> ok end end).
{ok,<0.40.0>}
2> spawn_registered_fun:start(foo, fun() -> receive _ -> ok end end).
{error,already_running}
```
