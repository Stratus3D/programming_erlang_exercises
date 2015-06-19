#Exercises for Chapter 19

##1. Make key-value table ETS and DETS tables that store all exported functions from all exported modules. The key should be `{Function, Arity}`.
In `exercise_1/` there is a module named `module_exports` that contains a `store_exports/0` function. The function creates an ETS table and a DETS table and stores all exports in both of them.
