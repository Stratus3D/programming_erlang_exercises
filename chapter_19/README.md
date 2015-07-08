#Exercises for Chapter 19

##1. Make key-value table ETS and DETS tables that store all exported functions from all exported modules. The key should be `{Function, Arity}`.
In `exercise_1/` there is a module named `module_exports` that contains a `store_exports/0` function. The function creates an ETS table and a DETS table and stores all exports in both of them.

##2. Implement a shared ETS table. Implement a function named `count:me(Mod, Line)`. Every time it is invoked it should increment a counter for that module and line. You can call this function by adding `count:me(?MODULE, ?LINE)` to your code. Also write functions to initialize and retrieve counts.
In `exercise_2/` there is a module named `count` that exports three functions - `me/2, initialize/0, all_counters/0`.

##3. Write a plagiarism detector. Hash 40 character blocks of text and store them in an ETS table. Repeat the hashing and compare each of the hashes with those stored in the ETS table.
