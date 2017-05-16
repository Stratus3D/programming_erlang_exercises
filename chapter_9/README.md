# Exercises for Chapter 9

## 1. Write a small module. Write type specs for the exported functions. Make a type error in one of the functions and run dialyzer against the module.
Code for this exercise is stored in `exercise_1/`. The command I used to run dialyzer was `dialyzer simple_types.erl`. You will also need to have a PLT file generated.

## 2. Find the type annotations in the standard libraries. Read the type annotations in the `lists` module.

## 3. Why is it a good idea to think about types before you write code?

Types are part of your API. The functions you define will only accept certain types, so it's important to figure out what types of data your API should be handling. This doesn't mean you always must have valid type specs during development though, once the types have been thought through it's fine to comment them out during a "cowboy coding" session and uncomment them when you are finishing with the function or module you are developing.

## 4. Create a module that exports an opaque type. Then create a second module that uses the type in such a way as to cause an abstraction violation.
Code for this exercise is stored in `exercise_4/`.
