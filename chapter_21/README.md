#Exercises for Chapter 21

##1. Add an error to dict.erl. Compile the module.
I have altered `dict.erl` so a certain line in it will cause it to crash. `dict.erl` is stored in the `src/` directory.

##2. Write a simple test module that calls dict in various ways to uncover the error.
Test module is in `exercise_2/`. In this directory, run:

    erlc exercise_1/dict.erl exercise_2/dict_test.erl
    erl

And then in the Erlang console run the tests:

    dict_test:test().


##3. Use the code coverage tool to see how many lines in the dict module are being executed by the test module.

