# Exercises for Chapter 21

## 1. Add an error to dict.erl. Compile the module.
I have altered `dict.erl` so a certain line in it will cause it to crash. `dict.erl` is stored in the `exercise_1/` directory.

## 2. Write a simple test module that calls dict in various ways to uncover the error.
Test module is in `exercise_2/`. In this directory, run:

    erlc exercise_1/dict.erl exercise_2/dict_test.erl
    erl

And then in the Erlang console run the tests:

    dict_test:test().


## 3. Use the code coverage tool to see how many lines in the dict module are being executed by the test module.
The code coverage script is stored in the `exercise_3` directory. To run the code coverage analysis compile the code and start an erl console:

    erlc exercise_1/dict.erl exercise_2/dict_test.erl exercise_3/dict_test_coverage.erl
    erl

And then in the Erlang console run the command to generate the code coverage results for the tests written in exercise 2:

    dict_test_coverage:coverage().

## 4. Use the dbg module to find the error in the dict module
The dbg script is stored in the `exercise_4` directory. To run the dbg tracing on the dict module compile the code and start an erl console:

    erlc exercise_1/dict.erl exercise_2/dict_test.erl exercise_3/dict_test_coverage.erl
    erl

And then in the Erlang console run:

    dict_test_debug:trace().
