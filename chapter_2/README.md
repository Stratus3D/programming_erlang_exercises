# Exercises for Chapter 2

## 1. Start and stop the Erlang shell

To start the shell run `erl`:

    $ erl
    Erlang R16B01 (erts-5.10.2) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

    Eshell V5.10.2  (abort with ^G)
    1>

Then to exit the shell run:

    ^C
    BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
           (v)ersion (k)ill (D)b-tables (d)istribution

Then type `a`.

## 2. Run a few commands in the shell

    $ erl
    Erlang R16B01 (erts-5.10.2) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

    Eshell V5.10.2  (abort with ^G)
    1> Text = "Hello World~n". %% Here we assign a list/string to the variable `Text`
    "Hello World~n"
    2> io:format(Text). %% Here we print the variable `Text` with io:format/1
    Hello World
    ok
    3>

## 3. Make a small modification to hello.erl
I updated it by allowing it to take an a single argument and greet the variable instead of `World`. See source file in exercise_3/.

## 4. Run the client and server code
The source files are the exercise_2 directory. A put_file/3 function as been added to the client. The server has also been updated to handle the new command.
