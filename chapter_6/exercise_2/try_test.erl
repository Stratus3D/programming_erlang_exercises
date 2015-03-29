-module(try_test).

-export([generate_exception/1, demo1/0, demo2/0, demo3/0, polite_and_detailed_messages/0]).

generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> error(a).

demo1() ->
    [catcher(I) || I <- [1,2,3,4,5]].

catcher(N) ->
    try generate_exception(N) of
        Val -> {N, normal, Val}
    catch
        throw:X -> {N, caught, throw, X};
        exit:X  -> {N, caught, exited, X};
        error:X -> {N, caught, error, X}
    end.

demo2() ->
    [{I, (catch generate_exception(I))} || I <- [1,2,3,4,5]].

demo3() ->
    try generate_exception(5)
    catch
        error:X ->
            {X, erlang:get_stacktrace()}
    end.

% Exercise 6.2 says we need to provide two messages for each
% exception. To do that I duplicated the catcher function above
% and updated the return value so that is it always two tuples.
% The first item in the tuple is a tuple representing the polite
% error message for the user. The second is a tuple of four items
% for the developer. The last item in the tuple is the stacktrace.
polite_and_detailed_message(N) ->
    try generate_exception(N) of
        Val -> {Val, {N, normal, Val, erlang:get_stacktrace()}}
    catch
        throw:X -> {{caught_throw, X}, {N, caught, throw, X, erlang:get_stacktrace()}};
        exit:X  -> {{caught_exit, X}, {N, caught, exited, X, erlang:get_stacktrace()}};
        error:X -> {{caught_error, X}, {N, caught, error, X, erlang:get_stacktrace()}}
    end.

% In order to demonstrate the polite_and_detailed_message
% function, I created this function called
% polite_and_detailed_messages. It is almost the same as
% demo1/0.
polite_and_detailed_messages() ->
    [polite_and_detailed_message(I) || I <- [1,2,3,4,5]].
