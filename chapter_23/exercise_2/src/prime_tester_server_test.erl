-module(prime_tester_server_test).

-export([run/0]).

run() ->
    % Start the application
    ok = application:start(sellaprime),

    CallPrimeTester = fun(Number) ->
                              io:format("Calling prime server"),
                              Result = prime_tester_server:is_prime(Number),
                              io:format("Got a result from prime server: ~p", [Result])
                      end,

    % Queue some tasks
    spawn(fun() -> CallPrimeTester(7) end),
    spawn(fun() -> CallPrimeTester(8) end),
    spawn(fun() -> CallPrimeTester(9) end),

    % Stop the application
    ok = application:stop(sellaprime).
