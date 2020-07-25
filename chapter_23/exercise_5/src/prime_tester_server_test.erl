-module(prime_tester_server_test).

-export([run/1]).

run(Nodes) ->
    % Start the application
    ok = application:start(sellaprime),

    CallPrimeTester = fun(Number) ->
                              io:format("Calling prime server~n"),
                              Result = prime_tester_server:is_prime(Number),
                              io:format("Got a result from prime server: ~p~n", [Result])
                      end,

    % Queue some tasks
    spawn_monitor(fun() -> CallPrimeTester(7) end),
    spawn_monitor(fun() -> CallPrimeTester(8) end),
    spawn_monitor(fun() -> CallPrimeTester(9) end),

    % Stop the application
    %ok = application:stop(sellaprime).
    ok.
