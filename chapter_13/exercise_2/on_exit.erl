-module(on_exit).
-export([test/0, spawn_and_print_crash/3, test_spawn_and_print_crash/0]).

% Function that will be spawned in our test. Automatically kills itself after
% 5 seconds
test() ->
  receive
    Msg ->
      io:write(Msg)
  after
    5000 ->
      exit(self(), kill)
  end.

% Spawn a process and add an on_exit handler function that prints out the
% details of the crash
spawn_and_print_crash(Module, Function, Args) ->
  Pid = spawn(Module, Function, Args),
  StartTime = now(),
  Handler = fun(Why) ->
                TimeDifference = calculate_time_difference(now(), StartTime),
                io:format("==================================================~n"),
                io:format("Process ~w died in ~w millisconds with reason: ~w~n", [Pid, TimeDifference, Why]),
                io:format("==================================================~n")
            end,
  on_exit(Pid, Handler).

% The on_exit function that spawns a monitor and calls `Fun` when the process
% dies
on_exit(Pid, Fun) ->
  spawn(fun() ->
        Ref = monitor(process, Pid),
        receive
          {'DOWN', Ref, process, Pid, Why} ->
                Fun(Why)
        end
  end).

% Utility function for calculating how long the process lived
calculate_time_difference(Current, Past) ->
  {Mega, Secs, _} = Current,
  {PastMega, PastSecs, _} = Past,
  (Mega * 1000000 + Secs) - (PastMega * 1000000 + PastSecs).

% Function to test this behavior
test_spawn_and_print_crash() ->
  ?MODULE:spawn_and_print_crash(?MODULE, test, []).
