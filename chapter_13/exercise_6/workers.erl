-module(workers).
-export([test/0, spawn_and_restart/4, test_spawn_and_restart/0]).

test() ->
  receive
    Msg ->
      io:write(Msg),
      test()
  after
    5000 ->
      io:format("I'm still running~n"),
      test()
  end.

spawn_and_restart(Module, Function, Args, Seconds) ->
  Pid = spawn(Module, Function, Args),
  StartTime = now(),

  spawn(fun() ->
        Ref = monitor(process, Pid),
        receive
          {'DOWN', Ref, process, Pid, Why} ->
            TimeDifference = calculate_time_difference(now(), StartTime),
            io:format("==================================================~n"),
            io:format("Process ~w died in ~w millisconds with reason: ~w~n", [Pid, TimeDifference, Why]),
            ?MODULE:spawn_and_restart(Module, Function, Args, Seconds),
            io:format("Process Restarted~n"),
            io:format("==================================================~n"),
            exit(self(), kill)
        end
  end),
  Pid.

calculate_time_difference(Current, Past) ->
  {Mega, Secs, _} = Current,
  {PastMega, PastSecs, _} = Past,
  (Mega * 1000000 + Secs) - (PastMega * 1000000 + PastSecs).

test_spawn_and_restart() ->
  Pid = ?MODULE:spawn_and_restart(?MODULE, test, [], 30),
  receive
    something ->
      blah
  after
    20000 ->
      exit(Pid, kill)
  end.
