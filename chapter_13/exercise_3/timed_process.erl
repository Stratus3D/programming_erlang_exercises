-module(timed_process).
-export([test/0, my_spawn/4, test_my_spawn/0]).

test() ->
  receive
    Msg ->
      io:write(Msg)
  after
    10000 ->
      exit(self(), kill)
  end.

my_spawn(Module, Function, Args, Seconds) ->
  Pid = spawn(Module, Function, Args),
  kill_after(Pid, Seconds).

kill_after(Pid, Seconds) ->
  StartTime = now(),

  spawn(fun() ->
        Ref = monitor(process, Pid),
        receive
          {'DOWN', Ref, process, Pid, Why} ->
            TimeDifference = calculate_time_difference(now(), StartTime),
            io:format("==================================================~n"),
            io:format("Process ~w died in ~w millisconds with reason: ~w~n", [Pid, TimeDifference, Why]),
            io:format("==================================================~n")
        after
          Seconds ->
            io:format("==================================================~n"),
            io:format("Process ~w ran too long. Killed after ~w seconds~n", [Pid, Seconds]),
            io:format("==================================================~n"),
            exit(Pid, kill)
        end
  end).

calculate_time_difference(Current, Past) ->
  {Mega, Secs, _} = Current,
  {PastMega, PastSecs, _} = Past,
  (Mega * 1000000 + Secs) - (PastMega * 1000000 + PastSecs).

test_my_spawn() ->
  ?MODULE:my_spawn(?MODULE, test, [], 3).
