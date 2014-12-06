-module(on_exit).
-export([test/0, spawn_and_print_crash/3, test_spawn_and_print_crash/0]).

test() ->
  receive
    Msg ->
      io:write(Msg)
  after
    5000 ->
      exit(self(), kill)
  end.

spawn_and_print_crash(Module, Function, Args) ->
  {Pid, Ref} = spawn(Module, Function, Args),
  on_exit_print(Pid, now()).

on_exit_print(Pid, StartTime) ->
  spawn(fun() ->
        Ref = monitor(process, Pid),
        receive
          {'DOWN', Ref, process, Pid, Why} ->
            TimeDifference = calculate_time_difference(now(), StartTime),
            io:format("==================================================~n"),
            io:format("Process ~w died in ~w millisconds with reason: ~w~n", [Pid, TimeDifference, Why]),
            io:format("==================================================~n")
        end
  end).

calculate_time_difference(Current, Past) ->
  {Mega, Secs, _} = Current,
  {PastMega, PastSecs, _} = Past,
  (Mega * 1000000 + Secs) - (PastMega * 1000000 + PastSecs).

test_spawn_and_print_crash() ->
  ?MODULE:spawn_and_print_crash(?MODULE, test, []).
