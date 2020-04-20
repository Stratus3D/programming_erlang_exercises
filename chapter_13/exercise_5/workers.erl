-module(workers).
-export([test/0, definition/0, spawn_and_restart_workers/1, spawn_and_restart/3, test_spawn_and_restart/0]).

% Define a list of worker processes to spawn. Each item is a triple containing
% module name, function name, and list of arguments. Here we define two
% identical worker processes.
definition() ->
    [
     {?MODULE, test, []},
     {?MODULE, test, []}
    ].

% Server process for workers
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

% Spawn all worker processes and return the initial list of worker pids
spawn_and_restart_workers(Workers) ->
    lists:map(fun({Module, Function, Arguments}) ->
                          spawn_and_restart(Module, Function, Arguments)
                  end, Workers).

% Spawn worker and process to restart it when it crashes
spawn_and_restart(Module, Function, Args) ->
  Pid = spawn(Module, Function, Args),
  StartTime = erlang:monotonic_time(),

  spawn_link(fun() ->
        Ref = monitor(process, Pid),
        receive
          {'DOWN', Ref, process, Pid, Why} ->
            TimeDifference = calculate_time_difference(erlang:monotonic_time(), StartTime),
            io:format("==================================================~n"),
            io:format("Process ~w died in ~w millisconds with reason: ~w~n", [Pid, TimeDifference, Why]),
            ?MODULE:spawn_and_restart(Module, Function, Args),
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
