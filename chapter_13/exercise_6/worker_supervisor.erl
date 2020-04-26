-module(worker_supervisor).
-export([test/0, definition/0, start/1, get_workers/1, supervisor_init/1, test_supervisor/0]).

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

% SupervisorPid = worker_supervisor:start(worker_supervisor:definition()).
% WorkerData = worker_supervisor:get_workers(SupervisorPid).
% [{Pid, _, _}|_] = WorkerData.
% exit(Pid, test).

% API Functions
start(Workers) ->
    spawn_link(?MODULE, supervisor_init, [Workers]).

get_workers(Pid) ->
    Pid ! {get_workers, self()},
    receive
        {Pid, workers, WorkerData} ->
            WorkerData
    end.

% Callback functions
% Spawn worker processes and collect worker process info
supervisor_init(Workers) ->
    WorkerData = lists:map(fun({Module, Function, Arguments}) ->
                                   spawn_with_time(Module, Function, Arguments)
                           end, Workers),

    io:format("~p~n", [WorkerData]),

    % Invoke the handle_crashes recursive function to restart any failed processes
    handle_crashes(WorkerData).

% Spawn worker and process to restart it when it crashes
spawn_with_time(Module, Function, Args) ->
  {Pid, Ref} = spawn_monitor(Module, Function, Args),
  StartTime = erlang:monotonic_time(),
  {Pid, Ref, StartTime, {Module, Function, Args}}.

% Restart processes when they crash
handle_crashes(WorkerData) ->
  receive
      {get_workers, Pid} ->
          Pid ! {self(), workers, WorkerData},

          handle_crashes(WorkerData);
      {'DOWN', Ref, process, Pid, Why} ->
          % Find Pid in ProcessData
          {Pid, Ref, StartTime, _MFA} = lists:keyfind(Pid, 1, WorkerData),

          % Compute time difference
          TimeDifference = calculate_time_difference(erlang:monotonic_time(), StartTime),
          io:format("==================================================~n"),
          io:format("Process ~w died in ~w millisconds with reason: ~w~n", [Pid, TimeDifference, Why]),

          % Kill all the worker processes
          lists:foreach(fun({WorkerPid, _WorkerRef, _WorkerMFA}) ->
                                erlang:is_alive(WorkerPid) and exit(WorkerPid, kill)
                        end, WorkerData),

          % Respawn the all the worker processes
          NewWorkerData = lists:map(fun({_WorkerPid, _WorkerRef, {Module, Function, Arguments}}) ->
                                            spawn_with_time(Module, Function, Arguments)
                                    end, WorkerData),
          io:format("All Processes Restarted~n"),
          io:format("==================================================~n"),

          % Recursively call this function to handle later crashes
          handle_crashes(NewWorkerData)
  end.


calculate_time_difference(Current, Past) ->
  Current - Past.

test_supervisor() ->
    % Spawn the supervsior
    SupervisorPid = worker_supervisor:start(worker_supervisor:definition()),

    % Get the worker data
    WorkerData = worker_supervisor:get_workers(SupervisorPid),

    % Kill one of the workers and see it get restarted
    [{Pid, _, _, _}|_] = WorkerData,
    exit(Pid, test).
