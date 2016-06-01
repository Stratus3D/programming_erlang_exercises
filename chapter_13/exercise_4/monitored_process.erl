-module(monitored_process).
-export([loop/0, spawn_registered/1, monitor_process/2, test/0]).

-define(FIVE_SECONDS, 5000).

% Exported functions
spawn_registered(Name) ->
    % Spawn the process
    Pid = spawn(monitored_process, loop, []),
    % Then register it under the given name
    register(Name, Pid),
    % Return the pid to the caller
    Pid.

loop() ->
    receive
    after
        ?FIVE_SECONDS ->
            % After 5 seconds print a message, then call loop/0 again
            io:format("I'm still running~n", []),
            loop()
    end.

monitor_process(Pid, RestartFun) ->
    on_exit(Pid, fun(_Why) ->
                         NewPid = RestartFun(),
                         monitor_process(NewPid, RestartFun)
                 end).

% Test function
test() ->
    ProcessName = looping_process,

    % Spawn the process
    Pid = spawn_registered(ProcessName),

    % Then monitor it
    monitor_process(Pid, fun() ->
                                 spawn_registered(ProcessName)
                         end),

    % Kill the process
    exit(Pid, kill),
    % Wait for it to die
    timer:sleep(500),

    % ...and make sure it is restarted
    NewPid = whereis(ProcessName),
    true = is_pid(NewPid),

    passed.

% Private functions
on_exit(Pid, Fun) ->
    % Spawn a monitor process
    spawn(fun() ->
                  % Monitor the process we care about
                  Ref = monitor(process, Pid),
                  receive
                      {'DOWN', Ref, process, Pid, Why} ->
                          % When the process we are monitor dies, invoke the
                          % callback function
                          Fun(Why)
                  end
          end).
