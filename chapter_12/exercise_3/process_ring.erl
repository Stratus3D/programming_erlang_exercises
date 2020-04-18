-module(process_ring).

-export([run/3, loop/1]).

% Spawn a ring of `Processes` number processes and send `Message` around the
% ring `Loops` times.
%
% Example usage:
% > process_ring:run(1000, 1000, message).
% Spawning 1000 processes and sending the message around the ring 1000 times took 485268 microseconds

run(Processes, Loops, Message) when is_integer(Processes), is_integer(Loops) ->

    {Time, FirstPid} = timer:tc(fun() ->
                     % Spawn processes in loop
                     FirstPid = spawn_ring(Processes, self()),

                     % Send process around the ring
                     send_around_ring(Loops, FirstPid, Message),

                     FirstPid
             end),

    % Print out the results
    FormatString = "Spawning ~p processes and sending the message around the ring ~p times took ~p milliseconds~n",
    io:format(FormatString, [Processes, Loops, Time]),

    % Tear down the ring
    FirstPid ! stop.

send_around_ring(0, _FirstPid, _Message) ->
    ok;
send_around_ring(Loops, FirstPid, Message) ->
    FirstPid ! Message,
    receive
        Message -> send_around_ring(Loops - 1, FirstPid, Message)
    end.

spawn_ring(0, PreviousPid) ->
    PreviousPid;
spawn_ring(Processes, PreviousPid) ->
    Pid = spawn_link(?MODULE, loop, [PreviousPid]),
    spawn_ring(Processes - 1, Pid).

% Server loop for processes in ring
loop(NextPid) ->
    receive
        stop ->
            NextPid ! stop,
            ok;
        Value ->
            NextPid ! Value,
            loop(NextPid)
    end.
