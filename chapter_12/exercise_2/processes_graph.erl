-module(processes_graph).

-export([generate_data/3, spawn_and_time/1]).

% generate_data(OutputFilename, End, Step)
%
% Invoke spawn_and_time/1 repeatedly with an number that increases by `Step`
% until `End` is reached. Write the resulting times to the file specified as
% `OutputFilename` formatted as CSV.
%
% Example
% processes_graph:generate_data("data.csv", 50, 25).

generate_data(OutputFilename, End, Step) when is_integer(End), is_integer(Step) ->
    {ok, File} = file:open(OutputFilename, [write]),
    Start = 1,
    Steps = lists:seq(Start, End + 1, Step),
    io:format("Steps: ~p", [Steps]),

    Max = erlang:system_info(process_limit),
    io:format("Maximum allowed processes: ~p~n", [Max]),

    RunStats = fun(N) ->
            [T1, T2] = spawn_and_time(N),
            io:format(File, "~p, ~p, ~p~n", [N, T1, T2])
    end,

    lists:foreach(RunStats, Steps),

    ok = file:close(File).


% spawn_and_time(N)
%
% Create N processes then destroy them. See how much time this takes and return
% the runtime and wall block time.

spawn_and_time(N) ->
    statistics(runtime),
    statistics(wall_clock),
    L = for(1, N, fun() -> spawn(fun() -> wait() end) end),
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    lists:foreach(fun(Pid) -> Pid ! die end, L),
    U1 = Time1 * 1000 / N,
    U2 = Time2 * 1000 / N,
    [U1, U2].

wait() ->
    receive
        die -> void
    end.

for(N, N, F) -> [F()];
for(I, N, F) -> [F()|for(I+1, N, F)].
