-module(spawn_registered_fun).

-export([start/2]).

% Example usage:
%
% 1> spawn_registered_fun:start(foo, fun() -> receive _ -> ok end end).
% {ok,<0.40.0>}
% 2> spawn_registered_fun:start(foo, fun() -> receive _ -> ok end end).
% {error,already_running}

% Use guards to ensure the arguments passed in are what this function requires
start(AnAtom, Fun) when is_atom(AnAtom), is_function(Fun, 0) ->
    Sender = self(),

    % Since we can only execute Fun after we are sure the the register/2 call
    % has succeeded we need to invoke register/2 and then if it succeeds execute
    % Fun. This is most easily done inside the newly spawned process. If the
    % register/2 call fails we can simply send an error back to the spawning
    % process and let the spawned process exit normally.
    Runner = fun() ->
                     try register(AnAtom, self()) of
                         true ->
                             Sender ! {ok, self()},
                             Fun()
                     catch
                         error:_ ->
                             Sender ! {error, {already_running, self()}}
                     end
             end,

    % Spawn the Runner function as a linked process so we are alware of errors
    Pid = spawn_link(Runner),

    % Receive the messages back from the Runner process so we know the status
    % of the register/2 call so we can return the appropriate value to the
    % caller of this function.
    receive
        {ok, Pid} -> {ok, Pid};
        {error, {already_running, Pid}} -> {error, already_running}
    end.


