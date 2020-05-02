-module(yaf_server).

-export([start/0, start_file_server_loop/3, loop/2]).

start() ->
    lib_chan:start_server("lib_chan.conf").

start_file_server_loop(MM, _ArgsC, ArgsServer) ->
    {_, Directory} = lists:keyfind(directory, 1, ArgsServer),

    loop(MM, Directory).

loop(MM, Directory) ->
    receive
        {chan, MM, {read, Path}} ->
            % Read file at Path and send the contents to the client
            MM ! {send, file:read_file(path(Directory, Path))},
            loop(MM, Directory);

        {chan, MM, {write, Path, Contents}} ->
            % Write Contents to file at Path
            MM ! {send, file:write_file(path(Directory, Path), Contents)},
            loop(MM, Directory);

        {chan, MM, {ls, Path}} ->
            % List files in directory at Path
            MM ! {send, file:list_dir(path(Directory, Path))},
            loop(MM, Directory);

        {chan_closed, MM} ->
            loop(MM, Directory)
    end.

path(Directory, Path) ->
    filename:flatten([Directory, "/", Path]).
