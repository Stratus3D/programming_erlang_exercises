-module(afile_client).

-export([ls/1, get_file/2, put_file/3]).

ls(Server) ->
    Server ! {self(), list_dir},
    receive
        {Server, FileList} ->
            FileList
    end.

get_file(Server, File) ->
    Server ! {self(), {get_file, File}},
    receive
        {Server, Content} ->
            Content
    end.

%% The exercise called for the addition of a put_file function
put_file(Server, File, Contents) ->
    Server ! {self(), {put_file, File, Contents}},
    receive
        {Server, Status} ->
            Status
    end.
