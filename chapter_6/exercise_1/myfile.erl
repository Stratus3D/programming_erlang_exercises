-module(myfile).

-export([read/1]).

% myfile:read returns the contents of the file at
% the specified path or raise an exception if the file
% doesn't exist or an error occurs while reading.
%
% While the exercise doesn't specify it, I am assuming
% that we want to control the exception that is raised
% so I am using a case statement.
read(File) ->
  case file:read_file(File) of
        {ok, Bin} ->
            % Read was successful, return contents of file.
            Bin;
        {error, Why} ->
            % Raise an exception with `{failed, Why}` reason.
            throw({failed, Why})
    end.
