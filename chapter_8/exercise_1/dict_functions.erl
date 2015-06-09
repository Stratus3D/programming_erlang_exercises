-module(dict_functions).

-export([count/0]).

% count/0 prints the count of the functions exported from `dict` to STDOUT
count() ->
    % module_info(exports) returns a list of all exported functions
    Functions = dict:module_info(exports),

    % We take the length of the list that contains the exported functions
    NumFunctions = length(Functions),

    % And then we print the number of exported functions
    io:fwrite("Dict exports ~w functions.\n", [NumFunctions]),
    ok.
