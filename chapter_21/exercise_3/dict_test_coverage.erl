-module(dict_test_coverage).

-export([coverage/0]).

% Check the code coverage of the dict_test:test/0 test on the dict module.

coverage() ->
    % Start the code coverage tool
    {ok, _CoverPid} = cover:start(),

    % Compile the dict module we are testing with cover
    {ok, _Mod} = cover:compile("exercise_1/dict.erl"),

    % Run tests which exercise the dict module
    dict_test:test(),

    % Write the results to a file
    {ok, _File} = cover:analyse_to_file(dict).
