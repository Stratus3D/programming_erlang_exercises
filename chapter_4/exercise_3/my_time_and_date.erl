-module(my_time_and_date).

-export([my_time_func/1, my_date_string/0]).

my_time_func(F) ->
    % Get the microseconds before invoking the function
    {_, _, StartMicro} = now(),
    % Invoke the function
    F(),
    % Get the microseconds again
    {_, _, FinishMicro} = now(),
    % Compute the number of microseconds it took for the
    % function to return
    FinishMicro - StartMicro.

my_date_string() ->
    % Get the year month and day from the date/0 function
    {Year, Month, Day} = date(),
    % Get the hour minute and second from the time/0 function
    {Hour, Minute, Second} = time(),
    % Use io_lib to format the values into a readable date time
    TimeList = io_lib:format(
            "~.2.0w/~.2.0w/~.4.0w ~.2.0w:~.2.0w:~.2.0w",
            [Month, Day, Year, Hour, Minute, Second]),
    % Flatten the TimeList as it contains nested lists (nested
    % lists prevent the list from being formatted as a string).
    lists:flatten(TimeList).
