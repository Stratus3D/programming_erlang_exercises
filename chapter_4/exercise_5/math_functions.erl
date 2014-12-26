-module(math_functions).

-export([even/1, odd/1]).

even(Number) ->
    % Use `rem` to check if the remainder is 0
    % if it is return true, otherwise false
    0 =:= Number rem 2.

odd(Number) ->
    % Use `rem` to check if the remainder is 1
    % if it is return true, otherwise false
    % (for odd numbers the remainder of
    % division by 2 will always be 1)
    1 =:= Number rem 2.
