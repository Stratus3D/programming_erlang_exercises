-module(math_functions).

-export([even/1, odd/1, filter/2]).

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

filter(Fun, List) ->
    % Invoke filter/3, which does the real work
    % then reverse the resulting list, as
    % filter/3 returns a list in reverse order.
    lists:reverse(filter(Fun, List, [])).

filter(_Fun, [], Result) ->
    % If there are no more items in the list
    % return the result
    Result;
filter(Fun, [Item|Remaining], Result) ->
    % If another item still exists in the list
    % Apply `Fun` to it and check the result,
    % if true, add the item to the result.
    case Fun(Item) of
        true ->
            filter(Fun, Remaining, [Item|Result]);
        _ ->
            filter(Fun, Remaining, Result)
    end.
