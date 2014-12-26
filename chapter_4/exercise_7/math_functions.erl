-module(math_functions).

-export([even/1, odd/1, filter/2, split1/1, split2/1]).

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

split1(List) ->
    % For the first version of split we use an
    % accumulator. So we defined split/2 to pass
    % along the accumulator to each recursive call
    split(List, {[], []}).

split([], {Even, Odd}) ->
    % The Even and Odd lists were constructed in
    % reverse so we reverse them to correct the
    % order.
    {lists:reverse(Even), lists:reverse(Odd)};
split([Item|List], {Even, Odd}) ->
    % In order to determine what list an item
    % should be added to we pass it to even/1. If
    % it returns true we add it to the Even list,
    % otherwise we add it to the Odd list.
    case ?MODULE:even(Item) of
        true ->
            split(List, {[Item|Even], Odd});
        false ->
            split(List, {Even, [Item|Odd]})
    end.

split2(List) ->
    % In the second version of the split function
    % we simply invoke the filter/2 function and
    % pass in a reference to the even/1 or odd/1
    % functions. The first call returns all the even
    % items and the second returns all the odd items.
    Even = filter(fun even/1, List),
    Odd = filter(fun odd/1, List),
    % Then we simply return both lists
    {Even, Odd}.
