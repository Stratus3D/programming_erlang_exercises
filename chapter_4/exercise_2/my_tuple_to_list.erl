-module(my_tuple_to_list).

-export([my_tuple_to_list/1]).

% Convert a tuple to a list without using the BIF
my_tuple_to_list(Tuple) ->
    Size = size(Tuple),
    % Get the size of the tuple  so we know how many items
    % to copy over to the list.
    do_tuple_to_list(Size, Tuple, []).

do_tuple_to_list(0, _Tuple, List) ->
    % Once there are no remaining items in the
    % tuple return the list
    List;

do_tuple_to_list(Remaining, Tuple, List) ->
    % Take the last item of the tuple and added it to
    % the beginning of the list (list is created in
    % reverse order).
    NewItem = element(Remaining, Tuple),
    do_tuple_to_list(Remaining - 1, Tuple, [NewItem|List]).
