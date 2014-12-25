-module(my_tuple_to_list).

-export([my_tuple_to_list/1]).

my_tuple_to_list(Tuple) ->
    Size = size(Tuple),
    do_tuple_to_list(Size, Tuple, []).

do_tuple_to_list(0, _Tuple, List) ->
    List;
do_tuple_to_list(Remaining, Tuple, List) ->
    NewItem = element(Remaining, Tuple),
    do_tuple_to_list(Remaining - 1, Tuple, [NewItem|List]).
