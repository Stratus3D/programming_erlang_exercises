-module(map_search).

-export([map_search_pred/2]).

map_search_pred(Map, Pred) ->
    Proplist = maps:to_list(Map),
    proplist_search_pred(Proplist, Pred).

proplist_search_pred([], _Pred) ->
    false;
proplist_search_pred([{Key, Value}|Map], Pred) ->
    case Pred(Key, Value) of
        true ->
            {Key, Value};
        false ->
            proplist_search_pred(Map, Pred)
    end.
