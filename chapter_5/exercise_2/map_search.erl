-module(map_search).

-export([map_search_pred/2]).

map_search_pred(Map, Pred) ->
    % We need to convert the map to a list before we can recurse over it
    Proplist = maps:to_list(Map),
    % Once we have a proplist we can recurse over it and return the key/value
    % pair matching the `Pred` function.
    proplist_search_pred(Proplist, Pred).

proplist_search_pred([], _Pred) ->
    % If no matching key/value pairs are found return false
    false;
proplist_search_pred([{Key, Value}|Map], Pred) ->
    case Pred(Key, Value) of
        true ->
            % If the predicate function returns true return the key/value pair
            {Key, Value};
        false ->
            % Otherwise continue to search for a matching pair
            proplist_search_pred(Map, Pred)
    end.
