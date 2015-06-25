-module(count).

-export([initialize/0, me/2, all_counters/0]).

-define(TABLE_NAME, counter).

initialize() ->
    % Create a new ets table
    ets:new(?TABLE_NAME, [set, public]).

me(Module, Line) ->
    % Keys are composed of modules and line numbers
    Key = {Module, Line},

    case ets:lookup(?TABLE_NAME, Key) of
        [] ->
            % Insert new element with a count of one if no elements match
            ets:insert(?TABLE_NAME, {Key, 1});
        _ ->
            % We can use `update_counter` to update the counter if it exists
            ets:update_counter(?TABLE_NAME, Key, {2, 1})
    end.

all_counters() ->
    % We iterate over all the items in the table, adding them to a list.
    ets:foldl(fun(Item, Acc) ->
                      [Item|Acc]
              end, [], ?TABLE_NAME).
