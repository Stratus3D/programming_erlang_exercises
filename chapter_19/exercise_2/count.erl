-module(count).

-export([initialize/0, me/2, all_counters/0]).

initialize() ->
   ets:new(counter, [set, public]).

me(Module, Line) ->
    ets:insert(counter, {{Module, Line}, 1}).

all_counters() ->
    ets:foldl(fun(Item, Acc) ->
                      [Item|Acc]
              end, [], Tab).
