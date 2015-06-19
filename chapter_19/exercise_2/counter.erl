-module(counter).

-export([initialize/0, me/2]).

initialize() ->
   ets:new(counter, [set, public]).

me(Module, Line) ->
   ets:insert(counter, {{Module, Line}, 1}).
