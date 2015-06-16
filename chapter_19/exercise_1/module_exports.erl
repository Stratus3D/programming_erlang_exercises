-module(module_exports).

-export([store_exports/0]).

store_exports() ->
    Ets = ets:new(module_exports, [bag]),
    ok.
