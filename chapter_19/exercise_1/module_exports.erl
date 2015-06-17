-module(module_exports).

-export([store_exports/0]).

store_exports() ->
    % Create ETS table
    Ets = ets:new(module_exports, [bag, protected]),

    % Create DETS table
    {ok, Name} = dets:open_file(?MODULE, []),


    ok.
