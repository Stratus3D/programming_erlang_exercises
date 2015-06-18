-module(module_exports).

-export([store_exports/0]).

store_exports() ->
    io:format("Creating tables...~n"),

    % Create ETS table
    Ets = ets:new(module_exports, [bag, protected]),

    % Create DETS table
    {ok, DetsName} = dets:open_file(?MODULE, []),

    % Insert function
    InsertIntoBoth = fun({Mod, Exports}) ->
                             lists:map(fun(Export) ->
                                               ets:insert(Ets, {Export, Mod}),
                                               dets:insert(DetsName, {Export, Mod})
                                       end, Exports)
                     end,

    io:format("Populating tables with module exports...~n"),
    % Populate the tables
    lists:map(InsertIntoBoth, all_functions()),

    io:format("Complete~n"),

    % Close the dets table properly
    io:format("Closing DETS table...~n"),
    dets:close(DetsName),
    io:format("Table closed~n"),
    ok.

loaded_modules() ->
    % Get all loaded modules
    Modules = code:all_loaded(),

    % Take the list of tuples and convert it to a list of module names (atoms)
    ModuleNames = lists:map(fun({Name, _Path}) ->
                      Name
              end, Modules),
    ModuleNames.

all_functions() ->
    % Returns an array of all functions from all modules. Include duplicates
    Modules = loaded_modules(),

    % Map modules and module exports to `{module, [Export]}` tuples
    AllExports = lists:map(fun(Mod) ->
                    {Mod, Mod:module_info(exports)}
              end, Modules),
    lists:flatten(AllExports).
