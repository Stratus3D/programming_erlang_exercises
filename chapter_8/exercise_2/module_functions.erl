-module(module_functions).

-export([print_export_details/0, loaded_modules/0,
         module_with_most_exports/0, most_common_function_name/0,
         unambiguous_function_names/0]).

print_export_details() ->
    % Print loaded modules
    LoadedModules = loaded_modules(),
    io:fwrite("\nAll loaded modules:\n ~w.\n", [LoadedModules]),

    % Print name of module with most exports
    Module = module_with_most_exports(),
    io:fwrite("\n~w exports more functions than all other modules.\n", [Module]),

    % Print the most common function name
    FunName = most_common_function_name(),
    io:fwrite("\n~w is the most common function name.\n", [FunName]),

    % Print all unambiguous function names
    UnambiguousFunctions = unambiguous_function_names(),
    io:fwrite("\nAll unambiguous functions:\n ~w.\n", [UnambiguousFunctions]),

    ok.

loaded_modules() ->
    % Get all loaded modules
    Modules = code:all_loaded(),

    % Take the list of tuples and convert it to a list of module names (atoms)
    ModuleNames = lists:map(fun({Name, _Path}) ->
                      Name
              end, Modules),
    ModuleNames.

module_with_most_exports() ->
    Modules = loaded_modules(),
    {ModuleName, _Exports} = lists:foldl(fun(NewModuleName, {Module, ExportsLength}) ->
                       NewModuleLength = length(NewModuleName:module_info(exports)),
                       case (NewModuleLength >= ExportsLength) of
                           true ->
                               {NewModuleName, NewModuleLength};
                           _ ->
                               {Module, ExportsLength}
                       end
                end, {none, 0}, Modules),
    ModuleName.

most_common_function_name() ->
    FunctionNames = all_function_names(),
    
    ok.

unambiguous_function_names() ->
    FunctionNames = all_function_names(),
    [].

% Returns an array of all function names from all modules. Includes duplicates
all_function_names() ->
    Modules = loaded_modules(),
    AllExports = lists:map(fun(Mod) ->
                    Mod:module_info(exports)
              end, Modules),
    lists:flatten(AllExports).
