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
    ok.

most_common_function_name() ->
    ok.

unambiguous_function_names() ->
    [].
