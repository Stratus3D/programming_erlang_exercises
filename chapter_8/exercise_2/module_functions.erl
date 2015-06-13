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
    io:fwrite("\nThe ~w module exports more functions than all other modules.\n", [Module]),

    % Print the most common function name
    FunName = most_common_function_name(),
    io:fwrite("\n'~w' is the most common function name.\n", [FunName]),

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
    Functions = all_function_names(),

    % We only care about function names, so we remove arity
    FunctionNames = remove_function_arity(Functions),

    % Get the number of occurances of each name
    FunctionNameCounts = count_function_names(FunctionNames),

    % Find the most common function name
    {Name, _Count} = lists:foldl(fun({Fun, Count}, {MostCommonName, MostCommonNameCount}) ->
                        case (Count > MostCommonNameCount) of
                            true ->
                                {Fun, Count};
                            _ ->
                                {MostCommonName, MostCommonNameCount}
                        end
                    end, {'_', 0}, FunctionNameCounts),
    Name.

unambiguous_function_names() ->
    Functions = all_function_names(),

    % We only care about function names, so we remove arity
    FunctionNames = remove_function_arity(Functions),

    % Get the number of occurances of each name
    FunctionNameCounts = count_function_names(FunctionNames),

    % Find all unambiguous functions return their names
    lists:filtermap(fun({Fun, Count}) ->
                            case Count of
                                1 ->
                                    {true, Fun};
                                _ ->
                                    false
                            end
                    end, FunctionNameCounts).

count_function_names(FunctionNames) ->
    FoldFun = fun(Item, Acc) ->
                      case proplists:lookup(Item, Acc) of
                          none ->
                              % We haven't count this function yet, count it
                              % now.
                              Count = length(lists:filter(fun(Val) ->
                                                                  Item =:= Val
                                                          end, FunctionNames)),
                              [{Item, Count}|Acc];
                          _ ->
                              % We already have it. Skip
                              Acc
                      end
              end,
    lists:foldl(FoldFun, [], FunctionNames).

% Returns an array of all function names from all modules. Includes duplicates
all_function_names() ->
    Modules = loaded_modules(),
    AllExports = lists:map(fun(Mod) ->
                    Mod:module_info(exports)
              end, Modules),
    lists:flatten(AllExports).

% Remove function arity
remove_function_arity(Functions) ->
    remove_function_arity(Functions, []).

remove_function_arity([], Result) ->
    Result;
remove_function_arity([{Name, _Arity}|Functions], Result) ->
    remove_function_arity(Functions, [Name|Result]).
