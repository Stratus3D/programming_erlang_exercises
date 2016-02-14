#Exercises for Chapter 5

##1. Write some functions to read configuration files containing JSON and turn them into Erlang maps. Also write code to validate the data in the configuration files.
Unlike what's shown in the book, the map module doesn't actually have a `to_json/1` and `from_json/1` functions. jsx is a JSON library that has similar functions, so we will use it instead. Solution to the exercise is in the `exercise_1` directory. Example usage:

    1> json_configuration:config_to_map("sample_config.json").
    [{<<"param1">>,<<"value1">>},
     {<<"param2">>,<<"value2">>},
     {<<"param3">>,[<<"one">>,<<"two">>,<<"three">>]},
     {<<"param4">>,[]},
     {<<"param5">>,
      [{<<"sub1">>,<<"test1">>},{<<"sub2">>,<<"test2">>}]}]
    2> json_configuration:verify_config("sample_config.json").
    true

##2. Write a function named `map_search_pred` that takes a map `Map` and a predicate `Pred` that returns the first element which `Pred` returns true.
Solution to the exercise is in the `exercise_2` directory. Example usage:

    % Function that returns true when the key is `baz`
    1> Fun = fun(Key, Val) ->
    1>   case Key of
    1>     baz ->
    1>       true;
    1>     _ ->
    1>       false
    1>   end
    1> end.
    #Fun<erl_eval.12.54118792>

    % Passing a map and the predicate function to `map_search_pred`
    2> map_search:map_search_pred(#{first => 'Joe', last => 'Armstrong'}, Fun).
    {baz,bim}
    3>

##3. Make an Erlang module with functions equivalent to the methods defined in Ruby's Hash class.
