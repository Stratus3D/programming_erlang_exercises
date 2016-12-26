-module(simple_types).

-export([add/2, concat/2]).

% Function spec that is correct
-spec add(X :: number(), Y :: number()) -> number().

add(X, Y) ->
    X + Y.

% Function spec with incorrect function argument types
-spec concat(String1 :: atom(), String2 :: string()) -> list().

concat(String1, String2) when is_integer(String1) ->
    [String1] ++ String2;

concat(String1, String2) when is_integer(String2) ->
    String1 ++ [String2];

concat(String1, String2) when is_integer(String1), is_integer(String2) ->
    [String1, String2].
