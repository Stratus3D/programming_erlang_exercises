-module(hello).

-export([start/1]).

start(Name) ->
    io:format("Hello " ++ Name ++ "~n").
