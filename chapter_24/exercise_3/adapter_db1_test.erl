%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(adapter_db1_test).

-export([test/0]).

test() ->
    % Test the server
    {ok, _Pids} = adapter_db1:start_link(),

    % Store a key in memory
    adapter_db1:put(key1, memory, val1),
    % Store a key on disk
    adapter_db1:put(key2, memory, val2),

    % Lookup the key in memory
    {ok, val1} = adapter_db1:lookup(key1),
    % Lookup the key on disk
    {ok, val2} = adapter_db1:lookup(key2),

    % Lookup a non-existant key
    error = adapter_db1:lookup(nokey),
    ok.
