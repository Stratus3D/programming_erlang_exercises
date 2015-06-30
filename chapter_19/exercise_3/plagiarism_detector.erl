-module(plagiarism_detector).

% API
-export([]).

hash_block(Term) ->
    erlang:phash(Term).
