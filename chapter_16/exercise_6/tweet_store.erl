-module(tweet_store).

-export([init/1, store/2, fetch/1]).

-define(FILENAME, "tweet_store.data").
-define(TWEET_SIZE, 140).

init(K) ->
    BufferSize = K * ?TWEET_SIZE,
    Store = open_store(),
    % Allocate a file of the correct size
    ok = file:allocate(Store, 0, BufferSize),
    close_store(Store).

store(N, Tweet) when is_list(Tweet), length(Tweet) =< ?TWEET_SIZE ->
    store(N, list_to_binary(Tweet));
store(N, Tweet) when is_binary(Tweet), byte_size(Tweet) =< ?TWEET_SIZE ->
    Store = open_store(),
    % Write tweet to file at the right location
    ok = file:pwrite(Store, (N - 1) * ?TWEET_SIZE, Tweet),
    close_store(Store).

fetch(N) ->
    Store = open_store(),
    % Read 140 bytes from the location for tweet N
    case file:pread(Store, (N - 1) * ?TWEET_SIZE, ?TWEET_SIZE) of
        {ok, Tweet} ->
            ok = close_store(Store),
            trim_trailing_bytes(Tweet);
        eof ->
            ok = close_store(Store),
            eof
    end.

% Private helper functions
open_store() ->
    {ok, File} = file:open(?FILENAME, [read, write, binary, raw]),
    File.

close_store(File) ->
    file:close(File).

% From https://stackoverflow.com/questions/5699739/how-do-i-get-binary-byte-length-in-erlang
trim_trailing_bytes(List) when is_list(List) ->
    list_to_binary(lists:reverse(lists:dropwhile(fun(0) -> true; (_) -> false end, lists:reverse(List))));
trim_trailing_bytes(Binary) ->
    trim_trailing_bytes(binary_to_list(Binary)).
