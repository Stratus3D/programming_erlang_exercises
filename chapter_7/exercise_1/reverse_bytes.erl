-module(reverse_bytes).

-export([reverse/1]).

% We take a binary
reverse(Binary) when is_binary(Binary) ->
    % TODO: Return reversed binary
    Binary.
