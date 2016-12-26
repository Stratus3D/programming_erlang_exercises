-module(reverse_bits).

-export([reverse/1]).

% We take a binary and then need to reverse all the bits.
reverse(Binary) ->
    % Call the recursive function reverse/2 with the binary and an empty binary
    % as the accumlator.
    reverse(Binary, <<>>).

reverse(<<>>, Result) ->
    % Reverse the binary until we get to the end, then return the accumlator
    Result;
reverse(<<Bit:1, Bin/bitstring>>, Result) ->
    % Take the first bit off the front of the binary and push it onto the
    % accumlator binary. Call the function again with the remaining bits
    reverse(Bin, <<Bit:1, Result/bitstring>>).
