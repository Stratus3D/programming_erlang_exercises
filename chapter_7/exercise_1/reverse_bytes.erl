-module(reverse_bytes).

-export([reverse/1]).

% We take a binary and then need to reverse all the bytes. Reversing lists is
% very efficient (and very easy) in Erlang. So we convert the binary to a list,
% reverse the list, and the conver the list back to a binary.
reverse(Binary) when is_binary(Binary) ->
    list_to_binary(lists:reverse(binary_to_list(Binary))).
