-module(term_to_packet).

-export([term_to_packet/1]).

term_to_packet(Term) ->
    BinTerm = term_to_binary(Term), % The payload
    Length = byte_size(BinTerm), % The value for the 4 byte header
    % Then we construct the binary. First we set the length as the first 4 bytes.
    % Then the BinTerm binary with the payload.
    <<Length:4, BinTerm/binary>>.
