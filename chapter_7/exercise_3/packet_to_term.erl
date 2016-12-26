-module(packet_to_term).

-export([packet_to_term/1]).

-define(LENGTH_HEADER, 32). % 8 (byte) * 4

packet_to_term(Packet) ->
    % Extract the length from the head of the binary
    <<Length:?LENGTH_HEADER/integer, BinTerm:Length/binary>> = Packet,
    % Take the packet payload portion of the  binary and convert it back to an
    % Erlang term
    binary_to_term(BinTerm).
