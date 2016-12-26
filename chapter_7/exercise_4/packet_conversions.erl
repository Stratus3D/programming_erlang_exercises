-module(packet_conversions).

-export([test/0]).

-define(LENGTH_HEADER, 32). % 8 (byte) * 4

test() ->
    % Test conversion from term to packet
    Term = {foo, bar, baz},
    ExpectedLength = byte_size(term_to_binary(Term)),
    ExpectedTotalLength = ExpectedLength + 4,

    % Assert that the binary returned is in the format we expect
    Packet = <<Length:?LENGTH_HEADER/integer, Payload/binary>> = term_to_packet:term_to_packet(Term),

    % Assert the binary is the right size
    ExpectedLength = Length,
    ExpectedTotalLength = byte_size(Packet),
    Term = binary_to_term(Payload),

    % Test conversion from packet to term
    Term = packet_to_term:packet_to_term(Packet),
    ok.
