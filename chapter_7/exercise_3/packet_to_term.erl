-module(packet_to_term).

-export([packet_to_term/1]).

packet_to_term(Packet) ->
    <<Length:4/integer, BinTerm/binary>> = Packet,
    erlang:display(Length),
    binary_to_term(BinTerm).
