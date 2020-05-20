-module(nano_client).

-export([send/3]).

send(Mod, Func, Args) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    ok = gen_udp:send(Socket, "localhost", 2345, term_to_binary({Mod, Func, Args})),
    Value = receive
        {udp, Socket, _, _, Bin} ->
            io:format("Client received binary = ~p~n", [Bin]),
            Val = binary_to_term(Bin),
            io:format("Client result = ~p~n", [Val]),
            Val
    after 2000 ->
              {error, timeout}
    end,
    gen_udp:close(Socket),
    Value.
