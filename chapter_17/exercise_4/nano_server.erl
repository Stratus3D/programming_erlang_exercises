-module(nano_server).

-export([start/0, loop/1]).

start() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
                                         {reuseaddr, true},
                                         {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, EncryptedBin} ->
            Bin = encrypt:decrypt(EncryptedBin),
            io:format("Server received binary = ~p~n", [Bin]),
            MFA = {Mod, Func, Args} = binary_to_term(Bin),
            io:format("Server (unpacking)  ~p~n", [MFA]),
            Reply = apply(Mod, Func, Args),
            io:format("Server replying = ~p~n", [Reply]),
            gen_tcp:send(Socket, encrypt:encrypt(term_to_binary(Reply))),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.
