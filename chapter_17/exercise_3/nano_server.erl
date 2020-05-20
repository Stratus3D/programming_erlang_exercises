-module(nano_server).

-export([start/0, loop/1]).

start() ->
    {ok, Socket} = gen_udp:open(2345, [binary]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp, Socket, Host, Port, Bin} ->
            io:format("Server received binary = ~p~n", [Bin]),
            MFA = {Mod, Func, Args} = binary_to_term(Bin),
            io:format("Server (unpacking)  ~p~n", [MFA]),
            Reply = apply(Mod, Func, Args),
            io:format("Server replying = ~p~n", [Reply]),
            gen_udp:send(Socket, Host, Port, term_to_binary(Reply)),
            loop(Socket)
    end.
