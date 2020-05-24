-module(nano_client).

-export([send_msg/3, receive_msgs/2, send/3]).

% Define addresses as username - hostname tuples instead of an email address
% {username, hostname}

% Simple function for sending messages to a recipient identified by
% username/hostname combination.
send_msg(Username, Hostname, Msg) ->
    send(Username, Hostname, {send_msg, Msg}).

% Receive all messages sent to a specific address
receive_msgs(Username, Hostname) ->
    send(Username, Hostname, get_all_msgs).

send(Username, Hostname, Msg) ->
    {ok, Socket} = gen_tcp:connect(Hostname, 2345, [binary, {packet, 4}, {port, 0}]),
    ok = gen_tcp:send(Socket, term_to_binary({Username, Msg})),
    receive
        {tcp, Socket, Bin} ->
            io:format("Client received binary = ~p~n", [Bin]),
            Val = binary_to_term(Bin),
            io:format("Client result = ~p~n", [Val]),
            gen_tcp:close(Socket)
    end.
