-module(nano_server).

-export([start/0, loop/1]).

start() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
                                         {keepalive, true},
                                         {reuseaddr, true},
                                         {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Server received binary = ~p~n", [Bin]),
            {Username, Msg} = Raw = binary_to_term(Bin),
            io:format("Server (unpacking)  ~p~n", [Raw]),
            Reply = handle_message(Username, Msg),
            io:format("Server replying = ~p~n", [Reply]),
            gen_tcp:send(Socket, term_to_binary(Reply)),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n"),
            start()
    end.

handle_message(Username, Msg) ->
    % Dispatch messages to the correct function
    case Msg of
        {send_msg, EmailMessage} ->
            send_message(Username, EmailMessage);
        get_all_msgs ->
            read_messages(Username);
        _ ->
            unknown_message
    end.

% Read all messages for user off disk
read_messages(Username) ->
    Directory = ensure_mbox_directory_exists(Username),
    {ok, Filenames} = file:list_dir(Directory),
    lists:map(fun(Filename) ->
                      io:format("Filename: ~p", [Filename]),
                      {ok, Contents} = file:read_file(filename:join([Directory, Filename])),
                      Title = filename:basename(Filename),
                      {Title, Contents}
              end, Filenames).

send_message(Username, Msg) ->
    Directory = ensure_mbox_directory_exists(Username),
    io:format("Writing ~p~n", [Directory]),
    % Write message to disk
    Filename = lists:flatten(io_lib:format("~p-message.txt",[erlang:monotonic_time() * -1])),
    FullFilename = filename:join([Directory, Filename]),
    io:format("Writing ~p~n", [FullFilename]),
    ok = file:write_file(FullFilename, Msg).

ensure_mbox_directory_exists(Username) when is_binary(Username); is_list(Username) ->
    MboxDir = mbox_base_directory(),
    _ = filelib:ensure_dir(MboxDir),
    Directory = filename:join([MboxDir, [Username, "/"]]),
    % Trailing slash is a hack to get filelib:ensure_dir/1 to always work
    _ = filelib:ensure_dir(Directory),
    Directory.

mbox_base_directory() ->
    {ok, Home} = init:get_argument(home),
    filename:join([Home, "mbox/"]).
