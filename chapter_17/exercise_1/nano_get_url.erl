-module(nano_get_url).

-export([get_url/0, get_url/1, get_domain/0, get_domain/1]).

get_url() ->
    get_url("http://www.google.com/search?q=erlang").

get_url(Url) ->
    {_Protocol, Host, Port, Path} = parse_url(Url),
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, io_lib:format("GET ~s HTTP/1.0\r\n\r\n", [Path])),
    receive_data(Socket, []).

get_domain() ->
    get_url("www.google.com").

get_domain(Host) ->
    {ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
    {Headers, Body} = parse_response(receive_data(Socket, [])),
    io:format("Headers: ~s, Body: ~s", [Headers, Body]),
    % Check if redirect header is present
    ok.

receive_data(Socket, SoFar) ->
    receive
        {tcp, Socket, Bin} ->
            receive_data(Socket, [Bin|SoFar]);
        {tcp_closed, Socket} ->
            list_to_binary(lists:reverse(SoFar))
    end.

parse_url(Url) ->
    {ok, Parsed} = http_uri:parse(Url),
    % We ignore the query string for simplicity here
    {Protocol, _, Host, Port, Path, _Query} = Parsed,
    {Protocol, Host, Port, Path}.

parse_response(ResponseBin) ->
    % Find end of headers
    [Headers, Body] = binary:split(ResponseBin, <<"\r\n\r\n">>),

    % Parse headers
    HeaderList = binary:split(Headers, <<"\r\n">>),
    ParsedHeaders = lists:map(fun(Header) ->
                      [Header, Value] = binary:split(Header, <<":">>),
                      [trim(Header), trim(Value)]
              end, HeaderList),

    % Return body and parsed headers
    {ParsedHeaders, Body}.

trim(String) ->
    re:replace(String, "(^\\s+)|(\\s+$)", "", [global,{return,list}]).
