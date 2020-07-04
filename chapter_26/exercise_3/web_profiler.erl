-module(web_profiler).

-export([run/1, ping/2, receive_response/2]).

-define(LIST_OF_WEBSITES, [
    "http://stratus3d.com/",
    "http://lobste.rs/",
    "https://news.ycombinator.com/"
                          ]).

run(Timeout) ->
    % Map over the list
    {Time, _Result} = timer:tc(fun() ->
                     pmap(fun(Url) -> ping(Url, Timeout) end, ?LIST_OF_WEBSITES)
             end),

    % Return the time it took to execute all of the requests
    Time.

pmap(F, L) ->
    S = self(),
    Ref = make_ref(),
    Pids = lists:map(fun(I) ->
                       spawn(fun() -> do_f(S, Ref, F, I) end)
               end, L),

    % Gather the results
    gather(Pids, Ref).

do_f(Parent, Ref, F, I) ->
    Parent ! {self(), Ref, (catch F(I))}.

gather([Pid|T], Ref) ->
    receive
        {Pid, Ref, Ret} -> [Ret|gather(T, Ref)]
    end;
gather([], _) ->
    [].

ping(URL, Timeout) ->
    {_Protocol, Host, Port, Path} = parse_url(URL),
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]),
    % Send the request
    ok = gen_tcp:send(Socket, io_lib:format("HEAD ~s HTTP/1.0\r\n\r\n", [Path])),
    % Time the response
    {Time, Result} = timer:tc(fun receive_response/2, [Socket, Timeout]),

    % Format the return value of the function
    case Result of
        timeout ->
            timeout;
        _ ->
            {time, Time}
    end.

receive_response(Socket, Timeout) ->
    % And receive the response
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, Packet} -> Packet;
        {error, timeout} -> timeout
    end.

parse_url(Url) ->
    {ok, Parsed} = http_uri:parse(Url),
    % We ignore the query string for simplicity here
    {Protocol, _, Host, Port, Path, _Query} = Parsed,
    {Protocol, Host, Port, Path}.
