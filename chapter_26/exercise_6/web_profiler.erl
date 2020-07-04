-module(web_profiler).

-export([run/2, ping/2, receive_response/2, do_f/4]).

-define(LIST_OF_WEBSITES, [
    "http://stratus3d.com/",
    "http://lobste.rs/",
    "http://news.ycombinator.com/",
    "http://stackoverflow.com/"
                          ]).

% Since there is so much state that must be passed around for the lists:foldl/3
% state I defined a record to make things easier.
-record(pmap_state, {
          function,
          ref,
          max,
          nodes,
          results = [],
          pids = [],
          running_processes = 0
         }).


run(Nodes, Timeout) ->
    % Map over the list
    {Time, Result} = timer:tc(fun() ->
                     pmap(fun(Url) -> ping(Url, Timeout) end, ?LIST_OF_WEBSITES, 2, Nodes)
             end),

    io:format("Result: ~p~n", [Result]),

    % Return the time it took to execute all of the requests
    Time.

do_f(Parent, Ref, F, I) ->
    Parent ! {self(), Ref, (catch F(I))}.

gather(Pid, Ref) ->
    receive
        {Pid, Ref, Ret} -> Ret
    end.

pmap(F, L, Max, Nodes) ->
    Ref = make_ref(),
    InitialState = #pmap_state{max = Max, ref = Ref, function = F, nodes = Nodes},
    #pmap_state{pids = Pids, results = Result} = lists:foldl(fun map_element/2, InitialState, L),

    % Receive the remaining `Max` number results after the foldl
    RemainingResults = lists:map(fun(Pid) ->
                      gather(Pid, Ref)
              end, Pids),

    % Return the final results
    lists:reverse(RemainingResults ++ Result).

map_element(Element, #pmap_state{
                        running_processes = RunningProcesses,
                        max = Max,
                        pids = Pids,
                        ref = Ref,
                        function = F,
                        nodes = Nodes
                       } = State) when RunningProcesses < Max ->
    % Spawn process per item until limit is reached
    {Node, NewNodes} = next_node(Nodes),
    io:format("spawning~n"),
    Pid = spawn_on_node(Node, Ref, F, Element),
    State#pmap_state{running_processes = RunningProcesses + 1, pids = [Pid|Pids], nodes = NewNodes};

map_element(Element, #pmap_state{pids = Pids, results = Results, ref = Ref, function = F, nodes = Nodes} = State) ->
    % When limit is reached wait until we receive a message from one of the workers
    [Pid|RestPids] = lists:reverse(Pids),
    io:format("waiting for message back~n"),
    Result = gather(Pid, Ref),

    % When message is received spawn process for next item in list if one remains
    {Node, NewNodes} = next_node(Nodes),
    NewPid = spawn_on_node(Node, Ref, F, Element),
    State#pmap_state{results = [Result|Results], pids = [NewPid|lists:reverse(RestPids)], nodes = NewNodes}.

spawn_on_node(Node, Ref, F, Element) ->
    Self = self(),
    spawn(Node, ?MODULE, do_f, [Self, Ref, F, Element]).

next_node([Node|Nodes]) ->
    {Node, Nodes ++ [Node]}.

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
