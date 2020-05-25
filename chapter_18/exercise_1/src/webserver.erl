-module(webserver).

-export([start/1, init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

start(Port) ->
    {ok, _} = application:ensure_all_started(cowboy),

    Dispatch = cowboy_router:compile(
                 [
                  {'_', [{'_', webserver, []}]}
                 ]),

    {ok, _} = cowboy:start_clear(webserver, [{port, Port}], #{env => #{dispatch => Dispatch}}).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    Pid = spawn_link(shell1, start, [self()]),
    _ = register(shell1, Pid),
    {ok, State}.

websocket_handle(_Frame = {text, Msg}, State) ->
    % Using {labels, atom} is generally unsafe as malicious clients can send
    % random keys and use up all the space in the atom table. In this case I
    % chose to use this so I could reuse the shell.erl code from the book
    % without having to modify it to handle binary keys.
    Data = jsx:decode(Msg, [{labels, atom}, return_maps]),
    shell1 ! {self(), Data},
    {ok, State};
websocket_handle(Frame, State) ->
    {ok, State}.

websocket_info({log, Text}, State) ->
    {reply, {text, Text}, State};
websocket_info(Info, State) when is_map(Info) ->
    % If Info is a map we assume it is a message from shell1
    Bin = jsx:encode([Info]),
    {reply, {text, Bin}, State};
websocket_info({'EXIT', _Pid, Failure}, State) ->
    % Show the user the crash
    BV = shell1:bf("#> <font color='red'>~p</font><br>", [Failure]),
    Message = #{cmd => append_div, id => scroll, txt => BV},
    Bin = jsx:encode([Message]),

    % Restart
    Pid = spawn_link(shell1, start, [self()]),
    _ = register(shell1, Pid),
    {reply, {text, Bin}, State};
websocket_info(_Info, State) ->
    {ok, State}.
