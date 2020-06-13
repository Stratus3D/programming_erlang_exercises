%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(cgi_web_server).

-export([start/0, start_from_shell/1, init/2, handle1/3, terminate/3]).

-define(MODULE_WHITELIST, [ping_pong]).

start() ->
    start(12345).

start_from_shell([PortAsAtom]) ->
    PortAsInt = list_to_integer(atom_to_list(PortAsAtom)),
    start(PortAsInt).
%%

start(Port) ->
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile(
		 [
		  %% {URIHost, list({URIPath, Handler, Opts})}
		  {'_', [{'_', cgi_web_server, #{}}]}
		 ]),
    cowboy:start_clear(cgi_web_server,
		      [{port, Port}],
              #{env => #{dispatch => Dispatch}}
		     ).

init(Req, State) ->
    Path = cowboy_req:path(Req),
    handle1(Path, Req, State).

handle1(<<"/cgi">>, Req, State) ->
    Args = cowboy_req:parse_qs(Req),
    {ok, Bin, Req2} = cowboy_req:read_body(Req),
    Val = case Bin of
              <<>> ->
                  <<>>;
              _ ->
                  io:format("~p", [Bin]),
                  jsx:decode(Bin)
          end,
    Response = call(Args, Val),
    Json = jsx:encode(Response),
    {ok, Req3} = cowboy_req:reply(200, #{}, Json, Req2),
    {ok, Req3, State};
handle1(Path, Req, State) ->
    Response = read_file(Path),
    {ok, Req1} = cowboy_req:reply(200, #{}, Response, Req),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.

% Private functions
call([{<<"mod">>,MB},{<<"func">>,FB}], X) ->
    Mod = list_to_atom(binary_to_list(MB)),
    % Check if module is in whitelist
    case lists:member(Mod, ?MODULE_WHITELIST) of
        true ->
            Func = list_to_atom(binary_to_list(FB)),
            apply(Mod, Func, [X]);
        _ ->
            % Return an error if module not in whitelist
            [{error, invalid_module}]
    end.

read_file(Path) ->
    File = [$.|binary_to_list(Path)],
    case file:read_file(File) of
	{ok, Bin} -> Bin;
	_ -> ["<pre>cannot read:", File, "</pre>"]
    end.
