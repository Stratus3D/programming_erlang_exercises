%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(cgi_web_server).

-export([start/0, get_ip_info/0, start_from_shell/1, init/2, handle1/3, terminate/3]).

-include_lib("kernel/include/file.hrl").

-define(MODULE_WHITELIST, [ping_pong]).
-define(IP_WHITELIST, [{127,0,0,1}]).
-define(NODE_NAMES, [node()]).

-record(allowed_ip, {ip_address, created}).
-record(ip_connection, {ip_address, number_connections}).

start() ->
    % Initialize Mnesia database if it doesn't already exist
    initialize_mnesia(),
    start(12345).

get_ip_info() ->
    % Print out IP information from Mnesia
    mnesia:transaction(fun() ->
                               % Print whitelisted IPs
                               Keys = mnesia:foldl(fun(X, Acc) -> [X|Acc] end, [], allowed_ip),
                               io:format("Allowed IPs: ~p~n", [Keys]),

                               % Print IP access records
                               Keys2 = mnesia:foldl(fun(X, Acc) -> [X|Acc] end, [], ip_connection),
                               io:format("IPs Connections: ~p~n", [Keys2])
                       end).

initialize_mnesia() ->
    case mnesia:create_schema(?NODE_NAMES) of
        {error,{_Host,{already_exists,_host}}} ->
            ok;
        ok ->
            ok
    end,

    ok = mnesia:start(),

    DiskCopies = {disc_copies, ?NODE_NAMES},
    ensure_exists(mnesia:create_table(allowed_ip, [{attributes, record_info(fields, allowed_ip)}, DiskCopies])),
    ensure_exists(mnesia:create_table(ip_connection, [{attributes, record_info(fields, ip_connection)}, DiskCopies])),
    case mnesia:wait_for_tables([allowed_ip, ip_connection], 20000) of
        {timeout, _} ->
            io:format("Error! Timeout waiting for tables");
        ok ->
            seed_tables()
    end.

seed_tables() ->
    lists:foreach(fun(IP) ->
                          Result = mnesia:transaction(fun() ->
                                                              mnesia:write(#allowed_ip{
                                                                              ip_address=IP,
                                                                              created=erlang:timestamp()
                                                                             })
                                                      end),
                          io:format("Result ~p~n", [Result])
                  end, ?IP_WHITELIST).

ensure_exists({aborted, {already_exists, _Table}}) -> ok;
ensure_exists({atomic, ok}) -> ok;
ensure_exists(ok) -> ok.

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
    % Check if request is from whitelisted IP
    {IPAddress, _Port} = cowboy_req:peer(Req),
    io:format("IP Address: ~p~n", [IPAddress]),
    F = fun() ->
                case mnesia:read(allowed_ip, IPAddress, read) of
                    [] -> false;
                    [_] ->
                        % Record request if permitted
                        case mnesia:read(ip_connection, IPAddress, read) of
                            [] ->
                                % Insert a new record
                                IPConn = #ip_connection{ip_address=IPAddress, number_connections=1},
                                mnesia:write(IPConn);
                            [IPConn] ->
                                % Update record
                                NewNumberOfConns = IPConn#ip_connection.number_connections + 1,
                                mnesia:write(IPConn#ip_connection{number_connections=NewNumberOfConns})
                        end,
                        true
                end
        end,

    case mnesia:transaction(F) of
        {_, false} ->
            % Block request if not
            Req2 = cowboy_req:reply(403, #{}, <<"Sorry, your IP is blocked">>, Req),
            {ok, Req2, State};
        {_, true} ->
            % Handle request
            Path = cowboy_req:path(Req),
            handle1(Path, Req, State)
    end.

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
    Req3 = cowboy_req:reply(200, #{}, Json, Req2),
    {ok, Req3, State};
handle1(Path, Req, State) ->
    Response = read_file(Path),
    Req1 = cowboy_req:reply(200, #{}, Response, Req),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.

% Private functions
call([{<<"mod">>,MB},{<<"func">>,FB}], X) ->
    % Use list_to_existing_atom to prevent invalid parameters from flooding the
    % atom table
    Mod = list_to_existing_atom(binary_to_list(MB)),
    % Check if module is in whitelist
    case lists:member(Mod, ?MODULE_WHITELIST) of
        true ->
            % Use list_to_existing_atom to prevent invalid parameters from
            % flooding the atom table
            Func = list_to_existing_atom(binary_to_list(FB)),

            % Check if Mod needs to be recompiled
            % Is .erl file newer than the .beam file?
            case erl_file_modified(Mod) of
                true ->
                    % Reload module
                    reload_module(Mod);
                false -> ok
            end,
            apply(Mod, Func, [X]);
        _ ->
            % Return an error if module not in whitelist
            [{error, invalid_module}]
    end.

read_file(Path) ->
    % Invoke safe_relative_path on the path from the client to prevent directory
    % traversal attacks
    SafePath = filename:safe_relative_path(Path),
    case file:read_file(SafePath) of
        {ok, Bin} -> Bin;
        _ ->
            % HTML escape path to prevent injection of HTML into the error page
            % markup
	        ["<pre>cannot read:", xmerl_lib:export_text(Path), "</pre>"]
    end.

reload_module(Module) ->
    code:purge(Module),
    compile:file(erl_path(exercise_5, Module), [{outdir, beam_dir(Module)}]),
    code:load_file(Module).

erl_file_modified(Module) ->
    {ok, Application} = application:get_application(),
    Filename = erl_path(exercise_5, Module),
    {ok, #file_info{mtime=ModifiedTime}} = file:read_file_info(Filename),

    BeamFilename = beam_path(Module),
    {ok, #file_info{mtime=BeamModifiedTime}} = file:read_file_info(BeamFilename),
    %io:format("Modified Time: ~p~n", [ModifiedTime]),
    %io:format("Beam Modified Time: ~p~n", [BeamModifiedTime]),

    {Days, _Time} = calendar:time_difference(ModifiedTime, BeamModifiedTime),
    case Days of
        Days when Days >= 0 ->
            io:format("Beam file is up to date~n", []),
            false;
        _ ->
            io:format("Beam file is out of date. Recompile~n", []),
            true
    end.

% Get the path to ping_pong.beam
beam_path(ModuleName) ->
    code:which(ModuleName).

% Get the directory to write the recompiled ping_pong module to
beam_dir(ModuleName) ->
    filename:dirname(code:which(ModuleName)).

% Kind of hacky because we have to figure out where the .erl files live inside
% the rebar3 _build directory
erl_path(ApplicationName, ModuleName) ->
    % Get the path to the priv dir for this application.
    PrivDirPath = code:priv_dir(ApplicationName),
    ApplicationDir = filename:dirname(PrivDirPath),

    % Erlang files should be ../src/ relative to this directory
    filename:absname(["src/", ModuleName, ".erl"], ApplicationDir).
