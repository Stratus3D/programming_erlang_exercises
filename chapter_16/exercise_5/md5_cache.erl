-module(md5_cache).

-behaviour(gen_server).

-export([start_link/0, get_md5/1]).

-include_lib("kernel/include/file.hrl").

-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_md5(Filename) ->
    gen_server:call(?SERVER, {get_md5, Filename}).

% Callbacks
init([]) -> {ok, []}.

handle_call({get_md5, Filename}, _From, State) ->
    case proplists:lookup(Filename, State) of
        none ->
            % Compute MD5 hash
            {ok, Data} = file:read_file(Filename),
            Sum = erlang:md5(Data),
            % Get the last modified date of the file
            {ok, #file_info{mtime=ModifiedTime}} = file:read_file_info(Filename),
            io:format("MD5 hash is not up to date~n", []),

            % Update the server state with the cached MD5 sum and return the sum
            % to the caller
            {reply, {ok, Sum}, [{Filename, ModifiedTime, Sum}|State]};
        {Filename, Time, Md5} ->
            % Check if the last modified date has changed
            {ok, #file_info{mtime=ModifiedTime}} = file:read_file_info(Filename),
            {Days, _Time} = calendar:time_difference(ModifiedTime, Time),

            Sum = case Days of
                      Days when Days =/= 0 ->
                          % If it has changed update the hash
                          io:format("MD5 hash is not up to date~n", []),
                          {ok, Data} = file:read_file(Filename),
                          erlang:md5(Data);
                      _ ->
                          % Otherwise return the cached MD5 hash
                          io:format("MD5 hash is up to date~n", []),
                          Md5
                  end,

            NewState = lists:keyreplace(Filename, 1, State, {Filename, ModifiedTime, Sum}),
            {reply, {ok, Md5}, NewState}
    end.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
