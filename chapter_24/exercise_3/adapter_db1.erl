%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(adapter_db1).

-behaviour(gen_server).

%% API
-export([start_link/0, put/3, lookup/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {table_id, type}).

-define(SERVER, ?MODULE).
-define(DETS_FILE, ?MODULE).
-define(IN_MEMORY_CUTOFF, 128).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: any()}.

start_link() ->
    % Start two variations of this gen server
    % One for disk
    {ok, Pid1} = gen_server:start_link({local, disk_adapter_db1}, ?MODULE, [disk], []),
    % One for in memory
    {ok, Pid2} = gen_server:start_link({local, memory_adapter_db1}, ?MODULE, [memory], []),
    {ok, [Pid1, Pid2]}.

%%--------------------------------------------------------------------
%% @doc
%% Store a key in memory or on disk
%%
%% Call the appropriate gen server based on the second argumen
%% @end
%%--------------------------------------------------------------------
put(Key, memory, Val) ->
    gen_server:call(memory_adapter_db1, {put, Key, Val});
put(Key, disk, Val) ->
    gen_server:call(disk_adapter_db1, {put, Key, Val}).


%%--------------------------------------------------------------------
%% @doc
%% Lookup a key
%%
%% @end
%%--------------------------------------------------------------------
lookup(Key) ->
    % Lookup key from both servers
    % First from memory gen_server
    case gen_server:call(memory_adapter_db1, {lookup, Key}) of
        [] ->
            % If not found, try disk gen_server
            gen_server:call(disk_adapter_db1, {lookup, Key});
        Result ->
            Result
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([disk]) ->
    {ok, ?MODULE} = dets:open_file(?MODULE, [{file, ?DETS_FILE}, {type, set}]),
    {ok, #state{table_id = ?MODULE, type = disk}};

init([memory]) ->
    TID = ets:new(?MODULE, [set]),
    {ok, #state{table_id = TID, type = memory}}.

handle_call({lookup, Key}, _From, #state{table_id = TableId, type = memory} = State) ->
    % Memory gen_server looks up from ETS table
    ReturnValue = case ets:lookup(TableId, Key) of
                      [{_Key, Val} | []] ->
                          {ok, Val};
                      [] -> error;
                      {error, _} -> error
                  end,
  {reply, ReturnValue, State};

handle_call({lookup, Key}, _From, #state{table_id = TableId, type = disk} = State) ->
    % Disk gen_server looks up from DETS table
    ReturnValue = case dets:lookup(TableId, Key) of
                      [{_Key, Val} | []] -> {ok, Val};
                      [] -> error;
                      {error, _} -> error
                  end,
  {reply, ReturnValue, State};

handle_call({put, Key, Val}, _From, #state{table_id = TableId, type = disk} = State) ->
    % Disk gen_server inserts into DETS table
    ok = dets:insert(TableId, {Key,Val}),
    {reply, ok, State};

handle_call({put, Key, Val}, _From, #state{table_id = TableId, type = memory} = State) ->
    % Memory gen_server inserts into ETS table
    true = ets:insert(TableId, {Key,Val}),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
