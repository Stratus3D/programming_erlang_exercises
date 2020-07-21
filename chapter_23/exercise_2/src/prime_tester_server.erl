%%%-------------------------------------------------------------------
%%% @author trevor
%%% @copyright (C) 2020, trevor
%%% @doc
%%% The queue server to handle queuing sending individual
%%% @end
%%%-------------------------------------------------------------------
-module(prime_tester_server).

-behaviour(gen_server).

%% API
-export([start_link/1, is_prime/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {limit=0,
                pids=#{},
                queue=queue:new()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Limit :: integer()) -> {ok, Pid :: pid()} | ignore | {error, Error :: any()}.

start_link(Limit) when is_integer(Limit) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Limit}, []).

-spec is_prime(Number :: integer()) -> any().

is_prime(Number) ->
    gen_server:call(?SERVER, {is_prime, Number}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Limit}) ->
    {ok, #state{limit=Limit}}.

% If we still haven't hit our limit for workers, spawn one
handle_call({is_prime, Number}, From, State = #state{limit=Limit, pids=R}) when Limit > 0 ->
    % Start the worker
    {ok, Pid} = supervisor:start_child(prime_tester_worker_sup, [Number]),
    _Ref = erlang:monitor(process, Pid),

    % We will reply when the worker replies to us
    {noreply, State#state{limit=Limit-1, pids=maps:put(Pid, From, R)}};
handle_call({is_prime, Number}, From, State = #state{limit=Limit, queue=Q}) when Limit =< 0 ->
    % If we've already hit our limit for workers queue the request
    {noreply, State#state{queue=queue:in({From, Number}, Q)}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({worker_result, Pid, Result}, State = #state{pids=Pids}) ->
    % Lookup client pid in map
    case maps:is_key(Pid, Pids) of
        true ->
            % Send the reply back to the original `From` pid
            gen_server:reply(maps:get(Pid, Pids), {ok, Result});
        false ->
            % If we don't find a pid in a map ignore this message
            ok
    end,
    {noreply, State#state{pids=maps:remove(Pid, Pids)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

% Handle getting a down message from a finished worker
handle_info({'DOWN', _Ref, process, Pid, _}, State = #state{pids=Pids}) ->
    case maps:is_key(Pid, Pids) of
        true ->
            handle_down_worker(Pid, State);
        false -> %% Not our responsibility
            {noreply, State}
    end;
handle_info({worker_result, Pid, Result}, State = #state{pids=Pids}) ->
    % Lookup client pid in map
    case maps:is_key(Pid, Pids) of
        true ->
            % Send the reply back to the original `From` pid
            gen_server:reply(maps:get(Pid, Pids), {ok, Result});
        false ->
            % If we don't find a pid in a map ignore this message
            ok
    end,
    {noreply, State#state{pids=maps:remove(Pid, Pids)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% Handle getting return values back from the workers
handle_down_worker(Pid, State = #state{limit=Limit, pids=Pids, queue=Queue}) ->
    case queue:out(Queue) of
        {{value, {From, Number}}, Queue} ->
            {ok, Pid} = supervisor:start_child(prime_tester_worker_sup, [Number]),
            NewPid = erlang:monitor(process, Pid),
            NewPids = gb_sets:insert(NewPid, maps:remove(Pid,Pids)),
            gen_server:reply(From, {ok, Pid}),
            {noreply, State#state{pids=NewPids, queue=Queue}};
        {empty, _} ->
            % If nothing in the queue continue as before
            {noreply, State#state{limit=Limit+1, pids=gb_sets:delete(Pid, Pids)}}
    end.
