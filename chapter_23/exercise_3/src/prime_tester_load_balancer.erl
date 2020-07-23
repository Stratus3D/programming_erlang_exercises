%%%-------------------------------------------------------------------
%%% @author trevor
%%% @copyright (C) 2020, trevor
%%% @doc
%%% The queue server to handle queuing sending individual
%%% @end
%%%-------------------------------------------------------------------
-module(prime_tester_load_balancer).

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
                free_pids=[],
                jobs=#{},
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
    % Start up all workers
    Pids = lists:map(fun(_Num) ->
                             % Start the worker
                             {ok, Pid} = supervisor:start_child(prime_tester_worker_sup, []),
                             _Ref = erlang:monitor(process, Pid),
                             Pid
                     end, lists:seq(1, Limit)),

    % Store pids in state
    {ok, #state{free_pids=Pids}}.

handle_call({is_prime, Number}, From, State = #state{free_pids=[], queue=Q}) ->
    % If we've already hit our limit for workers queue the request
    {noreply, State#state{queue=queue:in({From, Number}, Q)}};
handle_call({is_prime, Number}, From, State = #state{free_pids=[Pid|Pids], jobs=R}) ->
    % If we still haven't hit our limit for workers, give the worker the new job
    ok = prime_tester_worker:is_prime(Pid, Number),

    % We will reply when the worker replies to us
    {noreply, State#state{free_pids=Pids, jobs=maps:put(Pid, {From, Number}, R)}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({worker_result, Pid, Result}, State = #state{free_pids=FreePids, jobs=Jobs, queue=Queue}) ->
    % Lookup client pid in map
    case maps:is_key(Pid, Jobs) of
        true ->
            % Send the reply back to the original `From` pid
            {From, _Number} = maps:get(Pid, Jobs),
            gen_server:reply(From, {ok, Result}),

            % Start the next job
            case queue:out(Queue) of
                {{value, {From, Number}}, NewQueue} ->
                    % Put the old pid back in the list of pids and get the new one
                    [NewPid|RestPids] = FreePids,
                    NewFreePids = RestPids ++ [Pid],

                    % Start the new job
                    ok = prime_tester_worker:is_prime(NewPid, Number),
                    NewJobs = maps:put(NewPid, {From, Number}, maps:remove(Pid, Jobs)),

                    {noreply, State#state{free_pids=NewFreePids, jobs=NewJobs, queue=NewQueue}};
                {empty, _} ->
                    % If empty no change
                    {noreply, State#state{free_pids=FreePids ++ [Pid], jobs=maps:remove(Pid, Jobs)}}
            end;
        false ->
            % If we don't find a pid in a map ignore this message
            {noreply, State#state{jobs=maps:remove(Pid, Jobs)}}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

% Handle getting a down message from a finished worker
handle_info({'DOWN', _Ref, process, Pid, _}, State = #state{jobs=Pids}) ->
    case maps:is_key(Pid, Pids) of
        true ->
            handle_down_worker(Pid, State);
        false -> %% Not our responsibility
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_down_worker(Pid, State = #state{limit=Limit, jobs=Pids, queue=Queue}) ->
    io:format("got worker down"),
    case queue:out(Queue) of
        {{value, {From, Number}}, NewQueue} ->
            {ok, Pid} = supervisor:start_child(prime_tester_worker_sup, [Number]),
            NewPid = erlang:monitor(process, Pid),
            NewPids = gb_sets:insert(NewPid, maps:remove(Pid,Pids)),
            gen_server:reply(From, {ok, Pid}),
            {noreply, State#state{jobs=NewPids, queue=NewQueue}};
        {empty, _} ->
            % If nothing in the queue continue as before
            {noreply, State#state{limit=Limit+1, jobs=gb_sets:delete(Pid, Pids)}}
    end.
