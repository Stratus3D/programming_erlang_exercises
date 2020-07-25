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
-export([start_link/2, is_prime/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

% id field is needed as an Mnesia ID
-record(state, {id=1,
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
-spec start_link(Limit :: integer(), Nodes :: list()) -> {ok, Pid :: pid()} | ignore | {error, Error :: any()}.

start_link(Limit, Nodes) when is_integer(Limit) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Limit, Nodes}, []).

-spec is_prime(Number :: integer()) -> any().

is_prime(Number) ->
    gen_server:call(?SERVER, {is_prime, Number}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Limit, Nodes}) ->
    % Start up all workers
    Pids = lists:map(fun(_Num) ->
                             % Start the worker
                             {ok, Pid} = supervisor:start_child(prime_tester_worker_sup, []),
                             _Ref = erlang:monitor(process, Pid),
                             Pid
                     end, lists:seq(1, Limit)),

    % Initialize Mnesia
    ok = lib_tester_db:init(Nodes, record_info(fields, state)),

    % Read Mnesia state
    State = case lib_tester_db:get_state() of
                '$end_of_table' ->
                    #state{};
                LoadedState ->
                    io:format("LoadedState: ~p", [LoadedState]),
                    LoadedState
            end,

    % Store pids in state
    {ok, State#state{free_pids=Pids}}.

handle_call({is_prime, Number}, From, State = #state{free_pids=[], queue=Q}) ->
    % If we've already hit our limit for workers queue the request
    NewState = State#state{queue=queue:in({From, Number}, Q)},
    ok = lib_tester_db:update_state(NewState),
    {noreply, NewState};
handle_call({is_prime, Number}, From, State = #state{free_pids=[Pid|Pids], jobs=R}) ->
    % If we still haven't hit our limit for workers, give the worker the new job
    ok = prime_tester_worker:is_prime(Pid, Number),

    % We will reply when the worker replies to us
    NewState = State#state{free_pids=Pids, jobs=maps:put(Pid, {From, Number}, R)},
    ok = lib_tester_db:update_state(NewState),
    {noreply, NewState};
handle_call(_Request, _From, State) ->
    io:format("Unrecognized message~n"),
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

                    NewState = State#state{free_pids=NewFreePids, jobs=NewJobs, queue=NewQueue},
                    ok = lib_tester_db:update_state(NewState),
                    {noreply, NewState};
                {empty, _} ->
                    % If empty no change
                    NewState = State#state{free_pids=FreePids ++ [Pid], jobs=maps:remove(Pid, Jobs)},
                    ok = lib_tester_db:update_state(NewState),
                    {noreply, NewState}
            end;
        false ->
            % If we don't find a pid in a map ignore this message
            NewState = State#state{jobs=maps:remove(Pid, Jobs)},
            ok = lib_tester_db:update_state(NewState),
            {noreply, NewState}
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

handle_down_worker(Pid, State = #state{free_pids=FreePids, jobs=Jobs, queue=Queue}) ->
    io:format("Got worker down. Sending error back to caller~n"),
    % Send error back to the caller
    {From, Number} = maps:get(Pid, Jobs),
    gen_server:reply(From, {error, {failed_to_check, Number}}),

    % Start a new worker in its place
    {ok, NewPid} = supervisor:start_child(prime_tester_worker_sup, []),
    _Ref = erlang:monitor(process, NewPid),

    case queue:out(Queue) of
        {{value, {From, Number}}, NewQueue} ->
            % Start the new job
            ok = prime_tester_worker:is_prime(NewPid, Number),
            NewJobs = maps:put(NewPid, {From, Number}, maps:remove(Pid, Jobs)),

            NewState = State#state{jobs=NewJobs, queue=NewQueue},
            ok = lib_tester_db:update_state(NewState),
            {noreply, NewState};
        {empty, _} ->
            % If nothing in the queue continue as before
            NewState = State#state{free_pids=[NewPid|FreePids], jobs=maps:remove(Pid, Jobs)},
            ok = lib_tester_db:update_state(NewState),
            {noreply, NewState}
    end.
