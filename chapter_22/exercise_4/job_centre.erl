-module(job_centre).

-behaviour(gen_server).

-define(JOB_TIME, 2000).

% API
-export([start_link/0, add_job/1, work_wanted/0, job_done/1, statistics/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% API functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_job(Fun) ->
    gen_server:call(?MODULE, {add_job, Fun}).

work_wanted() ->
    gen_server:call(?MODULE, checkout_job).

job_done(JobId) ->
    gen_server:call(?MODULE, {remove_job, JobId}).

statistics() ->
    gen_server:call(?MODULE, get_statistics).

% gen_server callbacks
init([]) ->
    % Initial the gen_server state with an integer for future IDs and a ets
    % table
    {ok, {0, ets:new(?MODULE,[])}}.

handle_call({add_job, Fun}, _From, {LastId, Tab}) ->
    % Generate another job ID by incrementing the last one
    JobId = LastId + 1,

    % Insert the ID and work function into the ets table
    true = ets:insert(Tab, job(JobId, Fun)),
    {reply, JobId, {JobId, Tab}};
handle_call(checkout_job, {FromPid, _} = _From, {_, Tab} = State) ->
    % Find a job
    case ets:match(Tab, {'$1', '$2', '_', '_', '_'}) of
        [] ->
            % If none return `no`
            {reply, no, State};
        [[JobId, JobFun]|_] ->
            % If one is found mark it as in_progess and return it to the caller
            % Monitor `From` to ensure process doesn't crash while handling job
            Ref = monitor(process, FromPid),
            ets:insert(Tab, job(JobId, JobFun, in_progress, FromPid, Ref)),

            % Set timer
            _TimerRef = erlang:send_after(?JOB_TIME, self(), {almost_up, JobId}),

            {reply, {JobId, JobFun}, State}
    end;
handle_call({remove_job, JobId}, _From, {_, Tab} = State) ->
    % Mark the job as done in the ETS table
    case find_job(Tab, JobId) of
        false ->
            {reply, not_found, State};
        {JobId, JobFun, _State, _Pid, _Ref} ->
            ets:insert(Tab, job(JobId, JobFun, finished)),
            {reply, ok, State}
    end;
handle_call(get_statistics, _From, {_, Tab} = State) ->
    % Dump job data from ets table
    Jobs = ets:tab2list(Tab),

    % Format the data and return the state of each job
    Results = lists:map(fun({JobID, _JobFun, JobState, _Pid, _Ref}) ->
                            {JobID, JobState}
                    end, Jobs),
    {reply, Results, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({youre_fired, JobId}, {_, Tab} = State) ->
    case find_job(Tab, JobId) of
        false -> ok;
        {JobId, JobFun, _State, Pid, Ref} ->
            % Kill the job if it hasn't already finished
            exit(Pid, youre_fired),
            demonitor(Ref),
            ets:insert(Tab, job(JobId, JobFun))
    end,
    {noreply, State};
handle_info({almost_up, JobId}, {_, Tab} = State) ->
    _TimerRef = erlang:send_after(?JOB_TIME, self(), {almost_up, JobId}),
    case find_job(Tab, JobId) of
        false -> ok;
        {JobId, _JobFun, _State, Pid, _Ref} ->
            Pid ! hurry_up,
            _TimerRef2 = erlang:send_after(2000, self(), {youre_fired, JobId})
    end,
    {noreply, State};
handle_info({'DOWN', Ref, process, Pid, Reason}, {_, Tab} = State) ->
    LookupByRefAndPid = [{{'_', '_', '_','$1','$2'},
                          [{'=:=','$1',{const,Pid}},{'=:=','$2',{const,Ref}}],
                          ['$_']}],
    case ets:match(Tab, LookupByRefAndPid) of
        [] ->
            % Unknown down message
            io:format("DOWN message from unknown process");
        [{JobId, JobFun, _State, Pid, Ref}|_] ->
            io:format("Worker process ~p crashed with reason ~p", [Pid, Reason]),
            ets:insert(Tab, job(JobId, JobFun))
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Helper functions for manipulating job state
job(JobId, JobFun) ->
    job(JobId, JobFun, pending).

job(JobId, JobFun, State) ->
    job(JobId, JobFun, State, undefined, undefined).

job(JobId, JobFun, State, Pid, Ref) ->
    {JobId, JobFun, State, Pid, Ref}.

% Helper function for looking up jobs
find_job(Tab, JobId) ->
    case ets:lookup(Tab, JobId) of
        [] ->
            false;
        [{JobId, JobFun, State, Pid, Ref}|_] ->
            {JobId, JobFun, State, Pid, Ref}
    end.
