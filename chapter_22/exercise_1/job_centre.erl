-module(job_centre).

-behaviour(gen_server).

% API
-export([start_link/0, add_job/1, work_wanted/0, job_done/1]).

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

% gen_server callbacks
init([]) ->
    % Initial the gen_server state with an integer for future IDs and a ets
    % table
    {ok, {0, ets:new(?MODULE,[])}}.

handle_call({add_job, Fun}, _From, {LastId, Tab}) ->
    % Generate another job ID by incrementing the last one
    JobId = LastId + 1,

    % Insert the ID and work function into the ets table
    true = ets:insert(Tab, {JobId, Fun}),
    {reply, JobId, {JobId, Tab}};
handle_call(checkout_job, _From, {_, Tab} = State) ->
    % Find a job
    case ets:match(Tab, {'$1', '$2'}) of
        [] ->
            % If none return `no`
            {reply, no, State};
        [[JobId, JobFun]|_] ->
            % If one is found mark it as in_progess and return it to the caller
            ets:insert(Tab, {JobId, JobFun, in_progres}),
            {reply, {JobId, JobFun}, State}
    end;
handle_call({remove_job, JobId}, _From, {_, Tab} = State) ->
    % Delete the job tuple from the ets table
    ets:delete(Tab, JobId),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
