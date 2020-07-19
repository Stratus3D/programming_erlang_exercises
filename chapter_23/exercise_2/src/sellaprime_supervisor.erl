%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(sellaprime_supervisor).
-behaviour(supervisor).		% see erl -man supervisor
-export([start/0, start_in_shell_for_testing/0, start_link/1, init/1]).

start() ->
    spawn(fun() ->
		  supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
	  end).
start_in_shell_for_testing() ->
    {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).
start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).
init([]) ->
    {ok, {{one_for_one, 3, 10},
	  [
	   {prime_tester_work_sup,
	    {prime_tester_worker_sup, start_link, []},
	    permanent,
	    10000,
	    supervisor,
	    [prime_tester_server]},
	   {prime_tester_server,
	    {prime_tester_server, start_link, [10]},
	    permanent,
	    10000,
	    worker,
	    [prime_server]}
	  ]}}.
