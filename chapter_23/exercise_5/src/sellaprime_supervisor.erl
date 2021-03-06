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
-export([start/1, start_in_shell_for_testing/1, start_link/1, init/1]).

start(Nodes) ->
    spawn(fun() ->
		  supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [Nodes])
	  end).
start_in_shell_for_testing(Nodes) ->
    {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [Nodes]),
    unlink(Pid).
start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).
init([Nodes]) ->
    {ok, {{one_for_all, 3, 10},
	  [
	   {prime_tester_worker_sup,
	    {prime_tester_worker_sup, start_link, []},
	    permanent,
	    10000,
	    supervisor,
	    [prime_tester_worker_sup]},
	   {prime_tester_load_balancer,
	    {prime_tester_load_balancer, start_link, [10, Nodes]},
	    permanent,
	    10000,
	    worker,
	    [prime_load_balancer]}
	  ]}}.
