-module(dict_test_debug).

-export([trace/0]).

trace() ->
    {ok, _Tracer} = dbg:tracer(),

    {ok, _Desc} = dbg:p(all, c),

    {ok, _Tpl} = dbg:tpl(dict, []),

    dict_test:test(),

    ok = dbg:stop(),

    ok.


% -module(math).
% -export([sum/2, diff/2]).
%
% sum(A, B) -> A + B.
% diff(A, B) -> A - B.
%
% Just start the tracer:
% > dbg:tracer()
%
% Say the tracer that you are interested in all calls for all processes:
% > dbg:p(all, c)
%
% Finally, say it that you want to trace the function sum from the module math:
% > dbg:tpl(math, sum, [])
%
% Now, try to call the function, as usual. The tracer is active!
% > math:sum(2, 3).
%
% To stop the trace:
% > dbg:stop().
%
% To trace all the functions within the module math:
% > dbg:tpl(math, [])
%
% To trace the return value for a given function:
% > dbg:tpl(math, sum, dbg:fun2ms(fun(_) -> return_trace() end)).
