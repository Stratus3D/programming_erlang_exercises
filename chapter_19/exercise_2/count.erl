-module(count).

% API
-export([initialize/0, me/2, all_counters/0]).

-define(TABLE_NAME, counter).
-define(PROCESS_NAME, counter).

% It probably would have been better to just use a gen_server here. Since
% this exercise is before the OTP section in the book I chose to use a simple
% RPC function.

% API
initialize() ->
    register(?PROCESS_NAME, spawn(fun() -> start_loop() end)).

me(Module, Line) ->
    rpc({increment, Module, Line}).

all_counters() ->
    rpc(all).

% Private Functions
rpc(Call) ->
    ?PROCESS_NAME ! {self(), Call},
    receive
        {?PROCESS_NAME, Reply} ->
            Reply
    end.

start_loop() ->
    % Create a new ets table before the loop function beings handling requests
    Tab = ets:new(?TABLE_NAME, [set, public]),

    % Start handling requests
    loop(Tab).

loop(Tab) ->
    receive
        {From, all} ->
            % We iterate over all the items in the table, adding them to a list.
            Result = ets:foldl(fun(Item, Acc) ->
                              [Item|Acc]
                      end, [], Tab),
            From ! {?PROCESS_NAME, Result};
        {From, {increment, Module, Line}} ->
            % Keys are composed of modules and line numbers
            Key = {Module, Line},

            Result = case ets:lookup(Tab, Key) of
                [] ->
                    % Insert new element with a count of one if no elements match
                    ets:insert(Tab, {Key, 1});
                _ ->
                    % We can use `update_counter` to update the counter if it exists
                    ets:update_counter(Tab, Key, {2, 1})
            end,
            From ! {?PROCESS_NAME, Result}
    end,
    loop(Tab).
