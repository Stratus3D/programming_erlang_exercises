%%%-------------------------------------------------------------------
%%% Heavily inspired by
%%% https://github.com/esumbar/programming-erlang-chapter-23/blob/7434c2eedce8b93b41ef4c68dd1690cbca5c8374/lib_tester_db.erl
%%%-------------------------------------------------------------------
-module(lib_tester_db).

%% API
-export([init/2, get_state/0, update_state/1]).

-define(TABLE, state).

%%%===================================================================
%%% API
%%%===================================================================

init(Nodes, Fields) ->
    ok = mnesia:start(),
    case mnesia:wait_for_tables([?TABLE], 1000) of
        ok ->
            io:format("Loaded tables~n"),
            ok;
        {timeout, _Time} ->
            io:format("Creating tables...~n"),
            mnesia:stop(),
            ok = mnesia:delete_schema(Nodes),
            ok = mnesia:create_schema(Nodes),
            {_, _} = rpc:multicall(Nodes, application,start,[mnesia]),
            {atomic, ok} = mnesia:create_table(?TABLE, [{disc_copies, Nodes}, {record_name, state}, {attributes, Fields}, {type, ordered_set}]),
            io:format("Created mnesia table on ~p nodes~n", [Nodes]),
            ok
    end.

get_state() ->
    {atomic, Result} = mnesia:transaction(fun() ->
                               [State|_] = mnesia:read(?TABLE, mnesia:first(?TABLE)),
                               State
                       end),
    Result.

update_state(NewState) ->
    ID = element(2, NewState),
    {atomic, Result} = mnesia:transaction(fun() ->
                                                  ok = mnesia:delete(?TABLE, ID, sticky_write),
                                                  mnesia:write(?TABLE, NewState, sticky_write)
                       end),
    Result.
