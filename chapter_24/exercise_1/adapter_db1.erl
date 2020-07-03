%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(adapter_db1).
-export([new/1, store/3, lookup/2]).

-define(DETS_FILE, ?MODULE).

new(persistent) ->
    {ok, ?MODULE} = dets:open_file(?MODULE, [{file, ?DETS_FILE}, {type, set}]),
    {?MODULE, persistent, ?MODULE};
new(dict) ->
    {?MODULE, dict, dict:new()};
new(lists) ->
    {?MODULE, list, []}.

store(Key, Val, {_, persistent, DetsName}) ->
  ok = dets:insert(DetsName, {Key,Val}),
  {?MODULE, persistent, DetsName};
store(Key, Val, {_, dict, D}) ->
    D1 = dict:store(Key, Val, D),
    {?MODULE, dict, D1};
store(Key, Val, {_, list, L}) ->
    L1 = lists:keystore(Key, 1, L, {Key,Val}),
    {?MODULE, list, L1}.

lookup(Key, {_, persistent, DetsName}) ->
  case dets:lookup(DetsName, Key) of
    [{_Key, Val} | []] -> {ok, Val};
    [] -> error;
    {error, _} -> error
  end;
lookup(Key, {_,dict,D}) ->
    dict:find(Key, D);
lookup(Key, {_,list,L}) ->
    case lists:keysearch(Key, 1, L) of
	{value, {Key,Val}} -> {ok, Val};
	false              -> error
    end.
