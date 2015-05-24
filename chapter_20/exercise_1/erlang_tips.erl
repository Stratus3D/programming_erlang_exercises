-module(erlang_tips).

-record(user, {name, email, password}).
-record(tip, {url, description, date}).
-record(abuse, {ip_address, num_visits}).

% TODO: Complete this module
-export([]).

start_database() ->
    ok.

create_database() ->
    % Create schema
    ok = mnesia:create_schema([node()]),

    % Start mnesia
    ok = mnesia:start(),

    % Create tables
    {atomic, ok} = mnesia:create_table(users, [{attributes, record_info(fields, user)}]),
    {atomic, ok} = mnesia:create_table(tips, [{attributes, record_info(fields, tip)}]),
    {atomic, ok} = mnesia:create_table(abuse, [{attributes, record_info(fields, abuse)}]),
    ok.
