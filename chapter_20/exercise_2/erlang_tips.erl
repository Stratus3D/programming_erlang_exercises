-module(erlang_tips).

-include_lib("stdlib/include/qlc.hrl").

-record(user, {name, email, password}).
-record(tip, {url, description, date}).
-record(abuse, {ip_address, num_visits}).

-export([all_users/0, get_user/1, add_user/3, all_tips/0, get_tip/1, add_tip/3,
        all_abuse/0, get_abuse/1, add_abuse/2, start_database/0,
        create_database/0, reset_tables/0]).

% Change these if your node names are different
-define(NODE_NAMES, ['node1@localhost', 'node2@localhost']).

%%%===================================================================
%%% Primary API
%%%===================================================================
all_users() ->
    % Get all users in the user table
    do(qlc:q([X || X <- mnesia:table(user)])).

get_user(Email) ->
    % Select a user from the user table by email address
    do(qlc:q([X || X <- mnesia:table(user), X#user.email == Email])).

add_user(Name, Email, Password) ->
    % Create a user record and store it in the database
    User = #user{name=Name, email=Email, password=Password},
    F = fun() ->
                mnesia:write(User)
        end,
    mnesia:transaction(F).

all_tips() ->
    % Get all tips in the tip table
    do(qlc:q([X || X <- mnesia:table(tip)])).

get_tip(Url) ->
    % Select a tip from the tip table by url
    do(qlc:q([X || X <- mnesia:table(tip), X#tip.url == Url])).

add_tip(Url, Description, Date) ->
    % Create a tip record and store it in the database
    Tip = #tip{url=Url, description=Description, date=Date},
    F = fun() ->
                mnesia:write(Tip)
        end,
    mnesia:transaction(F).

all_abuse() ->
    % Get all abuse records in the abuse table
    do(qlc:q([X || X <- mnesia:table(abuse)])).

get_abuse(IpAddress) ->
    % Select an abuse record from the abuse table by IP address
    do(qlc:q([X || X <- mnesia:table(abuse), X#abuse.ip_address == IpAddress])).

add_abuse(IpAddress, NumVisits) ->
    % Create an abuse record and store it in the database
    Abuse = #abuse{ip_address=IpAddress, num_visits=NumVisits},
    F = fun() ->
                mnesia:write(Abuse)
        end,
    mnesia:transaction(F).

start_database() ->
    % Get the database in a state where we can query it
    start_mnesia(?NODE_NAMES),
    mnesia:wait_for_tables([user,tip,abuse], 20000).

create_database() ->
    % Create schema
    ok = mnesia:create_schema(?NODE_NAMES),

    % Start mnesia on every node if it's not already started
    start_mnesia(?NODE_NAMES),

    % Table Storage
    DiskCopies = {disc_copies, ?NODE_NAMES},

    % Create tables
    {atomic, ok} = mnesia:create_table(user, [{attributes, record_info(fields, user)}, DiskCopies]),
    {atomic, ok} = mnesia:create_table(tip, [{attributes, record_info(fields, tip)}, DiskCopies]),
    {atomic, ok} = mnesia:create_table(abuse, [{attributes, record_info(fields, abuse)}, DiskCopies]),
    ok.

reset_tables() ->
    % Empty all the tables in the database
    mnesia:clear_table(user),
    mnesia:clear_table(tip),
    mnesia:clear_table(abuse).

%%%===================================================================
%%% Private functions
%%%===================================================================
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

start_mnesia(Nodes) ->
    lists:map(fun(Node) ->
                      ok = rpc:call(Node, mnesia, start, [])
              end, Nodes).
