-module(erlang_tips).

-include_lib("stdlib/include/qlc.hrl").

-record(user, {name, email, password}).
-record(tip, {url, description, date}).
-record(abuse, {ip_address, num_visits}).

-export([all_users/0, get_user/1, add_user/3, all_tips/0, get_tip/1, add_tip/3,
        all_abuse/0, get_abuse/1, add_abuse/2, start_database/0,
        create_database/0, reset_tables/0]).

% Primary API
all_users() ->
    do(qlc:q([X || X <- mnesia:table(user)])).

get_user(Email) ->
    do(qlc:q([X || X <- mnesia:table(user), X#user.email == Email])).

add_user(Name, Email, Password) ->
    User = #user{name=Name, email=Email, password=Password},
    F = fun() ->
                mnesia:write(User)
        end,
    mnesia:transaction(F).

all_tips() ->
    do(qlc:q([X || X <- mnesia:table(tip)])).

get_tip(Url) ->
    do(qlc:q([X || X <- mnesia:table(tip), X#tip.url == Url])).

add_tip(Url, Description, Date) ->
    Tip = #tip{url=Url, description=Description, date=Date},
    F = fun() ->
                mnesia:write(Tip)
        end,
    mnesia:transaction(F).

all_abuse() ->
    do(qlc:q([X || X <- mnesia:table(abuse)])).

get_abuse(IpAddress) ->
    do(qlc:q([X || X <- mnesia:table(abuse), X#abuse.ip_address == IpAddress])).

add_abuse(IpAddress, NumVisits) ->
    Abuse = #abuse{ip_address=IpAddress, num_visits=NumVisits},
    F = fun() ->
                mnesia:write(Abuse)
        end,
    mnesia:transaction(F).

start_database() ->
    mnesia:start(),
    mnesia:wait_for_tables([user,tip,abuse], 20000).

create_database() ->
    % Create schema
    ok = mnesia:create_schema([node()]),

    % Start mnesia if it's not already started
    mnesia:start(),

    % Table Storage
    DiskCopies = {disc_copies, [node()]},

    % Create tables
    {atomic, ok} = mnesia:create_table(user, [{attributes, record_info(fields, user)}, DiskCopies]),
    {atomic, ok} = mnesia:create_table(tip, [{attributes, record_info(fields, tip)}, DiskCopies]),
    {atomic, ok} = mnesia:create_table(abuse, [{attributes, record_info(fields, abuse)}, DiskCopies]),
    ok.

reset_tables() ->
    mnesia:clear_table(user),
    mnesia:clear_table(tip),
    mnesia:clear_table(abuse).

% Private functions
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
