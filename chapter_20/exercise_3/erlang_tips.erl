-module(erlang_tips).

-include_lib("stdlib/include/qlc.hrl").

-record(user, {name, email, password}).
-record(tip, {url, description, timestamp, user_name}).
-record(abuse, {ip_address, num_visits}).

-export([all_users/0, get_user/1, add_user/3, all_tips/0, get_tip/1, add_tip/4,
        all_abuse/0, get_abuse/1, add_abuse/2, start_database/0,
        create_database/0, reset_tables/0]).

% Change these if your node names are different
-define(NODE_NAMES, [node()]).

% Maximum number of tips per day
-define(MAX_TIPS_PER_DAY, 10).

-define(SECONDS_IN_A_DAY, 86400).

%%%===================================================================
%%% Primary API
%%%===================================================================
all_users() ->
    % Get all users in the user table
    do_timed(qlc:q([X || X <- mnesia:table(user)])).

get_user(Email) ->
    % Select a user from the user table by email address
    do_timed(qlc:q([X || X <- mnesia:table(user), X#user.email == Email])).

add_user(Name, Email, Password) ->
    % Create a user record and store it in the database
    User = #user{name=Name, email=Email, password=Password},
    F = fun() ->
                mnesia:write(User)
        end,
    mnesia:transaction(F).

all_tips() ->
    % Get all tips in the tip table
    do_timed(qlc:q([X || X <- mnesia:table(tip)])).

get_tip(Url) ->
    % Select a tip from the tip table by url
    do_timed(qlc:q([X || X <- mnesia:table(tip), X#tip.url == Url])).

add_tip(Url, Description, Timestamp, UserName) ->
    % Create a tip
    % We have to record the user's name now in order to determine how many
    % submissions they have made over the last day
    Tip = #tip{url=Url, description=Description, timestamp=Timestamp, user_name=UserName},

    % Compute the timestamp of 24 hours earlier than `Timestamp`
    Seconds = timestamp_to_seconds(Timestamp),
    SecondsMinusOneDay = Seconds - ?SECONDS_IN_A_DAY,
    TimestampMinusOneDay = seconds_to_timestamp(SecondsMinusOneDay),

    % Check how many tips have been submitted in the last day
    TipsFromLastDay = get_tips_in_date_range(UserName, TimestampMinusOneDay, Timestamp),

    case length(TipsFromLastDay) =< ?MAX_TIPS_PER_DAY of
        true ->
            % Store the tip record in the database
            F = fun() ->
                        mnesia:write(Tip)
                end,
            mnesia:transaction(F);
        false ->
            {error, {daily_tip_limit_met, ?MAX_TIPS_PER_DAY}}
    end.

all_abuse() ->
    % Get all abuse records in the abuse table
    do_timed(qlc:q([X || X <- mnesia:table(abuse)])).

get_abuse(IpAddress) ->
    % Select an abuse record from the abuse table by IP address
    do_timed(qlc:q([X || X <- mnesia:table(abuse), X#abuse.ip_address == IpAddress])).

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

seconds_to_timestamp(Seconds) ->
    {Seconds div 1000000, Seconds rem 1000000, 0}.

timestamp_to_seconds({Mega, Sec, _}) ->
    % Convert the now timestamp to seconds
    (Mega * 1000000) + Sec.

get_tips_in_date_range(UserName, DateStart, DateEnd) ->
    do(qlc:q([X || X <- mnesia:table(tip),
                   X#tip.timestamp =< DateEnd, X#tip.timestamp >= DateStart, X#tip.user_name =:= UserName
             ])).

do_timed(Q) ->
  % Here we time the call to do/1
  {Time, Result} = timer:tc(fun do/1, [Q]),

  % Then we print the number of microseconds the call took
  io:format("Query ~w took ~w microseconds.~n", [Q, Time]),
  % In a real world scenario we would likely make a call to an server
  % recording application metrics
  Result.

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

start_mnesia(Nodes) ->
    lists:map(fun(Node) ->
                      ok = rpc:call(Node, mnesia, start, [])
              end, Nodes).
