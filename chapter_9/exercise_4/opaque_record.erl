-module(opaque_record).

-record(user, {name, email, password}).

% Opaque type
-opaque user() :: #user{}.

-export_type([user/0]).

-export([new_user/0]).

% Function that returns value that is of the opaque type
-spec new_user() -> user().

new_user() ->
    #user{}.
