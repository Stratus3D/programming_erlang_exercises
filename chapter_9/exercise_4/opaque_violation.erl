-module(opaque_violation).

-export([test/0]).

test() ->
    % Function that returns a value that is of an opaque type
    User = opaque_record:new_user(),

    % Violate opaqueness by treating the User variable, which is the opaque
    % type, as a tuple.
    {user, Name, _Email, _Password} = User,
    Name.
