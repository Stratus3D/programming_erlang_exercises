-module(ping_pong).

-export([ping/1]).

% Sample CGI function
ping(_Arg) ->
    [<<"foobar">>].
