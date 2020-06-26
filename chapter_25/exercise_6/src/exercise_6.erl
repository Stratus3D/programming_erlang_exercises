-module(exercise_6).

-export([main/1]).

main(_Args) ->
    cgi_web_server:start(),
    timer:sleep(infinity).
