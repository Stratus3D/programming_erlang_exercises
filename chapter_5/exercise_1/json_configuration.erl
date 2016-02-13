-module(json_configuration).

-export([config_to_map/1, verify_config/1]).

config_to_map(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            jsx:decode(Binary);
        Error ->
            Error
    end.

verify_config(Filename) ->
    Config = config_to_map(Filename),
    case is_list(Config) of
        true ->
           is_proplist(Config);
        false ->
            false
    end.

% Private functions
is_proplist([]) ->
    true;
is_proplist([{_Key, _Value}|List]) ->
    is_proplist(List);
is_proplist([_|_List]) ->
    false.
