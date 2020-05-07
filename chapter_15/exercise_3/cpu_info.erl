-module(cpu_info).

-export([get/0, get/1]).

% Get all information about the CPU
get() ->
    cpu_info().

% Get a specific value from the CPU information
%
% Example:
%
% 1> cpu_info:get("Model name").
% {"Model name","Intel(R) Core(TM) i7-5500U CPU @ 2.40GHz"}
get(Key) ->
    Proplist = cpu_info(),
    proplists:get_value(Key, Proplist).

cpu_info() ->
    % Get the raw command output
    Output = os:cmd("lscpu"),
    % Split output into lines
    Lines = re:split(Output, "\n"),
    % Convert to key value tuples
    to_key_value_tuples(Lines).

to_key_value_tuples([]) -> [];
to_key_value_tuples([<<>>]) -> [];
to_key_value_tuples([Line|Lines]) ->
    [RawKey, RawValue] = re:split(Line, ":"),
    % Cleanup keys and values
    Value = trim(RawValue),
    Key = cleanup_key(RawKey),
    [{Key, Value}|to_key_value_tuples(Lines)].

cleanup_key(Key) ->
  remove_parens(trim(Key)).

remove_parens(String) ->
    re:replace(String, "\\([^)]*\\)", "", [global,{return,list}]).

trim(String) ->
    re:replace(String, "(^\\s+)|(\\s+$)", "", [global,{return,list}]).
