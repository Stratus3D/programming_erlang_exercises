-module(dict_test).

-export([test/0]).

test() ->
    % Create dictionary
    Dict = dict:new(),

    % Add to dictionary
    Dict2 = dict:append(somekey, someval, Dict),

    % Add another item to dictionary
    Dict3 = dict:append(anotherkey, anotherval, Dict2),

    % Fetch item from dictionary
    _Value = dict:fetch(somekey, Dict3),

    % Update item in dictionary
    Dict4 = dict:update(somekey, fun (_) ->
                                         new_elem
                                 end, Dict3),

    % Find item in dictionary
    {ok, _Value2} = dict:find(anotherkey, Dict4),

    % Delete item in dictionary
    _Dict5 = dict:erase(anotherkey, Dict4).
