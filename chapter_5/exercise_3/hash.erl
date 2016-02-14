-module(hash).

% Our own version of Ruby's Hash class
-export([
         any/2,
         all/2,
         delete/2,
         dig/2,
         empty/1,
         has_key/2,
         has_value/2,
         keys/1,
         values/1,
         merge/2,
         reject/2,
         select/2,
         shift/1,
         size/1,
         store/3
         ]).

% Return true if `Predicate` returns true for any of the pairs in the map
any(Hash, Predicate) ->
    Proplist = maps:to_list(Hash),
    lists:any(Predicate, Proplist).

% Return true if `Predicate` returns true for all of the pairs in the map
all(Hash, Predicate) ->
    Proplist = maps:to_list(Hash),
    lists:all(Predicate, Proplist).

% Delete a pair from the map and return the new map
delete(Hash, Key) ->
    maps:remove(Key, Hash).

% Retrieve a value from a nested map
dig(Hash, Keys) ->
    do_dig(Hash, Keys).

% Returns whether or not the map is empty
empty(Hash) ->
    maps:size(Hash) > 0.

% Returns whether or not `Key` is present as a key in map `Hash`
has_key(Hash, Key) ->
    lists:member(Key, keys(Hash)).

% Returns whether or not `Value` is present as a value in map `Hash`
has_value(Hash, Value) ->
    lists:member(Value, values(Hash)).

% Returns all the keys in the map
keys(Hash) ->
    maps:keys(Hash).

% Returns all the values in the map
values(Hash) ->
    maps:values(Hash).

% Merges two maps and returns the resulting map
merge(Hash1, Hash2) ->
    maps:merge(Hash1, Hash2).

% Returns a map containing all the pairs for which `Predicate` returned false
reject(Hash, Predicate) ->
    maps:filter(fun(Key, Value) ->
                        Predicate(Key, Value) =:= false
                end, Hash).

% Returns a map containing all the pairs for which `Predicate` returned true
select(Hash, Predicate) ->
    maps:filter(fun(Key, Value) ->
                        Predicate(Key, Value) =:= true
                end, Hash).

% Removes a key from the map and returns the new map
shift(Hash) ->
    [Key|_] = keys(Hash),
    maps:remove(Key, Hash).

% Returns the size of the map
size(Hash) ->
    maps:size(Hash).

% Stores a new key/value pair in the map and returns a new map
store(Hash, Key, Value) ->
    maps:put(Hash, Key, Value).

% Private functions
do_dig(Value, []) ->
    Value;
do_dig(Value, [Key|Keys]) ->
    case is_map(Value) of
        true -> % Retrieve a value from a nested map
            NewValue = maps:get(Key, Value),
            do_dig(NewValue, Keys);
        false -> % Add support for proplists too
            proplists:get_value(Key, Value)
    end.
