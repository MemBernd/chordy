-module(storage).
-compile(export_all).

create() ->
    [].

add(Key, Value, Store) ->
    [{Key, Value}|Store].

lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
    %io:format("From: ~w To: ~w Store: ~w~n", [From, To, Store]),
    lists:foldl(fun({Key, Value}, {Updated, Rest}) ->
        case key:between(Key, From, To) of
            false ->
                {[{Key, Value}|Updated], Rest};
            true ->
                {Updated, [{Key, Value}|Rest]}
        end
    end, {[],[]}, Store).

merge(Entries, Store) ->
    Entries ++ Store.
