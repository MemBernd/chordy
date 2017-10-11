-module(key).
-compile(export_all).
-define(randomness, 1000000000).

generate() ->
    rand:uniform(?randomness).

between(_, From, From) ->
    true;
between(Key, From, To) when From < Key, Key =< To ->
    true;
between(Key, From, To) when To < From, From < Key ->
    true;
between(Key, From, To) when Key =< To, To < From ->
    true;
between(_, _, _) ->
    false.
