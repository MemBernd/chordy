-module(client).
-compile(export_all).
-define(Timeout, 10000).

start(Name) ->
    spawn(fun() -> init(Name, []) end).

init(Name, Keys) ->
    client(Name, Keys, nil).

client(Name, Keys, Time) ->
    case Keys of
        [] ->
            Last = nil;
        [_] ->
            Last = lists:last(Keys)
        end,
    receive
        {insertRange, From, To, Npid} ->
            insertRange(From, To, Npid),
            client(Name, Keys, Time);
        {insert, Key, Value, Npid} ->
            insert(Key, Value, Npid),
            client(Name, Keys, Time);
        {lookupList, Npid} ->
            Ti = erlang:system_time(micro_seconds),
            lookupList(Keys, Npid),
            client(Name, Keys, Ti);
        {lookup, Key, Npid} ->
            lookup(Key, Npid),
            client(Name, Keys, Time);
        {Key, ok, Id} ->
            io:format("~w: Qref ~w into ~w~n", [Name, Key, Id]),
            client(Name, [Key|Keys], Time);
        {Qref, {Last, Value}, Id} ->
            Tfinal = erlang:system_time(micro_seconds) - Time,
            io:format("~w: All keys returned after ~w~n", [Name, Tfinal]),
            client(Name, Keys, nil);
        {Qref, {Key, Value}, Id} ->
            io:format("~w: Lookup from ~w with ~w: value ~w~n", [Name, Id, Key, Value]),
            client(Name, Keys, Time);
        {_, false, Id} ->
            io:format("~w responsible, but didn't find it.~n",[Id]),
            client(Name, Keys, Time);
        Msg ->
            io:format("Uncaught message: ~w~n",[Msg])
    end.

insertRange(From, To, Npid) when To >= From ->
    insert(To, "key"++integer_to_list(To), Npid),
    insertRange(From, To-1, Npid);
insertRange(_,_,_) ->
    done.

insert(Key, Value, Npid) ->
    Qref = rand:uniform(),
    Npid ! {add, Key, Value, Qref, self()}.

lookupList([], _) ->
    done;
lookupList([Key|Rest], Npid) ->
    lookup(Key, Npid),
    lookupList(Rest, Npid).

lookup(Key, Npid) ->
    Qref = rand:uniform(),
    Npid ! {lookup, Key, Qref, self()}.
