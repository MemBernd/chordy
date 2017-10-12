-module(client).
-compile(export_all).
-define(Timeout, 10000).

start(Name) ->
    spawn(fun() -> init(Name, []) end).

init(Name, Keys) ->
    client(Name, Keys).

client(Name, Keys) ->
    receive
        {insertRange, From, To, Npid} ->
            insertRange(From, To, Npid),
            client(Name, Keys);
        {insert, Key, Value, Npid} ->
            insert(Key, Value, Npid),
            client(Name, [Keys]);
        {lookupList, Npid} ->
            lookupList(Keys, Npid),
            client(Name, Keys);
        {lookup, Key, Npid} ->
            lookup(Key, Npid),
            client(Name, Keys);
        {Key, ok, Id} ->
            io:format("~w: Qref ~w into ~w~n", [Name, Key, Id]),
            client(Name, [Key|Keys]);
        {Qref, {Key, Value}, Id} ->
            io:format("~w: Lookup from ~w with ~w: value ~w~n", [Name, Id, Key, Value]),
            client(Name, Keys);
        {_, false, Id} ->
            io:format("~w responsible, but didn't find it.~n",[Id]),
            client(Name, Keys);
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
