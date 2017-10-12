
-module(node3).
-compile(export_all).
-define(randomness, 1000000000).
-define(Timeout, 5000).
-define(Stabilize, 2000).


node(Id, Predecessor, Successor, Store, Next) ->
    %io:format("~w: Pred: ~w, Succ: ~w~n", [Id, Predecessor, Successor]),
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store, Next);

        {notify, New} ->
            %io:format("execute notify() with store: ~w~n", [Store]),
            {Pred, Keep} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Keep, Next);

        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(Id, Predecessor, Successor, Store, Next);

        {status, Pred, Nx} ->
            {Succ, Nxt} = stabilize(Pred, Id, Successor, Nx),
            {_, Ref, _} = Successor,
            drop(Ref),
            node(Id, Predecessor, Succ, Store, Nxt);
        stabilize ->
            %io:format("~w doing succesor: ~w~n", [Id, Successor]),
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store, Next);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value,  Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added, Next);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged, Next);
        {'DOWN', Ref, Process, _, _} ->
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Store, Nxt);
        stop ->
            ok;
        Msg ->
            io:format("Node ~w, strange message received:~n~w~n", [Id, Msg])
    end.

monitor(Pid) ->
    erlang:monitor(process, Pid).

drop(nil) ->
    ok;
drop(Pid) ->
    %io:format("Pid: ~w~n", [Pid]),
    erlang:demonitor(Pid).

down(Ref, {_, Ref, _}, Successor, Next) ->
    {nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, _, Npid}) ->
    Nref = monitor(Npid),
    stabilize({Nkey, nil, Npid}),
    {Predecessor, {Nkey, Nref, Npid}, nil}.

add(Key, Value, Qref, Client, Id, {Pkey,_, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok, Id},
            storage:add(Key, Value, Store);
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
        end.
lookup(Key, Qref, Client, Id, {Pkey,_, _}, Successor, Store) ->
    %io:format("~w looking up ~w~n", [Id, Key]),
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result, Id};
        false ->
            {_, _, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
    end.

handover(Id, Store, Nkey, Npid) ->
    %io:format("~w: about to execute split on store: ~w~n", [Id, Store]),
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

stabilize(Pred, Id, Successor, Nx) ->
    {Skey, _, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify, {Id, nil, self()}},
            Sref = monitor(Spid),
            {{Skey, Sref, Spid}, Nx};
        {Id, _, _} ->
            Sref = monitor(Spid),
            {{Skey, Sref, Spid}, Nx};
        {Skey, _, _} ->
            Spid ! {notify, {Id, nil, self()}},
            Sref = monitor(Spid),
            {{Skey, Sref, Spid}, Nx};
        {Xkey, Ref, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    io:format("~w better Successor; key ~w pid ~w~n",[Id, Xkey, Xpid]),
                    Xpid ! {request, self()},
                    receive
                        {status, XPred, XSucc} ->
                            stabilize(XPred, Id, Pred, Successor)
                        after ?Timeout ->
                            io:format("better successor timed out.~n",[]),
                            Sref = monitor(Spid),
                            {{Skey, Sref, Spid}, Nx}
                    end;
                false ->
                    Spid ! {notify, {Id, nil, self()}},
                    Sref = monitor(Spid),
                    {{Skey, Sref, Spid}, Nx}
            end
    end.
schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({Skey,_, Spid}) ->
    %io:format("~w doing stabilize/1, sendig request to ~w.~n", [self(), Skey]),
    Spid ! {request, self()}.

request(Peer, Predecessor, Successor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, Successor};
        {Pkey, Ref, Ppid} ->
            Peer! {status, Predecessor, Successor}
    end.

notify({Nkey, _, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            Nref = monitor(Npid),
            {{Nkey, Nref, Npid}, Keep};
        {Pkey,Pref, Ppid} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    Keep = handover(Id, Store, Nkey, Npid),
                    Nref = monitor(Npid),
                    drop(Pref),
                    {{Nkey, Nref, Npid}, Keep};
                false ->
                    {Predecessor, Store}
        end
    end.

start(Id) ->
    start(Id, nil).
start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, storage:create(), nil).

connect(Id, nil) ->
    Ref = monitor(self()),
    {ok, {Id, Ref, self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            Ref = monitor(Peer),
            {ok, {Skey, Ref, Peer}}
        after ?Timeout ->
            io:format("Time out: no response ~n",[])
    end.

create_probe(Id, {Skey, _, Spid}) ->
    Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.
remove_probe(T, Nodes) ->
    Time = erlang:system_time(micro_seconds) - T,
   io:format("Time passed: ~w~nNodes passed: ~w~n", [Time, Nodes]).
forward_probe(Ref, T, Nodes, Id, {Skey, _, Spid}) ->
    io:format("~w: Probe message from ~w passing by.~n", [Id, Ref]),
    Spid ! {probe, Ref, [Id|Nodes], T}.
