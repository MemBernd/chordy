-module(node1).
-compile(export_all).
-define(randomness, 1000000000).
-define(Timeout, 5000).
-define(Stabilize, 1000).


node(Id, Predecessor, Successor) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);

        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);

        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);

        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor);
        Msg ->
            io:format("Node ~w, strange message received:~n~w~n", [Id, Msg])
    end.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} ->
            Successor;
        {Skey, _} ->
            Spid ! {notify, {Id, self()}},
            Successor;
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    Xpid ! {request, self()},
                    receive
                        {status, XPred} ->
                            stabilize(XPred, Id, {Xkey, Xpid})
                    end;
                false ->
                    Spid ! {notify, {Id, self()}},
                    Successor
            end
    end.
schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({Skey, Spid}) ->
    %io:format("~w doing stabilize/1, sendig request to ~w.~n", [self(), Skey]),
    Spid ! {request, self()}.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->
            {Nkey, Npid};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    {Nkey, Npid};
                false ->
                    Predecessor
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
    node(Id, Predecessor, Successor).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, Peer}}
        after ?Timeout ->
            io:format("Time out: no response ~n",[])
    end.

create_probe(Id, {Skey, Spid}) ->
    Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.
remove_probe(T, Nodes) ->
    io:format("Time passed: ~w~nNodes passed: ~w~n", [T, Nodes]).
forward_probe(Ref, T, Nodes, Id, {Skey, Spid}) ->
    io:format("~w: Probe message from ~w passing by.~n", [Id, Ref]),
    Spid ! {probe, Ref, [Id|Nodes], T}.
