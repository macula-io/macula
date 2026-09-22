%% @doc CT helper for the Plumtree + PubSub acceptance suite: a fleet of in-VM stations that gossip signed
%% publications, as a node does in 12.
%%
%% Each station is a process with an identity key and, per realm, a `hecate_plumtree' state (eager and lazy push) and
%% a `hecate_pubsub' state (topic to subscribers). A station publishes a PUBLISH it signs with its identity key; Plumtree
%% verifies every publication, pushes it to eager peers and announces it to lazy ones, and hands each new one over as a
%% delivery, which reaches the station's local subscribers to its topic. A station logs the payloads it delivered.
%%
%% `connect/4' wires two stations' Plumtree peer sets directly: no HyParView handshake, which is
%% `macula_hyparview_SUITE''s concern. Frames travel as their encoded bytes through a router process that delivers
%% them with the sender's node_id and the realm, and settle/1 waits until no frame is in flight.
-module(plumtree_fleet_helper).

-export([
    start_fleet/2,
    stop_fleet/1,
    connect/4,
    subscribe/4,
    publish/5,
    deliveries/3
]).

-record(station, {
    name    :: atom(),
    pid     :: pid(),
    node_id :: macula_node_keys:node_id()
}).

-define(PROFILE, pq_pure).

%%=====================================================================
%% Fleet lifecycle
%%=====================================================================

%% @doc A fleet of stations named Names, each a member of every realm, by 32-byte id, in Realms.
start_fleet(Names, Realms) when is_list(Names), is_list(Realms) ->
    Router = spawn_link(fun() -> router_loop(#{}) end),
    Stations = [build_station(Name, Realms, Router) || Name <- Names],
    Router ! {table, maps:from_list([{S#station.node_id, S#station.pid} || S <- Stations])},
    #{router => Router, stations => maps:from_list([{S#station.name, S} || S <- Stations])}.

stop_fleet(#{router := Router, stations := Stations}) ->
    maps:foreach(fun(_Name, #station{pid = Pid}) -> Pid ! stop end, Stations),
    Router ! stop,
    ok.

build_station(Name, Realms, Router) ->
    Key = macula_test_identity:key(),
    {ok, NodeId} = macula_node_keys:node_id(Key),
    PerRealm = maps:from_list([{Realm, realm_state(NodeId, Realm)} || Realm <- Realms]),
    Pid = spawn_link(fun() ->
                         station_loop(#{key => Key, node_id => NodeId, router => Router, realms => PerRealm,
                                        delivered => #{}, seen => 0})
                     end),
    #station{name = Name, pid = Pid, node_id = NodeId}.

realm_state(NodeId, Realm) ->
    {ok, Plumtree} = hecate_plumtree:new(NodeId, Realm),
    #{plumtree => Plumtree, pubsub => hecate_pubsub:new(Realm, ?PROFILE)}.

%%=====================================================================
%% Test API
%%=====================================================================

%% @doc Wire NameA and NameB into each other's Plumtree peer set for Realm.
connect(#{stations := Map} = _Net, NameA, NameB, Realm) ->
    #station{pid = PidA, node_id = A} = maps:get(NameA, Map),
    #station{pid = PidB, node_id = B} = maps:get(NameB, Map),
    ok = call(PidA, {add_peer, Realm, B}),
    ok = call(PidB, {add_peer, Realm, A}).

%% @doc Station Name subscribes itself to Topic in Realm.
subscribe(#{stations := Map} = _Net, Name, Realm, Topic) ->
    #station{pid = Pid} = maps:get(Name, Map),
    call(Pid, {subscribe, Realm, Topic}).

%% @doc Station Name publishes Payload to Topic in Realm, and the fleet settles.
publish(#{stations := Map} = Net, Name, Realm, Topic, Payload) ->
    #station{pid = Pid} = maps:get(Name, Map),
    ok = call(Pid, {publish, Realm, Topic, Payload}),
    settle(Net).

%% @doc The payloads station Name delivered to its subscribers to Topic in Realm, newest first.
deliveries(#{stations := Map} = _Net, Name, {Realm, Topic}) ->
    #station{pid = Pid} = maps:get(Name, Map),
    call(Pid, {deliveries, Realm, Topic}).

%%=====================================================================
%% Settling: no frame in flight
%%=====================================================================

%% Every frame goes through the router, which delivers in order, so once the router and then every station have
%% answered a call, every frame sent before that has been handled. A round in which no station handled a frame means
%% nothing is left in flight.
settle(#{router := Router, stations := Map} = Net) ->
    Seen = fun() ->
               ok = call(Router, sync),
               lists:sum([call(Pid, seen) || #station{pid = Pid} <- maps:values(Map)])
           end,
    settled(Seen(), Seen, Net).

settled(Count, Seen, Net) ->
    next_round(Seen(), Count, Seen, Net).

next_round(Count, Count, _Seen, _Net) -> ok;
next_round(Later, _Count, Seen, Net) -> settled(Later, Seen, Net).

%%=====================================================================
%% Station
%%=====================================================================

station_loop(State) ->
    receive
        stop ->
            ok;
        {frame, From, Realm, Bytes} ->
            station_loop(handle_frame(From, Realm, Bytes, State));
        {call, Caller, Ref, Request} ->
            {Reply, State1} = handle_call(Request, State),
            Caller ! {Ref, Reply},
            station_loop(State1)
    end.

handle_call({add_peer, Realm, Peer}, State) ->
    {ok, update_realm(State, Realm, fun(#{plumtree := P} = R) -> R#{plumtree := hecate_plumtree:add_peer(P, Peer)} end)};
handle_call({subscribe, Realm, Topic}, #{node_id := Self} = State) ->
    {ok, update_realm(State, Realm, fun(#{pubsub := S} = R) -> R#{pubsub := hecate_pubsub:subscribe(S, Topic, Self)} end)};
handle_call({publish, Realm, Topic, Payload}, #{key := Key, realms := Realms} = State) ->
    Now = erlang:system_time(millisecond),
    Publish = macula_frame:publish(#{realm => Realm, topic => Topic, seq => 1, published_at => Now,
                                     payload => Payload}, Key),
    #{plumtree := Plumtree} = maps:get(Realm, Realms),
    {Plumtree1, Actions, Deliveries} = hecate_plumtree:publish(Plumtree, Publish, Now),
    {ok, handled(Realm, Plumtree1, Actions, Deliveries, State)};
handle_call({deliveries, Realm, Topic}, #{delivered := Delivered} = State) ->
    {maps:get({Realm, Topic}, Delivered, []), State};
handle_call(seen, #{seen := Seen} = State) ->
    {Seen, State}.

%% A GOSSIP carries its realm inside its signed publication, so a frame arrives with the realm it was sent in, as a
%% station link delivers a frame on the realm's overlay subscription.
handle_frame(From, Realm, Bytes, #{realms := Realms, seen := Seen} = State) ->
    {ok, Frame, <<>>} = macula_frame:decode(Bytes),
    #{plumtree := Plumtree} = maps:get(Realm, Realms),
    Clocks = #{wall => erlang:system_time(millisecond), monotonic => erlang:monotonic_time(millisecond)},
    {Plumtree1, Actions, Deliveries} = hecate_plumtree:process(Plumtree, From, Frame, Clocks),
    handled(Realm, Plumtree1, Actions, Deliveries, State#{seen := Seen + 1}).

handled(Realm, Plumtree, Actions, Deliveries, State) ->
    [send(State, Realm, Peer, Out) || {send, Peer, Out} <- Actions],
    State1 = update_realm(State, Realm, fun(R) -> R#{plumtree := Plumtree} end),
    lists:foldl(fun({_MsgId, Publication}, Acc) -> delivered(Realm, Publication, Acc) end, State1, Deliveries).

%% A delivery reaches the station's own subscribers to its topic, and is logged when there is one.
delivered(Realm, #{topic := Topic, payload := Payload}, #{realms := Realms, delivered := Delivered} = State) ->
    #{pubsub := PubSub} = maps:get(Realm, Realms),
    logged(hecate_pubsub:subscribers(PubSub, Topic), {Realm, Topic}, Payload, Delivered, State).

logged([], _Key, _Payload, _Delivered, State) -> State;
logged([_ | _], Key, Payload, Delivered, State) ->
    State#{delivered := Delivered#{Key => [Payload | maps:get(Key, Delivered, [])]}}.

update_realm(#{realms := Realms} = State, Realm, Fun) ->
    State#{realms := Realms#{Realm := Fun(maps:get(Realm, Realms))}}.

send(#{router := Router, node_id := Self}, Realm, To, Frame) ->
    Router ! {route, Self, To, Realm, macula_frame:encode(Frame)},
    ok.

%%=====================================================================
%% Router
%%=====================================================================

router_loop(Table) ->
    receive
        {table, Table1} ->
            router_loop(Table1);
        {route, From, To, Realm, Bytes} ->
            deliver(maps:find(To, Table), From, Realm, Bytes),
            router_loop(Table);
        {call, Caller, Ref, sync} ->
            Caller ! {Ref, ok},
            router_loop(Table);
        stop ->
            ok
    end.

deliver({ok, Pid}, From, Realm, Bytes) -> Pid ! {frame, From, Realm, Bytes};
deliver(error, _From, _Realm, _Bytes) -> ok.

%%=====================================================================
%% Internals
%%=====================================================================

call(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {call, self(), Ref, Request},
    receive {Ref, Reply} -> Reply
    after 5000 -> erlang:error({no_reply, Request})
    end.
