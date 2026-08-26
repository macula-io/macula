%% @doc CT helper for the Plumtree + PubSub acceptance suite.
%%
%% Each station is a process that holds, per realm:
%% <ul>
%%   <li>a `hecate_plumtree' state (eager + lazy push)</li>
%%   <li>a `hecate_pubsub' state (topic → subscriber set)</li>
%% </ul>
%%
%% Stations share an in-VM router that delivers frames between
%% station inboxes. On receiving a frame the station dispatches to
%% whichever module owns the frame type:
%% <ul>
%%   <li>`plumtree_*' → `hecate_plumtree:process/3' (delivers
%%       payloads locally; we then forward delivered payloads to
%%       `hecate_pubsub:process/3' so local subscribers fire).</li>
%%   <li>`subscribe' / `unsubscribe' / `event' →
%%       `hecate_pubsub:process/3' directly; `event' frames also
%%       enter the plumtree dispatcher via the local publish path.</li>
%% </ul>
%%
%% `connect/4' wires two stations' plumtree peer sets directly (no
%% HyParView handshake — that's `macula-hyparview''s concern, kept
%% out of this package's test surface entirely).
-module(plumtree_fleet_helper).

-export([
    start_fleet/3,
    stop_fleet/1,
    connect/4,
    subscribe/4,
    publish/5,
    deliveries/3
]).

-record(station, {
    name   :: atom(),
    pid    :: pid(),
    kp     :: macula_identity:key_pair(),
    pubkey :: macula_identity:pubkey()
}).

%%=====================================================================
%% Fleet lifecycle
%%=====================================================================

start_fleet(Names, Realms, _Opts) when is_list(Names), is_list(Realms) ->
    Router = spawn_router(),
    Stations = [build_station(N, Realms, Router) || N <- Names],
    Map = maps:from_list([{S#station.pubkey, S#station.pid}
                          || S <- Stations]),
    Router ! {table, Map},
    NameMap = maps:from_list([{S#station.name, S} || S <- Stations]),
    #{router => Router, stations => NameMap, realms => Realms}.

stop_fleet(#{router := Router, stations := Stations}) ->
    maps:foreach(fun(_N, S) -> S#station.pid ! stop end, Stations),
    Router ! stop,
    ok.

build_station(Name, Realms, Router) ->
    Kp = macula_identity:generate(),
    Pub = macula_identity:public(Kp),
    Pid = spawn(fun() ->
        station_loop(init_state(Name, Kp, Pub, Realms, Router))
    end),
    #station{name = Name, pid = Pid, kp = Kp, pubkey = Pub}.

init_state(Name, Kp, Pub, Realms, Router) ->
    Per = maps:from_list(
            [{R, #{plum   => hecate_plumtree:new(Kp, R),
                   pubsub => hecate_pubsub:new(R)}}
             || R <- Realms]),
    #{
        name   => Name,
        kp     => Kp,
        pubkey => Pub,
        router => Router,
        realms => Per,
        delivered => #{}   %% {realm, topic} → [payload]
    }.

%%=====================================================================
%% Public helpers
%%=====================================================================

%% @doc Wire A→B and B→A into each other's plumtree peer set for
%% `Realm' directly — no HyParView handshake.
connect(#{stations := Map} = _Net, NameA, NameB, Realm) ->
    #station{pid = PidA, pubkey = PubA} = maps:get(NameA, Map),
    #station{pid = PidB, pubkey = PubB} = maps:get(NameB, Map),
    call_station(PidA, {wire_peer, Realm, PubB}),
    call_station(PidB, {wire_peer, Realm, PubA}),
    ok.

subscribe(#{stations := Map} = _Net, Name, Realm, Topic) ->
    #station{pid = Pid, pubkey = Pub} = maps:get(Name, Map),
    call_station(Pid, {local_subscribe, Realm, Topic, Pub}).

publish(#{stations := Map} = _Net, Name, Realm, Topic, Payload) ->
    #station{pid = Pid} = maps:get(Name, Map),
    call_station(Pid, {local_publish, Realm, Topic, Payload}).

deliveries(#{stations := Map} = _Net, Name, {Realm, Topic}) ->
    #station{pid = Pid} = maps:get(Name, Map),
    call_station(Pid, {deliveries, Realm, Topic}).

%%=====================================================================
%% Station loop
%%=====================================================================

station_loop(State) ->
    receive
        stop -> ok;
        {frame, Frame}             -> station_loop(dispatch_frame(Frame, State));
        {control, From, Ref, Msg}  ->
            {Reply, State2} = control(Msg, State),
            From ! {Ref, Reply},
            station_loop(State2);
        _Other ->
            station_loop(State)
    end.

%%---------------------------------------------------------------------
%% Control messages (synchronous)
%%---------------------------------------------------------------------

control({wire_peer, Realm, Peer}, State) ->
    {ok, update_realm(State, Realm, fun(#{plum := Pl} = RS) ->
        RS#{plum := hecate_plumtree:add_peer(Pl, Peer)}
    end)};

control({local_subscribe, Realm, Topic, Sub}, State) ->
    {ok, update_realm(State, Realm, fun(#{pubsub := PS} = RS) ->
        RS#{pubsub := hecate_pubsub:subscribe(PS, Topic, Sub)}
    end)};

control({local_publish, Realm, Topic, Payload}, State) ->
    RS = realm_state(State, Realm),
    PubId   = maps:get(pubkey, State),
    EventF  = hecate_pubsub:build_event(
                maps:get(pubsub, RS),
                #{topic => Topic, realm => Realm,
                  publisher => PubId, seq => 0,
                  payload => Payload, published_at_ms => 0},
                maps:get(kp, State)),
    MsgId   = crypto:strong_rand_bytes(16),
    {Plum1, Sends, Delivered} =
        hecate_plumtree:publish(maps:get(plum, RS), MsgId, EventF),
    %% "Local delivered" — feed into pubsub for local subscribers too.
    State2 = deliver_payloads(Delivered, Realm, State, RS),
    emit_plumtree_sends(Sends, Realm, State2),
    State3 = update_realm(State2, Realm, fun(R) -> R#{plum := Plum1} end),
    {ok, State3};

control({deliveries, Realm, Topic}, State) ->
    {maps:get({Realm, Topic}, maps:get(delivered, State), []), State}.

%%---------------------------------------------------------------------
%% Frame dispatch
%%---------------------------------------------------------------------

dispatch_frame(#{frame_type := FT} = Frame, State) ->
    case FT of
        plumtree_gossip -> dispatch_plumtree(Frame, State);
        plumtree_ihave  -> dispatch_plumtree(Frame, State);
        plumtree_graft  -> dispatch_plumtree(Frame, State);
        plumtree_prune  -> dispatch_plumtree(Frame, State);
        subscribe       -> dispatch_pubsub(Frame, State);
        unsubscribe     -> dispatch_pubsub(Frame, State);
        event           -> dispatch_pubsub(Frame, State);
        _               -> State
    end.

%%---------------------------------------------------------------------
%% Plumtree dispatch
%%---------------------------------------------------------------------

dispatch_plumtree(#{realm := Realm} = Frame, State) ->
    RS = realm_state(State, Realm),
    From = maps:get(sender, Frame, undefined),
    {Plum1, Sends, Delivered} =
        hecate_plumtree:process(maps:get(plum, RS), From, Frame),
    State1 = deliver_payloads(Delivered, Realm, State, RS),
    emit_plumtree_sends(Sends, Realm, State1),
    update_realm(State1, Realm, fun(R) -> R#{plum := Plum1} end).

emit_plumtree_sends([], _Realm, _State) -> ok;
emit_plumtree_sends([{send, Peer, Frame} | Rest], Realm, State) ->
    route(State, Peer, frame_with_sender(Frame, Realm, State)),
    emit_plumtree_sends(Rest, Realm, State).

frame_with_sender(Frame, _Realm, State) ->
    Frame#{sender => maps:get(pubkey, State)}.

deliver_payloads([], _Realm, State, _RS) -> State;
deliver_payloads([{_MsgId, Payload} | Rest], Realm, State, RS) ->
    State1 = feed_pubsub(Payload, Realm, State, RS),
    deliver_payloads(Rest, Realm, State1, RS).

feed_pubsub(#{frame_type := event, topic := T} = Frame, Realm, State, RS) ->
    {_, Subs} = hecate_pubsub:process(maps:get(pubsub, RS),
                                      undefined, Frame),
    log_delivery(State, Realm, T, maps:get(payload, Frame), Subs);
feed_pubsub(_Other, _Realm, State, _RS) ->
    State.

log_delivery(State, Realm, Topic, Payload, Subs) when Subs =/= [] ->
    Key = {Realm, Topic},
    Delivered = maps:get(delivered, State),
    Existing  = maps:get(Key, Delivered, []),
    State#{delivered := Delivered#{Key => [Payload | Existing]}};
log_delivery(State, _Realm, _Topic, _Payload, _Subs) ->
    State.

%%---------------------------------------------------------------------
%% Pubsub dispatch (direct SUBSCRIBE/UNSUBSCRIBE/EVENT — rare in tests)
%%---------------------------------------------------------------------

dispatch_pubsub(#{realm := Realm} = Frame, State) ->
    RS = realm_state(State, Realm),
    {PS1, _} = hecate_pubsub:process(maps:get(pubsub, RS), undefined, Frame),
    update_realm(State, Realm, fun(R) -> R#{pubsub := PS1} end).

%%=====================================================================
%% Internals
%%=====================================================================

realm_state(State, Realm) ->
    maps:get(Realm, maps:get(realms, State)).

update_realm(State, Realm, Fun) ->
    Realms = maps:get(realms, State),
    RS  = maps:get(Realm, Realms),
    RS1 = Fun(RS),
    State#{realms := Realms#{Realm => RS1}}.

route(State, DstPubKey, Msg) ->
    maps:get(router, State) ! {route, DstPubKey, Msg},
    ok.

call_station(Pid, Msg) ->
    Ref = make_ref(),
    Pid ! {control, self(), Ref, Msg},
    receive {Ref, Reply} -> Reply
    after 500 -> {error, station_timeout}
    end.

%%=====================================================================
%% Router
%%=====================================================================

spawn_router() ->
    spawn(fun() -> router_loop(undefined) end).

router_loop(Table) ->
    receive
        {table, T}             -> router_loop(T);
        {route, Dst, Msg}      -> route_to(Table, Dst, Msg), router_loop(Table);
        stop                   -> ok
    end.

route_to(undefined, _, _) -> ok;
route_to(Table, Dst, Msg) ->
    case maps:find(Dst, Table) of
        {ok, Pid} -> Pid ! {frame, Msg};
        error     -> ok
    end.
