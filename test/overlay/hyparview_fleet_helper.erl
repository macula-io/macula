%% @doc CT helper for the HyParView acceptance suite: a fleet of in-VM stations that admit one another through
%% realm-gated JOIN, as a node does in 12.
%%
%% Each station is a process with an identity key and, per realm, a HyParView view and a gated ctx(): the realm's key
%% id, the profile, and the station's own endorsement for the NEIGHBOR frames it sends. Admission is
%% `macula_hyparview_proto:process/4''s own: a JOIN carries the joiner's endorsement in its `record' field, and the
%% receiving station verifies it against the realm key and the sender.
%%
%% Frames travel as their encoded bytes, through a router process that delivers them with the sender's node_id, the
%% way a station link delivers an overlay frame with its sender. settle/1 waits until no frame is in flight, so a test
%% can assert what a join did not do.
-module(hyparview_fleet_helper).

-export([
    start_fleet/2,
    stop_fleet/1,
    endorse/4,
    join/5,
    active_view/3,
    node_id_of/2
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

%% @doc A fleet of stations named Names, each a member of every realm in Realms, a list of
%% #{realm := RealmId, realm_key := RealmKey}.
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
    PerRealm = maps:from_list([{Realm, #{view => macula_hyparview_view:new(NodeId),
                                         ctx => gated_ctx(Realm, RealmKey, NodeId)}}
                               || #{realm := Realm, realm_key := RealmKey} <- Realms]),
    Pid = spawn_link(fun() -> station_loop(#{node_id => NodeId, router => Router, realms => PerRealm, seen => 0}) end),
    #station{name = Name, pid = Pid, node_id = NodeId}.

gated_ctx(Realm, RealmKey, NodeId) ->
    #{self_id => NodeId, realm => Realm, now => 0,
      realm_key_id => macula_node_keys:key_id(RealmKey), profile => ?PROFILE,
      self_endorsement => endorsement(Realm, RealmKey, NodeId)}.

%%=====================================================================
%% Test API
%%=====================================================================

node_id_of(#{stations := Map}, Name) ->
    (maps:get(Name, Map))#station.node_id.

%% @doc The wire form of a realm member endorsement for station Name, signed by SigningKey.
endorse(Net, SigningKey, Realm, Name) ->
    endorsement(Realm, SigningKey, node_id_of(Net, Name)).

%% @doc JoinerName sends SeedName a JOIN for Realm carrying Endorsement, and the fleet settles.
join(#{stations := Map} = Net, JoinerName, SeedName, Realm, Endorsement) ->
    #station{pid = Joiner} = maps:get(JoinerName, Map),
    ok = call_station(Joiner, {send_join, Realm, node_id_of(Net, SeedName), Endorsement}),
    settle(Net).

active_view(#{stations := Map}, Name, Realm) ->
    #station{pid = Pid} = maps:get(Name, Map),
    call_station(Pid, {active_view, Realm}).

%%=====================================================================
%% Settling: no frame in flight
%%=====================================================================

%% Every frame goes through the router, which delivers in order, so once the router and then every station have
%% answered a call, every frame sent before that has been handled. A round in which no station handled a frame means
%% nothing is left in flight.
settle(#{router := Router, stations := Map} = Net) ->
    Seen = fun() ->
               ok = call(Router, sync),
               lists:sum([call_station(Pid, seen) || #station{pid = Pid} <- maps:values(Map)])
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
        {frame, From, Bytes} ->
            station_loop(handle_frame(From, Bytes, State));
        {call, Caller, Ref, Request} ->
            {Reply, State1} = handle_call(Request, State),
            Caller ! {Ref, Reply},
            station_loop(State1)
    end.

handle_call({send_join, Realm, Seed, Endorsement}, #{node_id := Self} = State) ->
    send(State, Seed, macula_hyparview_endorsement:build_join(Realm, Self, Endorsement)),
    {ok, State};
handle_call({active_view, Realm}, #{realms := Realms} = State) ->
    #{view := View} = maps:get(Realm, Realms),
    {macula_hyparview_view:active(View), State};
handle_call(seen, #{seen := Seen} = State) ->
    {Seen, State}.

handle_frame(From, Bytes, #{realms := Realms, seen := Seen} = State) ->
    {ok, #{realm := Realm} = Frame, <<>>} = macula_frame:decode(Bytes),
    #{view := View, ctx := Ctx} = RealmState = maps:get(Realm, Realms),
    {View1, Actions} = macula_hyparview_proto:process(View, From, Frame, Ctx#{now := erlang:system_time(millisecond)}),
    [send(State, Peer, Out) || {send, Peer, Out} <- Actions],
    State#{realms := Realms#{Realm := RealmState#{view := View1}}, seen := Seen + 1}.

send(#{router := Router, node_id := Self}, To, Frame) ->
    Router ! {route, Self, To, macula_frame:encode(Frame)},
    ok.

%%=====================================================================
%% Router
%%=====================================================================

router_loop(Table) ->
    receive
        {table, Table1} ->
            router_loop(Table1);
        {route, From, To, Bytes} ->
            deliver(maps:find(To, Table), From, Bytes),
            router_loop(Table);
        {call, Caller, Ref, sync} ->
            Caller ! {Ref, ok},
            router_loop(Table);
        stop ->
            ok
    end.

deliver({ok, Pid}, From, Bytes) -> Pid ! {frame, From, Bytes};
deliver(error, _From, _Bytes) -> ok.

%%=====================================================================
%% Internals
%%=====================================================================

endorsement(Realm, SigningKey, Member) ->
    Unsigned = macula_record:realm_member_endorsement(Realm, #{realm => Realm, member_node => Member,
                                                               roles => [<<"peer">>]}),
    macula_record:encode(macula_record:sign(Unsigned, SigningKey)).

call_station(Pid, Request) ->
    call(Pid, Request).

call(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {call, self(), Ref, Request},
    receive {Ref, Reply} -> Reply
    after 5000 -> erlang:error({no_reply, Request})
    end.
