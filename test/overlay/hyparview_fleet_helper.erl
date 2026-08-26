%% @doc CT helper for the HyParView acceptance suite.
%%
%% Each station is a process that holds, per realm, a
%% `macula_hyparview_view' (active + passive partial view). Stations
%% share an in-VM router that delivers frames between station
%% inboxes. On receiving a `hyparview_*' frame the station dispatches
%% to `macula_hyparview_proto:process/4', which emits outbound frames
%% via the router and mutates the view.
%%
%% `join/5' drives the realm-join handshake: the joiner signs a JOIN
%% frame, attaches the admin-signed endorsement, sends it to a seed;
%% the seed verifies via `macula_hyparview_endorsement:
%% verify_endorsement/3' and admits the joiner into its active view.
%% Non-endorsed or bogus-endorsed joiners are silently dropped.
-module(hyparview_fleet_helper).

-export([
    start_fleet/3,
    stop_fleet/1,
    endorse/4,
    join/5,
    active_view/3,
    pubkey_of/2
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

%% @doc Build a fleet of stations. The realm id IS the admin pubkey
%% in our model — same convention the SDK constructors use.
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
            [{R, #{view => macula_hyparview_view:new(Pub)}} || R <- Realms]),
    #{
        name   => Name,
        kp     => Kp,
        pubkey => Pub,
        router => Router,
        realms => Per
    }.

%%=====================================================================
%% Public helpers
%%=====================================================================

pubkey_of(#{stations := Map}, Name) ->
    (maps:get(Name, Map))#station.pubkey.

endorse(#{stations := Map}, Admin, Realm, Name) ->
    #station{pubkey = Pub} = maps:get(Name, Map),
    R = macula_record:realm_member_endorsement(
          Realm,
          #{realm => Realm, member_node => Pub, roles => [<<"peer">>]}),
    macula_record:sign(R, Admin).

active_view(#{stations := Map} = _Net, Name, Realm) ->
    #station{pid = Pid} = maps:get(Name, Map),
    call_station(Pid, {active_view, Realm}).

%% @doc Run the admission handshake: `JoinerName' sends a signed
%% `hyparview_join' frame to `SeedName' bundled with `Endorsement'
%% (admin-signed member endorsement). Seed verifies via
%% `macula_hyparview_endorsement' and either admits or drops.
join(#{stations := Map} = _Net, JoinerName, SeedName, Realm, Endorsement) ->
    #station{pid = JPid}    = maps:get(JoinerName, Map),
    #station{pubkey = SPub} = maps:get(SeedName,   Map),
    call_station(JPid, {send_join, Realm, SPub, Endorsement}),
    %% Handshake is a chain of one-shot messages; give the router a
    %% brief window to fan out NEIGHBOR(high) replies.
    timer:sleep(20),
    ok.

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

control({send_join, Realm, SeedPub, Endorsement}, State) ->
    JKp  = maps:get(kp, State),
    JPub = maps:get(pubkey, State),
    Frame0 = macula_frame:hyparview_join(
               #{realm => Realm, new_member => JPub}),
    %% Test-only side channel for the endorsement so the seed can
    %% verify it — production wire format carries it in the JOIN
    %% frame's own `record' field (see `macula_hyparview_endorsement:
    %% build_join/4').
    Frame = macula_frame:sign(Frame0, JKp),
    Env   = #{frame => Frame, endorsement => Endorsement,
              realm => Realm, joiner => JPub},
    route(State, SeedPub, {join_envelope, Env}),
    {ok, State};

control({active_view, Realm}, State) ->
    RS = realm_state(State, Realm),
    {macula_hyparview_view:active(maps:get(view, RS)), State}.

%%---------------------------------------------------------------------
%% Frame dispatch
%%---------------------------------------------------------------------

dispatch_frame({join_envelope, #{frame := Frame, endorsement := End,
                                 realm := Realm, joiner := Joiner}}, State) ->
    handle_join_envelope(Frame, End, Realm, Joiner, State);
dispatch_frame(#{frame_type := FT} = Frame, State) ->
    case FT of
        hyparview_join          -> dispatch_overlay(Frame, State);
        hyparview_forward_join  -> dispatch_overlay(Frame, State);
        hyparview_neighbor      -> dispatch_overlay(Frame, State);
        hyparview_disconnect    -> dispatch_overlay(Frame, State);
        hyparview_shuffle       -> dispatch_overlay(Frame, State);
        hyparview_shuffle_reply -> dispatch_overlay(Frame, State);
        _                       -> State
    end.

handle_join_envelope(Frame, Endorsement, Realm, Joiner, State) ->
    case macula_hyparview_endorsement:verify_endorsement(Endorsement, Realm, Joiner) of
        {ok, _Roles} ->
            admit_joiner(Frame, Realm, Joiner, State);
        {error, _} ->
            State   %% silently drop
    end.

admit_joiner(_Frame, Realm, Joiner, State) ->
    %% Admit joiner into active view and reply NEIGHBOR(high).
    State1 = update_realm(State, Realm, fun(#{view := V} = RS) ->
        RS#{view := macula_hyparview_view:add_active(V, Joiner)}
    end),
    Reply = macula_frame:sign(macula_frame:hyparview_neighbor(
                                #{realm => Realm, priority => high}),
                              maps:get(kp, State1)),
    route(State1, Joiner, Reply),
    State1.

%%---------------------------------------------------------------------
%% Overlay dispatch
%%---------------------------------------------------------------------

dispatch_overlay(#{realm := Realm} = Frame, State) ->
    RS = realm_state(State, Realm),
    Ctx = #{
        self_id  => maps:get(pubkey, State),
        identity => maps:get(kp, State),
        arwl     => 6, prwl => 3,
        shuffle_ttl => 4
    },
    From = maps:get(new_member, Frame, undefined),
    {NewView, Actions} = macula_hyparview_proto:process(
                           maps:get(view, RS), From, Frame, Ctx),
    apply_overlay_actions(Actions, State,
                          fun(ST) ->
                              update_realm(ST, Realm,
                                           fun(R) -> R#{view := NewView} end)
                          end).

apply_overlay_actions([], State, Finalize) ->
    Finalize(State);
apply_overlay_actions([{send, Peer, Frame} | Rest], State, Finalize) ->
    route(State, Peer, Frame),
    apply_overlay_actions(Rest, State, Finalize);
apply_overlay_actions([_ | Rest], State, Finalize) ->
    apply_overlay_actions(Rest, State, Finalize).

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
