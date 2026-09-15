%% EUnit tests for hecate_pubsub_registry.
%%
%% Phase 2 multi-identity refactor: registry is anonymous; tests
%% spawn one per fixture and pass the pid through every API call.
%% pubsub_servers are spawn-linked by the registry directly (the
%% old `hecate_pubsub_server_sup' is gone).
-module(hecate_pubsub_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%---------------------------------------------------------------------
%% Helpers
%%---------------------------------------------------------------------

realm()   -> crypto:strong_rand_bytes(32).
id(N)     -> <<N:256>>.
keypair() -> macula_identity:generate().

setup() ->
    process_flag(trap_exit, true),
    {ok, Reg} = hecate_pubsub_registry:start_link(#{}),
    unlink(Reg),
    Reg.

cleanup(Reg) ->
    case is_process_alive(Reg) of
        true  -> catch hecate_pubsub_registry:stop(Reg), ok;
        false -> ok
    end.

%%---------------------------------------------------------------------
%% Generator
%%---------------------------------------------------------------------

registry_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun(Reg) -> ?_test(register_creates_server(Reg)) end,
         fun(Reg) -> ?_test(register_idempotent_returns_same_pid(Reg)) end,
         fun(Reg) -> ?_test(register_after_child_death_yields_fresh_pid(Reg)) end,
         fun(Reg) -> ?_test(lookup_unknown_realm_returns_not_found(Reg)) end,
         fun(Reg) -> ?_test(lookup_after_register_returns_pid(Reg)) end,
         fun(Reg) -> ?_test(child_death_clears_map(Reg)) end,
         fun(Reg) -> ?_test(dispatch_subscribe_routes_to_server(Reg)) end,
         fun(Reg) -> ?_test(dispatch_event_returns_local_subscribers(Reg)) end,
         fun(Reg) -> ?_test(dispatch_unknown_realm_returns_not_found(Reg)) end,
         fun(Reg) -> ?_test(dispatch_after_child_death_returns_not_found(Reg)) end,
         fun(Reg) -> ?_test(distinct_realms_isolated(Reg)) end,
         fun(Reg) -> ?_test(list_realms_reports_active_realms(Reg)) end,
         fun(Reg) -> ?_test(shutdown_propagates_to_children(Reg)) end,
         fun(Reg) -> ?_test(relay_publish_unknown_realm_is_not_found(Reg)) end,
         fun(Reg) -> ?_test(relay_publish_returns_event_and_subscribers(Reg)) end,
         fun(Reg) -> ?_test(purge_subscriber_clears_a_single_realm(Reg)) end,
         fun(Reg) -> ?_test(purge_subscriber_fans_out_across_realms(Reg)) end,
         fun(Reg) -> ?_test(purge_subscriber_keeps_other_subscribers(Reg)) end,
         fun(Reg) -> ?_test(purge_subscriber_tolerates_no_realms(Reg)) end,
         fun(Reg) -> ?_test(purge_subscriber_tolerates_a_dead_server(Reg)) end
     ]}.

%%---------------------------------------------------------------------
%% Per-identity isolation — top-level (no fixture, two registries)
%%---------------------------------------------------------------------

distinct_registries_isolate_realm_state_test() ->
    process_flag(trap_exit, true),
    R = realm(),
    {ok, RegA} = hecate_pubsub_registry:start_link(#{}),
    {ok, RegB} = hecate_pubsub_registry:start_link(#{}),
    unlink(RegA), unlink(RegB),
    Kp = keypair(),
    {ok, PidA} = hecate_pubsub_registry:register(RegA, R, Kp),
    {ok, PidB} = hecate_pubsub_registry:register(RegB, R, Kp),
    ?assertNotEqual(PidA, PidB),
    %% Subscribe in registry A.
    Sub = id(1),
    ok = hecate_pubsub_server:subscribe(PidA, <<"t">>, Sub),
    ?assertEqual(1, hecate_pubsub_server:subscriber_count(PidA)),
    ?assertEqual(0, hecate_pubsub_server:subscriber_count(PidB)),
    catch hecate_pubsub_registry:stop(RegA),
    catch hecate_pubsub_registry:stop(RegB).

%%---------------------------------------------------------------------
%% Register / lookup
%%---------------------------------------------------------------------

register_creates_server(Reg) ->
    R  = realm(),
    Kp = keypair(),
    {ok, Pid} = hecate_pubsub_registry:register(Reg, R, Kp),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    ?assertEqual(R, hecate_pubsub_server:realm(Pid)).

register_idempotent_returns_same_pid(Reg) ->
    R  = realm(),
    Kp = keypair(),
    {ok, Pid1} = hecate_pubsub_registry:register(Reg, R, Kp),
    {ok, Pid2} = hecate_pubsub_registry:register(Reg, R, Kp),
    ?assertEqual(Pid1, Pid2).

register_after_child_death_yields_fresh_pid(Reg) ->
    R  = realm(),
    Kp = keypair(),
    {ok, Pid1} = hecate_pubsub_registry:register(Reg, R, Kp),
    %% Kill the server abruptly. The EXIT message will reach the
    %% registry; wait for it to be processed by polling lookup.
    exit(Pid1, kill),
    wait_until(fun() ->
        hecate_pubsub_registry:lookup(Reg, R) =:= {error, not_found}
    end, 1000),
    {ok, Pid2} = hecate_pubsub_registry:register(Reg, R, Kp),
    ?assertNotEqual(Pid1, Pid2),
    ?assert(is_process_alive(Pid2)).

lookup_unknown_realm_returns_not_found(Reg) ->
    ?assertEqual({error, not_found},
                 hecate_pubsub_registry:lookup(Reg, realm())).

lookup_after_register_returns_pid(Reg) ->
    R  = realm(),
    Kp = keypair(),
    {ok, Pid} = hecate_pubsub_registry:register(Reg, R, Kp),
    ?assertEqual({ok, Pid}, hecate_pubsub_registry:lookup(Reg, R)).

%%---------------------------------------------------------------------
%% Child lifecycle
%%---------------------------------------------------------------------

child_death_clears_map(Reg) ->
    R  = realm(),
    Kp = keypair(),
    {ok, Pid} = hecate_pubsub_registry:register(Reg, R, Kp),
    exit(Pid, kill),
    wait_until(fun() ->
        hecate_pubsub_registry:lookup(Reg, R) =:= {error, not_found}
    end, 1000),
    ?assertEqual({error, not_found}, hecate_pubsub_registry:lookup(Reg, R)).

%%---------------------------------------------------------------------
%% Dispatch
%%---------------------------------------------------------------------

dispatch_subscribe_routes_to_server(Reg) ->
    R     = realm(),
    Kp    = keypair(),
    SubKp = keypair(),
    SubId = macula_identity:public(SubKp),
    {ok, Pid} = hecate_pubsub_registry:register(Reg, R, Kp),

    Frame = macula_frame:sign(macula_frame:subscribe(#{
        topic      => <<"news">>,
        realm      => R,
        subscriber => SubId
    }), SubKp),

    {ok, Subs} = hecate_pubsub_registry:dispatch_frame(Reg, R, SubId, Frame),
    ?assertEqual([], Subs),
    ?assert(hecate_pubsub_server:is_subscribed(Pid, <<"news">>, SubId)).

dispatch_event_returns_local_subscribers(Reg) ->
    R     = realm(),
    Kp    = keypair(),
    SubKp = keypair(),
    SubId = macula_identity:public(SubKp),
    {ok, _Pid} = hecate_pubsub_registry:register(Reg, R, Kp),

    %% Subscribe via the registry's dispatch path.
    SubF = macula_frame:sign(macula_frame:subscribe(#{
        topic      => <<"news">>,
        realm      => R,
        subscriber => SubId
    }), SubKp),
    {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R, SubId, SubF),

    %% Now an inbound EVENT must match the local subscriber.
    EventF = macula_frame:sign(macula_frame:event(#{
        topic         => <<"news">>,
        realm         => R,
        publisher     => macula_identity:public(Kp),
        seq           => 1,
        payload       => <<"hello">>,
        delivered_via => plumtree
    }), Kp),

    {ok, Matched} = hecate_pubsub_registry:dispatch_frame(
                      Reg, R, macula_identity:public(Kp), EventF),
    ?assertEqual([SubId], Matched).

dispatch_unknown_realm_returns_not_found(Reg) ->
    R   = realm(),
    Kp  = keypair(),
    Pub = macula_identity:public(Kp),
    Frame = macula_frame:sign(macula_frame:subscribe(#{
        topic      => <<"x">>,
        realm      => R,
        subscriber => Pub
    }), Kp),
    ?assertEqual({error, not_found},
                 hecate_pubsub_registry:dispatch_frame(Reg, R, Pub, Frame)).

dispatch_after_child_death_returns_not_found(Reg) ->
    R     = realm(),
    Kp    = keypair(),
    {ok, Pid} = hecate_pubsub_registry:register(Reg, R, Kp),
    exit(Pid, kill),
    wait_until(fun() ->
        hecate_pubsub_registry:lookup(Reg, R) =:= {error, not_found}
    end, 1000),
    Pub = macula_identity:public(Kp),
    Frame = macula_frame:sign(macula_frame:subscribe(#{
        topic      => <<"x">>,
        realm      => R,
        subscriber => Pub
    }), Kp),
    ?assertEqual({error, not_found},
                 hecate_pubsub_registry:dispatch_frame(Reg, R, Pub, Frame)).

%%---------------------------------------------------------------------
%% Multi-realm isolation
%%---------------------------------------------------------------------

distinct_realms_isolated(Reg) ->
    R1  = realm(),
    R2  = realm(),
    Kp  = keypair(),
    {ok, P1} = hecate_pubsub_registry:register(Reg, R1, Kp),
    {ok, P2} = hecate_pubsub_registry:register(Reg, R2, Kp),
    ?assertNotEqual(P1, P2),

    ok = hecate_pubsub_server:subscribe(P1, <<"t">>, id(1)),
    ?assertEqual(1, hecate_pubsub_server:subscriber_count(P1)),
    ?assertEqual(0, hecate_pubsub_server:subscriber_count(P2)),

    %% Event tagged with R2 must not match P1's subscribers.
    EventF = macula_frame:sign(macula_frame:event(#{
        topic         => <<"t">>,
        realm         => R2,
        publisher     => macula_identity:public(Kp),
        seq           => 1,
        payload       => <<"x">>,
        delivered_via => plumtree
    }), Kp),
    {ok, Matched} = hecate_pubsub_registry:dispatch_frame(
                      Reg, R2, macula_identity:public(Kp), EventF),
    ?assertEqual([], Matched).

%%---------------------------------------------------------------------
%% list_realms
%%---------------------------------------------------------------------

list_realms_reports_active_realms(Reg) ->
    Kp = keypair(),
    R1 = realm(),
    R2 = realm(),
    ?assertEqual([], hecate_pubsub_registry:list_realms(Reg)),
    {ok, _} = hecate_pubsub_registry:register(Reg, R1, Kp),
    {ok, _} = hecate_pubsub_registry:register(Reg, R2, Kp),
    Got = lists:sort(hecate_pubsub_registry:list_realms(Reg)),
    ?assertEqual(lists:sort([R1, R2]), Got).

%%---------------------------------------------------------------------
%% Shutdown propagation
%%---------------------------------------------------------------------

shutdown_propagates_to_children(Reg) ->
    R1 = realm(),
    R2 = realm(),
    Kp = keypair(),
    {ok, P1} = hecate_pubsub_registry:register(Reg, R1, Kp),
    {ok, P2} = hecate_pubsub_registry:register(Reg, R2, Kp),

    Ref1 = erlang:monitor(process, P1),
    Ref2 = erlang:monitor(process, P2),

    %% Stop the registry. As the linked parent of both pubsub_servers,
    %% its termination cascades down via OTP exit signals.
    ok = hecate_pubsub_registry:stop(Reg),

    receive {'DOWN', Ref1, process, P1, _} -> ok
    after 1000 -> ?assert(false) end,
    receive {'DOWN', Ref2, process, P2, _} -> ok
    after 1000 -> ?assert(false) end,

    ?assertNot(is_process_alive(P1)),
    ?assertNot(is_process_alive(P2)).

%%---------------------------------------------------------------------
%% Inbound PUBLISH relay (Phase 1 of PLAN_V2_PARITY)
%%---------------------------------------------------------------------

relay_publish_unknown_realm_is_not_found(Reg) ->
    R   = realm(),
    Kp  = keypair(),
    Pub = macula_identity:public(Kp),
    Frame = macula_frame:sign(macula_frame:publish(#{
        topic           => <<"x">>,
        realm           => R,
        publisher       => Pub,
        seq             => 0,
        payload         => <<"hi">>,
        published_at_ms => erlang:system_time(millisecond)
    }), Kp),
    %% No server registered for R and no default_identity in fixture
    %% opts (`#{}`) — strict not_found semantics.
    ?assertEqual({error, not_found},
                 hecate_pubsub_registry:relay_publish(Reg, R, Frame)).

relay_publish_returns_event_and_subscribers(Reg) ->
    R       = realm(),
    Station = keypair(),
    %% Daemon publishing the event.
    DaemonKp = keypair(),
    DaemonId = macula_identity:public(DaemonKp),
    %% A local subscriber.
    SubId    = id(7),
    %% Materialise the realm with the station's identity.
    {ok, Server} = hecate_pubsub_registry:register(Reg, R, Station),
    %% Subscribe locally for <<"weather.measured_v1">>.
    ok = hecate_pubsub_server:subscribe(Server,
                                         <<"weather.measured_v1">>, SubId),
    %% Daemon publishes a PUBLISH frame.
    PublishFrame = macula_frame:sign(macula_frame:publish(#{
        topic           => <<"weather.measured_v1">>,
        realm           => R,
        publisher       => DaemonId,
        seq             => 42,
        payload         => #{temp => 20},
        published_at_ms => erlang:system_time(millisecond)
    }), DaemonKp),
    {ok, EventFrame, Matched} =
        hecate_pubsub_registry:relay_publish(Reg, R, PublishFrame),
    %% EVENT preserves the daemon's publisher pubkey + seq so pool
    %% dedup keys (realm, publisher, seq) line up across stations.
    ?assertEqual(event,                macula_frame:frame_type(EventFrame)),
    ?assertEqual(<<"weather.measured_v1">>, maps:get(topic, EventFrame)),
    ?assertEqual(R,                    maps:get(realm, EventFrame)),
    ?assertEqual(DaemonId,             maps:get(publisher, EventFrame)),
    ?assertEqual(42,                   maps:get(seq, EventFrame)),
    ?assertEqual(#{temp => 20},        maps:get(payload, EventFrame)),
    ?assertEqual(direct,               maps:get(delivered_via, EventFrame)),
    %% Wire signature is the station's, not the daemon's.
    ?assertEqual({ok, EventFrame},
                 macula_frame:verify(EventFrame,
                                     macula_identity:public(Station))),
    %% Local sub matched.
    ?assertEqual([SubId], Matched).

%%---------------------------------------------------------------------
%% purge_subscriber
%%---------------------------------------------------------------------

purge_subscriber_clears_a_single_realm(Reg) ->
    R  = realm(),
    Kp = keypair(),
    Sub = id(1),
    {ok, Server} = hecate_pubsub_registry:register(Reg, R, Kp),
    ok = hecate_pubsub_server:subscribe(Server, <<"t">>, Sub),
    ok = hecate_pubsub_registry:purge_subscriber(Reg, Sub),
    ?assertEqual(0, hecate_pubsub_server:topic_count(Server)).

%% A departed peer or daemon has no notion of "which realm" it was
%% subscribed under — this is the whole reason `purge_subscriber/2'
%% lives on the registry rather than requiring the caller to know.
purge_subscriber_fans_out_across_realms(Reg) ->
    R1 = realm(),
    R2 = realm(),
    Kp = keypair(),
    Sub = id(1),
    {ok, S1} = hecate_pubsub_registry:register(Reg, R1, Kp),
    {ok, S2} = hecate_pubsub_registry:register(Reg, R2, Kp),
    ok = hecate_pubsub_server:subscribe(S1, <<"a">>, Sub),
    ok = hecate_pubsub_server:subscribe(S2, <<"b">>, Sub),
    ok = hecate_pubsub_registry:purge_subscriber(Reg, Sub),
    ?assertEqual(0, hecate_pubsub_server:topic_count(S1)),
    ?assertEqual(0, hecate_pubsub_server:topic_count(S2)).

purge_subscriber_keeps_other_subscribers(Reg) ->
    R  = realm(),
    Kp = keypair(),
    {ok, Server} = hecate_pubsub_registry:register(Reg, R, Kp),
    ok = hecate_pubsub_server:subscribe(Server, <<"t">>, id(1)),
    ok = hecate_pubsub_server:subscribe(Server, <<"t">>, id(2)),
    ok = hecate_pubsub_registry:purge_subscriber(Reg, id(1)),
    ?assertEqual([id(2)], hecate_pubsub_server:subscribers(Server, <<"t">>)).

purge_subscriber_tolerates_no_realms(Reg) ->
    ?assertEqual(ok, hecate_pubsub_registry:purge_subscriber(Reg, id(1))).

%% A server dying between `list_realms'-time bookkeeping and this
%% call's fan-out is a race, not an error: the registry's own `EXIT'
%% handling clears the stale entry independently. purge_subscriber
%% must not crash the registry when it hits a dead pid.
purge_subscriber_tolerates_a_dead_server(Reg) ->
    R  = realm(),
    Kp = keypair(),
    {ok, Pid} = hecate_pubsub_registry:register(Reg, R, Kp),
    exit(Pid, kill),
    wait_until(fun() -> not is_process_alive(Pid) end, 1000),
    ?assertEqual(ok, hecate_pubsub_registry:purge_subscriber(Reg, id(1))),
    ?assert(is_process_alive(Reg)).

%%---------------------------------------------------------------------
%% A realm's server lives while a subscription holds it
%%---------------------------------------------------------------------

%% A PUBLISH relayed for a realm with no server still gets its EVENT, signed
%% by the station, for fan-out to peer stations, and starts no process.
relay_publish_to_a_realm_without_a_server_starts_no_process_test() ->
    with_station_registry(fun(Reg, Station) ->
        R        = realm(),
        DaemonKp = keypair(),
        Links    = registry_links(Reg),
        {ok, EventFrame, Matched} =
            hecate_pubsub_registry:relay_publish(Reg, R, publish_frame(R, DaemonKp, 1)),
        ?assertEqual(event, macula_frame:frame_type(EventFrame)),
        ?assertEqual(macula_identity:public(DaemonKp), maps:get(publisher, EventFrame)),
        ?assertEqual(1, maps:get(seq, EventFrame)),
        ?assertEqual({ok, EventFrame},
                     macula_frame:verify(EventFrame, macula_identity:public(Station))),
        ?assertEqual([], Matched),
        ?assertEqual({error, not_found}, hecate_pubsub_registry:lookup(Reg, R)),
        ?assertEqual(Links, registry_links(Reg))
    end).

%% A PUBLISH relayed for a realm with no server is refused when the frame
%% names another realm: no EVENT is built for it, and no process starts.
relay_publish_of_a_frame_naming_another_realm_is_refused_test() ->
    with_station_registry(fun(Reg, _Station) ->
        [R, Other] = [realm(), realm()],
        Links = registry_links(Reg),
        ?assertEqual({error, realm_mismatch},
                     hecate_pubsub_registry:relay_publish(Reg, R, publish_frame(Other, keypair(), 1))),
        ?assertEqual({error, not_found}, hecate_pubsub_registry:lookup(Reg, R)),
        ?assertEqual(Links, registry_links(Reg))
    end).

%% Only a SUBSCRIBE materialises a realm. An UNSUBSCRIBE or an EVENT for a
%% realm with no server starts none.
a_frame_other_than_subscribe_starts_no_server_test() ->
    with_station_registry(fun(Reg, _Station) ->
        R     = realm(),
        SubKp = keypair(),
        SubId = macula_identity:public(SubKp),
        Links = registry_links(Reg),
        ?assertEqual({ok, []}, hecate_pubsub_registry:dispatch_frame(
                                 Reg, R, SubId, unsubscribe_frame(R, SubKp, <<"news">>))),
        ?assertEqual({ok, []}, hecate_pubsub_registry:dispatch_frame(
                                 Reg, R, SubId, event_frame(R, SubKp, <<"news">>))),
        ?assertEqual({error, not_found}, hecate_pubsub_registry:lookup(Reg, R)),
        ?assertEqual(Links, registry_links(Reg))
    end).

%% A realm a SUBSCRIBE materialised stops when its last subscription leaves,
%% and not before.
the_last_unsubscribe_stops_the_realms_server_test() ->
    with_station_registry(fun(Reg, _Station) ->
        R     = realm(),
        SubKp = keypair(),
        SubId = macula_identity:public(SubKp),
        [{ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R, SubId, subscribe_frame(R, SubKp, T))
         || T <- [<<"news">>, <<"sport">>]],
        {ok, Server} = hecate_pubsub_registry:lookup(Reg, R),
        Ref = erlang:monitor(process, Server),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R, SubId, unsubscribe_frame(R, SubKp, <<"news">>)),
        ?assertEqual({ok, Server}, hecate_pubsub_registry:lookup(Reg, R)),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R, SubId, unsubscribe_frame(R, SubKp, <<"sport">>)),
        ?assertEqual({error, not_found}, hecate_pubsub_registry:lookup(Reg, R)),
        ?assertEqual(down, down_within(Ref, 1_000)),
        ?assertEqual([], hecate_pubsub_registry:list_realms(Reg))
    end).

%% A subscriber that sends the same SUBSCRIBE twice holds one subscription, as
%% a released macula-ts client does: one UNSUBSCRIBE leaves no entry behind,
%% and the realm's server stops.
a_repeated_subscribe_is_one_subscription_test() ->
    with_station_registry(fun(Reg, _Station) ->
        R     = realm(),
        SubKp = keypair(),
        SubId = macula_identity:public(SubKp),
        [{ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R, SubId, subscribe_frame(R, SubKp, <<"news">>))
         || _ <- [first, repeated]],
        {ok, Server} = hecate_pubsub_registry:lookup(Reg, R),
        ?assertEqual(1, hecate_pubsub_server:subscriber_count(Server)),
        Ref = erlang:monitor(process, Server),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R, SubId, unsubscribe_frame(R, SubKp, <<"news">>)),
        ?assertEqual({error, not_found}, hecate_pubsub_registry:lookup(Reg, R)),
        ?assertEqual(down, down_within(Ref, 1_000))
    end).

%% A purge that takes a realm's last subscription stops that realm's server;
%% a realm another subscriber still holds stays.
a_purge_that_empties_a_realm_stops_its_server_test() ->
    with_station_registry(fun(Reg, _Station) ->
        [R1, R2] = [realm(), realm()],
        GoneKp   = keypair(),
        StayKp   = keypair(),
        Gone     = macula_identity:public(GoneKp),
        Stay     = macula_identity:public(StayKp),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R1, Gone, subscribe_frame(R1, GoneKp, <<"a">>)),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R2, Gone, subscribe_frame(R2, GoneKp, <<"b">>)),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R2, Stay, subscribe_frame(R2, StayKp, <<"b">>)),
        {ok, S1} = hecate_pubsub_registry:lookup(Reg, R1),
        {ok, S2} = hecate_pubsub_registry:lookup(Reg, R2),
        Ref1 = erlang:monitor(process, S1),
        ok = hecate_pubsub_registry:purge_subscriber(Reg, Gone),
        ?assertEqual({error, not_found}, hecate_pubsub_registry:lookup(Reg, R1)),
        ?assertEqual(down, down_within(Ref1, 1_000)),
        ?assertEqual({ok, S2}, hecate_pubsub_registry:lookup(Reg, R2)),
        ?assertEqual([Stay], hecate_pubsub_server:subscribers(S2, <<"b">>))
    end).

%% A realm the station registered itself stays when its last subscription
%% leaves: the station publishes on it.
a_registered_realm_stays_when_its_last_subscription_leaves_test() ->
    with_station_registry(fun(Reg, Station) ->
        R     = realm(),
        SubKp = keypair(),
        SubId = macula_identity:public(SubKp),
        {ok, Server} = hecate_pubsub_registry:register(Reg, R, Station),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R, SubId, subscribe_frame(R, SubKp, <<"news">>)),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R, SubId, unsubscribe_frame(R, SubKp, <<"news">>)),
        ?assertEqual({ok, Server}, hecate_pubsub_registry:lookup(Reg, R)),
        ?assert(is_process_alive(Server))
    end).

%% A SUBSCRIBE that would materialise a realm past the registry's maximum is
%% refused and starts no server. Realms the station registered do not count,
%% and a realm that stopped frees its place.
a_subscribe_past_the_realm_maximum_is_refused_test() ->
    with_station_registry(#{max_subscribed_realms => 2}, fun(Reg, Station) ->
        [Pinned, R1, R2, R3] = [realm(), realm(), realm(), realm()],
        SubKp = keypair(),
        SubId = macula_identity:public(SubKp),
        {ok, _} = hecate_pubsub_registry:register(Reg, Pinned, Station),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R1, SubId, subscribe_frame(R1, SubKp, <<"t">>)),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R2, SubId, subscribe_frame(R2, SubKp, <<"t">>)),
        Links = registry_links(Reg),
        ?assertEqual({error, too_many_realms},
                     hecate_pubsub_registry:dispatch_frame(Reg, R3, SubId, subscribe_frame(R3, SubKp, <<"t">>))),
        ?assertEqual({error, not_found}, hecate_pubsub_registry:lookup(Reg, R3)),
        ?assertEqual(Links, registry_links(Reg)),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R1, SubId, unsubscribe_frame(R1, SubKp, <<"t">>)),
        ?assertEqual({ok, []},
                     hecate_pubsub_registry:dispatch_frame(Reg, R3, SubId, subscribe_frame(R3, SubKp, <<"t">>)))
    end).

%% Registering a realm a SUBSCRIBE materialised pins it: it stays when its
%% last subscription leaves, and it no longer counts towards the maximum.
registering_a_subscribed_realm_pins_it_test() ->
    with_station_registry(#{max_subscribed_realms => 1}, fun(Reg, Station) ->
        [R, Other] = [realm(), realm()],
        SubKp = keypair(),
        SubId = macula_identity:public(SubKp),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R, SubId, subscribe_frame(R, SubKp, <<"news">>)),
        {ok, Server} = hecate_pubsub_registry:lookup(Reg, R),
        ?assertEqual({ok, Server}, hecate_pubsub_registry:register(Reg, R, Station)),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R, SubId, unsubscribe_frame(R, SubKp, <<"news">>)),
        ?assertEqual({ok, Server}, hecate_pubsub_registry:lookup(Reg, R)),
        ?assertEqual({ok, []},
                     hecate_pubsub_registry:dispatch_frame(Reg, Other, SubId, subscribe_frame(Other, SubKp, <<"news">>)))
    end).

%% A realm that holds a pattern subscription is never reaped: when its last
%% exact-topic subscription leaves, by UNSUBSCRIBE or by purge, its server
%% stays, and the pattern subscriber still gets a matching EVENT.
a_pattern_subscription_keeps_its_realm_test_() ->
    [{Name, fun() -> pattern_subscription_keeps_its_realm(How) end}
     || {Name, How} <- [{"the exact subscriber unsubscribes", unsubscribe},
                        {"the exact subscriber is purged", purge}]].

pattern_subscription_keeps_its_realm(How) ->
    with_station_registry(fun(Reg, _Station) ->
        R         = realm(),
        PatternKp = keypair(),
        ExactKp   = keypair(),
        PatternId = macula_identity:public(PatternKp),
        ExactId   = macula_identity:public(ExactKp),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R, PatternId,
                                                         subscribe_frame(R, PatternKp, <<"*/svc.do">>)),
        {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R, ExactId, subscribe_frame(R, ExactKp, <<"news">>)),
        {ok, Server} = hecate_pubsub_registry:lookup(Reg, R),
        ok = end_exact_subscription(How, Reg, R, ExactKp),
        ?assertEqual({ok, Server}, hecate_pubsub_registry:lookup(Reg, R)),
        ?assertEqual({ok, [PatternId]},
                     hecate_pubsub_registry:dispatch_frame(Reg, R, ExactId,
                                                           event_frame(R, ExactKp, <<"shop/svc.do">>)))
    end).

end_exact_subscription(unsubscribe, Reg, R, ExactKp) ->
    {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R, macula_identity:public(ExactKp),
                                                     unsubscribe_frame(R, ExactKp, <<"news">>)),
    ok;
end_exact_subscription(purge, Reg, _R, ExactKp) ->
    hecate_pubsub_registry:purge_subscriber(Reg, macula_identity:public(ExactKp)).

%% A registry that auto-materialises realms with the station's identity, as
%% the station starts it, stopped after Test.
with_station_registry(Test) ->
    with_station_registry(#{}, Test).

with_station_registry(Opts, Test) ->
    process_flag(trap_exit, true),
    Station = keypair(),
    {ok, Reg} = hecate_pubsub_registry:start_link(Opts#{identity => Station}),
    unlink(Reg),
    try Test(Reg, Station) after catch hecate_pubsub_registry:stop(Reg) end.

%% The processes the registry is linked to: its pubsub_servers.
registry_links(Reg) ->
    {links, Links} = erlang:process_info(Reg, links),
    lists:sort(Links).

subscribe_frame(R, Kp, Topic) ->
    macula_frame:sign(macula_frame:subscribe(#{topic => Topic, realm => R,
                                               subscriber => macula_identity:public(Kp)}), Kp).

unsubscribe_frame(R, Kp, Topic) ->
    macula_frame:sign(macula_frame:unsubscribe(#{topic => Topic, realm => R,
                                                 subscriber => macula_identity:public(Kp)}), Kp).

event_frame(R, Kp, Topic) ->
    macula_frame:sign(macula_frame:event(#{topic => Topic, realm => R,
                                           publisher => macula_identity:public(Kp), seq => 1,
                                           payload => <<"hello">>, delivered_via => plumtree}), Kp).

publish_frame(R, Kp, Seq) ->
    macula_frame:sign(macula_frame:publish(#{topic => <<"weather.measured_v1">>, realm => R,
                                             publisher => macula_identity:public(Kp), seq => Seq,
                                             payload => <<"hi">>,
                                             published_at_ms => erlang:system_time(millisecond)}), Kp).

down_within(Ref, Ms) ->
    receive
        {'DOWN', Ref, process, _Pid, _Reason} -> down
    after Ms ->
        alive
    end.

%%---------------------------------------------------------------------
%% Polling helper
%%---------------------------------------------------------------------

wait_until(_Pred, Budget) when Budget =< 0 ->
    erlang:error(wait_until_timeout);
wait_until(Pred, Budget) ->
    case Pred() of
        true  -> ok;
        false ->
            timer:sleep(10),
            wait_until(Pred, Budget - 10)
    end.
