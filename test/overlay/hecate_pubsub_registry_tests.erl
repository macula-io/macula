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
         fun(Reg) -> ?_test(relay_publish_returns_event_and_subscribers(Reg)) end
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

%% Production path: a registry started with `default_identity' auto-
%% registers a pubsub_server on relay_publish for an unknown realm so
%% the EVENT frame is built even with zero local subscribers.
%% Downstream bloom-fan in the dispatcher needs the EVENT regardless
%% of local interest.
relay_publish_auto_registers_when_default_identity_set_test() ->
    process_flag(trap_exit, true),
    Station = keypair(),
    {ok, Reg} = hecate_pubsub_registry:start_link(#{identity => Station}),
    unlink(Reg),
    R        = realm(),
    DaemonKp = keypair(),
    DaemonId = macula_identity:public(DaemonKp),
    PublishFrame = macula_frame:sign(macula_frame:publish(#{
        topic           => <<"weather.measured_v1">>,
        realm           => R,
        publisher       => DaemonId,
        seq             => 1,
        payload         => <<"hi">>,
        published_at_ms => erlang:system_time(millisecond)
    }), DaemonKp),
    {ok, EventFrame, Matched} =
        hecate_pubsub_registry:relay_publish(Reg, R, PublishFrame),
    ?assertEqual(event, macula_frame:frame_type(EventFrame)),
    ?assertEqual(<<"weather.measured_v1">>, maps:get(topic, EventFrame)),
    ?assertEqual(DaemonId, maps:get(publisher, EventFrame)),
    ?assertEqual([], Matched),  % zero local subs — bloom-fan extras live in dispatcher
    %% Realm now materialised.
    ?assertMatch({ok, _Pid}, hecate_pubsub_registry:lookup(Reg, R)),
    catch hecate_pubsub_registry:stop(Reg).

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
