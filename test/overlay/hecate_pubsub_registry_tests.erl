%% EUnit tests for hecate_pubsub_registry.
%%
%% Phase 2 multi-identity refactor: registry is anonymous; tests spawn one per fixture and pass the pid through every
%% API call. pubsub_servers are spawn-linked by the registry directly (the old `hecate_pubsub_server_sup' is gone).
%% Every server signs with a node identity key in the node's configured crypto profile.
-module(hecate_pubsub_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%---------------------------------------------------------------------
%% Helpers
%%---------------------------------------------------------------------

realm() -> crypto:strong_rand_bytes(32).
id(N)   -> <<N:256>>.

key() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    Key.

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
         fun(Reg) -> ?_test(relay_publish_passes_a_refusal_through(Reg)) end,
         fun(Reg) -> ?_test(purge_subscriber_clears_a_single_realm(Reg)) end,
         fun(Reg) -> ?_test(purge_subscriber_fans_out_across_realms(Reg)) end,
         fun(Reg) -> ?_test(purge_subscriber_keeps_other_subscribers(Reg)) end,
         fun(Reg) -> ?_test(purge_subscriber_tolerates_no_realms(Reg)) end,
         fun(Reg) -> ?_test(purge_subscriber_tolerates_a_dead_server(Reg)) end
     ]}.

%%---------------------------------------------------------------------
%% Per-identity isolation: top-level (no fixture, two registries)
%%---------------------------------------------------------------------

distinct_registries_isolate_realm_state_test() ->
    process_flag(trap_exit, true),
    R = realm(),
    {ok, RegA} = hecate_pubsub_registry:start_link(#{}),
    {ok, RegB} = hecate_pubsub_registry:start_link(#{}),
    unlink(RegA), unlink(RegB),
    Key = key(),
    {ok, PidA} = hecate_pubsub_registry:register(RegA, R, Key),
    {ok, PidB} = hecate_pubsub_registry:register(RegB, R, Key),
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
    R = realm(),
    {ok, Pid} = hecate_pubsub_registry:register(Reg, R, key()),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    ?assertEqual(R, hecate_pubsub_server:realm(Pid)).

register_idempotent_returns_same_pid(Reg) ->
    R = realm(),
    Key = key(),
    {ok, Pid1} = hecate_pubsub_registry:register(Reg, R, Key),
    {ok, Pid2} = hecate_pubsub_registry:register(Reg, R, Key),
    ?assertEqual(Pid1, Pid2).

register_after_child_death_yields_fresh_pid(Reg) ->
    R = realm(),
    Key = key(),
    {ok, Pid1} = hecate_pubsub_registry:register(Reg, R, Key),
    %% Kill the server abruptly. The EXIT message reaches the registry; wait for it to be processed by polling lookup.
    exit(Pid1, kill),
    wait_until(fun() -> hecate_pubsub_registry:lookup(Reg, R) =:= {error, not_found} end, 1000),
    {ok, Pid2} = hecate_pubsub_registry:register(Reg, R, Key),
    ?assertNotEqual(Pid1, Pid2),
    ?assert(is_process_alive(Pid2)).

lookup_unknown_realm_returns_not_found(Reg) ->
    ?assertEqual({error, not_found}, hecate_pubsub_registry:lookup(Reg, realm())).

lookup_after_register_returns_pid(Reg) ->
    R = realm(),
    {ok, Pid} = hecate_pubsub_registry:register(Reg, R, key()),
    ?assertEqual({ok, Pid}, hecate_pubsub_registry:lookup(Reg, R)).

%%---------------------------------------------------------------------
%% Child lifecycle
%%---------------------------------------------------------------------

child_death_clears_map(Reg) ->
    R = realm(),
    {ok, Pid} = hecate_pubsub_registry:register(Reg, R, key()),
    exit(Pid, kill),
    wait_until(fun() -> hecate_pubsub_registry:lookup(Reg, R) =:= {error, not_found} end, 1000),
    ?assertEqual({error, not_found}, hecate_pubsub_registry:lookup(Reg, R)).

%%---------------------------------------------------------------------
%% Dispatch
%%---------------------------------------------------------------------

dispatch_subscribe_routes_to_server(Reg) ->
    R = realm(),
    SubId = id(5),
    {ok, Pid} = hecate_pubsub_registry:register(Reg, R, key()),
    Frame = subscribe_frame(R, <<"news">>, SubId),
    {ok, Subs} = hecate_pubsub_registry:dispatch_frame(Reg, R, SubId, Frame),
    ?assertEqual([], Subs),
    ?assert(hecate_pubsub_server:is_subscribed(Pid, <<"news">>, SubId)).

dispatch_event_returns_local_subscribers(Reg) ->
    R = realm(),
    SubId = id(5),
    {ok, _Pid} = hecate_pubsub_registry:register(Reg, R, key()),
    %% Subscribe via the registry's dispatch path.
    {ok, []} = hecate_pubsub_registry:dispatch_frame(Reg, R, SubId, subscribe_frame(R, <<"news">>, SubId)),
    %% Now an inbound EVENT must match the local subscriber.
    Publisher = key(),
    EventF = event_frame(R, <<"news">>, Publisher),
    {ok, Matched} = hecate_pubsub_registry:dispatch_frame(Reg, R, macula_node_keys:key_id(Publisher), EventF),
    ?assertEqual([SubId], Matched).

dispatch_unknown_realm_returns_not_found(Reg) ->
    R = realm(),
    Sub = id(6),
    ?assertEqual({error, not_found},
                 hecate_pubsub_registry:dispatch_frame(Reg, R, Sub, subscribe_frame(R, <<"x">>, Sub))).

dispatch_after_child_death_returns_not_found(Reg) ->
    R = realm(),
    {ok, Pid} = hecate_pubsub_registry:register(Reg, R, key()),
    exit(Pid, kill),
    wait_until(fun() -> hecate_pubsub_registry:lookup(Reg, R) =:= {error, not_found} end, 1000),
    Sub = id(6),
    ?assertEqual({error, not_found},
                 hecate_pubsub_registry:dispatch_frame(Reg, R, Sub, subscribe_frame(R, <<"x">>, Sub))).

%%---------------------------------------------------------------------
%% Multi-realm isolation
%%---------------------------------------------------------------------

distinct_realms_isolated(Reg) ->
    R1 = realm(),
    R2 = realm(),
    Key = key(),
    {ok, P1} = hecate_pubsub_registry:register(Reg, R1, Key),
    {ok, P2} = hecate_pubsub_registry:register(Reg, R2, Key),
    ?assertNotEqual(P1, P2),
    ok = hecate_pubsub_server:subscribe(P1, <<"t">>, id(1)),
    ?assertEqual(1, hecate_pubsub_server:subscriber_count(P1)),
    ?assertEqual(0, hecate_pubsub_server:subscriber_count(P2)),
    %% An event for R2 must not match P1's subscribers.
    Publisher = key(),
    {ok, Matched} = hecate_pubsub_registry:dispatch_frame(Reg, R2, macula_node_keys:key_id(Publisher),
                                                          event_frame(R2, <<"t">>, Publisher)),
    ?assertEqual([], Matched).

%%---------------------------------------------------------------------
%% list_realms
%%---------------------------------------------------------------------

list_realms_reports_active_realms(Reg) ->
    Key = key(),
    R1 = realm(),
    R2 = realm(),
    ?assertEqual([], hecate_pubsub_registry:list_realms(Reg)),
    {ok, _} = hecate_pubsub_registry:register(Reg, R1, Key),
    {ok, _} = hecate_pubsub_registry:register(Reg, R2, Key),
    ?assertEqual(lists:sort([R1, R2]), lists:sort(hecate_pubsub_registry:list_realms(Reg))).

%%---------------------------------------------------------------------
%% Shutdown propagation
%%---------------------------------------------------------------------

shutdown_propagates_to_children(Reg) ->
    R1 = realm(),
    R2 = realm(),
    Key = key(),
    {ok, P1} = hecate_pubsub_registry:register(Reg, R1, Key),
    {ok, P2} = hecate_pubsub_registry:register(Reg, R2, Key),
    Ref1 = erlang:monitor(process, P1),
    Ref2 = erlang:monitor(process, P2),
    %% Stop the registry. As the linked parent of both pubsub_servers, its termination cascades via OTP exit signals.
    ok = hecate_pubsub_registry:stop(Reg),
    receive {'DOWN', Ref1, process, P1, _} -> ok after 1000 -> ?assert(false) end,
    receive {'DOWN', Ref2, process, P2, _} -> ok after 1000 -> ?assert(false) end,
    ?assertNot(is_process_alive(P1)),
    ?assertNot(is_process_alive(P2)).

%%---------------------------------------------------------------------
%% Inbound PUBLISH relay (Phase 1 of PLAN_V2_PARITY)
%%---------------------------------------------------------------------

relay_publish_unknown_realm_is_not_found(Reg) ->
    R = realm(),
    %% No server registered for R and no default_identity in fixture opts (`#{}'): strict not_found semantics.
    ?assertEqual({error, not_found},
                 hecate_pubsub_registry:relay_publish(Reg, R, publish_frame(R, <<"x">>, key()))).

%% Production path: a registry started with `default_identity' auto-registers a pubsub_server on relay_publish for an
%% unknown realm so the EVENT frame is built even with zero local subscribers. Downstream bloom-fan in the dispatcher
%% needs the EVENT regardless of local interest.
relay_publish_auto_registers_when_default_identity_set_test() ->
    process_flag(trap_exit, true),
    {ok, Reg} = hecate_pubsub_registry:start_link(#{identity => key()}),
    unlink(Reg),
    R = realm(),
    Daemon = key(),
    #{publication := Publication} = PublishFrame = publish_frame(R, <<"weather.measured_v1">>, Daemon),
    {ok, EventFrame, Matched} = hecate_pubsub_registry:relay_publish(Reg, R, PublishFrame),
    ?assertEqual(event, macula_frame:frame_type(EventFrame)),
    ?assertEqual(Publication, maps:get(publication, EventFrame)),
    DaemonId = macula_node_keys:key_id(Daemon),
    ?assertMatch({ok, #{topic := <<"weather.measured_v1">>, publisher := DaemonId}}, verified(EventFrame)),
    ?assertEqual([], Matched),  % zero local subs: bloom-fan extras live in the dispatcher
    %% Realm now materialised.
    ?assertMatch({ok, _Pid}, hecate_pubsub_registry:lookup(Reg, R)),
    catch hecate_pubsub_registry:stop(Reg).

relay_publish_returns_event_and_subscribers(Reg) ->
    R = realm(),
    %% Daemon publishing the event.
    Daemon = key(),
    %% A local subscriber.
    SubId = id(7),
    %% Materialise the realm with the station's identity.
    {ok, Server} = hecate_pubsub_registry:register(Reg, R, key()),
    ok = hecate_pubsub_server:subscribe(Server, <<"weather.measured_v1">>, SubId),
    #{publication := Publication} = PublishFrame = publish_frame(R, <<"weather.measured_v1">>, Daemon),
    {ok, EventFrame, Matched} = hecate_pubsub_registry:relay_publish(Reg, R, PublishFrame),
    %% The EVENT carries the daemon's publication unchanged, so its publisher, seq and payload are the daemon's and
    %% pool dedup keys line up across stations.
    ?assertEqual(event, macula_frame:frame_type(EventFrame)),
    ?assertEqual(Publication, maps:get(publication, EventFrame)),
    ?assertEqual(direct, maps:get(delivered_via, EventFrame)),
    DaemonId = macula_node_keys:key_id(Daemon),
    ?assertMatch({ok, #{topic := <<"weather.measured_v1">>, realm := R, publisher := DaemonId, seq := 42,
                        payload := <<"20C">>}},
                 verified(EventFrame)),
    %% No station signature of its own: the publisher's signature goes end to end.
    ?assertNot(maps:is_key(signature, EventFrame)),
    %% Local sub matched.
    ?assertEqual([SubId], Matched).

relay_publish_passes_a_refusal_through(Reg) ->
    R = realm(),
    {ok, _Server} = hecate_pubsub_registry:register(Reg, R, key()),
    ?assertEqual({error, signature_invalid},
                 hecate_pubsub_registry:relay_publish(Reg, R, tampered(publish_frame(R, <<"t">>, key())))).

%%---------------------------------------------------------------------
%% purge_subscriber
%%---------------------------------------------------------------------

purge_subscriber_clears_a_single_realm(Reg) ->
    R = realm(),
    Sub = id(1),
    {ok, Server} = hecate_pubsub_registry:register(Reg, R, key()),
    ok = hecate_pubsub_server:subscribe(Server, <<"t">>, Sub),
    ok = hecate_pubsub_registry:purge_subscriber(Reg, Sub),
    ?assertEqual(0, hecate_pubsub_server:topic_count(Server)).

%% A departed peer or daemon has no notion of "which realm" it was subscribed under: this is the whole reason
%% `purge_subscriber/2' lives on the registry rather than requiring the caller to know.
purge_subscriber_fans_out_across_realms(Reg) ->
    R1 = realm(),
    R2 = realm(),
    Key = key(),
    Sub = id(1),
    {ok, S1} = hecate_pubsub_registry:register(Reg, R1, Key),
    {ok, S2} = hecate_pubsub_registry:register(Reg, R2, Key),
    ok = hecate_pubsub_server:subscribe(S1, <<"a">>, Sub),
    ok = hecate_pubsub_server:subscribe(S2, <<"b">>, Sub),
    ok = hecate_pubsub_registry:purge_subscriber(Reg, Sub),
    ?assertEqual(0, hecate_pubsub_server:topic_count(S1)),
    ?assertEqual(0, hecate_pubsub_server:topic_count(S2)).

purge_subscriber_keeps_other_subscribers(Reg) ->
    R = realm(),
    {ok, Server} = hecate_pubsub_registry:register(Reg, R, key()),
    ok = hecate_pubsub_server:subscribe(Server, <<"t">>, id(1)),
    ok = hecate_pubsub_server:subscribe(Server, <<"t">>, id(2)),
    ok = hecate_pubsub_registry:purge_subscriber(Reg, id(1)),
    ?assertEqual([id(2)], hecate_pubsub_server:subscribers(Server, <<"t">>)).

purge_subscriber_tolerates_no_realms(Reg) ->
    ?assertEqual(ok, hecate_pubsub_registry:purge_subscriber(Reg, id(1))).

%% A server dying between `list_realms'-time bookkeeping and this call's fan-out is a race, not an error: the
%% registry's own `EXIT' handling clears the stale entry independently. purge_subscriber must not crash the registry
%% when it hits a dead pid.
purge_subscriber_tolerates_a_dead_server(Reg) ->
    R = realm(),
    {ok, Pid} = hecate_pubsub_registry:register(Reg, R, key()),
    exit(Pid, kill),
    wait_until(fun() -> not is_process_alive(Pid) end, 1000),
    ?assertEqual(ok, hecate_pubsub_registry:purge_subscriber(Reg, id(1))),
    ?assert(is_process_alive(Reg)).

%%---------------------------------------------------------------------
%% Frame and polling helpers
%%---------------------------------------------------------------------

subscribe_frame(Realm, Topic, Sub) ->
    macula_frame:subscribe(#{topic => Topic, realm => Realm, subscriber => Sub}).

publish_frame(Realm, Topic, Key) ->
    macula_frame:publish(#{realm => Realm, topic => Topic, seq => 42, published_at => erlang:system_time(millisecond),
                           payload => <<"20C">>}, Key).

event_frame(Realm, Topic, Key) ->
    #{publication := Publication} = publish_frame(Realm, Topic, Key),
    macula_frame:event(#{publication => Publication, delivered_via => plumtree}).

tampered(#{publication := #{tbs := <<Head:20/binary, Byte, Tail/binary>>} = Publication} = Frame) ->
    Frame#{publication := Publication#{tbs := <<Head/binary, (Byte bxor 1), Tail/binary>>}}.

verified(Frame) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    macula_frame:verify_publication(Frame, Profile, erlang:system_time(millisecond)).

wait_until(_Pred, Budget) when Budget =< 0 ->
    erlang:error(wait_until_timeout);
wait_until(Pred, Budget) ->
    case Pred() of
        true  -> ok;
        false ->
            timer:sleep(10),
            wait_until(Pred, Budget - 10)
    end.
