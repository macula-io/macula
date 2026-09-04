%% EUnit tests for `macula_client' (the pool).
%%
%% These tests exercise the pool's bookkeeping end-to-end without a
%% live QUIC station. The pool spawns real `macula_station_link'
%% workers against unreachable seeds (port 1) — every link stays in
%% the disconnected state. We can still test:
%%
%%   - Pool startup with multiple seeds
%%   - Subscribe/unsubscribe state machine + topic_index ref counting
%%   - Multiple-consumers-one-topic fan-out registration
%%   - Subscriber DOWN tears down the sub
%%   - Pool close emits macula_event_gone to all subscribers
%%   - publish/5 with zero healthy links → transient error
%%   - Synthetic EVENT injected at the pool fans to local subs
%%   - Inbound EVENT dedup across simulated station copies
-module(macula_client_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).
-define(SEED1, #{host => <<"127.0.0.1">>, port => 1}).
-define(SEED2, #{host => <<"127.0.0.1">>, port => 2}).

%%------------------------------------------------------------------
%% Boot
%%------------------------------------------------------------------

connect_with_no_seeds_returns_pool_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([], #{}),
    ?assert(is_process_alive(Pool)),
    ok = macula_client:close(Pool),
    ok.

connect_with_unreachable_seed_returns_pool_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([?SEED1], #{}),
    ?assert(is_process_alive(Pool)),
    ok = macula_client:close(Pool),
    ok.

%% A caller who doesn't pass `identity' must not silently get one that
%% fails puzzle validation — that identity looks fine locally (the
%% connection reports healthy, subscribe returns ok) while every
%% puzzle-enforcing station rejects its handshake and delivers nothing.
%% See `macula_client:resolve_identity/1'.
connect_with_no_identity_opt_defaults_to_puzzle_hardened_test_() ->
    {timeout, 20, fun() ->
        {ok, _} = application:ensure_all_started(macula),
        {ok, Pool} = macula_client:connect([], #{}),
        {ok, #{self_node_id := NodeId}} = macula_client:status(Pool),
        ?assert(macula_identity:puzzle_valid(NodeId)),
        ok = macula_client:close(Pool)
    end}.

%% An explicitly-supplied identity must be used as-is, unhardened or
%% not — the pool's default only fills the gap when the caller gave it
%% nothing, it never overrides a caller's own choice.
connect_with_explicit_identity_opt_is_used_verbatim_test() ->
    {ok, _} = application:ensure_all_started(macula),
    Identity = macula_identity:generate(),
    {ok, Pool} = macula_client:connect([], #{identity => Identity}),
    {ok, #{self_node_id := NodeId}} = macula_client:status(Pool),
    ?assertEqual(macula_identity:public(Identity), NodeId),
    ok = macula_client:close(Pool).

%% call_station dials a station outside the seed set. Against an
%% unreachable one the handshake never completes, so within the deadline
%% we get a clean `not_connected' (not a hang or a crash) and the pool
%% survives. The happy path (a real station answering) is the
%% macula-station cross-station suite.
call_station_unreachable_returns_not_connected_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([], #{}),
    Result = macula_client:call_station(Pool, ?SEED1, ?REALM,
                                        <<"x.y">>, #{}, 300),
    ?assertEqual({error, not_connected}, Result),
    ?assert(is_process_alive(Pool)),
    ok = macula_client:close(Pool),
    ok.

%% expected_node_id reuse-by-identity (2026-08-29): a direct-dial call to
%% a station this pool has NO existing link to at all (under any name)
%% must fall through cleanly to an ordinary fresh dial, same outcome as
%% the plain case above -- the actual REUSE happens live (a live
%% station is what makes a match possible at all; see
%% hecate-om/test_live/hecate_om_capabilities_live_station_tests.erl's
%% org_scoped_call_reaches_only_the_targeted_org_test_, which now
%% reuses ONE pool for two sequential call_station calls specifically
%% because this fix makes that safe). This proves the new code path
%% added for that fix doesn't hang, crash, or misbehave in the "nothing
%% matches" case this suite CAN exercise without a live station.
call_station_with_expected_node_id_and_no_match_falls_through_to_dial_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([], #{}),
    SomeNodeId = crypto:strong_rand_bytes(32),
    Result = macula_client:call_station(Pool, ?SEED1, ?REALM, <<"x.y">>,
                                        #{}, 300, <<>>,
                                        #{expected_node_id => SomeNodeId,
                                          verify => none,
                                          pin_tls_cert => false}),
    ?assertEqual({error, not_connected}, Result),
    ?assert(is_process_alive(Pool)),
    ok = macula_client:close(Pool),
    ok.

%%------------------------------------------------------------------
%% subscribe/5 + unsubscribe/2 bookkeeping
%%------------------------------------------------------------------

subscribe_returns_subref_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([], #{}),
    {ok, SubRef} = macula_client:subscribe(Pool, ?REALM,
                                           <<"x.v1">>, self(), #{}),
    ?assert(is_reference(SubRef)),
    ok = macula_client:close(Pool),
    ok.

unsubscribe_is_idempotent_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([], #{}),
    {ok, SubRef} = macula_client:subscribe(Pool, ?REALM,
                                           <<"x.v1">>, self(), #{}),
    ok = macula_client:unsubscribe(Pool, SubRef),
    ok = macula_client:unsubscribe(Pool, SubRef),
    ok = macula_client:unsubscribe(Pool, make_ref()),
    ok = macula_client:close(Pool),
    ok.

%%------------------------------------------------------------------
%% Synthetic EVENT injection — fan-out to local subscribers
%%------------------------------------------------------------------

inbound_event_fans_to_local_subscriber_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         Topic = <<"weather.measured_v1">>,
         {ok, SubRef} = macula_client:subscribe(Pool, ?REALM,
                                                Topic, self(), #{}),
         %% Inject a synthetic inbound event as if a station_link
         %% had received it. The pool's handle_info matches on the
         %% {macula_event, _, Topic, Payload, Meta} shape.
         Pool ! {macula_event, make_ref(), Topic,
                 #{temp => 20},
                 #{realm => ?REALM,
                   publisher => <<1:256>>,
                   seq => 1,
                   delivered_via => direct}},
         receive
             {macula_event, R, T, P, _Meta} ->
                 ?assertEqual(SubRef, R),
                 ?assertEqual(Topic, T),
                 ?assertEqual(#{temp => 20}, P)
         after 2_000 -> erlang:error(no_event_delivered)
         end,
         ok = macula_client:close(Pool),
         ok
     end}.

%%------------------------------------------------------------------
%% Inbound EVENT dedup across simulated station copies
%%------------------------------------------------------------------

inbound_event_dedup_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         Topic = <<"x.v1">>,
         {ok, SubRef} = macula_client:subscribe(Pool, ?REALM,
                                                Topic, self(), #{}),
         %% Three identical events (same realm/publisher/seq).
         Meta = #{realm => ?REALM,
                  publisher => <<1:256>>,
                  seq => 7,
                  delivered_via => direct},
         Pool ! {macula_event, make_ref(), Topic, hello, Meta},
         Pool ! {macula_event, make_ref(), Topic, hello, Meta},
         Pool ! {macula_event, make_ref(), Topic, hello, Meta},
         receive
             {macula_event, SubRef, _, hello, _} -> ok
         after 1_000 -> erlang:error(no_first_delivery)
         end,
         %% No second delivery.
         receive
             {macula_event, SubRef, _, hello, _} ->
                 erlang:error(duplicate_delivered)
         after 200 -> ok
         end,
         ok = macula_client:close(Pool),
         ok
     end}.

%%------------------------------------------------------------------
%% Multiple consumers same topic → both get fan-out
%%------------------------------------------------------------------

multiple_consumers_same_topic_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         Topic = <<"x.v1">>,
         Test = self(),
         Sub1 = spawn(fun() ->
             {ok, R} = macula_client:subscribe(Pool, ?REALM,
                                               Topic, self(), #{}),
             Test ! {ready, self(), R},
             receive
                 {macula_event, _, _, P, _} -> Test ! {got, self(), P}
             after 2_000 -> Test ! {timeout, self()}
             end
         end),
         Sub2 = spawn(fun() ->
             {ok, R} = macula_client:subscribe(Pool, ?REALM,
                                               Topic, self(), #{}),
             Test ! {ready, self(), R},
             receive
                 {macula_event, _, _, P, _} -> Test ! {got, self(), P}
             after 2_000 -> Test ! {timeout, self()}
             end
         end),
         _R1 = wait_ready(Sub1),
         _R2 = wait_ready(Sub2),
         Pool ! {macula_event, make_ref(), Topic,
                 broadcast,
                 #{realm => ?REALM,
                   publisher => <<1:256>>,
                   seq => 1,
                   delivered_via => direct}},
         receive {got, Sub1, broadcast} -> ok
         after 1_000 -> erlang:error(sub1_no_event)
         end,
         receive {got, Sub2, broadcast} -> ok
         after 1_000 -> erlang:error(sub2_no_event)
         end,
         ok = macula_client:close(Pool),
         ok
     end}.

wait_ready(Pid) ->
    receive
        {ready, Pid, R} -> R
    after 1_000 -> erlang:error({sub_not_ready, Pid})
    end.

%%------------------------------------------------------------------
%% Subscriber pid dies → subscription cleared
%%------------------------------------------------------------------

subscriber_down_drops_subscription_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         Topic = <<"x.v1">>,
         Test = self(),
         Sub = spawn(fun() ->
             {ok, R} = macula_client:subscribe(Pool, ?REALM,
                                               Topic, self(), #{}),
             Test ! {ready, self(), R}
         end),
         _R = wait_ready(Sub),
         %% Subscriber has exited. Give the pool a beat to process
         %% the DOWN message.
         timer:sleep(50),
         %% Inject an event — must NOT be delivered anywhere
         %% (consumer is dead, nobody else subscribed).
         Pool ! {macula_event, make_ref(), Topic, ghost,
                 #{realm => ?REALM,
                   publisher => <<1:256>>,
                   seq => 1,
                   delivered_via => direct}},
         receive
             {macula_event, _, _, ghost, _} ->
                 erlang:error(event_to_dead_subscriber)
         after 200 -> ok
         end,
         ok = macula_client:close(Pool),
         ok
     end}.

%%------------------------------------------------------------------
%% Pool close emits macula_event_gone to every subscriber
%%------------------------------------------------------------------

close_notifies_subscribers_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         {ok, SubRef} = macula_client:subscribe(Pool, ?REALM,
                                                <<"x.v1">>, self(), #{}),
         ok = macula_client:close(Pool),
         %% Pattern-match SubRef in the receive so stale event_gone
         %% messages from earlier tests in the same eunit process
         %% are ignored.
         receive
             {macula_event_gone, SubRef, Reason} ->
                 ?assertEqual(pool_closed, Reason)
         after 2_000 -> erlang:error(no_event_gone)
         end,
         ok
     end}.

%%------------------------------------------------------------------
%% publish/5 with zero spawned links → transient error
%%------------------------------------------------------------------

publish_with_no_seeds_is_transient_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         R = macula_client:publish(Pool, ?REALM,
                                   <<"x.v1">>, hello, #{}),
         ?assertEqual({error, {transient, no_healthy_station}}, R),
         ok = macula_client:close(Pool),
         ok
     end}.

%%------------------------------------------------------------------
%% replication_factor default (2, since 10.19.0)
%%
%% A live QUIC handshake is out of scope for this suite (see the
%% module header) so `connected_link_pids/1' can't be exercised
%% directly here. Two things ARE real, direct behavior, not a doc
%% claim: (1) what value the pool actually resolves into its own
%% state when a caller passes no `replication_factor' opt at all, and
%% an explicit override is honored verbatim; (2) the exact selection
%% math publish/5 applies against that value, exported as a pure
%% function specifically so it's testable without a live link.
%%------------------------------------------------------------------

connect_default_replication_factor_is_two_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         {ok, S} = macula_client:status(Pool),
         ?assertEqual(2, maps:get(replication_factor, S)),
         ok = macula_client:close(Pool)
     end}.

connect_explicit_replication_factor_is_honored_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{replication_factor => 5}),
         {ok, S} = macula_client:status(Pool),
         ?assertEqual(5, maps:get(replication_factor, S)),
         ok = macula_client:close(Pool)
     end}.

%% With 3 connected targets and the new default (2), a publish selects
%% only the first 2 — proves the fix actually raises the fan-out past
%% the old default of 1 without over-amplifying to "every link".
select_publish_targets_caps_at_default_replication_test() ->
    Targets = [t1, t2, t3],
    ?assertEqual([t1, t2], macula_client:select_publish_targets(Targets, 2)),
    ok.

%% Fewer connected links than the replication factor: publish to
%% whatever is actually connected, not an error and not padded.
select_publish_targets_uses_all_when_fewer_than_replication_test() ->
    ?assertEqual([t1], macula_client:select_publish_targets([t1], 2)),
    ok.

%% No connected links at all: selects nothing, regardless of
%% replication_factor — this is what feeds the zero-links transient
%% error path above, not a crash on empty input.
select_publish_targets_empty_when_no_targets_test() ->
    ?assertEqual([], macula_client:select_publish_targets([], 2)),
    ok.

%% An explicit replication_factor above the old default (1) but below
%% the connected count fans to exactly that many, not fewer and not
%% "all of them" — the cap is real, not a floor.
select_publish_targets_honors_explicit_factor_above_one_test() ->
    ?assertEqual([t1, t2, t3],
                 macula_client:select_publish_targets([t1, t2, t3, t4], 3)),
    ok.

%% A crash/exit from one selected link must not escape and kill the
%% pool's spawned fan-out worker — with replication_factor's new
%% default of 2, an earlier link's already-accepted publish must
%% still make it back to the caller via summarize_publish/2 even if a
%% later selected link is dead. Proven directly against a real dead
%% pid (a genuine `noproc' exit from gen_server:call, not a fake): if
%% safe_link_publish/5 didn't catch it, THIS test process would take
%% the exit and fail/crash rather than reach the assertion below.
safe_link_publish_survives_a_dead_link_test() ->
    Pid = spawn(fun() -> ok end),
    %% Give it a moment to actually exit before calling.
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)),
    Result = macula_client:safe_link_publish(Pid, ?REALM, <<"x.v1">>, hello, 1),
    ?assertMatch({error, _}, Result),
    ok.

%%------------------------------------------------------------------
%% status/1
%%------------------------------------------------------------------

status_with_no_seeds_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         {ok, S} = macula_client:status(Pool),
         ?assertEqual([], maps:get(seeds, S)),
         ?assertEqual(0, maps:get(healthy_links, S)),
         ?assertEqual(0, maps:get(failed_links, S)),
         ?assertEqual(0, maps:get(subscriptions, S)),
         ?assertMatch(<<_:256>>, maps:get(self_node_id, S)),
         ok = macula_client:close(Pool)
     end}.

status_unreachable_seeds_count_as_failed_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([?SEED1, ?SEED2], #{}),
         {ok, S} = macula_client:status(Pool),
         ?assertEqual([?SEED1, ?SEED2], maps:get(seeds, S)),
         %% Both links are spawned but stuck in connect — neither has
         %% completed CONNECT/HELLO so both count as failed.
         ?assertEqual(0, maps:get(healthy_links, S)),
         ?assertEqual(2, maps:get(failed_links, S)),
         ok = macula_client:close(Pool)
     end}.

status_tracks_subscription_count_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         {ok, _S1} = macula_client:subscribe(Pool, ?REALM,
                                              <<"a.v1">>, self(), #{}),
         {ok, _S2} = macula_client:subscribe(Pool, ?REALM,
                                              <<"b.v1">>, self(), #{}),
         {ok, S} = macula_client:status(Pool),
         ?assertEqual(2, maps:get(subscriptions, S)),
         ok = macula_client:close(Pool)
     end}.

facade_status_delegates_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([?SEED1], #{}),
         {ok, ViaFacade} = macula:status(Pool),
         {ok, Direct}    = macula_client:status(Pool),
         ?assertEqual(Direct, ViaFacade),
         ok = macula_client:close(Pool)
     end}.

%%------------------------------------------------------------------
%% links/1 — per-link snapshot (node_id / host / pid / connected)
%%------------------------------------------------------------------

links_empty_pool_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         ?assertEqual({ok, []}, macula_client:links(Pool)),
         ok = macula_client:close(Pool)
     end}.

links_one_entry_per_spawned_link_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([?SEED1, ?SEED2], #{}),
         {ok, Links} = macula_client:links(Pool),
         ?assertEqual(2, length(Links)),
         %% Nothing is listening on port 1/2, so every link is stuck in
         %% connect: unconnected, no peer node_id yet — but the dial host
         %% is still resolvable from the seed.
         [?assertEqual(false, maps:get(connected, L)) || L <- Links],
         [?assertEqual(undefined, maps:get(node_id, L)) || L <- Links],
         [?assertEqual(<<"127.0.0.1">>, maps:get(host, L)) || L <- Links],
         [?assert(is_pid(maps:get(pid, L))) || L <- Links],
         ok = macula_client:close(Pool)
     end}.

links_host_from_url_seed_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect(
                        [<<"https://relay.example:4433">>], #{}),
         {ok, [Link]} = macula_client:links(Pool),
         ?assertEqual(<<"relay.example">>, maps:get(host, Link)),
         ok = macula_client:close(Pool)
     end}.

facade_links_delegates_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([?SEED1], #{}),
         ?assertEqual(macula_client:links(Pool), macula:links(Pool)),
         ok = macula_client:close(Pool)
     end}.

%%------------------------------------------------------------------
%% RPC fan-out — call/5, advertise/4, unadvertise/3
%%------------------------------------------------------------------

call_with_no_seeds_returns_no_healthy_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         R = macula_client:call(Pool, ?REALM, <<"ping.v1">>,
                                 #{}, 1_000),
         ?assertEqual({error, no_healthy_station}, R),
         ok = macula_client:close(Pool)
     end}.

call_with_unreachable_seeds_returns_no_healthy_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([?SEED1, ?SEED2], #{}),
         R = macula_client:call(Pool, ?REALM, <<"ping.v1">>,
                                 #{}, 1_000),
         ?assertEqual({error, no_healthy_station}, R),
         ok = macula_client:close(Pool)
     end}.

advertise_no_seeds_returns_no_healthy_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         Handler = fun(_) -> {ok, pong} end,
         R = macula_client:advertise(Pool, ?REALM,
                                      <<"foo.v1">>, Handler),
         ?assertEqual({error, no_healthy_station}, R),
         ok = macula_client:close(Pool)
     end}.

unadvertise_is_idempotent_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         %% No matching advertise — unadvertise still returns ok.
         ?assertEqual(ok, macula_client:unadvertise(
                            Pool, ?REALM, <<"never.v1">>)),
         ok = macula_client:close(Pool)
     end}.

facade_v2_rpc_delegates_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([], #{}),
    Handler = fun(_) -> {ok, ack} end,
    %% V2 advertise/5 (Pool, Realm, Procedure, Handler, Opts)
    ?assertEqual({error, no_healthy_station},
                 macula:advertise(Pool, ?REALM, <<"x.v1">>, Handler, #{})),
    %% V2 unadvertise/3
    ?assertEqual(ok, macula:unadvertise(Pool, ?REALM, <<"x.v1">>)),
    %% V2 call/5
    ?assertEqual({error, no_healthy_station},
                 macula:call(Pool, ?REALM, <<"y.v1">>, #{}, 500)),
    ok = macula_client:close(Pool).

advertise_rejects_wrong_handler_arity_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([], #{}),
    Three = fun(_, _, _) -> ok end,
    ?assertError(function_clause,
                 macula_client:advertise(Pool, ?REALM,
                                          <<"a.v1">>, Three)),
    ok = macula_client:close(Pool).

%%------------------------------------------------------------------
%% V1-legacy opt warning (A5)
%%------------------------------------------------------------------

connect_with_legacy_opts_starts_pool_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         %% V1 opts must not break startup — they're noticed and
         %% ignored. Pool is fully functional with stock defaults.
         {ok, Pool} = macula_client:connect(
                        [],
                        #{realm => <<"io.macula">>,
                          site  => #{<<"site_id">> => <<"abc">>},
                          connections => 4}),
         {ok, S} = macula_client:status(Pool),
         ?assertEqual(0, maps:get(healthy_links, S)),
         ok = macula_client:close(Pool)
     end}.

%%------------------------------------------------------------------
%% dedup_window_ms / dedup_sweep_ms tunable end-to-end (A6)
%%------------------------------------------------------------------

dedup_zero_window_disables_dedup_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         %% A 0-millisecond dedup window with a tight sweep means the
         %% sweep tick will purge entries between two synthetic
         %% events sharing the same (Realm, Publisher, Seq) — both
         %% reach the consumer.
         {ok, Pool} = macula_client:connect(
                        [],
                        #{dedup_window_ms => 0,
                          dedup_sweep_ms  => 50}),
         Topic = <<"dedup.zero_window_v1">>,
         %% `as_arrives': isolate the dedup LAYER. `ordered' (the
         %% default) also drops a repeated seq, which would mask what
         %% this test measures.
         {ok, SubRef} = macula_client:subscribe(Pool, ?REALM,
                                                 Topic, self(),
                                                 #{delivery => as_arrives}),
         Pub = <<9:256>>,
         Pool ! {macula_event, make_ref(), Topic, first,
                 #{realm => ?REALM, publisher => Pub,
                   seq => 1, delivered_via => direct}},
         receive {macula_event, SubRef, Topic, first, _} -> ok
         after 1_000 -> erlang:error(no_first) end,
         %% Wait for sweep to drop the dedup entry.
         timer:sleep(120),
         Pool ! {macula_event, make_ref(), Topic, again,
                 #{realm => ?REALM, publisher => Pub,
                   seq => 1, delivered_via => direct}},
         receive {macula_event, SubRef, Topic, again, _} -> ok
         after 1_000 -> erlang:error(dedup_swallowed_after_sweep) end,
         ok = macula_client:close(Pool)
     end}.

dedup_default_window_holds_duplicate_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         %% Stock 60_000ms window — second copy of the same
         %% (Publisher, Seq) is dropped.
         {ok, Pool} = macula_client:connect([], #{}),
         Topic = <<"dedup.default_window_v1">>,
         %% `as_arrives' so the dedup LAYER is the only filter under test.
         {ok, SubRef} = macula_client:subscribe(Pool, ?REALM,
                                                 Topic, self(),
                                                 #{delivery => as_arrives}),
         Pub = <<10:256>>,
         Meta = #{realm => ?REALM, publisher => Pub,
                  seq => 7, delivered_via => direct},
         Pool ! {macula_event, make_ref(), Topic, hello, Meta},
         Pool ! {macula_event, make_ref(), Topic, hello, Meta},
         receive {macula_event, SubRef, Topic, hello, _} -> ok
         after 1_000 -> erlang:error(no_event) end,
         receive
             {macula_event, SubRef, Topic, hello, _} ->
                 erlang:error(duplicate_not_swallowed)
         after 200 -> ok
         end,
         ok = macula_client:close(Pool)
     end}.

%%====================================================================
%% Link probes must never kill the pool
%%
%% `is_connected/1' and `peer_node_id/1' are 1s gen_server:calls issued
%% from INSIDE the pool process. A gen_server:call exits its CALLER on
%% both {noproc,_} and {timeout,_}, and the pool is the caller — so
%% probing one sick link used to destroy every subscription,
%% advertisement and pending call the pool was holding.
%%
%% The timeout path is the reachable one: it needs no race, just a link
%% that is alive and does not answer for a second. That is what a wedged
%% station looks like.
%%====================================================================

%% A process that accepts messages and answers nothing, i.e. exactly a
%% link whose station has stopped responding.
spawn_mute() ->
    spawn(fun Loop() -> receive _ -> Loop() end end).

safe_is_connected_survives_a_mute_link_test_() ->
    {timeout, 10, fun() ->
        Pid = spawn_mute(),
        %% Unguarded this exits the caller with {timeout, {gen_server,call,...}}
        %% after 1s. The test process IS the stand-in for the pool here.
        ?assertEqual(false, macula_client:safe_is_connected(Pid)),
        exit(Pid, kill)
    end}.

safe_is_connected_survives_a_dead_link_test() ->
    Pid = spawn_mute(),
    exit(Pid, kill),
    timer:sleep(10),
    %% {noproc, _} — the narrow race, still fatal unguarded.
    ?assertEqual(false, macula_client:safe_is_connected(Pid)).

safe_peer_node_id_survives_a_mute_link_test_() ->
    {timeout, 10, fun() ->
        Pid = spawn_mute(),
        ?assertEqual(undefined, macula_client:safe_peer_node_id(Pid)),
        exit(Pid, kill)
    end}.

safe_peer_node_id_survives_a_dead_link_test() ->
    Pid = spawn_mute(),
    exit(Pid, kill),
    timer:sleep(10),
    ?assertEqual(undefined, macula_client:safe_peer_node_id(Pid)).

%% The old code matched only {ok,_} and {error,not_connected}, so a third
%% reply shape was a case_clause in the pool — the same fatality by
%% another route.
safe_peer_node_id_absorbs_an_unexpected_reply_test() ->
    Pid = spawn(fun Loop() ->
                    receive {'$gen_call', From, peer_node_id} ->
                        gen_server:reply(From, {error, something_new}), Loop()
                    ; _ -> Loop() end
                end),
    ?assertEqual(undefined, macula_client:safe_peer_node_id(Pid)),
    exit(Pid, kill).
