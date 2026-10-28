%% Subscription sources (macula#147, WP2): a subscriber declares who may deliver
%% to it, and its own SDK refuses every fact from anywhere else.
%%
%% These are the tests the threat model names for the subscription side (issue
%% #147, section 7), plus the three decisions the note (comment 6461033269) took:
%% the check runs at the pool after its dedupe, no lookup runs in the pool
%% process, and a fact behind an in-flight lookup is held rather than dropped.
%%
%% The lookups are injected (`source_lookup' at connect) so every branch -- admit,
%% refuse, nothing known, still thinking -- is reachable without a DHT. What the
%% real lookups read is tested where they live: a member's endorsement slot sees a
%% withdrawal on the next lookup (`macula_hyparview_slot_endorsement_tests'), and
%% `org_member/4,5' answers the earlier of its two records' expiries
%% (`macula_org_member_tests').
-module(macula_subscriber_sources_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM_NAME, <<"io.example">>).
-define(REALM, crypto:hash(sha256, ?REALM_NAME)).
-define(TOPIC, <<"acme/ledger.posted">>).
-define(PUB, <<7:256>>).
-define(OTHER_PUB, <<8:256>>).

%%------------------------------------------------------------------
%% The declaration, resolved at subscribe
%%------------------------------------------------------------------

%% There is no default source. A subscription that declares none is refused at the
%% API, so a service cannot ship a subscription that anyone can deliver to
%% (threat model 5.4).
a_subscription_that_declares_no_source_is_refused_test_() ->
    {timeout, 30, fun() ->
        {Pool, _RealmKey, _Test} = start(#{}, none),
        ?assertEqual({error, {sources, required}},
                     macula_client:subscribe(Pool, ?REALM, ?TOPIC, self(), #{})),
        ?assertEqual({error, {sources, required}},
                     macula:subscribe(Pool, ?REALM, ?TOPIC, self(), #{sources => undefined})),
        %% Nothing was registered, so nothing is delivered either.
        ?assertEqual(0, subscriptions(Pool)),
        stop(Pool)
    end}.

%% `open' is the explicit opt-out and is not a source: it passes through untouched.
an_explicit_opt_out_is_not_a_source_test() ->
    ?assertEqual({ok, open},
                 macula_subscriber_sources:declared(open, ?REALM, #{})).

%% A record-backed source cannot be enforced in a realm whose key the pool never
%% pinned, so it is refused rather than served open.
a_record_source_with_no_pinned_realm_key_is_refused_test() ->
    ?assertEqual({error, {source_class, no_realm_key}},
                 macula_subscriber_sources:declared(members, ?REALM, #{})),
    ?assertEqual({error, {source_class, no_realm_key}},
                 macula_subscriber_sources:declared({org, <<"acme">>}, ?REALM, #{})).

a_record_source_with_a_pinned_key_resolves_test() ->
    #{realm_key := RealmKey} = parties(),
    Keys = #{?REALM => carried(RealmKey)},
    ?assertEqual({ok, members}, macula_subscriber_sources:declared(members, ?REALM, Keys)),
    ?assertEqual({ok, {org, <<"acme">>}},
                 macula_subscriber_sources:declared({org, <<"acme">>}, ?REALM, Keys)).

%% `{nodes, Ids}' names its publishers itself, so it needs no realm key at all.
nodes_need_no_realm_key_test() ->
    NodeIds = [<<1:256>>, <<2:256>>],
    ?assertEqual({ok, {nodes, NodeIds}},
                 macula_subscriber_sources:declared({nodes, NodeIds}, ?REALM, #{})).

a_source_naming_no_nodes_is_refused_test() ->
    Keys = #{?REALM => <<0:2048>>},
    ?assertEqual({error, {source_class, {nodes, empty}}},
                 macula_subscriber_sources:declared({nodes, []}, ?REALM, Keys)),
    ?assertEqual({error, {source_class, {nodes, not_a_list}}},
                 macula_subscriber_sources:declared({nodes, <<1:256>>}, ?REALM, Keys)),
    ?assertEqual({error, {source_class, {nodes, not_node_ids}}},
                 macula_subscriber_sources:declared({nodes, [<<"not-a-node-id">>]}, ?REALM, Keys)).

%% An improper list is not a list of node ids. `[_ | _]' matches an improper cons
%% as happily as a proper one, `lists:all/2' over it raises, and on OTP 28
%% `is_list/1' answers true for it, so it has to be walked to be caught.
an_improper_node_list_is_refused_test() ->
    ?assertEqual({error, {source_class, {nodes, not_a_list}}},
                 macula_subscriber_sources:declared({nodes, [<<1:256>> | <<2:256>>]}, ?REALM, #{})).

%% An organisation is named by its own name; anything else is a refusal, not a
%% lookup for a nonsense org.
a_source_naming_no_organisation_is_refused_test() ->
    ?assertEqual({error, {source_class, {org, invalid}}},
                 macula_subscriber_sources:declared({org, 42}, ?REALM, #{?REALM => <<0:2048>>})),
    ?assertEqual({error, {source_class, {org, invalid}}},
                 macula_subscriber_sources:declared({org, <<>>}, ?REALM, #{?REALM => <<0:2048>>})).

%% `role' is a call class, not a source: no publish capability exists for it to
%% rest on (realm#98 refuses `pubsub/publish' in a role), so it is deferred and
%% refused by name (§9).
role_is_not_a_source_test() ->
    ?assertEqual({error, {source_class, {unknown, role}}},
                 macula_subscriber_sources:declared(role, ?REALM, #{?REALM => <<0:2048>>})),
    ?assertEqual({error, {source_class, {unknown, elevated}}},
                 macula_subscriber_sources:declared(elevated, ?REALM, #{?REALM => <<0:2048>>})).

%%------------------------------------------------------------------
%% The check at the pool, on the verified publisher
%%------------------------------------------------------------------

%% A named-nodes source admits the publishers it lists and refuses every other,
%% counted by reason -- and the refusal happens on the `direct' channel exactly as
%% on `plumtree', because `delivered_via' is not a way round the check.
a_named_nodes_source_admits_only_its_list_test_() ->
    {timeout, 30, fun() ->
        {Pool, _RealmKey, _Test} = start(#{}, {nodes, [?PUB]}),
        inject(Pool, ?TOPIC, ?PUB, 1, direct),
        ?assertEqual([1], collect(1, 1000)),
        inject(Pool, ?TOPIC, ?OTHER_PUB, 2, direct),
        inject(Pool, ?TOPIC, ?OTHER_PUB, 3, plumtree),
        ?assertEqual(ok, collect_none(300)),
        {ok, Status} = macula_client:status(Pool),
        ?assertEqual(#{not_listed => 2}, maps:get(source_refused, Status)),
        ?assertEqual(#{}, maps:get(source_dropped, Status)),
        ?assertEqual(0, maps:get(source_held, Status)),
        stop(Pool)
    end}.

%% Two subscriptions on one topic may declare different sources, and each gets the
%% answer its own declaration earned: the check is per subscription, on the same
%% publication, after the pool's dedupe.
two_subscriptions_on_one_topic_are_checked_apart_test_() ->
    {timeout, 30, fun() ->
        {Pool, _RealmKey, _Test} = start(#{}, {nodes, [?PUB]}),
        {ok, OpenRef} = macula_client:subscribe(Pool, ?REALM, ?TOPIC, self(),
                                                #{sources => open, delivery => as_arrives}),
        inject(Pool, ?TOPIC, ?OTHER_PUB, 1, direct),
        %% The open subscription takes it, the named one refuses it.
        ?assertEqual([{OpenRef, 1}], collect_refs(1, 1000)),
        {ok, Status} = macula_client:status(Pool),
        ?assertEqual(#{not_listed => 1}, maps:get(source_refused, Status)),
        stop(Pool)
    end}.

%% A `members' source asks the lookup, and admits or refuses on its answer. The
%% reason is kept per verdict, so an operator can see why a publisher was refused.
a_members_source_admits_what_the_lookup_admits_test_() ->
    {timeout, 30, fun() ->
        Until = now_ms() + 60_000,
        {Pool, _RealmKey, _Test} = start(#{}, members, reporting({admit, Until})),
        inject(Pool, ?TOPIC, ?PUB, 1, direct),
        ?assertEqual([1], collect(1, 1000)),
        ?assertEqual([?PUB], looked_up()),
        stop(Pool)
    end}.

a_members_source_refuses_what_the_lookup_refuses_test_() ->
    {timeout, 30, fun() ->
        {Pool, _RealmKey, _Test} = start(#{}, members, reporting({refuse, not_found})),
        inject(Pool, ?TOPIC, ?PUB, 1, direct),
        ?assertEqual(ok, collect_none(300)),
        ?assertEqual([?PUB], looked_up()),
        {ok, Status} = macula_client:status(Pool),
        ?assertEqual(#{not_found => 1}, maps:get(source_refused, Status)),
        stop(Pool)
    end}.

%% A lookup that could not find out is not a verdict: nothing is cached, nothing
%% is delivered, and the facts behind it are counted as dropped rather than
%% refused, so the next fact from that publisher asks again.
a_lookup_that_could_not_find_out_drops_the_facts_test_() ->
    {timeout, 30, fun() ->
        {Pool, _RealmKey, _Test} = start(#{}, members, reporting({unavailable, no_station})),
        %% Both wait behind the same one lookup, and both go when it fails.
        inject(Pool, ?TOPIC, ?PUB, 1, direct),
        inject(Pool, ?TOPIC, ?PUB, 2, direct),
        ?assertEqual(ok, collect_none(400)),
        {ok, Status} = macula_client:status(Pool),
        ?assertEqual(#{}, maps:get(source_refused, Status)),
        ?assertEqual(#{lookup_unavailable => 2}, maps:get(source_dropped, Status)),
        ?assertEqual(0, maps:get(source_verdicts, Status)),
        %% Nothing was cached, so the next fact asks again.
        inject(Pool, ?TOPIC, ?PUB, 3, direct),
        ?assertEqual(ok, collect_none(400)),
        ?assertEqual([?PUB, ?PUB], looked_up()),
        {ok, Again} = macula_client:status(Pool),
        ?assertEqual(#{lookup_unavailable => 3}, maps:get(source_dropped, Again)),
        stop(Pool)
    end}.

%% The organisation source carries the organisation to the lookup, so a pool
%% serving several organisations asks about the one the subscription named.
an_org_source_asks_about_its_own_organisation_test_() ->
    {timeout, 30, fun() ->
        Until = now_ms() + 60_000,
        Test = self(),
        {Pool, _RealmKey, _} = start(#{}, {org, <<"acme">>},
                                        fun(Source, _Context, Publisher) ->
                                            Test ! {looked_up_source, Source, Publisher},
                                            {admit, Until}
                                        end),
        inject(Pool, ?TOPIC, ?PUB, 1, direct),
        ?assertEqual([1], collect(1, 1000)),
        ?assertEqual([{{org, <<"acme">>}, ?PUB}], looked_up_with_source()),
        stop(Pool)
    end}.

%% A node outside the organisation is refused, with the reason the lookup gave:
%% the two sources name two different sets on the subscription side.
an_org_source_refuses_a_node_outside_the_organisation_test_() ->
    {timeout, 30, fun() ->
        {Pool, _RealmKey, _Test} = start(#{}, {org, <<"acme">>},
                                         reporting({refuse, {not_a_member, {procedure_delegation, not_found}}})),
        inject(Pool, ?TOPIC, ?PUB, 1, direct),
        ?assertEqual(ok, collect_none(300)),
        {ok, Status} = macula_client:status(Pool),
        ?assertEqual(#{ {not_a_member, {procedure_delegation, not_found}} => 1},
                     maps:get(source_refused, Status)),
        stop(Pool)
    end}.

%%------------------------------------------------------------------
%% Held behind a lookup: waiting, not dropped (the note, decision 4)
%%------------------------------------------------------------------

%% The first fact from a new publisher waits behind its one lookup instead of
%% being dropped, or a subscriber would lose the first fact of every publisher --
%% and the first after every expiry.
a_fact_behind_a_lookup_is_held_and_released_test_() ->
    {timeout, 30, fun() ->
        Until = now_ms() + 60_000,
        {Pool, _RealmKey, Test} = start(#{}, members, controlled()),
        [inject(Pool, ?TOPIC, ?PUB, Seq, direct) || Seq <- [1, 2]],
        {Worker, [?PUB]} = await_lookup(Test),
        %% Both facts wait, in arrival order, and the pool says so.
        ?assertEqual(ok, collect_none(200)),
        {ok, Held} = macula_client:status(Pool),
        ?assertEqual(2, maps:get(source_held, Held)),
        ?assertEqual(1, maps:get(source_lookups, Held)),
        Worker ! {verdict, {admit, Until}},
        ?assertEqual([1, 2], collect(2, 1000)),
        {ok, Released} = macula_client:status(Pool),
        ?assertEqual(0, maps:get(source_held, Released)),
        ?assertEqual(#{}, maps:get(source_dropped, Released)),
        stop(Pool)
    end}.

%% Past the held cap a fact is dropped and counted, not held: the queue behind one
%% lookup is bounded, and the bound fails closed.
past_the_held_cap_a_fact_is_dropped_test_() ->
    {timeout, 30, fun() ->
        Until = now_ms() + 60_000,
        {Pool, _RealmKey, Test} = start(#{subscribe_sources => #{held_per_key => 1}},
                                        members, controlled()),
        [inject(Pool, ?TOPIC, ?PUB, Seq, direct) || Seq <- [1, 2, 3]],
        {Worker, _Publishers} = await_lookup(Test),
        {ok, Held} = macula_client:status(Pool),
        ?assertEqual(1, maps:get(source_held, Held)),
        ?assertEqual(#{held_full => 2}, maps:get(source_dropped, Held)),
        Worker ! {verdict, {admit, Until}},
        ?assertEqual([1], collect(1, 1000)),
        stop(Pool)
    end}.

%% A held fact is never kept past its own expiry: if the lookup answers after the
%% fact expired, the fact is dropped and counted rather than delivered.
a_held_fact_past_its_expiry_is_dropped_test_() ->
    {timeout, 30, fun() ->
        Until = now_ms() + 60_000,
        {Pool, _RealmKey, Test} = start(#{}, members, controlled()),
        inject(Pool, ?TOPIC, ?PUB, 1, direct, now_ms() + 100),
        {Worker, _Publishers} = await_lookup(Test),
        timer:sleep(200),
        Worker ! {verdict, {admit, Until}},
        ?assertEqual(ok, collect_none(400)),
        {ok, Status} = macula_client:status(Pool),
        ?assertEqual(#{held_expired => 1}, maps:get(source_dropped, Status)),
        stop(Pool)
    end}.

%% A lookup that never answers is ended at its deadline and what it held is
%% dropped and counted: it must not hold a publisher's facts for ever, nor take the
%% pool down with it.
a_lookup_that_never_answers_is_ended_test_() ->
    {timeout, 30, fun() ->
        Never = fun(_Source, _Context, _Publisher) -> timer:sleep(60_000), {admit, now_ms()} end,
        {Pool, _RealmKey, _Test} = start(#{subscribe_sources => #{lookup_timeout_ms => 100}},
                                         members, Never),
        inject(Pool, ?TOPIC, ?PUB, 1, direct),
        ?assertEqual(ok, collect_none(600)),
        {ok, Status} = macula_client:status(Pool),
        ?assertEqual(#{lookup_unavailable => 1}, maps:get(source_dropped, Status)),
        ?assertEqual(0, maps:get(source_lookups, Status)),
        ?assertEqual(0, maps:get(source_held, Status)),
        ?assertMatch({ok, _}, macula_client:status(Pool)),
        stop(Pool)
    end}.

%%------------------------------------------------------------------
%% The lookup's bounds (§6.3), each tested alone
%%------------------------------------------------------------------

%% Past the realm's per-window lookup cap no lookup is made at all: the fact is
%% dropped and counted with that reason, which is the bound that carries a
%% rotating-id attacker.
past_the_lookup_window_facts_are_dropped_without_a_lookup_test_() ->
    {timeout, 30, fun() ->
        {Pool, _RealmKey, Test} = start(#{subscribe_sources => #{lookups_per_window => 1}},
                                        members, controlled()),
        inject(Pool, ?TOPIC, ?PUB, 1, direct),
        {Worker, [?PUB]} = await_lookup(Test),
        inject(Pool, ?TOPIC, ?OTHER_PUB, 2, direct),
        ?assertEqual(ok, collect_none(300)),
        {ok, Status} = macula_client:status(Pool),
        ?assertEqual(#{lookup_window => 1}, maps:get(source_dropped, Status)),
        Worker ! {verdict, {refuse, not_found}},
        ?assertEqual(ok, collect_none(300)),
        stop(Pool)
    end}.

%% The distinct-publisher cap bounds the cache, and it is the least recently
%% verified verdict that goes -- a rotating-id attacker pushes out its own
%% verdicts, not a steady publisher's.
the_publisher_cap_forgets_the_oldest_verdict_test() ->
    Bounds = maps:merge(bounds(), #{max_publishers => 2}),
    Until = now_ms() + 60_000,
    Cache0 = macula_subscriber_sources:cache_put(#{}, key(?PUB), admit, Until, Bounds, now_ms()),
    Cache1 = macula_subscriber_sources:cache_put(Cache0, key(?OTHER_PUB), admit, Until, Bounds, now_ms() + 1),
    ?assertEqual(2, map_size(Cache1)),
    Cache2 = macula_subscriber_sources:cache_put(Cache1, key(<<9:256>>), admit, Until, Bounds, now_ms() + 2),
    ?assertEqual(2, map_size(Cache2)),
    ?assertMatch({miss, _}, macula_subscriber_sources:cache_get(Cache2, key(?PUB), now_ms() + 3)),
    ?assertMatch({ok, admit, _, _}, macula_subscriber_sources:cache_get(Cache2, key(?OTHER_PUB), now_ms() + 4)).

%%------------------------------------------------------------------
%% The verdict cache (§6.2)
%%------------------------------------------------------------------

%% A verdict is never honoured past the record that produced it, and an expired
%% entry is a miss the moment it is asked.
a_verdict_is_not_honoured_past_its_record_test() ->
    Now = now_ms(),
    Bounds = bounds(),
    Cache = macula_subscriber_sources:cache_put(#{}, key(?PUB), admit, Now + 1_000, Bounds, Now),
    ?assertMatch({ok, admit, _, _}, macula_subscriber_sources:cache_get(Cache, key(?PUB), Now + 999)),
    ?assertMatch({miss, _}, macula_subscriber_sources:cache_get(Cache, key(?PUB), Now + 1_000)).

%% The cache's own life is shorter than a record's 24 h ceiling, so a withdrawal
%% is seen within the cache's refresh rather than at the record's expiry.
the_cache_life_caps_a_long_record_test() ->
    Now = now_ms(),
    Bounds = bounds(),
    Cache = macula_subscriber_sources:cache_put(#{}, key(?PUB), admit, Now + 86_400_000, Bounds, Now),
    Ttl = maps:get(cache_ttl_ms, Bounds),
    ?assertMatch({ok, admit, _, _}, macula_subscriber_sources:cache_get(Cache, key(?PUB), Now + Ttl - 1)),
    ?assertMatch({miss, _}, macula_subscriber_sources:cache_get(Cache, key(?PUB), Now + Ttl)).

%% A verdict that is still in use is refreshed before it expires (§6.3), once per
%% life of the entry: a steady publisher therefore never waits on a lookup.
a_verdict_still_in_use_is_refreshed_before_it_expires_test_() ->
    {timeout, 30, fun() ->
        Until = now_ms() + 600,
        {Pool, _RealmKey, _} = start(#{subscribe_sources => #{refresh_margin_ms => 100}},
                                     members, reporting({admit, Until})),
        inject(Pool, ?TOPIC, ?PUB, 1, direct),
        ?assertEqual([1], collect(1, 1000)),
        %% The entry's own refresh moment is 100 ms before it expires, so a fact
        %% arriving now is served from the cache without another lookup.
        inject(Pool, ?TOPIC, ?PUB, 2, direct),
        ?assertEqual([2], collect(1, 1000)),
        ?assertEqual([?PUB], looked_up()),
        timer:sleep(450),
        %% Past that moment, the next fact refreshes while it is served.
        inject(Pool, ?TOPIC, ?PUB, 3, direct),
        ?assertEqual([3], collect(1, 1000)),
        ?assertEqual([?PUB, ?PUB], looked_up()),
        stop(Pool)
    end}.

%%------------------------------------------------------------------
%% The bounds option itself
%%------------------------------------------------------------------

a_bound_that_is_not_a_bound_does_not_start_the_pool_test_() ->
    {timeout, 30, fun() ->
        {ok, _} = application:ensure_all_started(macula),
        ?assertMatch({error, {sources, {subscribe_sources, {unknown, max_publishers_per_day}}}},
                     macula_client:connect([], #{subscribe_sources => #{max_publishers_per_day => 1}})),
        ?assertMatch({error, {sources, {subscribe_sources, {held_per_key, 0}}}},
                     macula_client:connect([], #{subscribe_sources => #{held_per_key => 0}})),
        ?assertMatch({error, {sources, {subscribe_sources, invalid}}},
                     macula_client:connect([], #{subscribe_sources => not_a_map}))
    end}.

%%------------------------------------------------------------------
%% helpers
%%------------------------------------------------------------------

%% A pool with a pinned realm key, one subscription on ?TOPIC, and optionally the
%% injected lookup. `Sources => none' subscribes nothing, so a test can make its
%% own subscriptions.
start(ConnOpts, Sources) ->
    start(ConnOpts, Sources, fun(_Source, _Context, _Publisher) -> {refuse, not_found} end).

start(ConnOpts, Sources, Lookup) ->
    {ok, _} = application:ensure_all_started(macula),
    #{realm_key := RealmKey} = parties(),
    {ok, Pool} = macula_client:connect([], ConnOpts#{realm_trust => #{?REALM => carried(RealmKey)},
                                                     source_lookup => Lookup}),
    _ = subscribed(Pool, Sources),
    {Pool, RealmKey, self()}.

%% `as_arrives': these tests are about who may deliver, not about the reorder
%% buffer, and `ordered' would hold a new publisher's first fact for its timeout
%% on top of everything else the suite is doing.
subscribed(_Pool, none) ->
    ok;
subscribed(Pool, Sources) ->
    macula_client:subscribe(Pool, ?REALM, ?TOPIC, self(),
                            #{sources => Sources, delivery => as_arrives}).

stop(Pool) ->
    try macula_client:close(Pool) catch _:_ -> ok end,
    ok.

%% Each (publisher, seq) stands in for one verified publication over the given
%% delivery channel, with its own publication hash and a minute to live.
inject(Pool, Topic, Publisher, Seq, DeliveredVia) ->
    inject(Pool, Topic, Publisher, Seq, DeliveredVia, now_ms() + 60_000).

inject(Pool, Topic, Publisher, Seq, DeliveredVia, ExpiresAt) ->
    Pool ! {macula_event, make_ref(), Topic, Seq,
            #{realm => ?REALM, publisher => Publisher, seq => Seq,
              delivered_via => DeliveredVia,
              publication_hash => crypto:hash(sha384, <<Publisher/binary, Seq:64>>),
              expires_at => ExpiresAt}},
    ok.

%% A lookup that holds its answer until the test sends one, so the "held" path is
%% reachable: it reports the publishers it was asked about, then waits.
controlled() ->
    Test = self(),
    fun(_Source, _Context, Publisher) ->
        Test ! {looked_up_controlled, self(), Publisher},
        receive {verdict, Verdict} -> Verdict end
    end.

%% The worker's pid and the publishers it asked about, once it has asked.
await_lookup(Test) ->
    await_lookup(Test, [], 1000).

await_lookup(_Test, Publishers, Timeout) ->
    receive
        {looked_up_controlled, Worker, Publisher} ->
            {Worker, lists:reverse([Publisher | Publishers])}
    after Timeout ->
        {none, lists:reverse(Publishers)}
    end.

collect(N, Timeout) ->
    collect(N, Timeout, []).

collect(0, _Timeout, Acc) ->
    lists:reverse(Acc);
collect(N, Timeout, Acc) ->
    receive
        {macula_event, _Ref, _Topic, Payload, _Meta} -> collect(N - 1, Timeout, [Payload | Acc])
    after Timeout ->
        lists:reverse(Acc)
    end.

collect_refs(N, Timeout) ->
    collect_refs(N, Timeout, []).

collect_refs(0, _Timeout, Acc) ->
    lists:reverse(Acc);
collect_refs(N, Timeout, Acc) ->
    receive
        {macula_event, Ref, _Topic, Payload, _Meta} -> collect_refs(N - 1, Timeout, [{Ref, Payload} | Acc])
    after Timeout ->
        lists:reverse(Acc)
    end.

collect_none(Timeout) ->
    receive
        {macula_event, _Ref, _Topic, Payload, _Meta} -> {unexpected_event, Payload}
    after Timeout ->
        ok
    end.

%% A lookup that answers the same way every time and reports each publisher it
%% was asked about, so a test can see how many lookups its facts cost.
reporting(Answer) ->
    Test = self(),
    fun(_Source, _Context, Publisher) ->
        Test ! {looked_up, Publisher},
        Answer
    end.

%% The publishers the injected lookup was asked about, in the order it was asked
%% (a worker may report after the test's own event arrived, so this waits a
%% moment).
looked_up() ->
    timer:sleep(100),
    looked_up_loop([]).

looked_up_loop(Acc) ->
    receive
        {looked_up, Publisher} -> looked_up_loop([Publisher | Acc]);
        {looked_up_controlled, _Worker, Publisher} -> looked_up_loop([Publisher | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

%% The same, keeping the source the lookup was asked about.
looked_up_with_source() ->
    timer:sleep(100),
    looked_up_with_source_loop([]).

looked_up_with_source_loop(Acc) ->
    receive
        {looked_up_source, Source, Publisher} -> looked_up_with_source_loop([{Source, Publisher} | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

subscriptions(Pool) ->
    {ok, Status} = macula_client:status(Pool),
    maps:get(subscriptions, Status).

key(Publisher) ->
    macula_subscriber_sources:cache_key(?REALM, members, Publisher).

bounds() ->
    {ok, Bounds} = macula_subscriber_sources:bounds(#{}),
    Bounds.

now_ms() -> erlang:system_time(millisecond).

parties() ->
    {ok, RealmKey} = macula_node_keys:generate(realm, pq_pure),
    #{realm_key => RealmKey}.

carried(Key) -> macula_node_keys:public_key(Key).
