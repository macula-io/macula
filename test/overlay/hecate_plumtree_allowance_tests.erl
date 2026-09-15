%% EUnit tests for the Plumtree IHAVE allowance (DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md, 3.1). A neighbour is on at most
%% 1,024 open missing entries: an IHAVE past that is not recorded, gets no GRAFT and is refused. A GRAFT unanswered for
%% 10 seconds costs the neighbour and takes it off that entry. A GOSSIP of that id, verified or refused, ends the whole
%% entry at no charge to its announcers, and sweep/2 keeps the open counts true. Calls take their clocks from the test.
-module(hecate_plumtree_allowance_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<42:256>>).
-define(OTHER_REALM, <<77:256>>).
-define(T0, 1789000000000).
-define(MINUTE, 60000).

an_ihave_past_1024_open_entries_is_not_recorded_and_is_refused_test() ->
    S1 = announced(fresh(), id(7), 1024, at(?T0, 0)),
    {S2, Actions, []} = hecate_plumtree:process(S1, id(7), ihave(msg_id()), at(?T0, 0)),
    ?assertEqual([{refused, id(7), ihave_allowance}], Actions),
    ?assertEqual({1024, 1024}, {hecate_plumtree:missing_count(S2), hecate_plumtree:open_count(id(7), S2)}).

a_graft_unanswered_for_10_seconds_costs_the_neighbour_its_entry_test() ->
    {S1, [{send, _, _Graft}], []} = hecate_plumtree:process(fresh(), id(7), ihave(msg_id()), at(?T0, 0)),
    {S2, Early} = hecate_plumtree:expired_grafts(S1, 9_999),
    {S3, Due} = hecate_plumtree:expired_grafts(S2, 10_000),
    ?assertEqual({[], [{refused, id(7), graft_unanswered}]}, {Early, Due}),
    ?assertEqual({0, 0}, {hecate_plumtree:missing_count(S3), hecate_plumtree:open_count(id(7), S3)}).

%% Two neighbours announce one publication, and one of them sends it: neither is charged, and both are off the entry.
a_verified_gossip_ends_the_entry_and_charges_no_announcer_test() ->
    Publish = publish_frame(?REALM, #{}),
    Id = msg_id_of(Publish),
    {S1, _, []} = hecate_plumtree:process(fresh(), id(7), ihave(Id), at(wall(), 0)),
    {S2, _, []} = hecate_plumtree:process(S1, id(8), ihave(Id), at(wall(), 0)),
    {S3, _, [{Id, _Verified}]} = hecate_plumtree:process(S2, id(7), gossip_of(Publish), at(wall(), 0)),
    {S4, Due} = hecate_plumtree:expired_grafts(S3, 10_000),
    ?assertEqual([], Due),
    ?assertEqual({0, 0, 0},
                 {hecate_plumtree:missing_count(S4), hecate_plumtree:open_count(id(7), S4),
                  hecate_plumtree:open_count(id(8), S4)}).

%% A second IHAVE 5 seconds later changes nothing: the neighbour is still charged 10 seconds after the first GRAFT.
a_repeated_ihave_from_an_announcer_changes_nothing_test() ->
    Id = msg_id(),
    {S1, [{send, _, _Graft}], []} = hecate_plumtree:process(fresh(), id(7), ihave(Id), at(?T0, 0)),
    {S2, Again, []} = hecate_plumtree:process(S1, id(7), ihave(Id), at(?T0, 5_000)),
    ?assertEqual({[], 1}, {Again, hecate_plumtree:open_count(id(7), S2)}),
    {_S3, Due} = hecate_plumtree:expired_grafts(S2, 10_000),
    ?assertEqual([{refused, id(7), graft_unanswered}], Due).

a_refused_gossip_ends_the_entry_and_is_reported_with_its_refusal_test() ->
    Publish = publish_frame(?OTHER_REALM, #{}),
    Id = msg_id_of(Publish),
    {S1, _, []} = hecate_plumtree:process(fresh(), id(7), ihave(Id), at(wall(), 0)),
    {S2, _, []} = hecate_plumtree:process(S1, id(8), ihave(Id), at(wall(), 0)),
    {S3, Actions, []} = hecate_plumtree:process(S2, id(8), gossip_of(Publish), at(wall(), 0)),
    {_S4, Due} = hecate_plumtree:expired_grafts(S3, 10_000),
    ?assertEqual({[{refused, id(8), wrong_realm}], []}, {Actions, Due}),
    ?assertEqual({0, 0, 0},
                 {hecate_plumtree:missing_count(S3), hecate_plumtree:open_count(id(7), S3),
                  hecate_plumtree:open_count(id(8), S3)}).

%% The GRAFT timer never runs: a sweep that forgets an entry lowers its announcers' open counts, at no charge.
a_sweep_without_the_graft_timer_keeps_the_open_counts_true_test() ->
    {S1, _, []} = hecate_plumtree:process(fresh(), id(7), ihave(msg_id()), at(?T0, 0)),
    {S2, _, []} = hecate_plumtree:process(S1, id(7), ihave(msg_id()), at(?T0 + 10 * ?MINUTE, 0)),
    S3 = hecate_plumtree:sweep(S2, ?T0 + 70 * ?MINUTE + 1),
    ?assertEqual({1, 1}, {hecate_plumtree:missing_count(S3), hecate_plumtree:open_count(id(7), S3)}),
    S4 = hecate_plumtree:sweep(S3, ?T0 + 80 * ?MINUTE + 1),
    ?assertEqual({0, 0}, {hecate_plumtree:missing_count(S4), hecate_plumtree:open_count(id(7), S4)}).

%% Freshness is judged at the wall clock the caller passes, for a GOSSIP and for a local publish.
a_publication_is_verified_at_the_wall_clock_the_caller_passes_test() ->
    Publish = publish_frame(?REALM, #{ttl_ms => ?MINUTE}),
    Late = wall() + ?MINUTE + 5 * ?MINUTE + 2_000,
    Peer = id(9),
    {_S, Actions, []} = hecate_plumtree:process(fresh(), Peer, gossip_of(Publish), at(Late, 0)),
    ?assertMatch([{refused, Peer, {expired, _}}], Actions),
    ?assertMatch({error, {expired, _}}, hecate_plumtree:publish(fresh(), Publish, Late)).

a_wrong_realm_an_ihave_past_the_allowance_and_an_unanswered_graft_are_charged_test_() ->
    [?_assert(macula_frame:charged_refusal(Kind)) || Kind <- [wrong_realm, ihave_allowance, graft_unanswered]].

%%---------------------------------------------------------------------
%% Helpers
%%---------------------------------------------------------------------

fresh() ->
    {ok, S} = hecate_plumtree:new(id(99), ?REALM),
    S.

id(N) -> <<N:256>>.

at(WallMs, MonotonicMs) ->
    #{wall => WallMs, monotonic => MonotonicMs}.

wall() ->
    erlang:system_time(millisecond).

msg_id() ->
    crypto:strong_rand_bytes(48).

%% Count IHAVEs for new ids from Peer.
announced(State, Peer, Count, Clocks) ->
    lists:foldl(fun(_, Acc) -> element(1, hecate_plumtree:process(Acc, Peer, ihave(msg_id()), Clocks)) end,
                State, lists:seq(1, Count)).

ihave(MsgId) ->
    macula_frame:plumtree_ihave(#{realm => ?REALM, msg_id => MsgId, round => 1}).

gossip_of(#{publication := Publication}) ->
    macula_frame:plumtree_gossip(#{publication => Publication, round => 0}).

msg_id_of(#{publication := #{tbs := Tbs}}) ->
    crypto:hash(sha384, Tbs).

%% A PUBLISH signed by a fresh publisher in the node's configured profile, published now.
publish_frame(Realm, Extra) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    Spec = #{realm => Realm, topic => <<"news">>, seq => 1, published_at => wall(), payload => <<"p">>},
    macula_frame:publish(maps:merge(Spec, Extra), Key).
