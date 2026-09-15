%% EUnit tests for Plumtree frames from a sender outside a node's push sets. A frame moves a peer between eager_push and
%% lazy_push and never adds one: only add_peer/2 and remove_peer/2 change who is in them, as HyParView changes its
%% active view. Through a station's relay a frame's sender is its origin, which need not be a neighbour. From such a
%% sender a GRAFT, a PRUNE and an IHAVE move no one, send nothing and are refused as not_a_peer, which is not charged. A
%% first GOSSIP from it delivers and forwards its verified publication but moves no one, and a duplicate gets no PRUNE.
%% The same frames from a peer still move it, and charged_refusal/1 classifies every refusal kind Plumtree returns.
-module(hecate_plumtree_non_peer_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<42:256>>).

%% In each case id(1) is an eager peer, id(2) a lazy peer, and id(9) no peer at all.

a_graft_from_a_non_peer_moves_no_one_and_sends_nothing_test() ->
    Publish = publish_frame(),
    {S1, _Pushes, _Delivered} = hecate_plumtree:publish(peers(), Publish, erlang:system_time(millisecond)),
    {S2, Actions, []} = process(S1, id(9), graft(msg_id_of(Publish))),
    ?assertEqual([{refused, id(9), not_a_peer}], Actions),
    ?assertEqual(sets_of(S1), sets_of(S2)).

a_prune_from_a_non_peer_moves_no_one_test() ->
    S1 = peers(),
    {S2, Actions, []} = process(S1, id(9), prune()),
    ?assertEqual([{refused, id(9), not_a_peer}], Actions),
    ?assertEqual(sets_of(S1), sets_of(S2)).

an_ihave_from_a_non_peer_records_nothing_and_sends_no_graft_test() ->
    S1 = peers(),
    {S2, Actions, []} = process(S1, id(9), ihave(msg_id_of(publish_frame()))),
    ?assertEqual([{refused, id(9), not_a_peer}], Actions),
    ?assertEqual({sets_of(S1), 0, 0},
                 {sets_of(S2), hecate_plumtree:missing_count(S2), hecate_plumtree:open_count(id(9), S2)}).

a_first_gossip_from_a_non_peer_delivers_and_forwards_but_moves_no_one_test() ->
    S1 = peers(),
    Publish = publish_frame(),
    Id = msg_id_of(Publish),
    {S2, Actions, Deliveries} = process(S1, id(9), gossip_of(Publish, 1)),
    ?assertMatch([{Id, _Verified}], Deliveries),
    ?assertEqual(sets_of(S1), sets_of(S2)),
    ?assertEqual([id(1), id(2)], lists:sort([To || {send, To, _Frame} <- Actions])).

a_duplicate_gossip_from_a_non_peer_gets_no_prune_test() ->
    Publish = publish_frame(),
    {S1, _Pushes, _Delivered} = process(peers(), id(1), gossip_of(Publish, 1)),
    {S2, Actions, []} = process(S1, id(9), gossip_of(Publish, 2)),
    ?assertEqual([], Actions),
    ?assertEqual(sets_of(S1), sets_of(S2)).

the_same_frames_from_a_peer_still_move_it_test() ->
    Publish = publish_frame(),
    {S1, _Pushes, _Delivered} = hecate_plumtree:publish(peers(), Publish, erlang:system_time(millisecond)),
    {S2, GraftActions, []} = process(S1, id(2), graft(msg_id_of(Publish))),
    {S3, PruneActions, []} = process(S2, id(1), prune()),
    ?assertMatch([{send, _To, _Gossip}], GraftActions),
    ?assertEqual({[], {[id(2)], [id(1)]}}, {PruneActions, sets_of(S3)}).

%% A wrapping process reports every refusal Plumtree returns through macula_peering:object_refused/2, which asks
%% macula_frame:charged_refusal/1. That function classifies each kind, so no report crashes the wrapper, and a frame
%% from a non-peer is not charged.
every_refusal_kind_plumtree_returns_is_classified_test() ->
    Kinds = [ihave_allowance, graft_unanswered, wrong_realm, not_a_peer, malformed_frame, signature_invalid,
             key_id_mismatch, {not_yet_valid, 1}, {expired, 1}],
    ?assertEqual([true, true, true, false, true, true, true, false, false],
                 [macula_frame:charged_refusal(Kind) || Kind <- Kinds]).

%% A node with id(1) eager and id(2) lazy: both join eager, then id(2)'s PRUNE moves it to lazy.
peers() ->
    {ok, S0} = hecate_plumtree:new(id(99), ?REALM),
    S1 = hecate_plumtree:add_peer(hecate_plumtree:add_peer(S0, id(1)), id(2)),
    {S2, [], []} = process(S1, id(2), prune()),
    S2.

process(State, From, Frame) ->
    hecate_plumtree:process(State, From, Frame, #{wall => erlang:system_time(millisecond), monotonic => 0}).

sets_of(State) ->
    {lists:sort(hecate_plumtree:eager_peers(State)), lists:sort(hecate_plumtree:lazy_peers(State))}.

id(N) -> <<N:256>>.

%% A PUBLISH signed by a fresh publisher in the node's configured profile.
publish_frame() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    macula_frame:publish(#{realm => ?REALM, topic => <<"news">>, seq => 1,
                           published_at => erlang:system_time(millisecond), payload => #{}}, Key).

msg_id_of(#{publication := #{tbs := Tbs}}) ->
    crypto:hash(sha384, Tbs).

ihave(MsgId) ->
    macula_frame:plumtree_ihave(#{realm => ?REALM, msg_id => MsgId, round => 1}).

graft(MsgId) ->
    macula_frame:plumtree_graft(#{realm => ?REALM, msg_id => MsgId, round => 1}).

prune() ->
    macula_frame:plumtree_prune(#{realm => ?REALM}).

gossip_of(#{publication := Publication}, Round) ->
    macula_frame:plumtree_gossip(#{publication => Publication, round => Round}).
