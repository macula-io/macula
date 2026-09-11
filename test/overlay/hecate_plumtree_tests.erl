%% EUnit tests for hecate_plumtree. A message is a publication signed by its publisher, its id is the SHA-384 of the
%% publication's tbs, and every node verifies a publication once before it delivers or forwards it. Frames carry no
%% signature of their own: the connection adds neighbour signatures (D17).
-module(hecate_plumtree_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<42:256>>).
-define(OTHER_REALM, <<77:256>>).

%%---------------------------------------------------------------------
%% Construction + view changes
%%---------------------------------------------------------------------

new_state_starts_empty_test() ->
    S = fresh(id(99)),
    ?assertEqual(id(99), hecate_plumtree:self_id(S)),
    ?assertEqual(?REALM, hecate_plumtree:realm(S)),
    ?assertEqual([], hecate_plumtree:eager_peers(S)),
    ?assertEqual([], hecate_plumtree:lazy_peers(S)),
    ?assertEqual(0, hecate_plumtree:received_count(S)).

a_node_without_a_crypto_profile_does_not_start_test() ->
    Prev = application:get_env(macula, crypto_profile),
    try
        ok = application:unset_env(macula, crypto_profile),
        ?assertEqual({error, crypto_profile_missing}, hecate_plumtree:new(id(99), ?REALM))
    after
        restore_profile(Prev)
    end.

add_peer_lands_in_eager_set_test() ->
    S1 = hecate_plumtree:add_peer(fresh(id(99)), id(1)),
    ?assertEqual([id(1)], hecate_plumtree:eager_peers(S1)).

remove_peer_drops_from_both_sets_test() ->
    S1 = hecate_plumtree:add_peer(fresh(id(99)), id(1)),
    S2 = hecate_plumtree:remove_peer(S1, id(1)),
    ?assertEqual([], hecate_plumtree:eager_peers(S2)),
    ?assertEqual([], hecate_plumtree:lazy_peers(S2)).

%%---------------------------------------------------------------------
%% Local publish
%%---------------------------------------------------------------------

publish_emits_gossip_to_each_eager_peer_test() ->
    S1 = with_peers(fresh(id(99)), [id(1), id(2), id(3)]),
    Publish = publish_frame(<<"hello">>),
    MsgId = msg_id_of(Publish),
    {S2, Actions, Deliveries} = hecate_plumtree:publish(S1, Publish),
    ?assertMatch([{MsgId, #{payload := <<"hello">>}}], Deliveries),
    %% Three GOSSIPs, one per eager peer.
    Sends = [F || {send, _, F} <- Actions, macula_frame:frame_type(F) =:= plumtree_gossip],
    ?assertEqual(3, length(Sends)),
    ?assertEqual(lists:sort([id(1), id(2), id(3)]), lists:sort([T || {send, T, _} <- Actions])),
    ?assert(hecate_plumtree:has_received(MsgId, S2)).

publish_emits_ihave_to_each_lazy_peer_test() ->
    %% A PRUNE from the peer moves it to lazy.
    S1 = hecate_plumtree:add_peer(fresh(id(99)), id(7)),
    {S2, _, _} = hecate_plumtree:process(S1, id(7), macula_frame:plumtree_prune(#{realm => ?REALM})),
    ?assertEqual([id(7)], hecate_plumtree:lazy_peers(S2)),
    %% Now publish: id(7) receives an IHAVE, not a GOSSIP.
    {_, Actions, _} = hecate_plumtree:publish(S2, publish_frame(<<"ok">>)),
    [{send, T, F}] = Actions,
    ?assertEqual(id(7), T),
    ?assertEqual(plumtree_ihave, macula_frame:frame_type(F)).

a_local_publish_that_does_not_verify_is_refused_test() ->
    ?assertEqual({error, signature_invalid}, hecate_plumtree:publish(fresh(id(99)), tampered(publish_frame(<<"x">>)))).

a_local_publish_for_another_realm_is_refused_test() ->
    ?assertEqual({error, wrong_realm}, hecate_plumtree:publish(fresh(id(99)), publish_frame(?OTHER_REALM, <<"x">>))).

the_message_id_is_the_sha384_of_the_publication_tbs_test() ->
    #{publication := #{tbs := Tbs}} = Publish = publish_frame(<<"id">>),
    {S1, _, [{MsgId, _}]} = hecate_plumtree:publish(fresh(id(99)), Publish),
    ?assertEqual(crypto:hash(sha384, Tbs), MsgId),
    ?assert(hecate_plumtree:has_received(MsgId, S1)).

%%---------------------------------------------------------------------
%% Receive GOSSIP: first time, duplicate, refused
%%---------------------------------------------------------------------

receive_first_gossip_delivers_and_forwards_test() ->
    Sender = id(11),
    Other = id(12),
    %% Other peer in eager view; gossip is re-forwarded to Other, not back to Sender.
    S1 = hecate_plumtree:add_peer(fresh(id(99)), Other),
    Publish = publish_frame(<<"x">>),
    MsgId = msg_id_of(Publish),
    {S2, Actions, Deliveries} = hecate_plumtree:process(S1, Sender, gossip_of(Publish, 0)),
    ?assertMatch([{MsgId, #{payload := <<"x">>}}], Deliveries),
    ?assert(lists:member(Sender, hecate_plumtree:eager_peers(S2))),
    Targets = [T || {send, T, _} <- Actions],
    ?assert(lists:member(Other, Targets)),
    ?assertNot(lists:member(Sender, Targets)).

a_forwarded_gossip_carries_the_publication_bytes_unchanged_test() ->
    S1 = hecate_plumtree:add_peer(fresh(id(99)), id(12)),
    #{publication := Publication} = Publish = publish_frame(<<"x">>),
    {_S2, [{send, _, Forward}], _} = hecate_plumtree:process(S1, id(11), gossip_of(Publish, 0)),
    ?assertEqual(Publication, maps:get(publication, Forward)),
    ?assertEqual(1, maps:get(round, Forward)),
    ?assertNot(maps:is_key(signature, Forward)).

receive_duplicate_gossip_prunes_sender_test() ->
    Sender = id(20),
    S1 = hecate_plumtree:add_peer(fresh(id(99)), Sender),
    Publish = publish_frame(<<"first">>),
    {S2, _, _} = hecate_plumtree:publish(S1, Publish),
    %% The sender duplicates the same GOSSIP.
    {S3, Actions, Deliveries} = hecate_plumtree:process(S2, Sender, gossip_of(Publish, 1)),
    ?assertEqual([], Deliveries),
    [{send, T, F}] = Actions,
    ?assertEqual(Sender, T),
    ?assertEqual(plumtree_prune, macula_frame:frame_type(F)),
    ?assert(lists:member(Sender, hecate_plumtree:lazy_peers(S3))),
    ?assertNot(lists:member(Sender, hecate_plumtree:eager_peers(S3))).

a_publication_reaching_a_node_twice_is_verified_once_test() ->
    Publish = publish_frame(<<"twice">>),
    S1 = hecate_plumtree:add_peer(fresh(id(99)), id(21)),
    ok = meck:new(macula_frame, [passthrough]),
    try
        {S2, _, [_]} = hecate_plumtree:process(S1, id(20), gossip_of(Publish, 0)),
        {_S3, _, []} = hecate_plumtree:process(S2, id(21), gossip_of(Publish, 1)),
        ?assertEqual(1, meck:num_calls(macula_frame, verify_publication, '_'))
    after
        meck:unload(macula_frame)
    end.

a_gossip_whose_publication_does_not_verify_is_dropped_test() ->
    S1 = hecate_plumtree:add_peer(fresh(id(99)), id(12)),
    #{publication := Bad} = tampered(publish_frame(<<"x">>)),
    Gossip = macula_frame:plumtree_gossip(#{publication => Bad, round => 0}),
    {S2, Actions, Deliveries} = hecate_plumtree:process(S1, id(11), Gossip),
    ?assertEqual({[], []}, {Actions, Deliveries}),
    ?assertEqual(0, hecate_plumtree:received_count(S2)),
    ?assertNot(lists:member(id(11), hecate_plumtree:eager_peers(S2))).

a_gossip_for_another_realm_is_dropped_test() ->
    Gossip = gossip_of(publish_frame(?OTHER_REALM, <<"x">>), 0),
    {S1, Actions, Deliveries} = hecate_plumtree:process(fresh(id(99)), id(11), Gossip),
    ?assertEqual({[], []}, {Actions, Deliveries}),
    ?assertEqual(0, hecate_plumtree:received_count(S1)).

%%---------------------------------------------------------------------
%% Receive IHAVE
%%---------------------------------------------------------------------

ihave_for_unknown_msg_emits_graft_test() ->
    Sender = id(30),
    MsgId = crypto:strong_rand_bytes(48),
    Frame = macula_frame:plumtree_ihave(#{realm => ?REALM, msg_id => MsgId, round => 2}),
    {S1, [{send, T, F}], []} = hecate_plumtree:process(fresh(id(99)), Sender, Frame),
    ?assertEqual(Sender, T),
    ?assertEqual(plumtree_graft, macula_frame:frame_type(F)),
    ?assertEqual(MsgId, maps:get(msg_id, F)),
    ?assertEqual(1, hecate_plumtree:missing_count(S1)).

ihave_for_known_msg_is_silent_test() ->
    Publish = publish_frame(<<"already">>),
    {S1, _, _} = hecate_plumtree:publish(fresh(id(99)), Publish),
    Frame = macula_frame:plumtree_ihave(#{realm => ?REALM, msg_id => msg_id_of(Publish), round => 1}),
    {S2, [], []} = hecate_plumtree:process(S1, id(31), Frame),
    ?assertEqual(0, hecate_plumtree:missing_count(S2)).

%%---------------------------------------------------------------------
%% Receive GRAFT
%%---------------------------------------------------------------------

graft_for_known_msg_replies_with_gossip_test() ->
    Sender = id(40),
    #{publication := Publication} = Publish = publish_frame(<<"payload">>),
    {S1, _, _} = hecate_plumtree:publish(fresh(id(99)), Publish),
    Frame = macula_frame:plumtree_graft(#{realm => ?REALM, msg_id => msg_id_of(Publish), round => 0}),
    {S2, [{send, T, F}], []} = hecate_plumtree:process(S1, Sender, Frame),
    ?assertEqual(Sender, T),
    ?assertEqual(plumtree_gossip, macula_frame:frame_type(F)),
    ?assertEqual(Publication, maps:get(publication, F)),
    %% Sender added to eager.
    ?assert(lists:member(Sender, hecate_plumtree:eager_peers(S2))).

graft_for_unknown_msg_is_silent_test() ->
    Sender = id(41),
    Frame = macula_frame:plumtree_graft(#{realm => ?REALM, msg_id => crypto:strong_rand_bytes(48), round => 0}),
    {S1, [], []} = hecate_plumtree:process(fresh(id(99)), Sender, Frame),
    %% Sender still added to eager so future publishes reach them.
    ?assert(lists:member(Sender, hecate_plumtree:eager_peers(S1))).

%%---------------------------------------------------------------------
%% Retention: a publication hash is kept until the publication expires, and no longer
%%---------------------------------------------------------------------

sweep_keeps_a_publication_through_its_expiry_and_forgets_it_after_test() ->
    Publish = publish_frame(<<"kept">>),
    MsgId = msg_id_of(Publish),
    {S1, _, [{MsgId, #{expires_at := ExpiresAt}}]} = hecate_plumtree:publish(fresh(id(99)), Publish),
    ?assert(hecate_plumtree:has_received(MsgId, hecate_plumtree:sweep(S1, ExpiresAt))),
    S2 = hecate_plumtree:sweep(S1, ExpiresAt + 1),
    ?assertNot(hecate_plumtree:has_received(MsgId, S2)),
    ?assertEqual(0, hecate_plumtree:received_count(S2)).

sweep_forgets_an_expired_publication_and_keeps_a_live_one_test() ->
    Short = publish_frame(?REALM, <<"short">>, #{ttl_ms => 1000}),
    Long = publish_frame(<<"long">>),
    {S1, _, [{ShortId, #{expires_at := ShortExpiry}}]} = hecate_plumtree:publish(fresh(id(99)), Short),
    {S2, _, [{LongId, _}]} = hecate_plumtree:publish(S1, Long),
    S3 = hecate_plumtree:sweep(S2, ShortExpiry + 1),
    ?assertNot(hecate_plumtree:has_received(ShortId, S3)),
    ?assert(hecate_plumtree:has_received(LongId, S3)),
    ?assertEqual(1, hecate_plumtree:received_count(S3)).

a_graft_for_a_forgotten_publication_gets_no_answer_test() ->
    Publish = publish_frame(<<"forgotten">>),
    MsgId = msg_id_of(Publish),
    {S1, _, [{MsgId, #{expires_at := ExpiresAt}}]} = hecate_plumtree:publish(fresh(id(99)), Publish),
    S2 = hecate_plumtree:sweep(S1, ExpiresAt + 1),
    Frame = macula_frame:plumtree_graft(#{realm => ?REALM, msg_id => MsgId, round => 0}),
    ?assertMatch({_, [], []}, hecate_plumtree:process(S2, id(42), Frame)).

%%---------------------------------------------------------------------
%% Receive PRUNE
%%---------------------------------------------------------------------

prune_demotes_sender_to_lazy_test() ->
    Sender = id(50),
    S1 = hecate_plumtree:add_peer(fresh(id(99)), Sender),
    {S2, [], []} = hecate_plumtree:process(S1, Sender, macula_frame:plumtree_prune(#{realm => ?REALM})),
    ?assert(lists:member(Sender, hecate_plumtree:lazy_peers(S2))),
    ?assertNot(lists:member(Sender, hecate_plumtree:eager_peers(S2))).

%%---------------------------------------------------------------------
%% End-to-end: publish from A reaches C via B with no duplicates
%%---------------------------------------------------------------------

three_node_chain_delivers_message_once_each_test() ->
    %% Topology: A and B, B and C (B is the bridge).
    A1 = hecate_plumtree:add_peer(fresh(id(1)), id(2)),
    B1 = with_peers(fresh(id(2)), [id(1), id(3)]),
    C1 = hecate_plumtree:add_peer(fresh(id(3)), id(2)),
    Publish = publish_frame(<<"hi">>),
    MsgId = msg_id_of(Publish),
    %% A publishes.
    {_AAfter, AActions, [{MsgId, _}]} = hecate_plumtree:publish(A1, Publish),
    [{send, BId, GossipAB}] = gossip_and_ihave(AActions),
    ?assertEqual(id(2), BId),
    %% B receives the frame as the wire hands it over, delivers, and forwards to C, not back to A.
    {_BAfter, BActions, [{MsgId, _}]} = hecate_plumtree:process(B1, id(1), wire(GossipAB)),
    [{send, CId, GossipBC}] = gossip_and_ihave(BActions),
    ?assertEqual(id(3), CId),
    %% C receives and delivers; its only peer is the sender, so it forwards nowhere.
    {_CAfter, CActions, [{MsgId, _}]} = hecate_plumtree:process(C1, id(2), wire(GossipBC)),
    ?assertEqual([], CActions).

%%=====================================================================
%% Helpers
%%=====================================================================

fresh(SelfId) ->
    {ok, S} = hecate_plumtree:new(SelfId, ?REALM),
    S.

with_peers(S, Peers) ->
    lists:foldl(fun(P, Acc) -> hecate_plumtree:add_peer(Acc, P) end, S, Peers).

id(N) -> <<N:256>>.

publish_frame(Payload) ->
    publish_frame(?REALM, Payload).

%% A PUBLISH signed by a fresh publisher in the node's configured profile.
publish_frame(Realm, Payload) ->
    publish_frame(Realm, Payload, #{}).

publish_frame(Realm, Payload, Extra) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    Spec = #{realm => Realm, topic => <<"news">>, seq => 1, published_at => erlang:system_time(millisecond),
             payload => Payload},
    macula_frame:publish(maps:merge(Spec, Extra), Key).

msg_id_of(#{publication := #{tbs := Tbs}}) ->
    crypto:hash(sha384, Tbs).

gossip_of(#{publication := Publication}, Round) ->
    macula_frame:plumtree_gossip(#{publication => Publication, round => Round}).

tampered(#{publication := #{tbs := <<Head:20/binary, Byte, Tail/binary>>} = Publication} = Frame) ->
    Frame#{publication := Publication#{tbs := <<Head/binary, (Byte bxor 1), Tail/binary>>}}.

wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

%% Filter actions to just GOSSIP/IHAVE for forwarding inspection.
gossip_and_ihave(Actions) ->
    [A || {send, _, F} = A <- Actions, lists:member(macula_frame:frame_type(F), [plumtree_gossip, plumtree_ihave])].

restore_profile(undefined) -> application:unset_env(macula, crypto_profile);
restore_profile({ok, Profile}) -> application:set_env(macula, crypto_profile, Profile).
