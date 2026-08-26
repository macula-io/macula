%% EUnit tests for hecate_plumtree.
-module(hecate_plumtree_tests).

-include_lib("eunit/include/eunit.hrl").

%%---------------------------------------------------------------------
%% Construction + view changes
%%---------------------------------------------------------------------

new_state_starts_empty_test() ->
    {Kp, S} = fresh(),
    ?assertEqual(macula_identity:public(Kp),
                 hecate_plumtree:self_id(S)),
    ?assertEqual([], hecate_plumtree:eager_peers(S)),
    ?assertEqual([], hecate_plumtree:lazy_peers(S)),
    ?assertEqual(0, hecate_plumtree:received_count(S)).

add_peer_lands_in_eager_set_test() ->
    {_Kp, S} = fresh(),
    S1 = hecate_plumtree:add_peer(S, id(1)),
    ?assertEqual([id(1)], hecate_plumtree:eager_peers(S1)).

remove_peer_drops_from_both_sets_test() ->
    {_Kp, S} = fresh(),
    S1 = hecate_plumtree:add_peer(S, id(1)),
    S2 = hecate_plumtree:remove_peer(S1, id(1)),
    ?assertEqual([], hecate_plumtree:eager_peers(S2)),
    ?assertEqual([], hecate_plumtree:lazy_peers(S2)).

%%---------------------------------------------------------------------
%% Local publish
%%---------------------------------------------------------------------

publish_emits_gossip_to_each_eager_peer_test() ->
    {_Kp, S0} = fresh(),
    S1 = lists:foldl(fun(P, V) -> hecate_plumtree:add_peer(V, P) end,
                     S0, [id(1), id(2), id(3)]),
    MsgId = msg_id(),
    {S2, Actions, Deliveries} =
        hecate_plumtree:publish(S1, MsgId, <<"hello">>),
    ?assertEqual([{MsgId, <<"hello">>}], Deliveries),
    %% Three GOSSIPs, one per eager peer.
    Sends = [F || {send, _, F} <- Actions,
                  macula_frame:frame_type(F) =:= plumtree_gossip],
    ?assertEqual(3, length(Sends)),
    %% Targets are the three peers.
    Targets = lists:sort([T || {send, T, _} <- Actions]),
    ?assertEqual(lists:sort([id(1), id(2), id(3)]), Targets),
    ?assert(hecate_plumtree:has_received(MsgId, S2)).

publish_emits_ihave_to_each_lazy_peer_test() ->
    {Kp, S0} = fresh(),
    %% Manually move a peer to lazy by simulating a duplicate
    %% gossip flow: add as eager, then a duplicate triggers
    %% PRUNE-style demotion. Easier shortcut: handle a PRUNE we
    %% receive from the peer, which moves it to lazy.
    S1 = hecate_plumtree:add_peer(S0, id(7)),
    PruneFrame = macula_frame:sign(macula_frame:plumtree_prune(
                                     #{realm => realm_for(S0)}), Kp),
    {S2, _, _} = hecate_plumtree:process(S1, id(7), PruneFrame),
    ?assertEqual([id(7)], hecate_plumtree:lazy_peers(S2)),
    %% Now publish — id(7) must receive an IHAVE, not a GOSSIP.
    {_, Actions, _} = hecate_plumtree:publish(S2, msg_id(), ok),
    [{send, T, F}] = Actions,
    ?assertEqual(id(7), T),
    ?assertEqual(plumtree_ihave, macula_frame:frame_type(F)).

%%---------------------------------------------------------------------
%% Receive GOSSIP — first time vs duplicate
%%---------------------------------------------------------------------

receive_first_gossip_delivers_and_forwards_test() ->
    {Kp, S0} = fresh(),
    Sender = id(11),
    Other  = id(12),
    %% Other peer in eager view; gossip should be re-forwarded to
    %% Other (not back to Sender).
    S1 = hecate_plumtree:add_peer(S0, Other),
    MsgId = msg_id(),
    Frame = macula_frame:sign(macula_frame:plumtree_gossip(
                                #{realm => realm_for(S0),
                                  msg_id => MsgId,
                                  round => 0,
                                  payload => <<"x">>}), Kp),
    {S2, Actions, Deliveries} =
        hecate_plumtree:process(S1, Sender, Frame),
    ?assertEqual([{MsgId, <<"x">>}], Deliveries),
    %% Sender added to eager.
    ?assert(lists:member(Sender, hecate_plumtree:eager_peers(S2))),
    %% Re-forwarded to Other only (not back to Sender).
    Targets = [T || {send, T, _} <- Actions],
    ?assert(lists:member(Other, Targets)),
    ?assertNot(lists:member(Sender, Targets)).

receive_duplicate_gossip_prunes_sender_test() ->
    {Kp, S0} = fresh(),
    Sender = id(20),
    %% Sender is currently eager.
    S1 = hecate_plumtree:add_peer(S0, Sender),
    %% First publish to mark MsgId received.
    MsgId = msg_id(),
    {S2, _, _} = hecate_plumtree:publish(S1, MsgId, <<"first">>),
    %% Now the sender duplicates the same GOSSIP.
    Dup = macula_frame:sign(macula_frame:plumtree_gossip(
                              #{realm => realm_for(S0),
                                msg_id => MsgId,
                                round => 1,
                                payload => <<"first">>}), Kp),
    {S3, Actions, Deliveries} =
        hecate_plumtree:process(S2, Sender, Dup),
    %% No new delivery.
    ?assertEqual([], Deliveries),
    %% PRUNE sent back to Sender.
    [{send, T, F}] = Actions,
    ?assertEqual(Sender, T),
    ?assertEqual(plumtree_prune, macula_frame:frame_type(F)),
    %% Sender now lazy, not eager.
    ?assert(lists:member(Sender, hecate_plumtree:lazy_peers(S3))),
    ?assertNot(lists:member(Sender, hecate_plumtree:eager_peers(S3))).

%%---------------------------------------------------------------------
%% Receive IHAVE
%%---------------------------------------------------------------------

ihave_for_unknown_msg_emits_graft_test() ->
    {Kp, S0} = fresh(),
    Sender = id(30),
    MsgId  = msg_id(),
    Frame  = macula_frame:sign(macula_frame:plumtree_ihave(
                                 #{realm => realm_for(S0),
                                   msg_id => MsgId,
                                   round => 2}), Kp),
    {S1, [{send, T, F}], []} =
        hecate_plumtree:process(S0, Sender, Frame),
    ?assertEqual(Sender, T),
    ?assertEqual(plumtree_graft, macula_frame:frame_type(F)),
    ?assertEqual(MsgId, maps:get(msg_id, F)),
    ?assertEqual(1, hecate_plumtree:missing_count(S1)).

ihave_for_known_msg_is_silent_test() ->
    {Kp, S0} = fresh(),
    Sender = id(31),
    MsgId  = msg_id(),
    {S1, _, _} = hecate_plumtree:publish(S0, MsgId, <<"already">>),
    Frame = macula_frame:sign(macula_frame:plumtree_ihave(
                                #{realm => realm_for(S0),
                                  msg_id => MsgId,
                                  round => 1}), Kp),
    {S2, [], []} = hecate_plumtree:process(S1, Sender, Frame),
    ?assertEqual(0, hecate_plumtree:missing_count(S2)).

%%---------------------------------------------------------------------
%% Receive GRAFT
%%---------------------------------------------------------------------

graft_for_known_msg_replies_with_gossip_test() ->
    {Kp, S0} = fresh(),
    Sender = id(40),
    MsgId  = msg_id(),
    {S1, _, _} = hecate_plumtree:publish(S0, MsgId, <<"payload">>),
    Frame = macula_frame:sign(macula_frame:plumtree_graft(
                                #{realm => realm_for(S0),
                                  msg_id => MsgId,
                                  round => 0}), Kp),
    {S2, [{send, T, F}], []} =
        hecate_plumtree:process(S1, Sender, Frame),
    ?assertEqual(Sender, T),
    ?assertEqual(plumtree_gossip, macula_frame:frame_type(F)),
    ?assertEqual(<<"payload">>, maps:get(payload, F)),
    %% Sender added to eager.
    ?assert(lists:member(Sender, hecate_plumtree:eager_peers(S2))).

graft_for_unknown_msg_is_silent_test() ->
    {Kp, S0} = fresh(),
    Sender = id(41),
    MsgId  = msg_id(),
    Frame = macula_frame:sign(macula_frame:plumtree_graft(
                                #{realm => realm_for(S0),
                                  msg_id => MsgId,
                                  round => 0}), Kp),
    {S1, [], []} = hecate_plumtree:process(S0, Sender, Frame),
    %% Sender still added to eager so future publishes reach them.
    ?assert(lists:member(Sender, hecate_plumtree:eager_peers(S1))).

%%---------------------------------------------------------------------
%% Receive PRUNE
%%---------------------------------------------------------------------

prune_demotes_sender_to_lazy_test() ->
    {Kp, S0} = fresh(),
    Sender = id(50),
    S1 = hecate_plumtree:add_peer(S0, Sender),
    Frame = macula_frame:sign(macula_frame:plumtree_prune(
                                #{realm => realm_for(S0)}), Kp),
    {S2, [], []} = hecate_plumtree:process(S1, Sender, Frame),
    ?assert(lists:member(Sender, hecate_plumtree:lazy_peers(S2))),
    ?assertNot(lists:member(Sender, hecate_plumtree:eager_peers(S2))).

%%---------------------------------------------------------------------
%% End-to-end: publish from A reaches C via B with no duplicates
%%---------------------------------------------------------------------

three_node_chain_delivers_message_once_each_test() ->
    {KpA, A0} = fresh(),
    {KpB, B0} = fresh(),
    {_KpC, C0} = fresh(),
    AId = hecate_plumtree:self_id(A0),
    BId = hecate_plumtree:self_id(B0),
    CId = hecate_plumtree:self_id(C0),
    Realm = realm_for(A0),
    %% Force B and C onto the same realm so frames decode the same.
    B1 = with_realm(B0, Realm),
    C1 = with_realm(C0, Realm),
    %% Topology: A ↔ B ↔ C (B is the bridge).
    A1 = hecate_plumtree:add_peer(A0, BId),
    B2 = lists:foldl(fun(P, V) -> hecate_plumtree:add_peer(V, P) end,
                     B1, [AId, CId]),
    C2 = hecate_plumtree:add_peer(C1, BId),
    MsgId = msg_id(),
    %% A publishes.
    {_AAfter, AActions, ADeliveries} =
        hecate_plumtree:publish(A1, MsgId, <<"hi">>),
    ?assertEqual([{MsgId, <<"hi">>}], ADeliveries),
    [{send, BId, GossipAB}] = strip_metadata(AActions, KpA),
    %% B receives, delivers, and forwards to C (not back to A).
    {_BAfter, BActions, BDeliveries} =
        hecate_plumtree:process(B2, AId, GossipAB),
    ?assertEqual([{MsgId, <<"hi">>}], BDeliveries),
    [{send, CId, GossipBC}] = strip_metadata(BActions, KpB),
    %% C receives and delivers; nowhere else to forward to.
    {_CAfter, CActions, CDeliveries} =
        hecate_plumtree:process(C2, BId, GossipBC),
    ?assertEqual([{MsgId, <<"hi">>}], CDeliveries),
    %% C may try to forward back to B but BId is the sender so it's
    %% excluded from the forwarding set.
    ?assertEqual([], CActions).

%%=====================================================================
%% Helpers
%%=====================================================================

fresh() ->
    Kp = macula_identity:generate(),
    {Kp, hecate_plumtree:new(Kp, crypto:strong_rand_bytes(32))}.

realm_for(S) -> hecate_plumtree:realm(S).

with_realm(S, R) -> S#{realm := R}.

id(N) -> <<N:256>>.

msg_id() -> crypto:strong_rand_bytes(16).

%% Filter actions to just GOSSIP/IHAVE for forwarding inspection.
strip_metadata(Actions, _Kp) ->
    [A || {send, _, F} = A <- Actions,
          (macula_frame:frame_type(F) =:= plumtree_gossip
           orelse macula_frame:frame_type(F) =:= plumtree_ihave)].
