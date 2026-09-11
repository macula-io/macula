%% EUnit tests for macula_hyparview_proto. Peers are node_ids; frames leave the protocol without a signature of their
%% own, since the connection that sends a frame adds its neighbour signature (D17); and a gated context admits a peer
%% only on an endorsement signed by the realm key whose key id it names, for that very peer.
-module(macula_hyparview_proto_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SELF, <<99:256>>).

%%---------------------------------------------------------------------
%% Outbound builders
%%---------------------------------------------------------------------

build_join_returns_a_join_without_a_signature_test() ->
    Frame = macula_hyparview_proto:build_join(ctx(?SELF)),
    ?assertEqual(hyparview_join, macula_frame:frame_type(Frame)),
    ?assertEqual(?SELF, maps:get(new_member, Frame)),
    ?assertNot(maps:is_key(signature, Frame)).

frames_the_protocol_sends_carry_no_signature_test() ->
    View0 = lists:foldl(fun(P, V) -> macula_hyparview_view:add_active(V, P) end,
                        macula_hyparview_view:new(?SELF, #{active_cap => 2, passive_cap => 20}),
                        [id(10), id(11)]),
    Joiner = id(1),
    Join = macula_hyparview_proto:build_join(ctx(Joiner)),
    {_View1, Actions} = macula_hyparview_proto:process(View0, Joiner, Join, ctx(?SELF)),
    ?assertMatch([_, _ | _], Actions),
    ?assertEqual([], [F || {send, _, F} <- Actions, maps:is_key(signature, F)]).

%%---------------------------------------------------------------------
%% JOIN handler
%%---------------------------------------------------------------------

join_adds_sender_to_active_and_replies_neighbor_high_test() ->
    Joiner = id(1),
    View0 = macula_hyparview_view:new(?SELF),
    Frame = macula_hyparview_proto:build_join(ctx(Joiner)),
    {View1, Actions} = macula_hyparview_proto:process(View0, Joiner, Frame, ctx(?SELF)),
    ?assert(macula_hyparview_view:is_active(Joiner, View1)),
    %% At least one action: NEIGHBOR(high) to the joiner.
    ?assert(lists:any(
              fun({send, T, F}) ->
                  T =:= Joiner andalso
                  macula_frame:frame_type(F) =:= hyparview_neighbor andalso
                  maps:get(priority, F) =:= high
              end, Actions)).

join_with_existing_actives_forwards_join_test() ->
    %% Pre-populate two existing active peers.
    View0 = lists:foldl(fun(P, V) -> macula_hyparview_view:add_active(V, P) end,
                        macula_hyparview_view:new(?SELF, #{active_cap => 5, passive_cap => 20}),
                        [id(10), id(11)]),
    Joiner = id(1),
    Frame = macula_hyparview_proto:build_join(ctx(Joiner)),
    {_View1, Actions} = macula_hyparview_proto:process(View0, Joiner, Frame, ctx(?SELF)),
    Forwards = [A || {send, T, F} = A <- Actions,
                     macula_frame:frame_type(F) =:= hyparview_forward_join,
                     T =/= Joiner],
    ?assertEqual(2, length(Forwards)),
    %% Each forward carries Joiner as new_member.
    [?assertEqual(Joiner, maps:get(new_member, F)) || {send, _, F} <- Forwards].

%%---------------------------------------------------------------------
%% FORWARD_JOIN handler
%%---------------------------------------------------------------------

forward_join_with_zero_ttl_accepts_test() ->
    NewM = id(7),
    Sender = id(8),
    View0 = macula_hyparview_view:new(?SELF),
    Frame = forward_join(NewM, 0, 6, 3),
    {View1, Actions} = macula_hyparview_proto:process(View0, Sender, Frame, ctx(?SELF)),
    ?assert(macula_hyparview_view:is_active(NewM, View1)),
    %% Reply NEIGHBOR(high) to the new member.
    ?assert(lists:any(fun({send, T, F}) ->
                          T =:= NewM andalso macula_frame:frame_type(F) =:= hyparview_neighbor
                      end, Actions)).

forward_join_with_empty_active_accepts_test() ->
    NewM = id(7),
    Sender = id(8),
    View0 = macula_hyparview_view:new(?SELF),
    Frame = forward_join(NewM, 6, 6, 3),
    {View1, _} = macula_hyparview_proto:process(View0, Sender, Frame, ctx(?SELF)),
    %% Active view was empty: accept regardless of TTL.
    ?assert(macula_hyparview_view:is_active(NewM, View1)).

forward_join_at_prwl_threshold_adds_to_passive_test() ->
    NewM = id(7),
    Sender = id(8),
    Other = id(9),
    %% Two existing actives so we don't take the empty-view path.
    View0 = lists:foldl(fun(P, V) -> macula_hyparview_view:add_active(V, P) end,
                        macula_hyparview_view:new(?SELF), [Sender, Other]),
    %% PRWL = 3, set ttl == prwl so the passive-add fires.
    Frame = forward_join(NewM, 3, 6, 3),
    {View1, _Actions} = macula_hyparview_proto:process(View0, Sender, Frame, ctx(?SELF)),
    ?assert(macula_hyparview_view:is_passive(NewM, View1)).

forward_join_above_zero_ttl_forwards_to_random_active_test() ->
    NewM = id(7),
    Sender = id(8),
    Other = id(9),
    View0 = lists:foldl(fun(P, V) -> macula_hyparview_view:add_active(V, P) end,
                        macula_hyparview_view:new(?SELF), [Sender, Other]),
    Frame = forward_join(NewM, 5, 6, 3),
    {_, Actions} = macula_hyparview_proto:process(View0, Sender, Frame, ctx(?SELF)),
    %% A single forward to Other (excludes Sender and NewM).
    [{send, T, F}] = Actions,
    ?assertEqual(Other, T),
    ?assertEqual(hyparview_forward_join, macula_frame:frame_type(F)),
    %% TTL decremented.
    ?assertEqual(4, maps:get(ttl, F)).

%%---------------------------------------------------------------------
%% NEIGHBOR handler
%%---------------------------------------------------------------------

neighbor_high_always_admits_test() ->
    Sender = id(1),
    View0 = macula_hyparview_view:new(?SELF, #{active_cap => 1, passive_cap => 4}),
    %% Pre-fill active so admission requires eviction.
    View1 = macula_hyparview_view:add_active(View0, id(2)),
    Ctx = ctx(?SELF),
    {View2, _} = macula_hyparview_proto:process(View1, Sender, neighbor_frame(high, Ctx), Ctx),
    ?assert(macula_hyparview_view:is_active(Sender, View2)),
    %% The pre-existing active was demoted to passive.
    ?assert(macula_hyparview_view:is_passive(id(2), View2)).

neighbor_low_admits_only_if_room_test() ->
    View0 = macula_hyparview_view:new(?SELF, #{active_cap => 1, passive_cap => 4}),
    View1 = macula_hyparview_view:add_active(View0, id(2)),
    Ctx = ctx(?SELF),
    {View2, _} = macula_hyparview_proto:process(View1, id(3), neighbor_frame(low, Ctx), Ctx),
    %% Active is full; low priority goes to passive.
    ?assertNot(macula_hyparview_view:is_active(id(3), View2)),
    ?assert(macula_hyparview_view:is_passive(id(3), View2)).

%%---------------------------------------------------------------------
%% DISCONNECT handler
%%---------------------------------------------------------------------

disconnect_demotes_sender_test() ->
    Sender = id(5),
    View0 = macula_hyparview_view:add_active(macula_hyparview_view:new(?SELF), Sender),
    Ctx = ctx(?SELF),
    Frame = macula_frame:hyparview_disconnect(#{realm => maps:get(realm, Ctx)}),
    {View1, _} = macula_hyparview_proto:process(View0, Sender, Frame, Ctx),
    ?assertNot(macula_hyparview_view:is_active(Sender, View1)),
    ?assert(macula_hyparview_view:is_passive(Sender, View1)).

%%---------------------------------------------------------------------
%% SHUFFLE / SHUFFLE_REPLY handlers
%%---------------------------------------------------------------------

shuffle_with_zero_ttl_replies_and_merges_test() ->
    Origin = id(20),
    Sender = id(21),
    Sample = [id(30), id(31)],
    View0 = macula_hyparview_view:add_passive(macula_hyparview_view:new(?SELF), id(40)),
    Ctx = ctx(?SELF),
    Frame = macula_frame:hyparview_shuffle(#{realm => maps:get(realm, Ctx), origin => Origin, ttl => 0,
                                             peer_sample => Sample}),
    {View1, [{send, T, ReplyFrame}]} = macula_hyparview_proto:process(View0, Sender, Frame, Ctx),
    ?assertEqual(Origin, T),
    ?assertEqual(hyparview_shuffle_reply, macula_frame:frame_type(ReplyFrame)),
    %% Incoming sample peers merged into passive view.
    ?assert(macula_hyparview_view:is_passive(id(30), View1)),
    ?assert(macula_hyparview_view:is_passive(id(31), View1)).

shuffle_with_positive_ttl_forwards_to_random_active_test() ->
    Origin = id(50),
    Sender = id(51),
    Other = id(52),
    View0 = lists:foldl(fun(P, V) -> macula_hyparview_view:add_active(V, P) end,
                        macula_hyparview_view:new(?SELF), [Sender, Other]),
    Ctx = ctx(?SELF),
    Frame = macula_frame:hyparview_shuffle(#{realm => maps:get(realm, Ctx), origin => Origin, ttl => 3,
                                             peer_sample => []}),
    {_, [{send, T, F}]} = macula_hyparview_proto:process(View0, Sender, Frame, Ctx),
    ?assertEqual(Other, T),
    ?assertEqual(hyparview_shuffle, macula_frame:frame_type(F)),
    ?assertEqual(2, maps:get(ttl, F)).

shuffle_reply_merges_into_passive_test() ->
    Sender = id(60),
    Sample = [id(70), id(71)],
    View0 = macula_hyparview_view:new(?SELF),
    Ctx = ctx(?SELF),
    Frame = macula_frame:hyparview_shuffle_reply(#{realm => maps:get(realm, Ctx), peer_sample => Sample}),
    {View1, []} = macula_hyparview_proto:process(View0, Sender, Frame, Ctx),
    ?assert(macula_hyparview_view:is_passive(id(70), View1)),
    ?assert(macula_hyparview_view:is_passive(id(71), View1)).

%%=====================================================================
%% Admission gating (realm_key_id and profile in ctx())
%%
%% Every test above builds ctx() without realm_key_id and passes, which
%% confirms gating is opt-in.
%%=====================================================================

gated_join_with_valid_endorsement_admits_test() ->
    {RealmKey, RealmId} = realm(),
    Joiner = id(1),
    Ctx = gated_ctx(RealmKey, RealmId),
    Frame = macula_frame:hyparview_join(#{realm => RealmId, new_member => Joiner,
                                          record => endorsement(RealmKey, RealmId, Joiner)}),
    {View1, Actions} = macula_hyparview_proto:process(macula_hyparview_view:new(?SELF), Joiner, Frame, Ctx),
    ?assert(macula_hyparview_view:is_active(Joiner, View1)),
    ?assert(lists:any(fun({send, T, F}) ->
                          T =:= Joiner andalso macula_frame:frame_type(F) =:= hyparview_neighbor
                      end, Actions)).

gated_join_without_endorsement_is_dropped_test() ->
    {RealmKey, RealmId} = realm(),
    Joiner = id(1),
    Frame = macula_frame:hyparview_join(#{realm => RealmId, new_member => Joiner}),
    {View1, Actions} = macula_hyparview_proto:process(macula_hyparview_view:new(?SELF), Joiner, Frame,
                                                      gated_ctx(RealmKey, RealmId)),
    ?assertNot(macula_hyparview_view:is_active(Joiner, View1)),
    ?assertEqual([], Actions).

gated_join_with_wrong_signer_is_dropped_test() ->
    {RealmKey, RealmId} = realm(),
    {Impostor, _} = realm(),
    Joiner = id(1),
    Frame = macula_frame:hyparview_join(#{realm => RealmId, new_member => Joiner,
                                          record => endorsement(Impostor, RealmId, Joiner)}),
    {View1, Actions} = macula_hyparview_proto:process(macula_hyparview_view:new(?SELF), Joiner, Frame,
                                                      gated_ctx(RealmKey, RealmId)),
    ?assertNot(macula_hyparview_view:is_active(Joiner, View1)),
    ?assertEqual([], Actions).

%% The endorsement must name the peer the JOIN arrived from, not only some member of the realm.
gated_join_with_an_endorsement_for_another_node_is_dropped_test() ->
    {RealmKey, RealmId} = realm(),
    Joiner = id(1),
    Frame = macula_frame:hyparview_join(#{realm => RealmId, new_member => Joiner,
                                          record => endorsement(RealmKey, RealmId, id(2))}),
    {View1, Actions} = macula_hyparview_proto:process(macula_hyparview_view:new(?SELF), Joiner, Frame,
                                                      gated_ctx(RealmKey, RealmId)),
    ?assertNot(macula_hyparview_view:is_active(Joiner, View1)),
    ?assertEqual([], Actions).

gated_forward_join_carries_and_verifies_endorsement_test() ->
    {RealmKey, RealmId} = realm(),
    Sender = id(1),
    NewMember = id(3),
    %% ttl=0 takes accept_into_active's endorsement-check path.
    Frame = macula_frame:hyparview_forward_join(#{realm => RealmId, new_member => NewMember, ttl => 0, arwl => 6,
                                                  prwl => 3, record => endorsement(RealmKey, RealmId, NewMember)}),
    {View1, _Actions} = macula_hyparview_proto:process(macula_hyparview_view:new(?SELF), Sender, Frame,
                                                       gated_ctx(RealmKey, RealmId)),
    ?assert(macula_hyparview_view:is_active(NewMember, View1)).

gated_forward_join_without_endorsement_is_dropped_test() ->
    {RealmKey, RealmId} = realm(),
    Sender = id(1),
    NewMember = id(3),
    Frame = macula_frame:hyparview_forward_join(#{realm => RealmId, new_member => NewMember, ttl => 0, arwl => 6,
                                                  prwl => 3}),
    {View1, Actions} = macula_hyparview_proto:process(macula_hyparview_view:new(?SELF), Sender, Frame,
                                                      gated_ctx(RealmKey, RealmId)),
    ?assertNot(macula_hyparview_view:is_active(NewMember, View1)),
    ?assertEqual([], Actions).

%% forward_join_to_others (the fan-out on admission) threads the inbound JOIN's endorsement into the frames it sends
%% onward, which is what lets downstream peers verify it, not just this one.
gated_join_forwards_carry_the_endorsement_test() ->
    {RealmKey, RealmId} = realm(),
    Joiner = id(1),
    Endorsement = endorsement(RealmKey, RealmId, Joiner),
    View0 = lists:foldl(fun(P, V) -> macula_hyparview_view:add_active(V, P) end,
                        macula_hyparview_view:new(?SELF, #{active_cap => 5, passive_cap => 20}),
                        [id(10), id(11)]),
    Frame = macula_frame:hyparview_join(#{realm => RealmId, new_member => Joiner, record => Endorsement}),
    {_View1, Actions} = macula_hyparview_proto:process(View0, Joiner, Frame, gated_ctx(RealmKey, RealmId)),
    Forwards = [F || {send, _, F} <- Actions, macula_frame:frame_type(F) =:= hyparview_forward_join],
    ?assertEqual(2, length(Forwards)),
    [?assertEqual(Endorsement, maps:get(record, F)) || F <- Forwards].

gated_neighbor_high_with_valid_endorsement_admits_test() ->
    {RealmKey, RealmId} = realm(),
    Sender = id(4),
    Frame = macula_frame:hyparview_neighbor(#{realm => RealmId, priority => high,
                                              record => endorsement(RealmKey, RealmId, Sender)}),
    {View1, _Actions} = macula_hyparview_proto:process(macula_hyparview_view:new(?SELF), Sender, Frame,
                                                       gated_ctx(RealmKey, RealmId)),
    ?assert(macula_hyparview_view:is_active(Sender, View1)).

gated_neighbor_high_without_endorsement_is_dropped_test() ->
    {RealmKey, RealmId} = realm(),
    Sender = id(4),
    Frame = macula_frame:hyparview_neighbor(#{realm => RealmId, priority => high}),
    {View1, Actions} = macula_hyparview_proto:process(macula_hyparview_view:new(?SELF), Sender, Frame,
                                                      gated_ctx(RealmKey, RealmId)),
    ?assertNot(macula_hyparview_view:is_active(Sender, View1)),
    ?assertEqual([], Actions).

%% neighbor/3 (the outbound builder used for JOIN/FORWARD_JOIN's own acks) attaches self_endorsement when ctx() carries
%% one, so a gated receiver on the other end doesn't drop our own ack.
neighbor_builder_attaches_self_endorsement_test() ->
    {RealmKey, RealmId} = realm(),
    SelfEndorsement = endorsement(RealmKey, RealmId, ?SELF),
    Ctx = (ctx(?SELF))#{realm => RealmId, self_endorsement => SelfEndorsement},
    JoinFrame = macula_frame:hyparview_join(#{realm => RealmId, new_member => id(1)}),
    {_View1, Actions} = macula_hyparview_proto:process(macula_hyparview_view:new(?SELF), id(1), JoinFrame, Ctx),
    [{send, _, Ack}] = [A || {send, _, F} = A <- Actions, macula_frame:frame_type(F) =:= hyparview_neighbor],
    ?assertEqual(SelfEndorsement, maps:get(record, Ack)).

%%=====================================================================
%% Helpers
%%=====================================================================

id(N) -> <<N:256>>.

ctx(Self) ->
    #{self_id => Self, realm => crypto:strong_rand_bytes(32)}.

gated_ctx(RealmKey, RealmId) ->
    (ctx(?SELF))#{realm => RealmId, realm_key_id => macula_node_keys:key_id(RealmKey), profile => pq_pure}.

%% A realm key and the realm id it signs endorsements for.
realm() ->
    {ok, RealmKey} = macula_node_keys:generate(realm, pq_pure),
    {RealmKey, id(77)}.

%% The wire form of an endorsement admitting Member to the realm, signed with Key.
endorsement(Key, RealmId, Member) ->
    Unsigned = macula_record:realm_member_endorsement(RealmId, #{realm => RealmId, member_node => Member,
                                                                 roles => [<<"member">>]}),
    macula_record:encode(macula_record:sign(Unsigned, Key)).

forward_join(NewMember, Ttl, Arwl, Prwl) ->
    macula_frame:hyparview_forward_join(#{realm => crypto:strong_rand_bytes(32), new_member => NewMember, ttl => Ttl,
                                          arwl => Arwl, prwl => Prwl}).

neighbor_frame(Priority, #{realm := R}) ->
    macula_frame:hyparview_neighbor(#{realm => R, priority => Priority}).
