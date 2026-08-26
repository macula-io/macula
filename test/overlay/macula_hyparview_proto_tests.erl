%% EUnit tests for macula_hyparview_proto.
-module(macula_hyparview_proto_tests).

-include_lib("eunit/include/eunit.hrl").

%%---------------------------------------------------------------------
%% Outbound builders
%%---------------------------------------------------------------------

build_join_returns_signed_frame_test() ->
    Kp = macula_identity:generate(),
    Self = macula_identity:public(Kp),
    Ctx = ctx(Kp, Self),
    Frame = macula_hyparview_proto:build_join(Ctx),
    ?assertEqual(hyparview_join, macula_frame:frame_type(Frame)),
    ?assertEqual(Self, maps:get(new_member, Frame)),
    ?assertMatch({ok, _}, macula_frame:verify(Frame, Self)).

%%---------------------------------------------------------------------
%% JOIN handler
%%---------------------------------------------------------------------

join_adds_sender_to_active_and_replies_neighbor_high_test() ->
    SelfKp = macula_identity:generate(),
    Self = macula_identity:public(SelfKp),
    Joiner = id(1),
    View0 = macula_hyparview_view:new(Self),
    Ctx = ctx(SelfKp, Self),
    Frame = macula_hyparview_proto:build_join(ctx(Joiner)),
    {View1, Actions} = macula_hyparview_proto:process(View0, Joiner, Frame, Ctx),
    ?assert(macula_hyparview_view:is_active(Joiner, View1)),
    %% At least one action: NEIGHBOR(high) to the joiner.
    ?assert(lists:any(
              fun({send, T, F}) ->
                  T =:= Joiner andalso
                  macula_frame:frame_type(F) =:= hyparview_neighbor andalso
                  maps:get(priority, F) =:= high
              end, Actions)).

join_with_existing_actives_forwards_join_test() ->
    SelfKp = macula_identity:generate(),
    Self = macula_identity:public(SelfKp),
    %% Pre-populate two existing active peers.
    View0 = lists:foldl(fun(P, V) -> macula_hyparview_view:add_active(V, P) end,
                        macula_hyparview_view:new(Self,
                                                #{active_cap => 5,
                                                  passive_cap => 20}),
                        [id(10), id(11)]),
    Joiner = id(1),
    Ctx = ctx(SelfKp, Self),
    Frame = macula_hyparview_proto:build_join(ctx(Joiner)),
    {_View1, Actions} = macula_hyparview_proto:process(View0, Joiner,
                                                    Frame, Ctx),
    Forwards = [A || {send, T, F} = A <- Actions,
                     macula_frame:frame_type(F) =:= hyparview_forward_join,
                     T =/= Joiner],
    ?assertEqual(2, length(Forwards)),
    %% Each forward carries Joiner as new_member.
    [?assertEqual(Joiner, maps:get(new_member, F)) ||
        {send, _, F} <- Forwards].

%%---------------------------------------------------------------------
%% FORWARD_JOIN handler
%%---------------------------------------------------------------------

forward_join_with_zero_ttl_accepts_test() ->
    SelfKp = macula_identity:generate(),
    Self   = macula_identity:public(SelfKp),
    NewM   = id(7),
    Sender = id(8),
    View0  = macula_hyparview_view:new(Self),
    Ctx    = ctx(SelfKp, Self),
    Frame  = build_fwd(NewM, 0, 6, 3, Sender),
    {View1, Actions} = macula_hyparview_proto:process(View0, Sender,
                                                   Frame, Ctx),
    ?assert(macula_hyparview_view:is_active(NewM, View1)),
    %% Reply NEIGHBOR(high) to the new member.
    ?assert(lists:any(fun({send, T, F}) ->
                          T =:= NewM andalso
                          macula_frame:frame_type(F) =:= hyparview_neighbor
                      end, Actions)).

forward_join_with_empty_active_accepts_test() ->
    SelfKp = macula_identity:generate(),
    Self   = macula_identity:public(SelfKp),
    NewM   = id(7),
    Sender = id(8),
    View0  = macula_hyparview_view:new(Self),
    Ctx    = ctx(SelfKp, Self),
    Frame  = build_fwd(NewM, 6, 6, 3, Sender),
    {View1, _} = macula_hyparview_proto:process(View0, Sender, Frame, Ctx),
    %% Active view was empty → accept regardless of TTL.
    ?assert(macula_hyparview_view:is_active(NewM, View1)).

forward_join_at_prwl_threshold_adds_to_passive_test() ->
    SelfKp = macula_identity:generate(),
    Self   = macula_identity:public(SelfKp),
    NewM   = id(7),
    Sender = id(8),
    Other  = id(9),
    %% Two existing actives so we don't take the empty-view path.
    View0  = lists:foldl(fun(P, V) -> macula_hyparview_view:add_active(V, P) end,
                         macula_hyparview_view:new(Self),
                         [Sender, Other]),
    Ctx    = ctx(SelfKp, Self),
    %% PRWL = 3, set ttl == prwl so the passive-add fires.
    Frame  = build_fwd(NewM, 3, 6, 3, Sender),
    {View1, _Actions} = macula_hyparview_proto:process(View0, Sender,
                                                   Frame, Ctx),
    ?assert(macula_hyparview_view:is_passive(NewM, View1)).

forward_join_above_zero_ttl_forwards_to_random_active_test() ->
    SelfKp = macula_identity:generate(),
    Self   = macula_identity:public(SelfKp),
    NewM   = id(7),
    Sender = id(8),
    Other  = id(9),
    View0  = lists:foldl(fun(P, V) -> macula_hyparview_view:add_active(V, P) end,
                         macula_hyparview_view:new(Self),
                         [Sender, Other]),
    Ctx    = ctx(SelfKp, Self),
    Frame  = build_fwd(NewM, 5, 6, 3, Sender),
    {_, Actions} = macula_hyparview_proto:process(View0, Sender, Frame, Ctx),
    %% A single forward to Other (excludes Sender + NewM).
    [{send, T, F}] = Actions,
    ?assertEqual(Other, T),
    ?assertEqual(hyparview_forward_join, macula_frame:frame_type(F)),
    %% TTL decremented.
    ?assertEqual(4, maps:get(ttl, F)).

%%---------------------------------------------------------------------
%% NEIGHBOR handler
%%---------------------------------------------------------------------

neighbor_high_always_admits_test() ->
    SelfKp = macula_identity:generate(),
    Self   = macula_identity:public(SelfKp),
    Sender = id(1),
    View0  = macula_hyparview_view:new(Self, #{active_cap => 1,
                                             passive_cap => 4}),
    %% Pre-fill active so admission requires eviction.
    View1  = macula_hyparview_view:add_active(View0, id(2)),
    Ctx    = ctx(SelfKp, Self),
    Frame  = sign_neighbor(high, Ctx),
    {View2, _} = macula_hyparview_proto:process(View1, Sender, Frame, Ctx),
    ?assert(macula_hyparview_view:is_active(Sender, View2)),
    %% The pre-existing active was demoted to passive.
    ?assert(macula_hyparview_view:is_passive(id(2), View2)).

neighbor_low_admits_only_if_room_test() ->
    SelfKp = macula_identity:generate(),
    Self   = macula_identity:public(SelfKp),
    View0  = macula_hyparview_view:new(Self, #{active_cap => 1,
                                             passive_cap => 4}),
    View1  = macula_hyparview_view:add_active(View0, id(2)),
    Ctx    = ctx(SelfKp, Self),
    Frame  = sign_neighbor(low, Ctx),
    {View2, _} = macula_hyparview_proto:process(View1, id(3), Frame, Ctx),
    %% Active is full; low priority → goes to passive.
    ?assertNot(macula_hyparview_view:is_active(id(3), View2)),
    ?assert(macula_hyparview_view:is_passive(id(3), View2)).

%%---------------------------------------------------------------------
%% DISCONNECT handler
%%---------------------------------------------------------------------

disconnect_demotes_sender_test() ->
    SelfKp = macula_identity:generate(),
    Self   = macula_identity:public(SelfKp),
    Sender = id(5),
    View0  = macula_hyparview_view:add_active(
               macula_hyparview_view:new(Self), Sender),
    Ctx    = ctx(SelfKp, Self),
    Frame  = macula_frame:sign(macula_frame:hyparview_disconnect(
                                 #{realm => maps:get(realm, Ctx)}),
                               SelfKp),
    {View1, _} = macula_hyparview_proto:process(View0, Sender, Frame, Ctx),
    ?assertNot(macula_hyparview_view:is_active(Sender, View1)),
    ?assert(macula_hyparview_view:is_passive(Sender, View1)).

%%---------------------------------------------------------------------
%% SHUFFLE / SHUFFLE_REPLY handlers
%%---------------------------------------------------------------------

shuffle_with_zero_ttl_replies_and_merges_test() ->
    SelfKp = macula_identity:generate(),
    Self   = macula_identity:public(SelfKp),
    Origin = id(20),
    Sender = id(21),
    Sample = [id(30), id(31)],
    View0  = macula_hyparview_view:add_passive(
               macula_hyparview_view:new(Self), id(40)),
    Ctx    = ctx(SelfKp, Self),
    Frame  = macula_frame:sign(macula_frame:hyparview_shuffle(
                                 #{realm => maps:get(realm, Ctx),
                                   origin => Origin,
                                   ttl    => 0,
                                   peer_sample => Sample}),
                               SelfKp),
    {View1, [{send, T, ReplyFrame}]} =
        macula_hyparview_proto:process(View0, Sender, Frame, Ctx),
    ?assertEqual(Origin, T),
    ?assertEqual(hyparview_shuffle_reply,
                 macula_frame:frame_type(ReplyFrame)),
    %% Incoming sample peers merged into passive view.
    ?assert(macula_hyparview_view:is_passive(id(30), View1)),
    ?assert(macula_hyparview_view:is_passive(id(31), View1)).

shuffle_with_positive_ttl_forwards_to_random_active_test() ->
    SelfKp = macula_identity:generate(),
    Self   = macula_identity:public(SelfKp),
    Origin = id(50),
    Sender = id(51),
    Other  = id(52),
    View0  = lists:foldl(fun(P, V) -> macula_hyparview_view:add_active(V, P) end,
                         macula_hyparview_view:new(Self),
                         [Sender, Other]),
    Ctx    = ctx(SelfKp, Self),
    Frame  = macula_frame:sign(macula_frame:hyparview_shuffle(
                                 #{realm => maps:get(realm, Ctx),
                                   origin => Origin,
                                   ttl    => 3,
                                   peer_sample => []}),
                               SelfKp),
    {_, [{send, T, F}]} = macula_hyparview_proto:process(View0, Sender,
                                                       Frame, Ctx),
    ?assertEqual(Other, T),
    ?assertEqual(hyparview_shuffle, macula_frame:frame_type(F)),
    ?assertEqual(2, maps:get(ttl, F)).

shuffle_reply_merges_into_passive_test() ->
    SelfKp = macula_identity:generate(),
    Self   = macula_identity:public(SelfKp),
    Sender = id(60),
    Sample = [id(70), id(71)],
    View0  = macula_hyparview_view:new(Self),
    Ctx    = ctx(SelfKp, Self),
    Frame  = macula_frame:sign(macula_frame:hyparview_shuffle_reply(
                                 #{realm => maps:get(realm, Ctx),
                                   peer_sample => Sample}), SelfKp),
    {View1, []} = macula_hyparview_proto:process(View0, Sender, Frame, Ctx),
    ?assert(macula_hyparview_view:is_passive(id(70), View1)),
    ?assert(macula_hyparview_view:is_passive(id(71), View1)).

%%=====================================================================
%% Admission gating (realm_admin_pubkey in ctx())
%%
%% Every existing test above constructs ctx() without
%% realm_admin_pubkey and still passes unmodified -- confirms gating
%% is genuinely opt-in, not a behaviour change for ungated callers.
%%=====================================================================

gated_join_with_valid_endorsement_admits_test() ->
    {SelfKp, Self, RealmKp, RealmId} = gated_fixture(),
    Joiner = macula_identity:public(macula_identity:generate()),
    Endorsement = endorsement(RealmKp, RealmId, Joiner),
    View0 = macula_hyparview_view:new(Self),
    Ctx = (ctx(SelfKp, Self))#{realm => RealmId, realm_admin_pubkey => RealmId},
    Frame = macula_frame:sign(macula_frame:hyparview_join(
              #{realm => RealmId, new_member => Joiner,
                record => Endorsement}), SelfKp),
    {View1, Actions} = macula_hyparview_proto:process(View0, Joiner, Frame, Ctx),
    ?assert(macula_hyparview_view:is_active(Joiner, View1)),
    ?assert(lists:any(
              fun({send, T, F}) ->
                  T =:= Joiner andalso
                  macula_frame:frame_type(F) =:= hyparview_neighbor
              end, Actions)).

gated_join_without_endorsement_is_dropped_test() ->
    {SelfKp, Self, _RealmKp, RealmId} = gated_fixture(),
    Joiner = macula_identity:public(macula_identity:generate()),
    View0 = macula_hyparview_view:new(Self),
    Ctx = (ctx(SelfKp, Self))#{realm => RealmId, realm_admin_pubkey => RealmId},
    Frame = macula_frame:sign(macula_frame:hyparview_join(
              #{realm => RealmId, new_member => Joiner}), SelfKp),
    {View1, Actions} = macula_hyparview_proto:process(View0, Joiner, Frame, Ctx),
    ?assertNot(macula_hyparview_view:is_active(Joiner, View1)),
    ?assertEqual([], Actions).

gated_join_with_wrong_signer_is_dropped_test() ->
    {SelfKp, Self, _RealmKp, RealmId} = gated_fixture(),
    Impostor = macula_identity:generate(),
    Joiner = macula_identity:public(macula_identity:generate()),
    %% Signed by someone who is NOT the realm admin.
    BadEndorsement = macula_record:sign(
        macula_record:realm_member_endorsement(
          macula_identity:public(Impostor),
          #{realm => macula_identity:public(Impostor),
            member_node => Joiner, roles => [<<"member">>]}),
        Impostor),
    View0 = macula_hyparview_view:new(Self),
    Ctx = (ctx(SelfKp, Self))#{realm => RealmId, realm_admin_pubkey => RealmId},
    Frame = macula_frame:sign(macula_frame:hyparview_join(
              #{realm => RealmId, new_member => Joiner,
                record => BadEndorsement}), SelfKp),
    {View1, Actions} = macula_hyparview_proto:process(View0, Joiner, Frame, Ctx),
    ?assertNot(macula_hyparview_view:is_active(Joiner, View1)),
    ?assertEqual([], Actions).

gated_forward_join_carries_and_verifies_endorsement_test() ->
    {SelfKp, Self, RealmKp, RealmId} = gated_fixture(),
    Sender = id(1),
    NewMember = macula_identity:public(macula_identity:generate()),
    Endorsement = endorsement(RealmKp, RealmId, NewMember),
    View0 = macula_hyparview_view:new(Self),
    Ctx = (ctx(SelfKp, Self))#{realm => RealmId, realm_admin_pubkey => RealmId},
    %% ttl=0 -> accept_into_active's endorsement-check path.
    Frame = macula_frame:sign(macula_frame:hyparview_forward_join(
              #{realm => RealmId, new_member => NewMember,
                ttl => 0, arwl => 6, prwl => 3, record => Endorsement}),
              SelfKp),
    {View1, _Actions} = macula_hyparview_proto:process(View0, Sender, Frame, Ctx),
    ?assert(macula_hyparview_view:is_active(NewMember, View1)).

gated_forward_join_without_endorsement_is_dropped_test() ->
    {SelfKp, Self, _RealmKp, RealmId} = gated_fixture(),
    Sender = id(1),
    NewMember = macula_identity:public(macula_identity:generate()),
    View0 = macula_hyparview_view:new(Self),
    Ctx = (ctx(SelfKp, Self))#{realm => RealmId, realm_admin_pubkey => RealmId},
    Frame = macula_frame:sign(macula_frame:hyparview_forward_join(
              #{realm => RealmId, new_member => NewMember,
                ttl => 0, arwl => 6, prwl => 3}), SelfKp),
    {View1, Actions} = macula_hyparview_proto:process(View0, Sender, Frame, Ctx),
    ?assertNot(macula_hyparview_view:is_active(NewMember, View1)),
    ?assertEqual([], Actions).

%% forward_join_to_others (the fan-out on admission) must thread the
%% inbound JOIN's endorsement into the frames it sends onward -- this
%% is what actually lets downstream peers verify it, not just this one.
gated_join_forwards_carry_the_endorsement_test() ->
    {SelfKp, Self, RealmKp, RealmId} = gated_fixture(),
    Joiner = macula_identity:public(macula_identity:generate()),
    Endorsement = endorsement(RealmKp, RealmId, Joiner),
    View0 = lists:foldl(fun(P, V) -> macula_hyparview_view:add_active(V, P) end,
                        macula_hyparview_view:new(Self, #{active_cap => 5,
                                                        passive_cap => 20}),
                        [id(10), id(11)]),
    Ctx = (ctx(SelfKp, Self))#{realm => RealmId, realm_admin_pubkey => RealmId},
    Frame = macula_frame:sign(macula_frame:hyparview_join(
              #{realm => RealmId, new_member => Joiner,
                record => Endorsement}), SelfKp),
    {_View1, Actions} = macula_hyparview_proto:process(View0, Joiner, Frame, Ctx),
    Forwards = [F || {send, _, F} <- Actions,
                     macula_frame:frame_type(F) =:= hyparview_forward_join],
    ?assertEqual(2, length(Forwards)),
    [?assertEqual(Endorsement, maps:get(record, F)) || F <- Forwards].

gated_neighbor_high_with_valid_endorsement_admits_test() ->
    {SelfKp, Self, RealmKp, RealmId} = gated_fixture(),
    Sender = macula_identity:public(macula_identity:generate()),
    Endorsement = endorsement(RealmKp, RealmId, Sender),
    View0 = macula_hyparview_view:new(Self),
    Ctx = (ctx(SelfKp, Self))#{realm => RealmId, realm_admin_pubkey => RealmId},
    Frame = macula_frame:sign(macula_frame:hyparview_neighbor(
              #{realm => RealmId, priority => high,
                record => Endorsement}), SelfKp),
    {View1, _Actions} = macula_hyparview_proto:process(View0, Sender, Frame, Ctx),
    ?assert(macula_hyparview_view:is_active(Sender, View1)).

gated_neighbor_high_without_endorsement_is_dropped_test() ->
    {SelfKp, Self, _RealmKp, RealmId} = gated_fixture(),
    Sender = macula_identity:public(macula_identity:generate()),
    View0 = macula_hyparview_view:new(Self),
    Ctx = (ctx(SelfKp, Self))#{realm => RealmId, realm_admin_pubkey => RealmId},
    Frame = macula_frame:sign(macula_frame:hyparview_neighbor(
              #{realm => RealmId, priority => high}), SelfKp),
    {View1, Actions} = macula_hyparview_proto:process(View0, Sender, Frame, Ctx),
    ?assertNot(macula_hyparview_view:is_active(Sender, View1)),
    ?assertEqual([], Actions).

%% neighbor/3 (the outbound builder used for JOIN/FORWARD_JOIN's own
%% acks) must attach self_endorsement when ctx() carries one, so a
%% gated receiver on the other end doesn't drop our own ack.
neighbor_builder_attaches_self_endorsement_test() ->
    {SelfKp, Self, RealmKp, RealmId} = gated_fixture(),
    SelfEndorsement = endorsement(RealmKp, RealmId, Self),
    Ctx = (ctx(SelfKp, Self))#{realm => RealmId,
                               self_endorsement => SelfEndorsement},
    View0 = macula_hyparview_view:new(Self),
    JoinFrame = macula_frame:sign(macula_frame:hyparview_join(
                  #{realm => RealmId, new_member => id(1)}), SelfKp),
    {_View1, Actions} = macula_hyparview_proto:process(View0, id(1), JoinFrame, Ctx),
    [{send, _, Ack}] = [A || {send, _, F} = A <- Actions,
                             macula_frame:frame_type(F) =:= hyparview_neighbor],
    ?assertEqual(SelfEndorsement, maps:get(record, Ack)).

%%=====================================================================
%% Helpers
%%=====================================================================

id(N) -> <<N:256>>.

ctx(Self) ->
    %% Used when only the self_id is known (other-side ctx).
    Kp = macula_identity:generate(),
    #{
        self_id  => Self,
        realm    => crypto:strong_rand_bytes(32),
        identity => Kp
    }.

ctx(Kp, Self) ->
    #{
        self_id  => Self,
        realm    => crypto:strong_rand_bytes(32),
        identity => Kp
    }.

%% {SelfKp, Self, RealmAdminKp, RealmId} -- RealmId IS the realm
%% admin's own pubkey (macula_record:realm_member_endorsement/2's own
%% convention: "Envelope key is the RealmId (admin signs)").
gated_fixture() ->
    SelfKp = macula_identity:generate(),
    Self = macula_identity:public(SelfKp),
    RealmKp = macula_identity:generate(),
    RealmId = macula_identity:public(RealmKp),
    {SelfKp, Self, RealmKp, RealmId}.

endorsement(RealmKp, RealmId, MemberNode) ->
    macula_record:sign(
      macula_record:realm_member_endorsement(
        RealmId, #{realm => RealmId, member_node => MemberNode,
                   roles => [<<"member">>]}),
      RealmKp).

build_fwd(NewMember, Ttl, Arwl, Prwl, _Sender) ->
    %% A fresh signing identity stands in for the sender; the
    %% handler only reads the frame's fields, not its signature.
    Kp = macula_identity:generate(),
    macula_frame:sign(macula_frame:hyparview_forward_join(
                        #{realm      => crypto:strong_rand_bytes(32),
                          new_member => NewMember,
                          ttl        => Ttl,
                          arwl       => Arwl,
                          prwl       => Prwl}), Kp).

sign_neighbor(Priority, #{identity := Kp, realm := R}) ->
    macula_frame:sign(macula_frame:hyparview_neighbor(
                        #{realm => R, priority => Priority}), Kp).
