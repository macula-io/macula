%% @doc HyParView protocol orchestrator (Part 3 §7.1).
%%
%% Pure functional layer on top of `macula_hyparview_view'. Given the
%% current view, an incoming HyParView frame, and a context (self
%% NodeId, realm, signing identity, ARWL/PRWL constants),
%% `process/4' returns the updated view plus a list of `{send,
%% TargetPeer, Frame}' actions the wrapping process should
%% transmit.
%%
%% == Message handlers ==
%%
%% <ul>
%%   <li><strong>JOIN</strong> — receiver becomes the joiner's
%%       contact: add_active(joiner), reply NEIGHBOR(high),
%%       FORWARD_JOIN(ttl=ARWL) to every other active peer.
%%       Eviction in the active view emits DISCONNECT to the
%%       demoted peer. When `ctx()' carries `realm_admin_pubkey',
%%       admission is gated on the frame's `record' field holding a
%%       `realm_member_endorsement' signed by that key for this
%%       exact (realm, joiner) pair (`macula_hyparview_endorsement:
%%       verify_endorsement/3') — a missing or invalid endorsement is
%%       dropped silently (no ack, no forward, view unchanged), never
%%       admitted. Without `realm_admin_pubkey' in `ctx()', admission
%%       is unconditional (opt-in gating, matches the frame spec's
%%       own "endorsement is optional" design).</li>
%%   <li><strong>FORWARD_JOIN</strong> — if ttl=0 or active view
%%       is empty: add_active(new_member) + reply NEIGHBOR(high).
%%       Otherwise: if ttl == PRWL, also add_passive(new_member);
%%       decrement ttl and forward to a random active peer that
%%       is not the sender. Carries the ORIGINAL JOIN's `record'
%%       (endorsement) through the whole forward chain and re-verifies
%%       it at every admission point — trust in the endorsement is
%%       never transitively assumed just because a neighbour forwarded
%%       it.</li>
%%   <li><strong>NEIGHBOR(high)</strong> — always add_active(sender),
%%       evicting if needed.</li>
%%   <li><strong>NEIGHBOR(low)</strong> — add_active(sender) only
%%       when the active view has room.</li>
%%   <li><strong>DISCONNECT</strong> — demote sender to passive.</li>
%%   <li><strong>SHUFFLE</strong> — if ttl > 0 and a forwardable
%%       neighbour exists, decrement ttl + forward; otherwise
%%       build SHUFFLE_REPLY against our own sample and send back
%%       to the origin, then merge the incoming sample into our
%%       passive view.</li>
%%   <li><strong>SHUFFLE_REPLY</strong> — merge the incoming
%%       sample into the passive view.</li>
%% </ul>
%%
%% Reference: plans/PLAN_MACULA_V2_PART3_DISCOVERY.md §7.1;
%% plans/PLAN_PHASE_5_BREAKDOWN.md Session 5.2.
-module(macula_hyparview_proto).

-export([
    build_join/1,
    build_shuffle/1,
    process/4
]).

-export_type([ctx/0, action/0, peer/0]).

-define(DEFAULT_ARWL,                 6).
-define(DEFAULT_PRWL,                 3).
-define(DEFAULT_SHUFFLE_TTL,          4).
-define(DEFAULT_SHUFFLE_ACTIVE,       3).
-define(DEFAULT_SHUFFLE_PASSIVE,      4).

-type peer() :: macula_identity:pubkey().

-type ctx() :: #{
    self_id              := peer(),
    realm                := <<_:256>>,
    identity             := macula_identity:key_pair(),
    arwl                 => non_neg_integer(),
    prwl                 => non_neg_integer(),
    shuffle_ttl          => non_neg_integer(),
    shuffle_active_size  => non_neg_integer(),
    shuffle_passive_size => non_neg_integer(),
    %% When present, JOIN/FORWARD_JOIN/NEIGHBOR admission is gated on a
    %% `realm_member_endorsement' record signed by this key (see the
    %% moduledoc). Absent = unconditional admission, today's behaviour.
    realm_admin_pubkey   => macula_identity:pubkey(),
    %% This peer's OWN signed endorsement, attached to every outgoing
    %% NEIGHBOR frame (`neighbor/3') this peer sends -- a NEIGHBOR is
    %% itself an admission event on the receiving end (see
    %% `on_neighbor'), so this peer must be able to prove its own
    %% membership too, not just the peers it admits. Required whenever
    %% `realm_admin_pubkey' is set on the OTHER side of a link; a
    %% NEIGHBOR built without it will be dropped by a gated receiver.
    self_endorsement     => macula_record:m_record()
}.

-type action() :: {send, peer(), macula_frame:frame()}.

%%=====================================================================
%% Outbound builders for events the local process initiates
%%=====================================================================

%% @doc Build a signed JOIN frame to send to a contact peer.
-spec build_join(ctx()) -> macula_frame:frame().
build_join(#{self_id := Self, realm := R, identity := Id}) ->
    sign_with(macula_frame:hyparview_join(
                #{realm => R, new_member => Self}), Id).

%% @doc Build a signed SHUFFLE frame for a periodic shuffle round.
%% The orchestrator picks a random active neighbour to send it to.
-spec build_shuffle(ctx()) -> {ok, macula_frame:frame()}.
build_shuffle(#{self_id := Self, realm := R, identity := Id} = Ctx) ->
    Ttl = maps:get(shuffle_ttl, Ctx, ?DEFAULT_SHUFFLE_TTL),
    %% Sample drawn from our own view by the caller — keep this
    %% function pure of the view structure. Caller computes the
    %% sample list and passes it in via process/4 if needed.
    F = macula_frame:hyparview_shuffle(
          #{realm => R, origin => Self, ttl => Ttl,
            peer_sample => []}),
    {ok, sign_with(F, Id)}.

%%=====================================================================
%% Process incoming frame
%%=====================================================================

-spec process(macula_hyparview_view:view(), peer(), macula_frame:frame(),
              ctx()) ->
        {macula_hyparview_view:view(), [action()]}.
process(View, FromId, #{frame_type := hyparview_join} = F, Ctx) ->
    on_join(View, FromId, F, Ctx);
process(View, FromId, #{frame_type := hyparview_forward_join} = F, Ctx) ->
    on_forward_join(View, FromId, F, Ctx);
process(View, FromId, #{frame_type := hyparview_neighbor} = F, Ctx) ->
    on_neighbor(View, FromId, F, Ctx);
process(View, FromId, #{frame_type := hyparview_disconnect} = _F, _Ctx) ->
    on_disconnect(View, FromId);
process(View, FromId, #{frame_type := hyparview_shuffle} = F, Ctx) ->
    on_shuffle(View, FromId, F, Ctx);
process(View, FromId, #{frame_type := hyparview_shuffle_reply} = F, _Ctx) ->
    on_shuffle_reply(View, FromId, F);
process(View, _FromId, _Frame, _Ctx) ->
    {View, []}.

%%=====================================================================
%% JOIN
%%=====================================================================

-spec on_join(macula_hyparview_view:view(), peer(),
              macula_frame:frame(), ctx()) ->
        {macula_hyparview_view:view(), [action()]}.
on_join(View, FromId, Frame, Ctx) ->
    admit_join(check_admission(Frame, FromId, Ctx), View, FromId, Frame, Ctx).

admit_join(ok, View, FromId, Frame, Ctx) ->
    NewMember = FromId,
    {View1, EvictionActions} = absorb_active(View, NewMember, Ctx),
    NeighborAck = neighbor(NewMember, high, Ctx),
    Endorsement = maps:get(record, Frame, undefined),
    Forwards = forward_join_to_others(View1, NewMember, FromId, Endorsement, Ctx),
    {View1, [NeighborAck | EvictionActions ++ Forwards]};
admit_join({error, _Reason}, View, _FromId, _Frame, _Ctx) ->
    %% Missing/invalid endorsement under a gated ctx() -- drop
    %% silently. No ack, no forward: the joiner is never told why,
    %% same as any other frame this module doesn't recognise.
    {View, []}.

%% @doc `ok' when `ctx()' carries no `realm_admin_pubkey' (ungated) or
%% `Frame' presents a valid endorsement for (realm(Ctx), ClaimedMember)
%% signed by that key. `{error, Reason}' otherwise.
-spec check_admission(macula_frame:frame(), peer(), ctx()) ->
        ok | {error, term()}.
check_admission(Frame, ClaimedMember, Ctx) ->
    verify_admission(maps:find(realm_admin_pubkey, Ctx), Frame, ClaimedMember, Ctx).

verify_admission(error, _Frame, _ClaimedMember, _Ctx) ->
    ok;
verify_admission({ok, AdminPubkey}, Frame, ClaimedMember, Ctx) ->
    verify_record(maps:find(record, Frame), AdminPubkey, ClaimedMember, Ctx).

verify_record(error, _AdminPubkey, _ClaimedMember, _Ctx) ->
    {error, missing_endorsement};
verify_record({ok, #{key := AdminPubkey} = Record}, AdminPubkey, ClaimedMember, Ctx) ->
    case macula_hyparview_endorsement:verify_endorsement(Record, realm(Ctx), ClaimedMember) of
        {ok, _Roles} -> ok;
        {error, _} = Err -> Err
    end;
verify_record({ok, _WrongSigner}, _AdminPubkey, _ClaimedMember, _Ctx) ->
    {error, untrusted_signer}.

-spec absorb_active(macula_hyparview_view:view(), peer(), ctx()) ->
        {macula_hyparview_view:view(), [action()]}.
absorb_active(View, Peer, Ctx) ->
    OldActive = sets_from_list(macula_hyparview_view:active(View)),
    View1 = macula_hyparview_view:add_active(View, Peer),
    NewActive = sets_from_list(macula_hyparview_view:active(View1)),
    Evicted = sets:to_list(sets:subtract(OldActive, NewActive)),
    Disconnects = [{send, E, build_disconnect(Ctx)} || E <- Evicted],
    {View1, Disconnects}.

-spec forward_join_to_others(macula_hyparview_view:view(), peer(), peer(),
                             macula_record:m_record() | undefined,
                             ctx()) -> [action()].
forward_join_to_others(View, NewMember, Origin, Endorsement, Ctx) ->
    Targets = [P || P <- macula_hyparview_view:active(View),
                    P =/= NewMember, P =/= Origin],
    Arwl = arwl(Ctx),
    Prwl = prwl(Ctx),
    Spec = with_record(#{realm      => realm(Ctx),
                          new_member => NewMember,
                          ttl        => Arwl,
                          arwl       => Arwl,
                          prwl       => Prwl}, Endorsement),
    [{send, T,
      sign_with(macula_frame:hyparview_forward_join(Spec),
                identity(Ctx))} || T <- Targets].

with_record(Spec, undefined) -> Spec;
with_record(Spec, Record) -> Spec#{record => Record}.

%%=====================================================================
%% FORWARD_JOIN
%%=====================================================================

-spec on_forward_join(macula_hyparview_view:view(), peer(),
                      macula_frame:frame(), ctx()) ->
        {macula_hyparview_view:view(), [action()]}.
on_forward_join(View, FromId, Frame, Ctx) ->
    NewMember = maps:get(new_member, Frame),
    Ttl       = maps:get(ttl, Frame),
    Prwl      = maps:get(prwl, Frame),
    classify_forward_join(NewMember, FromId, Ttl, Prwl, View, Frame, Ctx).

-spec classify_forward_join(peer(), peer(), non_neg_integer(),
                            non_neg_integer(),
                            macula_hyparview_view:view(),
                            macula_frame:frame(), ctx()) ->
        {macula_hyparview_view:view(), [action()]}.
classify_forward_join(NewMember, _From, 0, _Prwl, View, Frame, Ctx) ->
    accept_into_active(View, NewMember, Frame, Ctx);
classify_forward_join(NewMember, From, Ttl, Prwl, View, Frame, Ctx) ->
    case macula_hyparview_view:active_size(View) of
        N when N =< 1 ->
            accept_into_active(View, NewMember, Frame, Ctx);
        _ ->
            View1 = maybe_add_passive(NewMember, Ttl, Prwl, View),
            forward_to_random(View1, From, NewMember, Ttl - 1, Frame, Ctx)
    end.

%% Same admission gate as `on_join''s `admit_join/5' -- a FORWARD_JOIN
%% reaching an active-admission point carries the ORIGINAL JOIN's
%% `record' (endorsement), threaded through by `forward_join_to_others/5'
%% and `forward_to_random/6', and is re-verified here rather than
%% trusted just because a neighbour forwarded it.
-spec accept_into_active(macula_hyparview_view:view(), peer(),
                         macula_frame:frame(), ctx()) ->
        {macula_hyparview_view:view(), [action()]}.
accept_into_active(View, NewMember, Frame, Ctx) ->
    admit_forward_join(check_admission(Frame, NewMember, Ctx), View, NewMember, Ctx).

admit_forward_join(ok, View, NewMember, Ctx) ->
    {View1, Evictions} = absorb_active(View, NewMember, Ctx),
    Ack = neighbor(NewMember, high, Ctx),
    {View1, [Ack | Evictions]};
admit_forward_join({error, _Reason}, View, _NewMember, _Ctx) ->
    {View, []}.

-spec maybe_add_passive(peer(), non_neg_integer(), non_neg_integer(),
                        macula_hyparview_view:view()) ->
        macula_hyparview_view:view().
maybe_add_passive(NewMember, Ttl, Prwl, View) when Ttl =:= Prwl ->
    macula_hyparview_view:add_passive(View, NewMember);
maybe_add_passive(_NewMember, _Ttl, _Prwl, View) ->
    View.

-spec forward_to_random(macula_hyparview_view:view(), peer(), peer(),
                        non_neg_integer(), macula_frame:frame(),
                        ctx()) ->
        {macula_hyparview_view:view(), [action()]}.
forward_to_random(View, From, NewMember, NewTtl, Frame, Ctx) ->
    Candidates = [P || P <- macula_hyparview_view:active(View),
                       P =/= From, P =/= NewMember],
    pick_forward_target(Candidates, View, NewMember, NewTtl, Frame, Ctx).

-spec pick_forward_target([peer()], macula_hyparview_view:view(), peer(),
                          non_neg_integer(), macula_frame:frame(),
                          ctx()) ->
        {macula_hyparview_view:view(), [action()]}.
pick_forward_target([], View, NewMember, _NewTtl, Frame, Ctx) ->
    accept_into_active(View, NewMember, Frame, Ctx);
pick_forward_target(Cands, View, _NewMember, NewTtl, Frame, Ctx) ->
    Target = lists:nth(rand:uniform(length(Cands)), Cands),
    Spec = with_record(#{realm      => realm(Ctx),
                          new_member => maps:get(new_member, Frame),
                          ttl        => NewTtl,
                          arwl       => maps:get(arwl, Frame),
                          prwl       => maps:get(prwl, Frame)},
                        maps:get(record, Frame, undefined)),
    Forward = sign_with(macula_frame:hyparview_forward_join(Spec), identity(Ctx)),
    {View, [{send, Target, Forward}]}.

%%=====================================================================
%% NEIGHBOR
%%=====================================================================

%% A NEIGHBOR can arrive unsolicited (shuffle-driven promotion), not
%% only as an ack to a JOIN this peer itself initiated -- both
%% priorities admit into the active view (`low' only when there's
%% room) and are gated by the same `check_admission/3' JOIN/
%% FORWARD_JOIN uses, so trust is never assumed just because a frame
%% is shaped like an ack.
-spec on_neighbor(macula_hyparview_view:view(), peer(),
                  macula_frame:frame(), ctx()) ->
        {macula_hyparview_view:view(), [action()]}.
on_neighbor(View, FromId, #{priority := high} = Frame, Ctx) ->
    admit_neighbor_high(check_admission(Frame, FromId, Ctx), View, FromId, Ctx);
on_neighbor(View, FromId, #{priority := low} = Frame, Ctx) ->
    admit_neighbor_low(check_admission(Frame, FromId, Ctx), View, FromId).

admit_neighbor_high(ok, View, FromId, Ctx) ->
    {View1, Evictions} = absorb_active(View, FromId, Ctx),
    {View1, Evictions};
admit_neighbor_high({error, _Reason}, View, _FromId, _Ctx) ->
    {View, []}.

admit_neighbor_low(ok, View, FromId) ->
    case macula_hyparview_view:active_size(View)
            < macula_hyparview_view:active_cap(View) of
        true  -> {macula_hyparview_view:add_active(View, FromId), []};
        false -> {macula_hyparview_view:add_passive(View, FromId), []}
    end;
admit_neighbor_low({error, _Reason}, View, _FromId) ->
    {View, []}.

%%=====================================================================
%% DISCONNECT
%%=====================================================================

-spec on_disconnect(macula_hyparview_view:view(), peer()) ->
        {macula_hyparview_view:view(), [action()]}.
on_disconnect(View, FromId) ->
    {macula_hyparview_view:demote(View, FromId), []}.

%%=====================================================================
%% SHUFFLE / SHUFFLE_REPLY
%%=====================================================================

-spec on_shuffle(macula_hyparview_view:view(), peer(),
                 macula_frame:frame(), ctx()) ->
        {macula_hyparview_view:view(), [action()]}.
on_shuffle(View, FromId, Frame, Ctx) ->
    Ttl = maps:get(ttl, Frame),
    case Ttl > 0 of
        true  -> shuffle_forward(View, FromId, Frame, Ctx);
        false -> shuffle_reply(View, FromId, Frame, Ctx)
    end.

-spec shuffle_forward(macula_hyparview_view:view(), peer(),
                      macula_frame:frame(), ctx()) ->
        {macula_hyparview_view:view(), [action()]}.
shuffle_forward(View, From, Frame, Ctx) ->
    Origin = maps:get(origin, Frame),
    Cands = [P || P <- macula_hyparview_view:active(View),
                  P =/= From, P =/= Origin],
    pick_shuffle_target(Cands, View, From, Frame, Ctx).

-spec pick_shuffle_target([peer()], macula_hyparview_view:view(), peer(),
                          macula_frame:frame(), ctx()) ->
        {macula_hyparview_view:view(), [action()]}.
pick_shuffle_target([], View, From, Frame, Ctx) ->
    %% No-one to forward to — answer ourselves.
    shuffle_reply(View, From, Frame, Ctx);
pick_shuffle_target(Cands, View, _From, Frame, Ctx) ->
    Target = lists:nth(rand:uniform(length(Cands)), Cands),
    NewFrame = sign_with(macula_frame:hyparview_shuffle(
                           #{realm      => realm(Ctx),
                             origin     => maps:get(origin, Frame),
                             ttl        => maps:get(ttl, Frame) - 1,
                             peer_sample => maps:get(peer_sample,
                                                     Frame)}),
                         identity(Ctx)),
    {View, [{send, Target, NewFrame}]}.

-spec shuffle_reply(macula_hyparview_view:view(), peer(),
                    macula_frame:frame(), ctx()) ->
        {macula_hyparview_view:view(), [action()]}.
shuffle_reply(View, _FromId, Frame, Ctx) ->
    Origin = maps:get(origin, Frame),
    Sample = maps:get(peer_sample, Frame),
    LocalSample = collect_sample(View, Ctx),
    Reply = sign_with(macula_frame:hyparview_shuffle_reply(
                        #{realm => realm(Ctx),
                          peer_sample => LocalSample}),
                      identity(Ctx)),
    %% Merge incoming sample into our passive view.
    View1 = macula_hyparview_view:merge_shuffle(View, Sample),
    {View1, [{send, Origin, Reply}]}.

-spec on_shuffle_reply(macula_hyparview_view:view(), peer(),
                       macula_frame:frame()) ->
        {macula_hyparview_view:view(), [action()]}.
on_shuffle_reply(View, _FromId, Frame) ->
    Sample = maps:get(peer_sample, Frame),
    {macula_hyparview_view:merge_shuffle(View, Sample), []}.

-spec collect_sample(macula_hyparview_view:view(), ctx()) -> [peer()].
collect_sample(View, Ctx) ->
    NA = maps:get(shuffle_active_size, Ctx, ?DEFAULT_SHUFFLE_ACTIVE),
    NP = maps:get(shuffle_passive_size, Ctx, ?DEFAULT_SHUFFLE_PASSIVE),
    macula_hyparview_view:random_active_subset(View, NA)
        ++ macula_hyparview_view:random_passive_subset(View, NP).

%%=====================================================================
%% Frame helpers
%%=====================================================================

-spec neighbor(peer(), macula_frame:neighbor_priority(), ctx()) ->
        action().
neighbor(Target, Priority, Ctx) ->
    Spec = with_record(#{realm => realm(Ctx), priority => Priority},
                        maps:get(self_endorsement, Ctx, undefined)),
    F = macula_frame:hyparview_neighbor(Spec),
    {send, Target, sign_with(F, identity(Ctx))}.

-spec build_disconnect(ctx()) -> macula_frame:frame().
build_disconnect(Ctx) ->
    sign_with(macula_frame:hyparview_disconnect(
                #{realm => realm(Ctx)}),
              identity(Ctx)).

-spec sign_with(macula_frame:frame(), macula_identity:key_pair()) ->
        macula_frame:frame().
sign_with(Frame, Identity) ->
    macula_frame:sign(Frame, Identity).

%%=====================================================================
%% Context accessors with defaults
%%=====================================================================

realm(#{realm := R}) -> R.
identity(#{identity := I}) -> I.
arwl(C) -> maps:get(arwl, C, ?DEFAULT_ARWL).
prwl(C) -> maps:get(prwl, C, ?DEFAULT_PRWL).

sets_from_list(L) -> sets:from_list(L).
