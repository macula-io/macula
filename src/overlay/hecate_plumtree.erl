%% @doc Plumtree push-lazy gossip (Leitão, Pereira, Rodrigues 2007, Part 3 §7.2).
%%
%% Disseminates realm-scoped publications over HyParView's active view with a tree-emergent topology and lazy-push
%% recovery.
%%
%% A message is a publication signed by its publisher (`macula_frame:publish/2'), and its message id is the SHA-384 of
%% the publication's tbs. GOSSIP carries the publication bytes unchanged. Every node verifies a publication once, keyed
%% by that id, before it delivers or forwards it (`macula_frame:verify_publication/3'), under the node's configured
%% crypto profile: a publication that does not verify, or names another realm, stops at the first node that sees it,
%% and a copy that arrives again is recognised by its id without a second verification. Frames leave this module
%% without a signature of their own: the connection that sends a frame adds its neighbour signature (D17).
%%
%% == State ==
%%
%% <ul>
%%   <li><strong>eager_push</strong>: peers receiving full GOSSIP publications. The eager-push set <em>is</em> the
%%       Plumtree spanning tree.</li>
%%   <li><strong>lazy_push</strong>: peers receiving only IHAVE announcements. They graft into eager_push when they
%%       GRAFT in response to an IHAVE.</li>
%%   <li><strong>received</strong>: `MsgId => {Publication, ExpiresAt}' for the verified publications delivered
%%       locally, each kept until its publication expires (`sweep/2'). Used to recognise repeat GOSSIPs and to answer
%%       GRAFTs.</li>
%%   <li><strong>missing</strong>: `MsgId => {Peers, NotedAt}' for the peers who sent IHAVE for a publication not yet
%%       received in full, and when the first IHAVE for it came; forgotten after 70 minutes (`sweep/2').</li>
%% </ul>
%%
%% == Message handling ==
%%
%% <ul>
%%   <li><strong>Local publish</strong>: verify the PUBLISH, record its publication, deliver it locally, GOSSIP it to
%%       every eager peer and IHAVE it to every lazy peer.</li>
%%   <li><strong>Receive GOSSIP</strong>: the first time, verify the publication; when it verifies, deliver it, remove
%%       it from missing, eager-forward it to every other eager peer, IHAVE-forward it to every lazy peer and ensure the
%%       sender is eager, and when it does not, drop it. A duplicate is not verified again: PRUNE the sender and move
%%       it from eager to lazy.</li>
%%   <li><strong>Receive IHAVE</strong>: if already received, ignore. Else record the sender in missing and emit a
%%       GRAFT to the sender right away (Phase 5.3 MVP: a real deployment delays the GRAFT briefly to give the eager
%%       push a chance to win the race; eager grafting is correct but slightly heavier).</li>
%%   <li><strong>Receive GRAFT</strong>: for a publication this node holds, the sender becomes eager and gets the
%%       GOSSIP publication; for any other id nothing changes, the push sets included.</li>
%%   <li><strong>Receive PRUNE</strong>: move the sender from eager to lazy.</li>
%% </ul>
%%
%% This module is pure apart from reading the configured profile when a node starts, and the clock when it verifies a
%% publication or notes a missing one.
%% The wrapping process transmits the action list, feeds deliveries to the local consumer and calls `sweep/2' on a
%% timer, so a node remembers a publication hash until the publication expires, and no longer.
%%
%% Reference: plans/PLAN_MACULA_V2_PART3_DISCOVERY.md §7.2; plans/PLAN_PHASE_5_BREAKDOWN.md Session 5.3;
%% DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, Publications.
-module(hecate_plumtree).

-export([
    new/2,
    self_id/1,
    realm/1,
    eager_peers/1,
    lazy_peers/1,
    add_peer/2,
    remove_peer/2,
    has_received/2,
    received_count/1,
    missing_count/1,
    publish/2,
    process/3,
    sweep/2
]).

-export_type([state/0, peer/0, msg_id/0, action/0, delivery/0]).

%% The longest a publication can live: a published_at up to 5 minutes ahead, a ttl_ms of at most one hour and 5 minutes
%% of tolerance. No announcement can name a live publication for longer.
-define(MISSING_MAX_AGE_MS, 70 * 60000).

-type peer()     :: <<_:256>>.
-type msg_id()   :: <<_:384>>.

-type state() :: #{
    self_id    := peer(),
    realm      := <<_:256>>,
    profile    := macula_crypto_profile:profile(),
    eager_push := sets:set(peer()),
    lazy_push  := sets:set(peer()),
    received   := #{msg_id() => {macula_signed_object:object(), non_neg_integer()}},
    missing    := #{msg_id() => {sets:set(peer()), non_neg_integer()}}
}.

-type action()   :: {send, peer(), macula_frame:frame()}.
%% A delivery is the message id and the verified publication: publisher, realm, topic, seq, published_at, payload.
-type delivery() :: {msg_id(), map()}.

%%=====================================================================
%% Construction + view changes
%%=====================================================================

%% @doc A Plumtree node for a realm, in the node's configured crypto profile, under which it verifies every
%% publication. A node with no configured profile does not start.
-spec new(peer(), <<_:256>>) -> {ok, state()} | {error, macula_crypto_profile:refusal()}.
new(<<_:256>> = SelfId, <<_:256>> = Realm) ->
    started(macula_crypto_profile:configured(), SelfId, Realm).

started({ok, Profile}, SelfId, Realm) ->
    {ok, #{
        self_id    => SelfId,
        realm      => Realm,
        profile    => Profile,
        eager_push => sets:new(),
        lazy_push  => sets:new(),
        received   => #{},
        missing    => #{}
    }};
started({error, _} = Refusal, _SelfId, _Realm) ->
    Refusal.

%% @doc When HyParView promotes a peer to active, the Plumtree layer adds it to the eager-push set. Any GOSSIP we
%% publish reaches the new peer immediately.
-spec add_peer(state(), peer()) -> state().
add_peer(#{eager_push := E} = S, <<_:256>> = Peer) ->
    S#{eager_push := sets:add_element(Peer, E)}.

%% @doc When HyParView removes a peer from active, the Plumtree layer removes it from both push sets.
-spec remove_peer(state(), peer()) -> state().
remove_peer(#{eager_push := E, lazy_push := L} = S, <<_:256>> = Peer) ->
    S#{eager_push := sets:del_element(Peer, E),
       lazy_push  := sets:del_element(Peer, L)}.

%%=====================================================================
%% Inspection
%%=====================================================================

self_id(#{self_id := S})     -> S.
realm(#{realm := R})         -> R.
eager_peers(#{eager_push := E}) -> sets:to_list(E).
lazy_peers(#{lazy_push := L})   -> sets:to_list(L).
has_received(MsgId, #{received := R}) -> maps:is_key(MsgId, R).
received_count(#{received := R}) -> maps:size(R).
missing_count(#{missing := M})   -> maps:size(M).

%%=====================================================================
%% Local publish
%%=====================================================================

%% @doc Publish a PUBLISH this node made: verify its publication, record it, push it fully to every eager peer and
%% announce it to every lazy peer, and return it as the local delivery the caller's consumer handles. A publication
%% that does not verify, or names another realm, is refused; one already received is not delivered again.
-spec publish(state(), macula_frame:frame()) -> {state(), [action()], [delivery()]} | {error, term()}.
publish(State, #{frame_type := publish, publication := #{tbs := Tbs} = Publication} = Frame) ->
    MsgId = crypto:hash(sha384, Tbs),
    local_publish(has_received(MsgId, State), MsgId, Publication, Frame, State).

local_publish(true, _MsgId, _Publication, _Frame, State) ->
    {State, [], []};
local_publish(false, MsgId, Publication, Frame, State) ->
    published(verified(Frame, State), MsgId, Publication, State).

published({ok, #{expires_at := ExpiresAt} = Verified}, MsgId, Publication, State) ->
    State1 = mark_received(State, MsgId, Publication, ExpiresAt),
    {State1, build_pushes(State1, MsgId, 0, Publication, undefined), [{MsgId, Verified}]};
published({error, _} = Refusal, _MsgId, _Publication, _State) ->
    Refusal.

%%=====================================================================
%% Inbound dispatch
%%=====================================================================

-spec process(state(), peer(), macula_frame:frame()) -> {state(), [action()], [delivery()]}.
process(State, From, #{frame_type := plumtree_gossip} = F) ->
    on_gossip(From, F, State);
process(State, From, #{frame_type := plumtree_ihave} = F) ->
    on_ihave(From, F, State);
process(State, From, #{frame_type := plumtree_graft} = F) ->
    on_graft(From, F, State);
process(State, From, #{frame_type := plumtree_prune}) ->
    on_prune(From, State);
process(State, _From, _Frame) ->
    {State, [], []}.

%%=====================================================================
%% Retention
%%=====================================================================

%% @doc Forget, at `NowMs' in milliseconds of wall-clock time, every received publication that has expired (its
%% published_at plus its ttl_ms, or 10 minutes without one, plus 5 minutes) and every missing publication first
%% announced more than 70 minutes ago. A node keeps a publication hash until the publication expires, and no longer; a
%% later copy is refused by verification, and a GRAFT for it gets no answer.
-spec sweep(state(), integer()) -> state().
sweep(#{received := R, missing := M} = S, NowMs) when is_integer(NowMs) ->
    S#{received := maps:filter(live_at(NowMs), R),
       missing  := maps:filter(announced_since(NowMs - ?MISSING_MAX_AGE_MS), M)}.

live_at(NowMs) ->
    fun(_MsgId, {_Publication, ExpiresAt}) -> ExpiresAt >= NowMs end.

announced_since(Oldest) ->
    fun(_MsgId, {_Peers, NotedAt}) -> NotedAt >= Oldest end.

%%=====================================================================
%% Handlers
%%=====================================================================

on_gossip(From, #{publication := #{tbs := Tbs} = Publication} = Frame, State) ->
    MsgId = crypto:hash(sha384, Tbs),
    classify_gossip(has_received(MsgId, State), From, MsgId, Publication, Frame, State).

classify_gossip(true, From, _MsgId, _Publication, _Frame, State) ->
    %% A duplicate, recognised by its id and not verified again. The sender should not be eager: PRUNE it.
    {move_to_lazy(State, From), [{send, From, prune(State)}], []};
classify_gossip(false, From, MsgId, Publication, #{round := Round} = Frame, State) ->
    first_gossip(verified(Frame, State), From, MsgId, Round, Publication, State).

first_gossip({ok, #{expires_at := ExpiresAt} = Verified}, From, MsgId, Round, Publication, State) ->
    State1 = mark_received(State, MsgId, Publication, ExpiresAt),
    State2 = clear_missing(State1, MsgId),
    State3 = move_to_eager(State2, From),
    {State3, build_pushes(State3, MsgId, Round + 1, Publication, From), [{MsgId, Verified}]};
first_gossip({error, _Refused}, _From, _MsgId, _Round, _Publication, State) ->
    {State, [], []}.

on_ihave(From, #{msg_id := MsgId, round := Round}, State) ->
    classify_ihave(has_received(MsgId, State), From, MsgId, Round, State).

classify_ihave(true, _From, _MsgId, _Round, State) ->
    %% Already have it: nothing to do.
    {State, [], []};
classify_ihave(false, From, MsgId, Round, State) ->
    State1 = note_missing(State, MsgId, From),
    {State1, [{send, From, graft(State, MsgId, Round + 1)}], []}.

%% The tree moves only for a publication this node holds and sends: a GRAFT for any other id changes nothing.
on_graft(From, #{msg_id := MsgId, round := Round}, State) ->
    answer_graft(maps:find(MsgId, maps:get(received, State)), From, Round, State).

answer_graft(error, _From, _Round, State) ->
    {State, [], []};
answer_graft({ok, {Publication, _ExpiresAt}}, From, Round, State) ->
    {move_to_eager(State, From), [{send, From, gossip(Publication, Round)}], []}.

on_prune(From, State) ->
    {move_to_lazy(State, From), [], []}.

%% A publication verified under the node's profile and clock, for this node's realm.
verified(Frame, #{profile := Profile, realm := Realm}) ->
    in_realm(macula_frame:verify_publication(Frame, Profile, erlang:system_time(millisecond)), Realm).

in_realm({ok, #{realm := Realm}} = Verified, Realm) -> Verified;
in_realm({ok, _OtherRealm}, _Realm) -> {error, wrong_realm};
in_realm({error, _} = Refusal, _Realm) -> Refusal.

%%=====================================================================
%% State mutations
%%=====================================================================

mark_received(#{received := R} = S, MsgId, Publication, ExpiresAt) ->
    S#{received := R#{MsgId => {Publication, ExpiresAt}}}.

clear_missing(#{missing := M} = S, MsgId) ->
    S#{missing := maps:remove(MsgId, M)}.

note_missing(#{missing := M} = S, MsgId, From) ->
    {Peers, NotedAt} = maps:get(MsgId, M, {sets:new(), erlang:system_time(millisecond)}),
    S#{missing := M#{MsgId => {sets:add_element(From, Peers), NotedAt}}}.

move_to_eager(#{eager_push := E, lazy_push := L} = S, Peer) ->
    S#{eager_push := sets:add_element(Peer, E),
       lazy_push  := sets:del_element(Peer, L)}.

move_to_lazy(#{eager_push := E, lazy_push := L} = S, Peer) ->
    S#{eager_push := sets:del_element(Peer, E),
       lazy_push  := sets:add_element(Peer, L)}.

%%=====================================================================
%% Outbound builders
%%=====================================================================

build_pushes(#{eager_push := E, lazy_push := L} = State, MsgId, Round, Publication, ExceptPeer) ->
    Eager = [P || P <- sets:to_list(E), P =/= ExceptPeer],
    Lazy  = [P || P <- sets:to_list(L), P =/= ExceptPeer],
    [{send, P, gossip(Publication, Round)} || P <- Eager]
        ++ [{send, P, ihave(State, MsgId, Round)} || P <- Lazy].

gossip(Publication, Round) ->
    macula_frame:plumtree_gossip(#{publication => Publication, round => Round}).

ihave(#{realm := R}, MsgId, Round) ->
    macula_frame:plumtree_ihave(#{realm => R, msg_id => MsgId, round => Round}).

graft(#{realm := R}, MsgId, Round) ->
    macula_frame:plumtree_graft(#{realm => R, msg_id => MsgId, round => Round}).

prune(#{realm := R}) ->
    macula_frame:plumtree_prune(#{realm => R}).
