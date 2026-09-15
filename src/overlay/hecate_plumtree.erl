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
%%   <li><strong>missing</strong>: `MsgId => {#{Peer => GraftedAt}, NotedAt}' for the peers who sent IHAVE for a
%%       publication not yet received in full, when this node sent each its GRAFT (monotonic milliseconds), and when
%%       the first IHAVE for it came (wall-clock milliseconds); forgotten after 70 minutes (`sweep/2').</li>
%%   <li><strong>open</strong>: `Peer => Count', how many missing entries each neighbour is on. A neighbour is on at
%%       most 1,024 (DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md, 3.1).</li>
%% </ul>
%%
%% == Message handling ==
%%
%% <ul>
%%   <li><strong>Local publish</strong>: verify the PUBLISH, record its publication, deliver it locally, GOSSIP it to
%%       every eager peer and IHAVE it to every lazy peer.</li>
%%   <li><strong>Receive GOSSIP</strong>: the first time, verify the publication; when it verifies, deliver it, remove
%%       it from missing, eager-forward it to every other eager peer, IHAVE-forward it to every lazy peer and ensure the
%%       sender is eager, and when it does not, drop it and return the refusal. Either way its missing entry ends, at
%%       no charge to its announcers. A duplicate is not verified again: PRUNE the sender and move
%%       it from eager to lazy.</li>
%%   <li><strong>Receive IHAVE</strong>: if already received, or the sender is already on its missing entry, ignore.
%%       A neighbour on 1,024 open entries gets no new one: its IHAVE is not recorded, gets no GRAFT and returns a
%%       refusal. Else record the sender with the time of its GRAFT and emit a GRAFT to the sender right away
%%       (Phase 5.3 MVP: a real deployment delays the GRAFT briefly to give the eager
%%       push a chance to win the race; eager grafting is correct but slightly heavier).</li>
%%   <li><strong>Receive GRAFT</strong>: for a publication this node holds, the sender becomes eager and gets the
%%       GOSSIP publication; for any other id nothing changes, the push sets included.</li>
%%   <li><strong>Receive PRUNE</strong>: move the sender from eager to lazy.</li>
%%   <li><strong>A sender outside both push sets</strong>: a frame moves a peer between the sets and never adds one.
%%       Through a station's relay the sender is the frame's origin, which need not be a neighbour, so from such a
%%       sender a GRAFT, a PRUNE and an IHAVE move no one, send nothing and are refused as `not_a_peer', a first GOSSIP
%%       delivers and forwards its verified publication but moves no one, and a duplicate GOSSIP gets no PRUNE and is
%%       refused as `not_a_peer' too.</li>
%% </ul>
%%
%% This module is pure apart from reading the configured profile when a node starts. The caller passes the clocks:
%% wall-clock milliseconds for publication freshness and retention, and monotonic milliseconds for GRAFT timing.
%% The wrapping process transmits the send actions, reports each `{refused, Peer, Kind}' action through
%% `macula_peering:object_refused/2', feeds deliveries to the local consumer, calls `expired_grafts/2' about once a
%% second, and calls `sweep/2' on a timer, so a node remembers a publication hash until the publication expires, and
%% no longer.
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
    open_count/2,
    publish/3,
    process/4,
    expired_grafts/2,
    sweep/2
]).

-export_type([state/0, peer/0, msg_id/0, action/0, delivery/0, clocks/0]).

%% The longest a publication can live: a published_at up to 5 minutes ahead, a ttl_ms of at most one hour and 5 minutes
%% of tolerance. No announcement can name a live publication for longer.
-define(MISSING_MAX_AGE_MS, 70 * 60000).
%% A neighbour is on at most 1,024 open missing entries, and a GRAFT unanswered for 10 seconds costs it that entry
%% (DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md, 3.1).
-define(OPEN_ENTRIES_MAX, 1024).
-define(GRAFT_ANSWER_MS, 10_000).

-type peer()     :: <<_:256>>.
-type msg_id()   :: <<_:384>>.

-type state() :: #{
    self_id    := peer(),
    realm      := <<_:256>>,
    profile    := macula_crypto_profile:profile(),
    eager_push := sets:set(peer()),
    lazy_push  := sets:set(peer()),
    received   := #{msg_id() => {macula_signed_object:object(), non_neg_integer()}},
    missing    := #{msg_id() => {#{peer() => integer()}, integer()}},
    open       := #{peer() => pos_integer()}
}.

%% The caller's clocks: wall-clock milliseconds for publication freshness and retention, monotonic milliseconds for
%% GRAFT timing.
-type clocks() :: #{wall := integer(), monotonic := integer()}.

-type action()   :: {send, peer(), macula_frame:frame()}
                  | {refused, peer(), ihave_allowance | graft_unanswered | wrong_realm | not_a_peer
                                      | macula_frame_refusal()}.
-type macula_frame_refusal() :: malformed_frame | signature_invalid | key_id_mismatch
                              | {not_yet_valid, pos_integer()} | {expired, pos_integer()}.
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
        missing    => #{},
        open       => #{}
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
open_count(Peer, #{open := O})   -> maps:get(Peer, O, 0).

%%=====================================================================
%% Local publish
%%=====================================================================

%% @doc Publish a PUBLISH this node made: verify its publication, record it, push it fully to every eager peer and
%% announce it to every lazy peer, and return it as the local delivery the caller's consumer handles. A publication
%% that does not verify, or names another realm, is refused; one already received is not delivered again.
-spec publish(state(), macula_frame:frame(), integer()) -> {state(), [action()], [delivery()]} | {error, term()}.
publish(State, #{frame_type := publish, publication := #{tbs := Tbs} = Publication} = Frame, WallMs) ->
    MsgId = crypto:hash(sha384, Tbs),
    local_publish(has_received(MsgId, State), MsgId, Publication, Frame, State, WallMs).

local_publish(true, _MsgId, _Publication, _Frame, State, _WallMs) ->
    {State, [], []};
local_publish(false, MsgId, Publication, Frame, State, WallMs) ->
    published(verified(Frame, State, WallMs), MsgId, Publication, State).

published({ok, #{expires_at := ExpiresAt} = Verified}, MsgId, Publication, State) ->
    State1 = mark_received(State, MsgId, Publication, ExpiresAt),
    {State1, build_pushes(State1, MsgId, 0, Publication, undefined), [{MsgId, Verified}]};
published({error, _} = Refusal, _MsgId, _Publication, _State) ->
    Refusal.

%%=====================================================================
%% Inbound dispatch
%%=====================================================================

%% @doc Handle a Plumtree frame from the neighbour `From', at the caller's clocks.
-spec process(state(), peer(), macula_frame:frame(), clocks()) -> {state(), [action()], [delivery()]}.
process(State, From, #{frame_type := plumtree_gossip} = F, #{wall := WallMs}) ->
    on_gossip(From, F, State, WallMs);
process(State, From, #{frame_type := plumtree_ihave} = F, Clocks) ->
    on_ihave(From, F, State, Clocks);
process(State, From, #{frame_type := plumtree_graft} = F, _Clocks) ->
    on_graft(From, F, State);
process(State, From, #{frame_type := plumtree_prune}, _Clocks) ->
    on_prune(From, State);
process(State, _From, _Frame, _Clocks) ->
    {State, [], []}.

%%=====================================================================
%% Retention
%%=====================================================================

%% @doc Forget, at `NowMs' in milliseconds of wall-clock time, every received publication that has expired (its
%% published_at plus its ttl_ms, or 10 minutes without one, plus 5 minutes) and every missing publication first
%% announced more than 70 minutes ago. A node keeps a publication hash until the publication expires, and no longer; a
%% later copy is refused by verification, and a GRAFT for it gets no answer. Each neighbour still on a forgotten
%% missing entry has one open entry less, at no charge.
-spec sweep(state(), integer()) -> state().
sweep(#{received := R, missing := M} = S, NowMs) when is_integer(NowMs) ->
    Oldest = NowMs - ?MISSING_MAX_AGE_MS,
    Forgotten = [MsgId || {MsgId, {_Peers, NotedAt}} <- maps:to_list(M), NotedAt < Oldest],
    lists:foldl(fun ended_entry/2, S#{received := maps:filter(live_at(NowMs), R)}, Forgotten).

live_at(NowMs) ->
    fun(_MsgId, {_Publication, ExpiresAt}) -> ExpiresAt >= NowMs end.

%% @doc Take each neighbour off the missing entries whose GRAFT it has not answered within 10 seconds, at `MonoMs' in
%% monotonic milliseconds: one refused action per neighbour and entry, and an entry left with no neighbour ends. The
%% wrapping process calls this about once a second.
-spec expired_grafts(state(), integer()) -> {state(), [action()]}.
expired_grafts(#{missing := M} = S, MonoMs) when is_integer(MonoMs) ->
    Unanswered = [{MsgId, Peer} || {MsgId, {Peers, _NotedAt}} <- maps:to_list(M),
                                   {Peer, GraftedAt} <- maps:to_list(Peers),
                                   MonoMs - GraftedAt >= ?GRAFT_ANSWER_MS],
    {lists:foldl(fun taken_off/2, S, Unanswered), [{refused, Peer, graft_unanswered} || {_MsgId, Peer} <- Unanswered]}.

taken_off({MsgId, Peer}, #{missing := M} = S) ->
    {Peers, NotedAt} = maps:get(MsgId, M),
    one_open_less(Peer, S#{missing := remaining(maps:remove(Peer, Peers), NotedAt, MsgId, M)}).

remaining(Peers, _NotedAt, MsgId, M) when map_size(Peers) =:= 0 -> maps:remove(MsgId, M);
remaining(Peers, NotedAt, MsgId, M) -> M#{MsgId => {Peers, NotedAt}}.

%%=====================================================================
%% Handlers
%%=====================================================================

on_gossip(From, #{publication := #{tbs := Tbs} = Publication} = Frame, State, WallMs) ->
    MsgId = crypto:hash(sha384, Tbs),
    classify_gossip(has_received(MsgId, State), From, MsgId, Publication, Frame, State, WallMs).

classify_gossip(true, From, _MsgId, _Publication, _Frame, State, _WallMs) ->
    %% A duplicate, recognised by its id and not verified again. The sender should not be eager: PRUNE it, unless it
    %% is no peer at all, which gets nothing and is refused as not_a_peer, so the connection counts it.
    duplicate_from(is_peer(From, State), From, State);
classify_gossip(false, From, MsgId, Publication, #{round := Round} = Frame, State, WallMs) ->
    first_gossip(verified(Frame, State, WallMs), From, MsgId, Round, Publication, State).

duplicate_from(true, From, State) -> {move_to_lazy(State, From), [{send, From, prune(State)}], []};
duplicate_from(false, From, State) -> {State, [{refused, From, not_a_peer}], []}.

first_gossip({ok, #{expires_at := ExpiresAt} = Verified}, From, MsgId, Round, Publication, State) ->
    State1 = mark_received(State, MsgId, Publication, ExpiresAt),
    State2 = ended_entry(MsgId, State1),
    State3 = move_to_eager(State2, From),
    {State3, build_pushes(State3, MsgId, Round + 1, Publication, From), [{MsgId, Verified}]};
first_gossip({error, Refused}, From, MsgId, _Round, _Publication, State) ->
    {ended_entry(MsgId, State), [{refused, From, Refused}], []}.

%% An IHAVE for a publication not yet received records its sender with the time of its GRAFT and GRAFTs it, unless the
%% sender is already on that entry, which changes nothing, or is on 1,024 open entries already, which records nothing,
%% sends no GRAFT and is refused.
%% An IHAVE from a sender outside both push sets records nothing, sends no GRAFT and is refused as not_a_peer.
on_ihave(From, #{msg_id := MsgId, round := Round}, State, Clocks) ->
    Kind = ihave_kind(is_peer(From, State), has_received(MsgId, State), From, MsgId, State),
    classify_ihave(Kind, From, MsgId, Round, State, Clocks).

ihave_kind(false, _Received, _From, _MsgId, _State) ->
    not_a_peer;
ihave_kind(true, true, _From, _MsgId, _State) ->
    received;
ihave_kind(true, false, From, MsgId, #{missing := M, open := O}) ->
    announcer_kind(maps:find(MsgId, M), From, maps:get(From, O, 0)).

announcer_kind({ok, {Peers, _NotedAt}}, From, _Open) when is_map_key(From, Peers) -> announced;
announcer_kind(_Entry, _From, Open) when Open >= ?OPEN_ENTRIES_MAX -> over_allowance;
announcer_kind(_Entry, _From, _Open) -> new_announcer.

classify_ihave(not_a_peer, From, _MsgId, _Round, State, _Clocks) ->
    {State, [{refused, From, not_a_peer}], []};
classify_ihave(received, _From, _MsgId, _Round, State, _Clocks) ->
    {State, [], []};
classify_ihave(announced, _From, _MsgId, _Round, State, _Clocks) ->
    {State, [], []};
classify_ihave(over_allowance, From, _MsgId, _Round, State, _Clocks) ->
    {State, [{refused, From, ihave_allowance}], []};
classify_ihave(new_announcer, From, MsgId, Round, State, Clocks) ->
    {note_missing(State, MsgId, From, Clocks), [{send, From, graft(State, MsgId, Round + 1)}], []}.

%% The tree moves only for a publication this node holds and sends: a GRAFT for any other id changes nothing. A GRAFT
%% from a sender outside both push sets gets nothing, moves no one and is refused as not_a_peer.
on_graft(From, #{msg_id := MsgId, round := Round}, State) ->
    graft_from(is_peer(From, State), maps:find(MsgId, maps:get(received, State)), From, Round, State).

graft_from(false, _Held, From, _Round, State) -> {State, [{refused, From, not_a_peer}], []};
graft_from(true, Held, From, Round, State) -> answer_graft(Held, From, Round, State).

answer_graft(error, _From, _Round, State) ->
    {State, [], []};
answer_graft({ok, {Publication, _ExpiresAt}}, From, Round, State) ->
    {move_to_eager(State, From), [{send, From, gossip(Publication, Round)}], []}.

%% A PRUNE moves its sender from eager to lazy. From a sender outside both push sets it moves no one and is refused as
%% not_a_peer.
on_prune(From, State) ->
    prune_from(is_peer(From, State), From, State).

prune_from(true, From, State) -> {move_to_lazy(State, From), [], []};
prune_from(false, From, State) -> {State, [{refused, From, not_a_peer}], []}.

%% A publication verified under the node's profile at `WallMs', for this node's realm.
verified(Frame, #{profile := Profile, realm := Realm}, WallMs) ->
    in_realm(macula_frame:verify_publication(Frame, Profile, WallMs), Realm).

in_realm({ok, #{realm := Realm}} = Verified, Realm) -> Verified;
in_realm({ok, _OtherRealm}, _Realm) -> {error, wrong_realm};
in_realm({error, _} = Refusal, _Realm) -> Refusal.

%%=====================================================================
%% State mutations
%%=====================================================================

mark_received(#{received := R} = S, MsgId, Publication, ExpiresAt) ->
    S#{received := R#{MsgId => {Publication, ExpiresAt}}}.

%% A missing entry ends when its publication arrives, is refused or is forgotten. Each neighbour on it has one open
%% entry less, and none of them is charged.
ended_entry(MsgId, #{missing := M} = S) ->
    closed_for(maps:find(MsgId, M), MsgId, S).

closed_for({ok, {Peers, _NotedAt}}, MsgId, #{missing := M} = S) ->
    lists:foldl(fun one_open_less/2, S#{missing := maps:remove(MsgId, M)}, maps:keys(Peers));
closed_for(error, _MsgId, S) ->
    S.

one_open_less(Peer, #{open := O} = S) ->
    S#{open := fewer(maps:get(Peer, O) - 1, Peer, O)}.

fewer(0, Peer, O) -> maps:remove(Peer, O);
fewer(Count, Peer, O) -> O#{Peer := Count}.

note_missing(#{missing := M, open := O} = S, MsgId, From, #{wall := WallMs, monotonic := MonoMs}) ->
    {Peers, NotedAt} = maps:get(MsgId, M, {#{}, WallMs}),
    S#{missing := M#{MsgId => {Peers#{From => MonoMs}, NotedAt}}, open := O#{From => maps:get(From, O, 0) + 1}}.

%% A frame moves a peer between the push sets and never adds one: only add_peer/2 and remove_peer/2, as HyParView
%% changes its active view, change who is in them. A sender in neither set, such as a relayed origin that is no
%% neighbour, leaves both sets as they are.
move_to_eager(S, Peer) ->
    moved_to_eager(is_peer(Peer, S), S, Peer).

moved_to_eager(true, #{eager_push := E, lazy_push := L} = S, Peer) ->
    S#{eager_push := sets:add_element(Peer, E), lazy_push := sets:del_element(Peer, L)};
moved_to_eager(false, S, _Peer) ->
    S.

move_to_lazy(S, Peer) ->
    moved_to_lazy(is_peer(Peer, S), S, Peer).

moved_to_lazy(true, #{eager_push := E, lazy_push := L} = S, Peer) ->
    S#{eager_push := sets:del_element(Peer, E), lazy_push := sets:add_element(Peer, L)};
moved_to_lazy(false, S, _Peer) ->
    S.

%% Whether a sender is one of this node's peers, in either push set.
is_peer(Peer, #{eager_push := E, lazy_push := L}) ->
    sets:is_element(Peer, E) orelse sets:is_element(Peer, L).

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
