%% @doc Plumtree push-lazy gossip (Leitão, Pereira, Rodrigues 2007 —
%% Part 3 §7.2).
%%
%% Disseminates realm-scoped messages over HyParView's active view
%% with a tree-emergent topology and lazy-push recovery.
%%
%% == State ==
%%
%% <ul>
%%   <li><strong>eager_push</strong> — peers receiving full
%%       GOSSIP payloads. The eager-push set <em>is</em> the
%%       Plumtree spanning tree.</li>
%%   <li><strong>lazy_push</strong> — peers receiving only IHAVE
%%       announcements. They graft into eager_push when they GRAFT
%%       in response to an IHAVE.</li>
%%   <li><strong>received</strong> — `MsgId => Payload' map of
%%       messages we've delivered locally. Used to dedup repeat
%%       GOSSIPs and to answer GRAFTs.</li>
%%   <li><strong>missing</strong> — `MsgId => [Peer]' for peers
%%       who sent IHAVE for messages we have not yet received in
%%       full.</li>
%% </ul>
%%
%% == Message handling ==
%%
%% <ul>
%%   <li><strong>Local publish</strong> — record the message,
%%       deliver locally, GOSSIP to every eager peer, IHAVE to
%%       every lazy peer.</li>
%%   <li><strong>Receive GOSSIP</strong> — if first time: deliver,
%%       remove from missing, eager-forward to every other eager
%%       peer, IHAVE-forward to every lazy peer, ensure sender is
%%       eager. If duplicate: PRUNE the sender + move sender from
%%       eager to lazy.</li>
%%   <li><strong>Receive IHAVE</strong> — if already received,
%%       ignore. Else: record sender in `missing' and emit a
%%       GRAFT to the sender right away (Phase 5.3 MVP — a real
%%       deployment delays the GRAFT briefly to give the eager
%%       push a chance to win the race; the MVP eager-grafts
%%       which is correct but slightly heavier).</li>
%%   <li><strong>Receive GRAFT</strong> — sender becomes eager;
%%       reply with the GOSSIP payload if we have it, drop
%%       silently if not.</li>
%%   <li><strong>Receive PRUNE</strong> — move sender from eager
%%       to lazy.</li>
%% </ul>
%%
%% This module is pure. The wrapping process is responsible for
%% transmitting the action list and feeding deliveries to the
%% local consumer.
%%
%% Reference: plans/PLAN_MACULA_V2_PART3_DISCOVERY.md §7.2;
%% plans/PLAN_PHASE_5_BREAKDOWN.md Session 5.3.
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
    publish/3,
    process/3
]).

-export_type([state/0, peer/0, msg_id/0, action/0, delivery/0]).

-type peer()     :: macula_identity:pubkey().
-type msg_id()   :: <<_:128>>.

-type state() :: #{
    self_id    := peer(),
    realm      := <<_:256>>,
    identity   := macula_identity:key_pair(),
    eager_push := sets:set(peer()),
    lazy_push  := sets:set(peer()),
    received   := #{msg_id() => term()},
    missing    := #{msg_id() => sets:set(peer())}
}.

-type action()   :: {send, peer(), macula_frame:frame()}.
-type delivery() :: {msg_id(), term()}.

%%=====================================================================
%% Construction + view changes
%%=====================================================================

-spec new(macula_identity:key_pair(), <<_:256>>) -> state().
new(Identity, Realm) when is_binary(Realm), byte_size(Realm) =:= 32 ->
    #{
        self_id    => macula_identity:public(Identity),
        realm      => Realm,
        identity   => Identity,
        eager_push => sets:new(),
        lazy_push  => sets:new(),
        received   => #{},
        missing    => #{}
    }.

%% @doc When HyParView promotes a peer to active, the Plumtree
%% layer adds it to the eager-push set. Any GOSSIP we publish
%% will reach the new peer immediately.
-spec add_peer(state(), peer()) -> state().
add_peer(#{eager_push := E} = S, <<_:256>> = Peer) ->
    S#{eager_push := sets:add_element(Peer, E)}.

%% @doc When HyParView removes a peer from active, the Plumtree
%% layer removes it from both push sets.
-spec remove_peer(state(), peer()) -> state().
remove_peer(#{eager_push := E, lazy_push := L} = S,
            <<_:256>> = Peer) ->
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
%%
%% The local node publishes a message — record it, push fully to
%% every eager peer, lazily announce to every lazy peer. Returns
%% the new state, the list of outbound actions, and the local
%% delivery (the message itself, which the caller's consumer
%% should handle).
%%=====================================================================

-spec publish(state(), msg_id(), term()) ->
        {state(), [action()], [delivery()]}.
publish(State, <<_:128>> = MsgId, Payload) ->
    State1 = mark_received(State, MsgId, Payload),
    Actions = build_pushes(State1, MsgId, 0, Payload, undefined),
    {State1, Actions, [{MsgId, Payload}]}.

%%=====================================================================
%% Inbound dispatch
%%=====================================================================

-spec process(state(), peer(), macula_frame:frame()) ->
        {state(), [action()], [delivery()]}.
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
%% Handlers
%%=====================================================================

-spec on_gossip(peer(), macula_frame:frame(), state()) ->
        {state(), [action()], [delivery()]}.
on_gossip(From, Frame, State) ->
    MsgId   = maps:get(msg_id, Frame),
    Round   = maps:get(round, Frame),
    Payload = maps:get(payload, Frame),
    classify_gossip(has_received(MsgId, State),
                    From, MsgId, Round, Payload, State).

-spec classify_gossip(boolean(), peer(), msg_id(),
                      non_neg_integer(), term(), state()) ->
        {state(), [action()], [delivery()]}.
classify_gossip(true, From, _MsgId, _Round, _Payload, State) ->
    %% Duplicate — the sender should not be eager. PRUNE them.
    State1  = move_to_lazy(State, From),
    Action  = {send, From, signed_prune(State)},
    {State1, [Action], []};
classify_gossip(false, From, MsgId, Round, Payload, State) ->
    State1   = mark_received(State, MsgId, Payload),
    State2   = clear_missing(State1, MsgId),
    State3   = move_to_eager(State2, From),
    Pushes   = build_pushes(State3, MsgId, Round + 1, Payload, From),
    {State3, Pushes, [{MsgId, Payload}]}.

-spec on_ihave(peer(), macula_frame:frame(), state()) ->
        {state(), [action()], [delivery()]}.
on_ihave(From, Frame, State) ->
    MsgId = maps:get(msg_id, Frame),
    Round = maps:get(round, Frame),
    classify_ihave(has_received(MsgId, State),
                   From, MsgId, Round, State).

-spec classify_ihave(boolean(), peer(), msg_id(),
                     non_neg_integer(), state()) ->
        {state(), [action()], [delivery()]}.
classify_ihave(true, _From, _MsgId, _Round, State) ->
    %% Already have it — nothing to do.
    {State, [], []};
classify_ihave(false, From, MsgId, Round, State) ->
    State1 = note_missing(State, MsgId, From),
    Graft  = signed_graft(State, MsgId, Round + 1),
    {State1, [{send, From, Graft}], []}.

-spec on_graft(peer(), macula_frame:frame(), state()) ->
        {state(), [action()], [delivery()]}.
on_graft(From, Frame, State) ->
    MsgId = maps:get(msg_id, Frame),
    Round = maps:get(round, Frame),
    State1 = move_to_eager(State, From),
    answer_graft(maps:find(MsgId, maps:get(received, State1)),
                 From, MsgId, Round, State1).

-spec answer_graft({ok, term()} | error, peer(), msg_id(),
                   non_neg_integer(), state()) ->
        {state(), [action()], [delivery()]}.
answer_graft(error, _From, _MsgId, _Round, State) ->
    {State, [], []};
answer_graft({ok, Payload}, From, MsgId, Round, State) ->
    Gossip = signed_gossip(State, MsgId, Round, Payload),
    {State, [{send, From, Gossip}], []}.

-spec on_prune(peer(), state()) ->
        {state(), [action()], [delivery()]}.
on_prune(From, State) ->
    {move_to_lazy(State, From), [], []}.

%%=====================================================================
%% State mutations
%%=====================================================================

mark_received(#{received := R} = S, MsgId, Payload) ->
    S#{received := R#{MsgId => Payload}}.

clear_missing(#{missing := M} = S, MsgId) ->
    S#{missing := maps:remove(MsgId, M)}.

note_missing(#{missing := M} = S, MsgId, From) ->
    Existing = maps:get(MsgId, M, sets:new()),
    S#{missing := M#{MsgId => sets:add_element(From, Existing)}}.

move_to_eager(#{eager_push := E, lazy_push := L} = S, Peer) ->
    S#{eager_push := sets:add_element(Peer, E),
       lazy_push  := sets:del_element(Peer, L)}.

move_to_lazy(#{eager_push := E, lazy_push := L} = S, Peer) ->
    S#{eager_push := sets:del_element(Peer, E),
       lazy_push  := sets:add_element(Peer, L)}.

%%=====================================================================
%% Outbound builders
%%=====================================================================

build_pushes(#{eager_push := E, lazy_push := L} = State,
             MsgId, Round, Payload, ExceptPeer) ->
    Eager = [P || P <- sets:to_list(E), P =/= ExceptPeer],
    Lazy  = [P || P <- sets:to_list(L), P =/= ExceptPeer],
    [{send, P, signed_gossip(State, MsgId, Round, Payload)} || P <- Eager]
        ++ [{send, P, signed_ihave(State, MsgId, Round)} || P <- Lazy].

signed_gossip(#{realm := R, identity := Id}, MsgId, Round, Payload) ->
    macula_frame:sign(macula_frame:plumtree_gossip(
                        #{realm => R, msg_id => MsgId,
                          round => Round, payload => Payload}), Id).

signed_ihave(#{realm := R, identity := Id}, MsgId, Round) ->
    macula_frame:sign(macula_frame:plumtree_ihave(
                        #{realm => R, msg_id => MsgId,
                          round => Round}), Id).

signed_graft(#{realm := R, identity := Id}, MsgId, Round) ->
    macula_frame:sign(macula_frame:plumtree_graft(
                        #{realm => R, msg_id => MsgId,
                          round => Round}), Id).

signed_prune(#{realm := R, identity := Id}) ->
    macula_frame:sign(macula_frame:plumtree_prune(
                        #{realm => R}), Id).
