%% @doc HyParView active + passive partial-view data structure
%% (Leitão, Pereira, Rodrigues 2007 — implementation of Part 3 §7.1).
%%
%% Each realm member holds two bounded views of other members:
%%
%% <ul>
%%   <li><strong>Active view</strong> — symmetric, TCP-like.
%%       Default cap 5 (per Part 3 §7.1: `max(5, ceil(log₂(N)))'
%%       capped at 15). Plumtree gossip eager-pushes along
%%       active-view edges.</li>
%%   <li><strong>Passive view</strong> — candidate pool. Default
%%       cap is `4 × active'. Promoted into the active view on
%%       active-peer failure; refreshed via periodic shuffles
%%       with active neighbours.</li>
%% </ul>
%%
%% This module is pure. Mutating ops return a new view; the
%% wire-protocol orchestration (JOIN / FORWARD_JOIN / NEIGHBOR /
%% SHUFFLE / SHUFFLE_REPLY) lands in Session 5.2 on top of this.
%%
%% == Eviction policy ==
%%
%% Whenever a `add_active/2' or `promote/2' would push the active
%% view past `active_cap', a uniformly-random current active peer
%% is demoted to the passive view to make room. Symmetric removal
%% on the other side is the protocol layer's responsibility.
%%
%% Whenever `add_passive/2' or `merge_shuffle/2' would push the
%% passive view past `passive_cap', a uniformly-random current
%% passive peer is dropped to make room.
%%
%% Reference: plans/PLAN_MACULA_V2_PART3_DISCOVERY.md §7.1;
%% plans/PLAN_PHASE_5_BREAKDOWN.md Session 5.1.
-module(macula_hyparview_view).

-export([
    new/1, new/2,
    self_id/1,
    active_cap/1,
    passive_cap/1,
    active/1,
    passive/1,
    active_size/1,
    passive_size/1,
    is_active/2,
    is_passive/2,
    contains/2,
    add_active/2,
    add_passive/2,
    remove_active/2,
    remove_passive/2,
    promote/2,
    demote/2,
    random_active/1,
    random_active_subset/2,
    random_passive_subset/2,
    merge_shuffle/2,
    counts/1
]).

-export_type([view/0, peer/0, opts/0, counts/0]).

-define(DEFAULT_ACTIVE_CAP,     5).
-define(DEFAULT_PASSIVE_RATIO,  4).

-type peer() :: macula_identity:pubkey().

-type opts() :: #{
    active_cap   => pos_integer(),
    passive_cap  => pos_integer()
}.

-type view() :: #{
    self_id     := peer(),
    active_cap  := pos_integer(),
    passive_cap := pos_integer(),
    active      := sets:set(peer()),
    passive     := sets:set(peer())
}.

-type counts() :: #{
    active  := non_neg_integer(),
    passive := non_neg_integer()
}.

%%=====================================================================
%% Construction
%%=====================================================================

-spec new(peer()) -> view().
new(Self) ->
    new(Self, #{}).

-spec new(peer(), opts()) -> view().
new(<<_:256>> = Self, Opts) when is_map(Opts) ->
    AC = maps:get(active_cap, Opts, ?DEFAULT_ACTIVE_CAP),
    PC = maps:get(passive_cap, Opts, AC * ?DEFAULT_PASSIVE_RATIO),
    valid_caps(AC, PC),
    #{
        self_id     => Self,
        active_cap  => AC,
        passive_cap => PC,
        active      => sets:new(),
        passive     => sets:new()
    }.

-spec valid_caps(integer(), integer()) -> ok.
valid_caps(AC, PC) when AC >= 1, PC >= AC -> ok.

%%=====================================================================
%% Inspection
%%=====================================================================

self_id(#{self_id := S}) -> S.

active_cap(#{active_cap := C}) -> C.

passive_cap(#{passive_cap := C}) -> C.

active(#{active := A}) -> sets:to_list(A).

passive(#{passive := P}) -> sets:to_list(P).

active_size(#{active := A}) -> sets:size(A).

passive_size(#{passive := P}) -> sets:size(P).

is_active(Peer, #{active := A}) -> sets:is_element(Peer, A).

is_passive(Peer, #{passive := P}) -> sets:is_element(Peer, P).

contains(Peer, V) ->
    is_active(Peer, V) orelse is_passive(Peer, V).

counts(#{active := A, passive := P}) ->
    #{active => sets:size(A), passive => sets:size(P)}.

%%=====================================================================
%% Active view mutations
%%=====================================================================

-spec add_active(view(), peer()) -> view().
add_active(V, Peer) ->
    dispatch_add_active(eligible(Peer, V), Peer, V).

-spec dispatch_add_active(eligibility(), peer(), view()) -> view().
dispatch_add_active(self, _Peer, V)        -> V;
dispatch_add_active(already_active, _P, V) -> V;
dispatch_add_active(was_passive, Peer, V)  ->
    promote_internal(Peer, V);
dispatch_add_active(fresh, Peer, V) ->
    insert_active(Peer, V).

-spec insert_active(peer(), view()) -> view().
insert_active(Peer, #{active := A, active_cap := C} = V) ->
    case sets:size(A) >= C of
        false -> V#{active := sets:add_element(Peer, A)};
        true  -> insert_active(Peer, demote_random_active(V))
    end.

-spec demote_random_active(view()) -> view().
demote_random_active(#{active := A, passive := P} = V) ->
    Victim = random_element(A),
    A1 = sets:del_element(Victim, A),
    P1 = bounded_passive_add(Victim, P, V),
    V#{active := A1, passive := P1}.

-spec remove_active(view(), peer()) -> view().
remove_active(#{active := A} = V, Peer) ->
    V#{active := sets:del_element(Peer, A)}.

%%=====================================================================
%% Passive view mutations
%%=====================================================================

-spec add_passive(view(), peer()) -> view().
add_passive(V, Peer) ->
    dispatch_add_passive(eligible(Peer, V), Peer, V).

-spec dispatch_add_passive(eligibility(), peer(), view()) -> view().
dispatch_add_passive(self, _Peer, V)        -> V;
dispatch_add_passive(already_active, _P, V) -> V;
dispatch_add_passive(was_passive, _P, V)    -> V;
dispatch_add_passive(fresh, Peer, #{passive := P} = V) ->
    V#{passive := bounded_passive_add(Peer, P, V)}.

-spec remove_passive(view(), peer()) -> view().
remove_passive(#{passive := P} = V, Peer) ->
    V#{passive := sets:del_element(Peer, P)}.

%%=====================================================================
%% View transitions
%%=====================================================================

%% @doc Move a peer from passive to active. The peer must already
%% be in the passive view; otherwise this is equivalent to
%% `add_active/2'.
-spec promote(view(), peer()) -> view().
promote(V, Peer) ->
    dispatch_add_active(eligible(Peer, V), Peer, V).

-spec promote_internal(peer(), view()) -> view().
promote_internal(Peer, #{passive := P} = V) ->
    insert_active(Peer, V#{passive := sets:del_element(Peer, P)}).

%% @doc Move a peer from active to passive (e.g. on graceful peer
%% disconnect that should be retained as a candidate).
-spec demote(view(), peer()) -> view().
demote(#{active := A, passive := P} = V, Peer) ->
    case sets:is_element(Peer, A) of
        false -> V;
        true ->
            A1 = sets:del_element(Peer, A),
            P1 = bounded_passive_add(Peer, P, V),
            V#{active := A1, passive := P1}
    end.

%%=====================================================================
%% Random sampling
%%=====================================================================

-spec random_active(view()) -> {ok, peer()} | empty.
random_active(#{active := A}) ->
    sample_one(A).

-spec random_active_subset(view(), non_neg_integer()) -> [peer()].
random_active_subset(#{active := A}, K) ->
    sample_subset(A, K).

-spec random_passive_subset(view(), non_neg_integer()) -> [peer()].
random_passive_subset(#{passive := P}, K) ->
    sample_subset(P, K).

%%=====================================================================
%% Shuffle merge
%%
%% Incoming peers from a SHUFFLE_REPLY are added to the passive
%% view (any duplicates of self / active / passive are silently
%% dropped). Bounded eviction kicks in if the passive view would
%% overflow.
%%=====================================================================

-spec merge_shuffle(view(), [peer()]) -> view().
merge_shuffle(V, Peers) ->
    lists:foldl(fun(P, Acc) -> add_passive(Acc, P) end, V, Peers).

%%=====================================================================
%% Internal helpers
%%=====================================================================

-type eligibility() :: self | already_active | was_passive | fresh.

-spec eligible(peer(), view()) -> eligibility().
eligible(Peer, #{self_id := Self}) when Peer =:= Self -> self;
eligible(Peer, #{active := A, passive := P}) ->
    case {sets:is_element(Peer, A), sets:is_element(Peer, P)} of
        {true,  _}    -> already_active;
        {false, true} -> was_passive;
        {false, false} -> fresh
    end.

-spec bounded_passive_add(peer(), sets:set(peer()), view()) ->
        sets:set(peer()).
bounded_passive_add(Peer, P, #{passive_cap := C}) ->
    P1 = case sets:size(P) >= C of
             false -> P;
             true  -> sets:del_element(random_element(P), P)
         end,
    sets:add_element(Peer, P1).

-spec random_element(sets:set(peer())) -> peer().
random_element(S) ->
    L = sets:to_list(S),
    lists:nth(rand:uniform(length(L)), L).

-spec sample_one(sets:set(peer())) -> {ok, peer()} | empty.
sample_one(S) ->
    case sets:size(S) of
        0 -> empty;
        _ -> {ok, random_element(S)}
    end.

-spec sample_subset(sets:set(peer()), non_neg_integer()) -> [peer()].
sample_subset(_S, 0) ->
    [];
sample_subset(S, K) when is_integer(K), K > 0 ->
    L = sets:to_list(S),
    Shuffled = [X || {_, X} <- lists:keysort(
                                  1, [{rand:uniform(), Y} || Y <- L])],
    lists:sublist(Shuffled, K).
