%% @doc Observed-Remove Set CRDT (Part 3 §7.4).
%%
%% Realm-shared mutable state (member lists, chat threads,
%% directory metadata) converges across nodes without coordination.
%% Each `add/2' tags the element with a unique 16-byte random tag;
%% `remove/2' tombstones every <em>currently observed</em> tag.
%% Concurrent add+remove of the same element keeps the element
%% (the new tag wasn't observed by the remover) — the property
%% that makes OR-Set the right CRDT for "did Alice add a member
%% just before I removed?" semantics.
%%
%% == State ==
%%
%% <ul>
%%   <li>`data :: #{Element => sets:set(Tag)}' — currently-active
%%       tags per element. An element is "in the set" iff its tag
%%       set is non-empty.</li>
%%   <li>`tombstones :: sets:set(Tag)' — tags removed by an
%%       observed `remove/2'. New deltas carrying these tags are
%%       suppressed so a delayed re-broadcast of a removed add
%%       cannot resurrect the element.</li>
%% </ul>
%%
%% == Convergence ==
%%
%% Two interfaces:
%%
%% <ul>
%%   <li>`merge/2' — full-state union for catch-up sync.
%%       Combines tag sets element-wise, subtracts the unioned
%%       tombstone set, drops elements that lose all live tags.</li>
%%   <li>`apply_delta/2' — incremental delta application for
%%       per-op gossip over Plumtree. Applies one
%%       `{add, Element, Tag}' or `{remove, [Tag]}' delta.</li>
%% </ul>
%%
%% Both produce the same final state given the same total set of
%% operations — the OR-Set's strong eventual consistency guarantee.
%%
%% Reference: plans/PLAN_MACULA_V2_PART3_DISCOVERY.md §7.4;
%% plans/PLAN_PHASE_5_BREAKDOWN.md Session 5.4.
-module(hecate_or_set).

-export([
    new/0,
    add/2,
    remove/2,
    members/1,
    contains/2,
    size/1,
    is_empty/1,
    tags_for/2,
    tombstones/1,
    tombstone_count/1,
    merge/2,
    apply_delta/2
]).

-export_type([or_set/0, element/0, tag/0, delta/0]).

-type element() :: term().
-type tag()     :: <<_:128>>.

-type or_set() :: #{
    data       := #{element() => sets:set(tag())},
    tombstones := sets:set(tag())
}.

-type delta() ::
      {add, element(), tag()}
    | {remove, [tag()]}.

%%=====================================================================
%% Construction
%%=====================================================================

-spec new() -> or_set().
new() ->
    #{data => #{}, tombstones => sets:new()}.

%%=====================================================================
%% Mutators
%%=====================================================================

%% @doc Add `Element' with a fresh tag. Returns the new set plus the
%% delta to broadcast.
-spec add(or_set(), element()) -> {or_set(), delta()}.
add(Set, Element) ->
    Tag = fresh_tag(),
    {add_with_tag(Set, Element, Tag), {add, Element, Tag}}.

%% @doc Remove `Element' by tombstoning every currently-observed
%% tag. If the element is absent the operation is a no-op (delta
%% carries an empty tag list).
-spec remove(or_set(), element()) -> {or_set(), delta()}.
remove(#{data := D, tombstones := T} = Set, Element) ->
    Existing = maps:get(Element, D, sets:new()),
    Tags = sets:to_list(Existing),
    NewTomb = sets:union(T, Existing),
    NewData = maps:remove(Element, D),
    {Set#{data := NewData, tombstones := NewTomb}, {remove, Tags}}.

%%=====================================================================
%% Inspection
%%=====================================================================

-spec members(or_set()) -> [element()].
members(#{data := D}) ->
    [E || E <- maps:keys(D), not is_empty_tags(maps:get(E, D))].

-spec contains(or_set(), element()) -> boolean().
contains(#{data := D}, Element) ->
    case maps:find(Element, D) of
        {ok, S} -> not is_empty_tags(S);
        error   -> false
    end.

-spec size(or_set()) -> non_neg_integer().
size(Set) -> length(members(Set)).

-spec is_empty(or_set()) -> boolean().
is_empty(Set) -> members(Set) =:= [].

-spec tags_for(or_set(), element()) -> [tag()].
tags_for(#{data := D}, Element) ->
    sets:to_list(maps:get(Element, D, sets:new())).

-spec tombstones(or_set()) -> [tag()].
tombstones(#{tombstones := T}) -> sets:to_list(T).

-spec tombstone_count(or_set()) -> non_neg_integer().
tombstone_count(#{tombstones := T}) -> sets:size(T).

%%=====================================================================
%% Convergence
%%=====================================================================

%% @doc Full-state union — element-wise tag union, then subtract
%% the merged tombstones, then drop elements with no live tags.
-spec merge(or_set(), or_set()) -> or_set().
merge(#{data := DA, tombstones := TA},
      #{data := DB, tombstones := TB}) ->
    AllTombs = sets:union(TA, TB),
    AllElements = lists:usort(maps:keys(DA) ++ maps:keys(DB)),
    Merged = lists:foldl(
               fun(E, Acc) ->
                   TagsA = maps:get(E, DA, sets:new()),
                   TagsB = maps:get(E, DB, sets:new()),
                   Live  = sets:subtract(sets:union(TagsA, TagsB),
                                         AllTombs),
                   keep_if_live(E, Live, Acc)
               end, #{}, AllElements),
    #{data => Merged, tombstones => AllTombs}.

-spec keep_if_live(element(), sets:set(tag()),
                   #{element() => sets:set(tag())}) ->
        #{element() => sets:set(tag())}.
keep_if_live(_E, Live, Acc) ->
    case is_empty_tags(Live) of
        true  -> Acc;
        false -> Acc#{_E => Live}
    end.

%% @doc Apply a single delta (the per-op product of `add/2' or
%% `remove/2'). Idempotent — re-applying the same delta is a
%% no-op. Tombstoned tags are silently suppressed in `add' deltas
%% so a delayed re-broadcast cannot resurrect a removed element.
-spec apply_delta(or_set(), delta()) -> or_set().
apply_delta(Set, {add, Element, Tag}) ->
    apply_add(sets:is_element(Tag, maps:get(tombstones, Set)),
              Set, Element, Tag);
apply_delta(Set, {remove, Tags}) ->
    apply_remove(Set, Tags).

-spec apply_add(boolean(), or_set(), element(), tag()) -> or_set().
apply_add(true,  Set, _Element, _Tag) -> Set;
apply_add(false, Set, Element, Tag)   -> add_with_tag(Set, Element, Tag).

-spec apply_remove(or_set(), [tag()]) -> or_set().
apply_remove(#{data := D, tombstones := T} = Set, Tags) ->
    TagSet = sets:from_list(Tags),
    NewTomb = sets:union(T, TagSet),
    NewData = strip_tags(D, TagSet),
    Set#{data := NewData, tombstones := NewTomb}.

-spec strip_tags(#{element() => sets:set(tag())}, sets:set(tag())) ->
        #{element() => sets:set(tag())}.
strip_tags(Data, Removed) ->
    maps:fold(fun(E, TagSet, Acc) ->
                  Live = sets:subtract(TagSet, Removed),
                  keep_if_live(E, Live, Acc)
              end, #{}, Data).

%%=====================================================================
%% Internal
%%=====================================================================

-spec fresh_tag() -> tag().
fresh_tag() ->
    crypto:strong_rand_bytes(16).

-spec add_with_tag(or_set(), element(), tag()) -> or_set().
add_with_tag(#{data := D} = Set, Element, Tag) ->
    Existing = maps:get(Element, D, sets:new()),
    Set#{data := D#{Element => sets:add_element(Tag, Existing)}}.

-spec is_empty_tags(sets:set(tag())) -> boolean().
is_empty_tags(S) -> sets:size(S) =:= 0.
