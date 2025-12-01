%%%-------------------------------------------------------------------
%%% @doc Conflict-Free Replicated Data Types (CRDTs) for shared state.
%%%
%%% Implements CRDTs for eventually-consistent distributed state management:
%%% - LWW-Register (Last-Write-Wins Register) - single value with timestamp
%%% - OR-Set (Observed-Remove Set) - set with add/remove semantics
%%% - G-Counter (Grow-only Counter) - monotonically increasing counter
%%% - PN-Counter (Positive-Negative Counter) - increment/decrement counter
%%%
%%% These CRDTs replace Ra/Raft consensus for Macula's masterless architecture.
%%% State is synchronized via gossip protocol between nodes.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_crdt).

%% LWW-Register API
-export([
    new_lww_register/0,
    new_lww_register/1,
    lww_set/3,
    lww_get/1,
    lww_merge/2,
    lww_timestamp/1,
    lww_value/1
]).

%% OR-Set API
-export([
    new_or_set/0,
    or_add/2,
    or_add/3,
    or_remove/2,
    or_contains/2,
    or_elements/1,
    or_size/1,
    or_merge/2
]).

%% G-Counter API
-export([
    new_gcounter/0,
    gcounter_increment/1,
    gcounter_increment/2,
    gcounter_value/1,
    gcounter_merge/2
]).

%% PN-Counter API
-export([
    new_pncounter/0,
    pncounter_increment/1,
    pncounter_increment/2,
    pncounter_decrement/1,
    pncounter_decrement/2,
    pncounter_value/1,
    pncounter_merge/2
]).

%% Type definitions
-type timestamp() :: non_neg_integer().
-type unique_tag() :: binary().

-type lww_register() :: #{
    value => term(),
    timestamp => timestamp(),
    node => node()
}.

%% OR-Set: each element has unique tags for add operations
%% An element is in the set if it has at least one tag not in tombstones
-type or_set() :: #{
    elements => #{term() => sets:set(unique_tag())},  % element -> set of add tags
    tombstones => sets:set(unique_tag())              % removed tags
}.

%% G-Counter: per-node counts that only grow
-type gcounter() :: #{node() => non_neg_integer()}.

%% PN-Counter: pair of G-Counters for increments and decrements
-type pncounter() :: #{
    p => gcounter(),  % positive (increments)
    n => gcounter()   % negative (decrements)
}.

-export_type([
    lww_register/0,
    timestamp/0,
    or_set/0,
    gcounter/0,
    pncounter/0
]).

%%==============================================================================
%% LWW-Register (Last-Write-Wins Register)
%%==============================================================================
%% A simple CRDT that resolves conflicts by keeping the value with
%% the highest timestamp. Ties are broken by node name (lexicographic order).

%% @doc Create a new empty LWW-Register
-spec new_lww_register() -> lww_register().
new_lww_register() ->
    #{
        value => undefined,
        timestamp => 0,
        node => node()
    }.

%% @doc Create a new LWW-Register with initial value
-spec new_lww_register(term()) -> lww_register().
new_lww_register(Value) ->
    #{
        value => Value,
        timestamp => erlang:system_time(microsecond),
        node => node()
    }.

%% @doc Set a value in the LWW-Register with timestamp
-spec lww_set(lww_register(), term(), timestamp()) -> lww_register().
lww_set(_Register, Value, Timestamp) when is_integer(Timestamp), Timestamp >= 0 ->
    #{
        value => Value,
        timestamp => Timestamp,
        node => node()
    }.

%% @doc Get the current value of the LWW-Register
-spec lww_get(lww_register()) -> term().
lww_get(#{value := Value}) ->
    Value.

%% @doc Get the timestamp of the LWW-Register
-spec lww_timestamp(lww_register()) -> timestamp().
lww_timestamp(#{timestamp := Timestamp}) ->
    Timestamp.

%% @doc Get the value of the LWW-Register (alias for lww_get)
-spec lww_value(lww_register()) -> term().
lww_value(Register) ->
    lww_get(Register).

%% @doc Merge two LWW-Registers
%% Keeps the value with the highest timestamp
%% Ties broken by node name (lexicographic order)
-spec lww_merge(lww_register(), lww_register()) -> lww_register().
lww_merge(
    #{timestamp := T1} = R1,
    #{timestamp := T2}
) when T1 > T2 ->
    R1;
lww_merge(
    #{timestamp := T1},
    #{timestamp := T2} = R2
) when T1 < T2 ->
    R2;
lww_merge(
    #{timestamp := T, node := N1} = R1,
    #{timestamp := T, node := N2} = R2
) ->
    %% Same timestamp - use lexicographic order of node names
    case N1 < N2 of
        true -> R1;
        false -> R2
    end.

%%==============================================================================
%% OR-Set (Observed-Remove Set)
%%==============================================================================
%% A set CRDT that supports both add and remove operations.
%% Each add creates a unique tag. Remove adds tags to tombstone set.
%% An element is present if it has at least one tag not tombstoned.

%% @doc Create a new empty OR-Set
-spec new_or_set() -> or_set().
new_or_set() ->
    #{
        elements => #{},
        tombstones => sets:new()
    }.

%% @doc Add an element to the OR-Set (auto-generates unique tag)
-spec or_add(or_set(), term()) -> or_set().
or_add(Set, Element) ->
    Tag = generate_unique_tag(),
    or_add(Set, Element, Tag).

%% @doc Add an element to the OR-Set with a specific tag
-spec or_add(or_set(), term(), unique_tag()) -> or_set().
or_add(#{elements := Elements, tombstones := Tombstones}, Element, Tag) ->
    CurrentTags = maps:get(Element, Elements, sets:new()),
    NewTags = sets:add_element(Tag, CurrentTags),
    #{
        elements => maps:put(Element, NewTags, Elements),
        tombstones => Tombstones
    }.

%% @doc Remove an element from the OR-Set
%% Removes all current tags for that element (add-wins semantics)
-spec or_remove(or_set(), term()) -> or_set().
or_remove(#{elements := Elements, tombstones := Tombstones} = Set, Element) ->
    case maps:find(Element, Elements) of
        {ok, Tags} ->
            %% Move all tags to tombstones
            NewTombstones = sets:union(Tombstones, Tags),
            #{
                elements => maps:remove(Element, Elements),
                tombstones => NewTombstones
            };
        error ->
            %% Element not in set - no-op
            Set
    end.

%% @doc Check if element is in the OR-Set
-spec or_contains(or_set(), term()) -> boolean().
or_contains(#{elements := Elements, tombstones := Tombstones}, Element) ->
    case maps:find(Element, Elements) of
        {ok, Tags} ->
            %% Element is present if it has any non-tombstoned tags
            ActiveTags = sets:subtract(Tags, Tombstones),
            sets:size(ActiveTags) > 0;
        error ->
            false
    end.

%% @doc Get all elements in the OR-Set
-spec or_elements(or_set()) -> [term()].
or_elements(#{elements := Elements, tombstones := Tombstones}) ->
    maps:fold(
        fun(Element, Tags, Acc) ->
            ActiveTags = sets:subtract(Tags, Tombstones),
            case sets:size(ActiveTags) > 0 of
                true -> [Element | Acc];
                false -> Acc
            end
        end,
        [],
        Elements
    ).

%% @doc Get the number of elements in the OR-Set
-spec or_size(or_set()) -> non_neg_integer().
or_size(Set) ->
    length(or_elements(Set)).

%% @doc Merge two OR-Sets
%% Union of elements, union of tombstones
-spec or_merge(or_set(), or_set()) -> or_set().
or_merge(
    #{elements := E1, tombstones := T1},
    #{elements := E2, tombstones := T2}
) ->
    %% Merge elements: for each key, union the tag sets
    MergedElements = maps:fold(
        fun(Element, Tags2, Acc) ->
            Tags1 = maps:get(Element, Acc, sets:new()),
            maps:put(Element, sets:union(Tags1, Tags2), Acc)
        end,
        E1,
        E2
    ),
    %% Merge tombstones: union
    MergedTombstones = sets:union(T1, T2),
    #{
        elements => MergedElements,
        tombstones => MergedTombstones
    }.

%%==============================================================================
%% G-Counter (Grow-only Counter)
%%==============================================================================
%% A counter that can only be incremented.
%% Each node maintains its own count; total is sum of all nodes.

%% @doc Create a new G-Counter
-spec new_gcounter() -> gcounter().
new_gcounter() ->
    #{}.

%% @doc Increment the G-Counter by 1 for current node
-spec gcounter_increment(gcounter()) -> gcounter().
gcounter_increment(Counter) ->
    gcounter_increment(Counter, 1).

%% @doc Increment the G-Counter by N for current node
-spec gcounter_increment(gcounter(), pos_integer()) -> gcounter().
gcounter_increment(Counter, N) when is_integer(N), N > 0 ->
    Node = node(),
    Current = maps:get(Node, Counter, 0),
    maps:put(Node, Current + N, Counter).

%% @doc Get the total value of the G-Counter
-spec gcounter_value(gcounter()) -> non_neg_integer().
gcounter_value(Counter) ->
    maps:fold(fun(_Node, Count, Acc) -> Acc + Count end, 0, Counter).

%% @doc Merge two G-Counters (take max of each node's count)
-spec gcounter_merge(gcounter(), gcounter()) -> gcounter().
gcounter_merge(C1, C2) ->
    maps:fold(
        fun(Node, Count2, Acc) ->
            Count1 = maps:get(Node, Acc, 0),
            maps:put(Node, max(Count1, Count2), Acc)
        end,
        C1,
        C2
    ).

%%==============================================================================
%% PN-Counter (Positive-Negative Counter)
%%==============================================================================
%% A counter that supports both increment and decrement.
%% Implemented as pair of G-Counters (positive - negative).

%% @doc Create a new PN-Counter
-spec new_pncounter() -> pncounter().
new_pncounter() ->
    #{
        p => new_gcounter(),
        n => new_gcounter()
    }.

%% @doc Increment the PN-Counter by 1
-spec pncounter_increment(pncounter()) -> pncounter().
pncounter_increment(Counter) ->
    pncounter_increment(Counter, 1).

%% @doc Increment the PN-Counter by N
-spec pncounter_increment(pncounter(), pos_integer()) -> pncounter().
pncounter_increment(#{p := P, n := N}, Amount) when is_integer(Amount), Amount > 0 ->
    #{
        p => gcounter_increment(P, Amount),
        n => N
    }.

%% @doc Decrement the PN-Counter by 1
-spec pncounter_decrement(pncounter()) -> pncounter().
pncounter_decrement(Counter) ->
    pncounter_decrement(Counter, 1).

%% @doc Decrement the PN-Counter by N
-spec pncounter_decrement(pncounter(), pos_integer()) -> pncounter().
pncounter_decrement(#{p := P, n := N}, Amount) when is_integer(Amount), Amount > 0 ->
    #{
        p => P,
        n => gcounter_increment(N, Amount)
    }.

%% @doc Get the value of the PN-Counter (positive - negative)
-spec pncounter_value(pncounter()) -> integer().
pncounter_value(#{p := P, n := N}) ->
    gcounter_value(P) - gcounter_value(N).

%% @doc Merge two PN-Counters
-spec pncounter_merge(pncounter(), pncounter()) -> pncounter().
pncounter_merge(#{p := P1, n := N1}, #{p := P2, n := N2}) ->
    #{
        p => gcounter_merge(P1, P2),
        n => gcounter_merge(N1, N2)
    }.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private Generate a unique tag for OR-Set operations
-spec generate_unique_tag() -> unique_tag().
generate_unique_tag() ->
    %% Combine timestamp, node, and random bytes for uniqueness
    Timestamp = erlang:system_time(nanosecond),
    NodeBin = atom_to_binary(node(), utf8),
    Random = crypto:strong_rand_bytes(8),
    TimestampBin = integer_to_binary(Timestamp),
    <<TimestampBin/binary, "-", NodeBin/binary, "-", Random/binary>>.
