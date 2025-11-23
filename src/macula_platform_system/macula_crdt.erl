%% @doc Conflict-Free Replicated Data Types (CRDTs) for shared state
%% Implements LWW-Register (Last-Write-Wins Register) for eventual consistency
-module(macula_crdt).

%% API exports
-export([
    new_lww_register/0,
    new_lww_register/1,
    lww_set/3,
    lww_get/1,
    lww_merge/2,
    lww_timestamp/1,
    lww_value/1
]).

%% Type definitions
-type timestamp() :: non_neg_integer().
-type lww_register() :: #{
    value => term(),
    timestamp => timestamp(),
    node => node()
}.

-export_type([lww_register/0, timestamp/0]).

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
%% Internal Functions
%%==============================================================================

%% Future: G-Counter (Grow-only Counter)
%% Future: PN-Counter (Positive-Negative Counter)
%% Future: OR-Set (Observed-Remove Set)
%% Future: LWW-Element-Set (Last-Write-Wins Element Set)
