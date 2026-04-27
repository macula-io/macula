%% @doc BOLT#4-style error taxonomy for CALL failures (Part 4 §6.1).
%%
%% Adapted from Lightning Network's BOLT#4 onion-failure codes, which
%% have proven in adversarial conditions that a small, specific
%% taxonomy prevents retry loops and enables post-mortem. Every CALL
%% ERROR frame carries one of these 16 codes plus the reporting
%% station's signature so downstream hops cannot forge "not my fault".
%%
%% The retry policy column is advisory — the caller's CALL state
%% machine (Phase 4 §6.2 / §6.4) is the actual decision point. Codes
%% are stable across V2 minor versions; new codes append at the next
%% free integer.
%%
%% Reference: plans/PLAN_MACULA_V2_PART4_LIFECYCLE.md §6.1;
%% plans/PLAN_MACULA_V2_PART6_PROTOCOL.md §13.
-module(macula_bolt4).

-export([
    code/1,
    name/1,
    info/1,
    table/0,
    is_retryable/1
]).

-export_type([code/0, name/0, retry_policy/0, info/0]).

-type code() :: 16#00..16#0F.

-type name() ::
      ok
    | unknown_next_peer
    | temporary_relay_failure
    | relay_disabled
    | node_not_found_at_target_relay
    | target_realm_refused
    | loop_detected
    | expiry_too_soon
    | upstream_congestion
    | invalid_path_header
    | crypto_puzzle_invalid
    | realm_not_authoritative_here
    | tombstoned
    | payload_too_large
    | signature_invalid
    | unknown_error.

-type retry_policy() ::
      none
    | different_path
    | same_path_after_backoff
    | application
    | crypto_drop
    | caller_recompute
    | caller_recompute_with_lookup
    | caller_extends_deadline
    | exponential_backoff
    | log_and_caution.

-type info() :: #{
    code  := code(),
    name  := name(),
    retry := retry_policy()
}.

%%=====================================================================
%% Public API
%%=====================================================================

%% @doc The complete 16-entry table. Order is by code.
-spec table() -> [info()].
table() ->
    [
        #{code => 16#00, name => ok,
          retry => none},
        #{code => 16#01, name => unknown_next_peer,
          retry => different_path},
        #{code => 16#02, name => temporary_relay_failure,
          retry => same_path_after_backoff},
        #{code => 16#03, name => relay_disabled,
          retry => different_path},
        #{code => 16#04, name => node_not_found_at_target_relay,
          retry => caller_recompute_with_lookup},
        #{code => 16#05, name => target_realm_refused,
          retry => application},
        #{code => 16#06, name => loop_detected,
          retry => caller_recompute},
        #{code => 16#07, name => expiry_too_soon,
          retry => caller_extends_deadline},
        #{code => 16#08, name => upstream_congestion,
          retry => exponential_backoff},
        #{code => 16#09, name => invalid_path_header,
          retry => caller_recompute},
        #{code => 16#0A, name => crypto_puzzle_invalid,
          retry => crypto_drop},
        #{code => 16#0B, name => realm_not_authoritative_here,
          retry => caller_recompute_with_lookup},
        #{code => 16#0C, name => tombstoned,
          retry => application},
        #{code => 16#0D, name => payload_too_large,
          retry => application},
        #{code => 16#0E, name => signature_invalid,
          retry => crypto_drop},
        #{code => 16#0F, name => unknown_error,
          retry => log_and_caution}
    ].

%% @doc Resolve a name to its integer code.
-spec code(name()) -> code().
code(Name) ->
    pluck(name, Name, code).

%% @doc Resolve an integer code to its symbolic name.
-spec name(code()) -> name().
name(Code) ->
    pluck(code, Code, name).

%% @doc Full info entry for a given name or code.
-spec info(name() | code()) -> info().
info(NameOrCode) ->
    case lookup(NameOrCode) of
        {ok, Entry} -> Entry;
        error       -> error({bolt4_unknown, NameOrCode})
    end.

%% @doc Whether the spec's recommended retry policy permits a retry
%% at all. `none' (success), `application' (handler-level remedy),
%% and `crypto_drop' (security-critical) are all non-retryable.
-spec is_retryable(name() | code()) -> boolean().
is_retryable(NameOrCode) ->
    classify_retry(maps:get(retry, info(NameOrCode))).

%%=====================================================================
%% Internal
%%=====================================================================

-spec pluck(atom(), term(), atom()) -> term().
pluck(LookupField, LookupValue, ResultField) ->
    case lookup_by(LookupField, LookupValue) of
        {ok, Entry} -> maps:get(ResultField, Entry);
        error       -> error({bolt4_unknown, LookupField, LookupValue})
    end.

-spec lookup(name() | code()) -> {ok, info()} | error.
lookup(N) when is_atom(N)    -> lookup_by(name, N);
lookup(N) when is_integer(N) -> lookup_by(code, N).

-spec lookup_by(atom(), term()) -> {ok, info()} | error.
lookup_by(Field, Value) ->
    case [E || E <- table(), maps:get(Field, E) =:= Value] of
        [E | _] -> {ok, E};
        []      -> error
    end.

-spec classify_retry(retry_policy()) -> boolean().
classify_retry(none)            -> false;
classify_retry(application)     -> false;
classify_retry(crypto_drop)     -> false;
classify_retry(_)               -> true.
