%% @doc Foundation trust anchor — firmware-embedded pubkeys for the
%% Macula Foundation FROST-Ed25519 aggregated signer.
%%
%% The Foundation signs Tier A seed lists, protocol parameters, realm
%% trust lists, and T3 attestations (Part 6 §9.14–§9.17). Stations
%% verify those records against a small, firmware-embedded set of
%% foundation pubkeys — the root of trust for the Tier A bootstrap
%% path (Part 5 §4).
%%
%% == Custodians ==
%%
%% Production deployments embed five foundation pubkeys, each held by
%% an independent custodian. FROST m-of-n thresholds keep the signing
%% key resilient: any m custodians can sign, no single custodian can
%% unilaterally authorise a record. See Part 5 §12.
%%
%% == Rotation ==
%%
%% Replacing an embedded pubkey requires a firmware update. The
%% existing record cache survives rotation — records signed by a
%% retired key remain valid until their `valid_until' elapses.
%%
%% == Placeholders ==
%%
%% The five keys returned by `pubkeys/0' at the time of this writing
%% are deterministic <b>placeholders</b> (`<<"macula-v2-foundation-*">>'
%% SHA-256 digests). They are NOT backed by live FROST shares. Any
%% record signed against a placeholder will fail verification because
%% no corresponding private key exists. Production firmware MUST
%% override `pubkeys/0' via `application:set_env(macula_record,
%% foundation_pubkeys, [...])' before any Tier A record is trusted.
%%
%% Callers that need to operate strictly against the live keys should
%% use `live_pubkeys/0', which returns the app-env override or an
%% empty list (never the placeholders).
%%
%% Reference: plans/PLAN_MACULA_V2_PART5_BOOTSTRAP.md §4, §12;
%% plans/PLAN_MACULA_V2_PART6_PROTOCOL.md §9.14–§9.17.
-module(macula_foundation).

-export([
    pubkeys/0,
    live_pubkeys/0,
    is_foundation/1,
    verify_record/1,
    placeholder_pubkeys/0,
    placeholder_mode/0
]).

-export_type([pubkey/0, verify_error/0]).

-type pubkey() :: macula_identity:pubkey().

-type verify_error() ::
        bad_record
      | signature_invalid
      | expired
      | not_foundation_signed
      | wrong_type.

-define(PLACEHOLDER_COUNT, 5).
-define(PLACEHOLDER_LABEL_PREFIX, "macula-v2-foundation-placeholder-").

-define(FOUNDATION_TYPES, [16#0D, 16#0E, 16#0F, 16#10]).

%%------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------

%% @doc Return the current foundation pubkey list.
%%
%% Resolution order:
%% <ol>
%%   <li>`application:get_env(macula_record, foundation_pubkeys, [])'
%%       if non-empty — production-embedded keys.</li>
%%   <li>Deterministic placeholder keys from `placeholder_pubkeys/0'
%%       — development / test only.</li>
%% </ol>
-spec pubkeys() -> [pubkey()].
pubkeys() ->
    resolve_pubkeys(application:get_env(macula_record, foundation_pubkeys)).

resolve_pubkeys({ok, []})              -> placeholder_pubkeys();
resolve_pubkeys({ok, L}) when is_list(L) -> L;
resolve_pubkeys(undefined)             -> placeholder_pubkeys().

%% @doc Return live (non-placeholder) foundation pubkeys, or `[]'.
%%
%% Use this when callers must refuse to trust placeholder keys — e.g.
%% production bootstrap paths that would otherwise accept a record
%% signed by no real private key.
-spec live_pubkeys() -> [pubkey()].
live_pubkeys() ->
    case application:get_env(macula_record, foundation_pubkeys) of
        {ok, L} when is_list(L), L =/= [] -> L;
        _                                 -> []
    end.

%% @doc True iff `Key' is one of the trusted foundation pubkeys.
-spec is_foundation(binary()) -> boolean().
is_foundation(Key) when is_binary(Key), byte_size(Key) =:= 32 ->
    lists:member(Key, pubkeys());
is_foundation(_) ->
    false.

%% @doc Verify a foundation-signed record.
%%
%% Succeeds iff the record type is one of the foundation tags
%% (`0x0D–0x10'), the envelope `k' field is a trusted foundation
%% pubkey, and the signature + expiry pass
%% `macula_record:verify/1'.
-spec verify_record(macula_record:record()) ->
        {ok, macula_record:record()} | {error, verify_error()}.
verify_record(Record) ->
    check_type(Record).

check_type(#{type := T} = R) when is_integer(T) ->
    case lists:member(T, ?FOUNDATION_TYPES) of
        true  -> check_key(R);
        false -> {error, wrong_type}
    end;
check_type(_) ->
    {error, bad_record}.

check_key(#{key := K} = R) ->
    case is_foundation(K) of
        true  -> macula_record:verify(R);
        false -> {error, not_foundation_signed}
    end.

%% @doc Placeholder foundation pubkeys — deterministic, publicly
%% derivable, NOT backed by any private key. Production firmware MUST
%% override via app env before any Tier A record is consumed.
-spec placeholder_pubkeys() -> [pubkey()].
placeholder_pubkeys() ->
    [placeholder_key(I) || I <- lists:seq(1, ?PLACEHOLDER_COUNT)].

placeholder_key(I) when is_integer(I), I > 0 ->
    Label = iolist_to_binary(
              [?PLACEHOLDER_LABEL_PREFIX, integer_to_list(I)]),
    crypto:hash(sha256, Label).

%% @doc `true' iff the running station is using placeholder keys.
%%
%% Production code should refuse to bootstrap under `true' — see
%% `live_pubkeys/0'.
-spec placeholder_mode() -> boolean().
placeholder_mode() ->
    live_pubkeys() =:= [].
