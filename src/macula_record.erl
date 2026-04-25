%%%-------------------------------------------------------------------
%%% @doc Signed, content-addressed records.
%%%
%%% Records are the SDK's primitive for typed, signed, application
%%% data put into the mesh's distributed storage. They are
%%% content-addressed: the `key' is the BLAKE3 hash of the
%%% deterministic CBOR encoding of the signed body, so two clients
%%% computing the same record arrive at the same key.
%%%
%%% == Anatomy ==
%%%
%%% A record has six fields plus an optional `expires_at':
%%%
%%% <ul>
%%%   <li>`type' — application-defined `0..255' tag (e.g. `16#02'
%%%       for `station_record', `16#11' for `content_announcement').
%%%       The SDK does not interpret payloads — type is just routing
%%%       metadata for `find_records_by_type/2'.</li>
%%%   <li>`payload' — opaque application bytes. Conventionally a
%%%       deterministic CBOR encoding via `macula_record_cbor', but
%%%       the SDK stores it verbatim.</li>
%%%   <li>`pubkey' — 32-byte Ed25519 public key of the publisher.</li>
%%%   <li>`ttl_ms' — record lifetime hint in milliseconds. The relay
%%%       expires the record after this elapses.</li>
%%%   <li>`sig' — 64-byte Ed25519 signature over the canonical body
%%%       (type + payload + pubkey + ttl_ms, deterministic CBOR).</li>
%%%   <li>`expires_at' — absolute epoch milliseconds, set by the
%%%       relay on store. Optional in records built locally.</li>
%%% </ul>
%%%
%%% == Signing scheme ==
%%%
%%% The signed bytes are the deterministic CBOR encoding (RFC 8949
%%% §4.2.1) of:
%%%
%%% ```
%%% [type, payload, pubkey, ttl_ms]
%%% '''
%%%
%%% Verifiers reconstruct this same byte string from the record's
%%% fields and check the Ed25519 signature against `pubkey'.
%%%
%%% == Content addressing ==
%%%
%%% The `key' is `blake3(canonical_body || sig)'. Including the
%%% signature in the hash prevents two distinct signers from
%%% producing the same key for the same logical content — each
%%% signed record has a unique address.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_record).

-export([build/4,
         key_of/1,
         canonical_body/1,
         verify/1]).

-export_type([record/0, record_type/0, record_key/0]).

-type record_type() :: 0..255.
-type record_key()  :: <<_:256>>.   %% 32-byte BLAKE3 of (body || sig)

-type record() :: #{
    type       := record_type(),
    payload    := binary(),
    pubkey     := <<_:256>>,
    ttl_ms     := non_neg_integer(),
    sig        := <<_:512>>,
    expires_at => non_neg_integer()
}.

%%%===================================================================
%%% Build + sign
%%%===================================================================

%% @doc Build a signed record. Computes the canonical body, signs
%% with the keypair's private half, and returns the record map. The
%% caller passes the result to `macula:put_record/2'.
-spec build(record_type(), binary(), macula_identity:key_pair(),
            non_neg_integer()) -> record().
build(Type, Payload, KeyPair, TtlMs)
  when is_integer(Type), Type >= 0, Type =< 255,
       is_binary(Payload),
       is_map(KeyPair),
       is_integer(TtlMs), TtlMs >= 0 ->
    Pub = macula_identity:public(KeyPair),
    Body = canonical_body(Type, Payload, Pub, TtlMs),
    Sig  = macula_identity:sign(Body, KeyPair),
    #{type    => Type,
      payload => Payload,
      pubkey  => Pub,
      ttl_ms  => TtlMs,
      sig     => Sig}.

%%%===================================================================
%%% Key derivation
%%%===================================================================

%% @doc The 32-byte content-address key for a record.
%% Computed as BLAKE3 of the canonical body concatenated with the
%% signature.
-spec key_of(record()) -> record_key().
key_of(#{type := T, payload := P, pubkey := Pub,
         ttl_ms := TTL, sig := Sig}) ->
    Body = canonical_body(T, P, Pub, TTL),
    macula_blake3_nif:hash(<<Body/binary, Sig/binary>>).

%%%===================================================================
%%% Canonical signing body
%%%===================================================================

%% @doc Returns the canonical byte string the signature covers.
%% Exposed so callers / verifiers can inspect what was signed
%% without re-deriving the encoding rules.
-spec canonical_body(record()) -> binary().
canonical_body(#{type := T, payload := P, pubkey := Pub, ttl_ms := TTL}) ->
    canonical_body(T, P, Pub, TTL).

canonical_body(Type, Payload, Pub, TtlMs) ->
    macula_record_cbor:encode([Type, Payload, Pub, TtlMs]).

%%%===================================================================
%%% Verify
%%%===================================================================

%% @doc Verify a record's signature. Returns `true' if `sig' is a
%% valid Ed25519 signature over `canonical_body/1' under `pubkey'.
-spec verify(record()) -> boolean().
verify(#{pubkey := Pub, sig := Sig} = Record)
  when is_binary(Pub), byte_size(Pub) =:= 32,
       is_binary(Sig), byte_size(Sig) =:= 64 ->
    Body = canonical_body(Record),
    macula_identity:verify(Body, Sig, Pub);
verify(_) ->
    false.
