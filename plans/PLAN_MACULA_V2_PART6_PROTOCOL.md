# PLAN — Macula V2, Part 6: Wire Protocol Catalog

**Parent:** `PLAN_MACULA_V2_ROOT.md`
**Depends on:** Part 1 (identity, record signing, PKARR), Part 3 (DHT ops, source routing), Part 4 (CALL state machine, BOLT#4 codes, SWIM), Part 5 (governance, parameters).
**Feeds:** Part 7 (Implementation — module-to-frame mapping), Part 8 (Verification — wire conformance tests).
**Status:** Draft — authored 2026-04-14.
**Scope:** Byte-level specifications for every frame and every record on the wire. Canonical encoding, signing rules, capability negotiation, error catalogue, extension semantics. No algorithmic behaviour (Parts 3-5 cover that) — Part 6 is "what the bytes look like".

---

## 1. Purpose

Part 6 is the contract. Any implementation — Macula SDK in Erlang, future Rust port, external interop — MUST produce/accept the byte layouts in this Part. Behavioural semantics live in Parts 3-5; Part 6 is where the semantics touch the wire.

Conformance test suites (Part 8) are derived from Part 6. A change here is a breaking change unless versioning says otherwise (§14).

---

## 2. Encoding conventions

### 2.1 Two encodings coexist

V2 uses two encodings, chosen for distinct audiences:

| Encoding | Used for | Why |
|----------|----------|-----|
| **CBOR** (RFC 8949, deterministic variant RFC 8949 §4.2.1) | PKARR records, signed payloads, long-lived data | Wide interop (PKARR compat), deterministic canonicalisation, library-rich. |
| **BERT** (Erlang external term format, `:erlang.term_to_binary/2` with `minor_version=2` + `compressed=0`) | In-mesh frames (CALL, PUBLISH, SWIM, DHT ops) | Native BEAM parse; no conversion cost; battle-tested in V1 frame encoder which V2 inherits (Part 1 §11). |

CBOR for *records we sign and store*. BERT for *frames we send fast and often*. Never mix.

### 2.2 Framing

Every Macula wire message on a QUIC stream is:

```
┌─────────┬──────────┬───────────────┐
│ Length  │ Header   │ Payload       │
│ 4 bytes │ see §3   │ per frame     │
└─────────┴──────────┴───────────────┘

Length = total bytes in Header + Payload (big-endian uint32).
```

Max frame size: 16 MiB. CALL payloads above that are chunked (Phase 7+); V2.0 targets are below.

### 2.3 Canonical byte ordering

- CBOR: RFC 8949 §4.2.1 deterministic encoding. Map keys sorted by bytewise-lexicographic order of their encoded form. Tiniest integer representation. No `undefined`/`null` distinction.
- BERT: as produced by `:erlang.term_to_binary(T, [{minor_version, 2}])`. Same term → identical bytes on any OTP 27+.

### 2.4 Strings and binaries

- **Text strings** (human labels): UTF-8, NFC-normalised. CBOR major type 3; BERT `binary` with tagged marker `{utf8, Binary}`.
- **Binary blobs** (keys, hashes, signatures): raw bytes. CBOR major type 2; BERT `binary`.

### 2.5 Integers

- Unsigned fixed-width where the field is naturally bounded (ports, versions, flags).
- Arbitrary-precision for counters that might grow (record version — though we use UUIDv7).

### 2.6 Timestamps

- **Wall-clock**: milliseconds since Unix epoch, int64. `ts_ms`.
- **Monotonic** (intra-session): microseconds since connection start, int64. `mono_us`.
- **UUIDv7**: 128 bits, time-ordered; used for call-ids, record versions.

### 2.7 Identifiers

| Type | Size | Encoding |
|------|------|---------|
| NodeId | 32 bytes | raw Ed25519 pubkey |
| RealmId | 32 bytes | raw Ed25519 pubkey |
| StationId | 32 bytes | raw Ed25519 pubkey |
| CallId | 16 bytes | UUIDv7 |
| RecordVersion | 16 bytes | UUIDv7 |

Display forms: base32-no-padding (Crockford alphabet) for human UIs; never on the wire.

---

## 3. Frame envelope (Header)

Every frame header (BERT-encoded map):

```erlang
#{
  version         => 2,                     %% protocol major version (byte)
  frame_type      => call | result | error
                   | publish | subscribe | event
                   | connect | hello | goodbye
                   | ping | find_node | find_value | store | replicate
                   | swim_ping | swim_ack | swim_suspect | swim_confirm,
  frame_id        => uuid_v7_bin(),         %% per-frame unique
  sent_at_ms      => integer(),             %% wall-clock ms
  capabilities    => integer(),             %% caller's advertised cap bits (§13)
  realm           => binary() | undefined,  %% RealmId, or undefined if realm-agnostic
  call_id         => binary() | undefined,  %% for call/result/error; UUIDv7
  source_route    => binary() | undefined   %% §11 layout; nil on same-station hops
}
```

Version byte is checked first. Mismatch on major ⇒ `protocol_version_unsupported` (BOLT#4 analogue, code `0x10`, Part 6-specific addition).

---

## 4. Connection establishment frames

### 4.1 CONNECT

Sent by the initiating side on first QUIC stream. Establishes peer identity.

```erlang
#{
  frame_type => connect,
  node_id    => <<32 bytes>>,                  %% initiator's NodeId
  station_id => <<32 bytes>>,                  %% initiator's StationId (may equal node_id)
  realms     => [<<32 bytes>>, ...],           %% realms initiator serves
  addresses  => [ip_address_rec(), ...],       %% initiator's declared addresses (§9.4)
  site       => site_rec() | undefined,        %% site record (Part 1 §8; optional)
  version    => 2,
  capabilities => integer(),                   %% §13
  puzzle_evidence => <<32 bytes>>,             %% SHA-256(pubkey) showing puzzle satisfied
  endorsements => [realm_endorsement(), ...],  %% realm endorsements initiator carries
  signature => <<64 bytes>>                    %% Ed25519 sig over canonical encoding
}
```

### 4.2 HELLO (response)

Responder answers with its identity + capability intersect.

```erlang
#{
  frame_type => hello,
  node_id    => <<32 bytes>>,
  station_id => <<32 bytes>>,
  realms     => [<<32 bytes>>, ...],
  addresses  => [ip_address_rec(), ...],
  site       => site_rec() | undefined,
  version    => 2,
  capabilities => integer(),                   %% responder's bits
  accepted   => boolean(),                     %% false ⇒ see `refusal_code`
  refusal_code => integer() | undefined,       %% §13 codes on refusal
  negotiated_capabilities => integer(),        %% AND of caller's + responder's
  signature => <<64 bytes>>
}
```

After HELLO accepted, peer enters REFRESH (Part 4 §7).

### 4.3 GOODBYE

Graceful disconnect (Part 4 §10 DRAINING).

```erlang
#{
  frame_type => goodbye,
  reason     => atom(),   %% operator_stop | draining | protocol_violation | ...
  detail     => binary() | undefined,
  signature  => <<64 bytes>>
}
```

---

## 5. CALL frames

### 5.1 CALL

Request-response invocation. BOLT#4 taxonomy (Part 4 §6.1) applies.

```erlang
#{
  frame_type  => call,
  call_id     => <<16 bytes>>,            %% UUIDv7, caller-assigned
  procedure   => binary(),                %% canonical URI (§5.4)
  realm       => <<32 bytes>>,            %% target realm
  payload     => term() | binary(),       %% application-defined
  deadline_ms => integer(),               %% absolute Unix ms; CALL expires after
  source_route => <<... bytes>>,          %% §11
  retry_budget => integer(),              %% remaining retries; decremented each attempt
  caller      => <<32 bytes>>,            %% NodeId of originating node
  signature   => <<64 bytes>>             %% over canonical encoding minus signature
}
```

### 5.2 RESULT

Success response.

```erlang
#{
  frame_type => result,
  call_id    => <<16 bytes>>,
  payload    => term() | binary(),
  responded_by => <<32 bytes>>,        %% NodeId that actually handled
  source_route_reverse => <<... bytes>>,
  signature  => <<64 bytes>>
}
```

### 5.3 ERROR

Structured failure. Signed by the hop reporting the failure.

```erlang
#{
  frame_type    => error,
  call_id       => <<16 bytes>>,
  code          => integer(),          %% §13 BOLT#4-style taxonomy
  name          => atom(),             %% `unknown_next_peer | ...`
  detail        => binary() | undefined,
  reported_by   => <<32 bytes>>,       %% NodeId or StationId signing this error
  offending_hop => <<32 bytes>> | undefined,   %% if known
  source_route_partial => <<... bytes>>,       %% path traversed to this point
  signature     => <<64 bytes>>
}
```

### 5.4 Procedure URI canonical form

```
{realm-id-base32}/{org}/{app}/{domain}/{name}_v{N}
```

- `realm-id-base32`: Crockford base32 of RealmId, no padding.
- `org`: lowercase ASCII, `[a-z0-9-]` (1–32 chars).
- `app`: same constraints.
- `domain`: same constraints.
- `name`: same constraints.
- `N`: positive integer.

Examples:
```
ABCD...XYZ/acme/weather/forecast/get_by_city_v1
ABCD...XYZ/acme/chat/message/post_v2
```

Canonicalisation before hashing: lowercase NFC, no trailing slash, no leading slash. `SHA-256(canonical_form)` yields the ProcedureKey (Part 3 §3.3).

---

## 6. PubSub frames

### 6.1 PUBLISH

Publisher sends to its home station; station fans out intra-realm (Plumtree) or cross-realm (DHT lookup).

```erlang
#{
  frame_type => publish,
  topic      => binary(),                %% canonical URI (analogous to procedure URI)
  realm      => <<32 bytes>>,
  publisher  => <<32 bytes>>,            %% NodeId
  seq        => integer(),               %% per-publisher monotonic
  payload    => term() | binary(),
  published_at_ms => integer(),
  ttl_ms     => integer() | undefined,   %% message expiry; default realm-configured
  signature  => <<64 bytes>>
}
```

### 6.2 SUBSCRIBE

Node subscribes at its home station.

```erlang
#{
  frame_type => subscribe,
  topic      => binary(),
  realm      => <<32 bytes>>,
  subscriber => <<32 bytes>>,            %% NodeId
  filter     => term() | undefined,      %% realm-defined predicate
  options    => #{qos => 0..2, ...},
  signature  => <<64 bytes>>
}
```

### 6.3 UNSUBSCRIBE

```erlang
#{
  frame_type => unsubscribe,
  topic      => binary(),
  realm      => <<32 bytes>>,
  subscriber => <<32 bytes>>,
  signature  => <<64 bytes>>
}
```

### 6.4 EVENT

Delivery of a published message to subscriber.

```erlang
#{
  frame_type => event,
  topic      => binary(),
  realm      => <<32 bytes>>,
  publisher  => <<32 bytes>>,
  seq        => integer(),
  payload    => term() | binary(),
  delivered_via => plumtree | dht | direct,
  signature  => <<64 bytes>>            %% publisher's signature; not re-signed per hop
}
```

EVENT signature is the publisher's — every subscriber can verify authenticity without trusting intermediaries.

---

## 7. DHT operation frames

(Part 3 §10 catalog; Part 6 specifies bytes.)

### 7.1 PING / PONG

```erlang
#{frame_type => ping, nonce => <<16 bytes>>, signature => <<64 bytes>>}
#{frame_type => pong, nonce => <<16 bytes>>, signature => <<64 bytes>>}
```

### 7.2 FIND_NODE / NODES

```erlang
%% Request
#{
  frame_type => find_node,
  key        => <<32 bytes>>,             %% target lookup key
  origin     => <<32 bytes>>,             %% caller's NodeId (for reverse response)
  depth      => integer(),                %% hop count; limits recursion
  signature  => <<64 bytes>>
}

%% Response
#{
  frame_type => nodes,
  key        => <<32 bytes>>,
  nodes      => [station_ref(), ...],     %% up to k=20 closest known
  signature  => <<64 bytes>>
}

station_ref() :: #{
  node_id     => <<32 bytes>>,
  station_id  => <<32 bytes>>,
  addresses   => [ip_address_rec()],
  tier        => 0..4,
  asn         => integer() | undefined,
  country     => binary(),                %% ISO-3166-1 alpha-2
  last_seen_at => integer()               %% wall-clock ms
}
```

### 7.3 FIND_VALUE / VALUE

```erlang
%% Request
#{
  frame_type => find_value,
  key        => <<32 bytes>>,
  origin     => <<32 bytes>>,
  signature  => <<64 bytes>>
}

%% Response: VALUE (found) or NODES (not found; closest known)
#{
  frame_type => value,
  key        => <<32 bytes>>,
  records    => [pkarr_record(), ...],    %% §9; one to k records
  signature  => <<64 bytes>>              %% responder's sig; individual records have own sigs
}
```

### 7.4 STORE / STORE_ACK

```erlang
%% Request
#{
  frame_type => store,
  record     => pkarr_record(),           %% §9
  signature  => <<64 bytes>>              %% requester's sig
}

%% Response
#{
  frame_type => store_ack,
  key        => <<32 bytes>>,
  stored     => boolean(),
  reason     => atom() | undefined,       %% e.g. quota | invalid_sig | expired
  signature  => <<64 bytes>>
}
```

### 7.5 REPLICATE / REPLICATE_ACK

```erlang
#{
  frame_type => replicate,
  record     => pkarr_record(),
  new_custodian => boolean(),             %% Part 3 §5.5 custody handover
  signature  => <<64 bytes>>
}

#{
  frame_type => replicate_ack,
  key        => <<32 bytes>>,
  accepted   => boolean(),
  signature  => <<64 bytes>>
}
```

---

## 8. SWIM frames

SWIM-Lifeguard (Part 4 §5.2) runs on its own low-priority stream per QUIC connection. Four message types; each fits <512 bytes to allow aggressive piggybacking.

### 8.1 SWIM_PING

```erlang
#{
  frame_type => swim_ping,
  round      => integer(),               %% SWIM round number
  incarnation => integer(),              %% peer's own incarnation
  piggyback  => [swim_update(), ...],    %% member updates being disseminated
  signature  => <<64 bytes>>
}
```

### 8.2 SWIM_ACK

```erlang
#{
  frame_type => swim_ack,
  round      => integer(),
  responder  => <<32 bytes>>,            %% NodeId of responder
  incarnation => integer(),
  piggyback  => [swim_update(), ...],
  signature  => <<64 bytes>>
}
```

### 8.3 SWIM_SUSPECT / SWIM_CONFIRM

```erlang
#{
  frame_type => swim_suspect,
  target     => <<32 bytes>>,
  target_incarnation => integer(),
  suspected_by => <<32 bytes>>,
  ttl        => integer(),               %% dissemination rounds remaining
  signature  => <<64 bytes>>
}

%% swim_confirm structurally identical; semantically "confirmed dead"
```

swim_update() records piggybacked in PING/ACK:
```erlang
#{
  target    => <<32 bytes>>,
  state     => alive | suspect | confirmed_failed,
  incarnation => integer(),
  observed_at => integer(),
  by        => <<32 bytes>>,
  signature => <<64 bytes>>             %% signed by observer so no forgery
}
```

---

## 9. PKARR record types

All PKARR records share envelope:

```cbor
{
  "t": <type-tag-uint>,          ; record type id (§9.1 table)
  "k": h'<32-byte pubkey>',      ; owning key (NodeId/RealmId/StationId as relevant)
  "v": h'<16-byte UUIDv7>',      ; version
  "c": <ts_ms int>,              ; created_at
  "x": <ts_ms int>,              ; expires_at
  "p": { ... type-specific payload ... },
  "s": h'<64-byte Ed25519 sig>'  ; signature over canonical CBOR of t/k/v/c/x/p
}
```

Signature domain separation: prefix `"macula-v2-record\0"` before canonical CBOR bytes, then Ed25519-sign.

### 9.1 Type tag table

| Tag | Name | Owning key | Payload type |
|-----|------|-----------|---------------|
| 0x01 | `node_record` | NodeId | §9.2 |
| 0x02 | `station_record` | StationId | §9.3 |
| 0x03 | `realm_directory` | RealmId | §9.4 |
| 0x04 | `realm_stations` | RealmId | §9.5 |
| 0x05 | `realm_member_endorsement` | RealmId | §9.6 |
| 0x06 | `procedure_advertisement` | NodeId | §9.7 |
| 0x07 | `topic_subscription_hint` | StationId | §9.8 |
| 0x08 | `gateway_capability` | StationId | §9.9 |
| 0x09 | `station_realm_endorsement` | StationId | §9.10 |
| 0x0A | `realm_station_endorsement` | RealmId | §9.11 |
| 0x0B | `cross_realm_trust` | RealmId (A) | §9.12 |
| 0x0C | `tombstone` | Owner of superseded | §9.13 |
| 0x0D | `foundation_seed_list` | Foundation FROST | §9.14 |
| 0x0E | `foundation_parameter` | Foundation FROST | §9.15 |
| 0x0F | `foundation_realm_trust_list` | Foundation FROST | §9.16 |
| 0x10 | `foundation_t3_attestation` | Foundation FROST | §9.17 |

### 9.2 node_record payload

```cbor
{
  "node_id": h'<32>',
  "station_id": h'<32>',            ; home station of this node
  "realms": [h'<32>', h'<32>', ...],
  "capabilities": <uint bitmask>,
  "caps_hint": "...",               ; optional human-readable
  "display_name": "..."             ; optional
}
```

### 9.3 station_record payload

```cbor
{
  "station_id": h'<32>',
  "addresses": [ ip_address_rec(), ... ],   ; §9.18
  "tier_declared": 0..4,
  "asn": <uint>,
  "country": "BE",                          ; ISO-3166-1 alpha-2
  "metro": "brussels",                      ; optional
  "site": site_rec() | null,                ; §9.19
  "capabilities": <uint bitmask>,
  "pubkey_puzzle_evidence": h'<32>'         ; SHA-256(pubkey), for quick verify
}
```

### 9.4 realm_directory payload

```cbor
{
  "realm_id": h'<32>',
  "name": "...",
  "admin_key": h'<32>',                     ; may equal realm_id; may differ (council)
  "policy_url": "...",
  "member_limit": <uint> | null,
  "federation_policy": { ... },
  "created_at": <ts_ms>
}
```

### 9.5 realm_stations payload

List of StationIds serving the realm (for fast "who's here" lookup).

```cbor
{
  "realm_id": h'<32>',
  "stations": [ { "station_id": h'<32>', "roles": ["directory", "replica", ...] }, ... ]
}
```

### 9.6 realm_member_endorsement payload

```cbor
{
  "realm": h'<32>',
  "member_node": h'<32>',
  "roles": ["..."],                         ; realm-defined
  "valid_from": <ts_ms>,
  "valid_until": <ts_ms>
}
```

### 9.7 procedure_advertisement payload

```cbor
{
  "procedure_uri": "<canonical>",
  "advertiser_node": h'<32>',
  "serving_station": h'<32>',
  "session_token_hint": h'<...>',           ; opaque; node-interpreted
  "rate_limit_qps": <uint>,
  "max_concurrency": <uint>
}
```

### 9.8 topic_subscription_hint payload

Aggregated per-station-per-realm (Part 3 §5.4).

```cbor
{
  "station_id": h'<32>',
  "realm": h'<32>',
  "topics": ["topic_uri_1", "topic_uri_2", ...]
}
```

### 9.9 gateway_capability payload

Detail in Part 2 §8.1.

```cbor
{
  "station_id": h'<32>',
  "tier": 1..4,
  "bw_sustained_mbps": <uint>,
  "bw_burst_mbps": <uint>,
  "uptime_30d": <float 0..100>,
  "asn": <uint>,
  "asn_owner": "...",
  "rpki_valid": <bool>,
  "multi_homed": <bool>,
  "country": "BE",
  "metro": "...",
  "contact": "...",
  "policy_url": "...",
  "endorsements": [ realm_endorsement_ref(), ... ],
  "foundation_attestation": h'<...>' | null
}
```

### 9.10–9.11 endorsements

```cbor
; station_realm_endorsement: station → realm
{ "station_id": h'<32>', "realm": h'<32>', "valid_from": ts, "valid_until": ts }

; realm_station_endorsement: realm → station
{ "realm": h'<32>', "station_id": h'<32>', "valid_from": ts, "valid_until": ts }
```

### 9.12 cross_realm_trust payload

```cbor
{
  "realm_a": h'<32>',
  "realm_b": h'<32>',
  "scope": ["directory_lookup", "member_auth", ...],
  "valid_until": <ts_ms>,
  "signature_a": h'<64>',                    ; A-admin sig
  "signature_b": h'<64>'                     ; B-admin sig
}
```

Note: cross_realm_trust is *double-signed*; the record envelope's single signature is realm_a's, but payload contains realm_b's cosignature.

### 9.13 tombstone payload

```cbor
{
  "superseded_key": h'<32>',
  "superseded_type": <uint tag>,
  "replaced_at": <ts_ms>,
  "reason": "revoked | retired | expired | moved",
  "detail": "..." | null
}
```

### 9.14 foundation_seed_list payload

```cbor
{
  "version": h'<16>',                        ; UUIDv7
  "valid_from": <ts_ms>,
  "valid_until": <ts_ms>,
  "seeds": [ { "node_id": h'<32>', "addresses": [...], "tier": 3|4 }, ... ]
}
```

Signed by foundation FROST threshold key (Part 5 §12.2). `s` in envelope is the threshold signature (Ed25519 of same length).

### 9.15 foundation_parameter payload

```cbor
{
  "param_name": "puzzle_difficulty" | "tRepublish_ms" | "tExpire_ms" | ...,
  "param_value": <term>,
  "version": h'<16>',
  "valid_from": <ts_ms>,
  "valid_until": <ts_ms>,
  "prior_version": h'<16>' | null
}
```

### 9.16 foundation_realm_trust_list payload

```cbor
{
  "realms_trusted": [h'<32>', ...],
  "realms_revoked": [h'<32>', ...],
  "version": h'<16>',
  "valid_until": <ts_ms>
}
```

### 9.17 foundation_t3_attestation payload

```cbor
{
  "station_id": h'<32>',
  "tier_attested": 3,
  "audit_date": <ts_ms>,
  "valid_until": <ts_ms>,
  "notes": "..."
}
```

### 9.18 ip_address_rec()

```cbor
{
  "v6": "2a02:...",                        ; IPv6 GUA
  "v4": "1.2.3.4" | null,
  "port": 7000,
  "kind": "primary" | "secondary" | "fallback",
  "pref": 0..100
}
```

### 9.19 site_rec()

```cbor
{
  "site_id": h'<32>',
  "name": "...",
  "city": "Tienen",
  "country": "BE",
  "lat": <float>,
  "lng": <float>,
  "site_type": "residential" | "office" | "colo" | ...
}
```

---

## 10. Signing rules

### 10.1 Canonical encoding

For CBOR records: RFC 8949 §4.2.1 deterministic encoding, map keys sorted, minimum integer encoding, definite lengths. The signed bytes are the CBOR encoding of the payload **without** the `s` field (signature).

For BERT frames: sort map keys lexicographically before `term_to_binary/2`. Strip `signature` field to nil/undefined before encoding, then sign the resulting binary.

### 10.2 Domain separation

Every Ed25519 signature is prefixed with a domain string:

| Domain string | Used for |
|---------------|---------|
| `"macula-v2-record\0"` | PKARR record signatures (§9) |
| `"macula-v2-frame\0"` | BERT frame signatures (§3-§8) |
| `"macula-v2-endorse\0"` | Realm endorsements (included in record domain above) |
| `"macula-v2-path\0"` | Source-routing `path_hash` (§11) |
| `"macula-v2-parameter\0"` | Foundation parameter records |

Mixing domains is a bug caught at signature verification — signatures across domains do not verify.

### 10.3 Threshold signatures (foundation)

FROST-Ed25519 (m-of-n). Produces a regular 64-byte Ed25519 signature verifiable against a single aggregated pubkey (the foundation key). Stations need no special logic for threshold — they verify against foundation pubkey embedded in firmware.

### 10.4 Signature verification order

When receiving a record:

1. Parse envelope; extract `k` (pubkey), `v` (version), `x` (expires_at).
2. Check `x > now`. If expired ⇒ reject with `expired`.
3. Verify Ed25519 signature against `k` over canonical(t,k,v,c,x,p).
4. If signature fails ⇒ reject with `signature_invalid` (BOLT#4 code 0x0E).
5. For tombstones: additionally verify version ≥ superseded record's version.
6. For records requiring realm endorsement: additionally verify embedded endorsement signature.

Failure-fast: stop at first failure and return structured error.

---

## 11. Source-routing header

Referenced from Part 3 §6.2. Byte layout (big-endian):

```
Offset  Size  Field
------  ----  -----
 0      1     version        (0x02 for V2)
 1      1     total_hops     (1..8)
 2      1     current_hop    (0..total_hops-1)
 3      1     flags          (bit 0: tier-strict path; bit 1: diversity-strict; bits 2-7 reserved)
 4      8     deadline_ms    (unix ms; uint64 big-endian)
12      16    path_hash      (SHA-256(domain-prefix || hop_ids) truncated to 16)
28      16*N  hops           (first 16 bytes of each NodeId in path, N = total_hops)
```

Total = 28 + 16 × total_hops bytes.
- 1 hop: 44 bytes (the headline "44-byte header").
- 8 hops: 156 bytes.

### 11.1 path_hash derivation

```
domain_prefix = "macula-v2-path\0"
hop_ids = concat(NodeId_1_full_32, NodeId_2_full_32, ..., NodeId_N_full_32)
full_hash = SHA-256(domain_prefix || hop_ids)
path_hash = full_hash[0..15]      ; truncate to 16 bytes
```

Each hop verifies `path_hash` matches when decoding, catching header tampering even though hops only see truncated 16-byte NodeId prefixes.

### 11.2 Per-hop processing

```
1. Verify current_hop < total_hops.
2. Verify deadline_ms > now.
3. Compute expected path_hash from full NodeIds (hop looks up the 16-byte prefixes in its routing table).
4. Verify computed hash matches header path_hash.
5. If current_hop == total_hops - 1 ⇒ self is terminal ⇒ deliver CALL to local handler.
6. Else identify next-hop NodeId via local lookup; check SWIM state.
7. If next-hop unknown or not alive ⇒ return ERROR with BOLT#4 unknown_next_peer (signed).
8. Else increment current_hop; forward frame.
```

---

## 12. Capability bits

Single 32-bit field declaring feature support. Known bits:

| Bit | Name | Meaning |
|-----|------|---------|
| 0 | `quic_0rtt` | Peer supports QUIC 0-RTT resumption |
| 1 | `call_v2` | Supports BOLT#4 failure codes |
| 2 | `source_route_v2` | Understands 44-byte source-route header |
| 3 | `pubsub_plumtree` | Uses Plumtree for realm gossip |
| 4 | `hyparview_join` | Supports HyParView join handshake |
| 5 | `swim_lifeguard` | Lifeguard extensions enabled |
| 6 | `dist_over_mesh` | Can act as dist-tunnel endpoint |
| 7 | `gateway_tier_1` | Declares tier ≥ 1 |
| 8 | `gateway_tier_2` | Declares tier ≥ 2 |
| 9 | `gateway_tier_3` | Declares tier ≥ 3 |
| 10 | `gateway_tier_4` | Declares tier 4 (foundation) |
| 11 | `crypto_puzzle_adaptive` | Accepts adaptive difficulty updates |
| 12 | `mainline_dht_bridge` | Runs reciprocal Mainline DHT node |
| 13 | `blockchain_anchor_reader` | Can read anchor records from chain |
| 14 | `onion_route_phase9` | Supports onion-routed lookup (Phase 9+) |
| 15 | `store_and_forward` | Supports Tier 3+ store-and-forward |
| 16-31 | reserved | future |

HELLO negotiation emits `negotiated_capabilities = caller & responder`. Features gated behind a bit only activate if both sides have it.

---

## 13. Error code table

Extends BOLT#4 taxonomy (Part 4 §6.1) with Part-6-specific codes. Signed by the reporting hop.

| Code | Name | Category |
|------|------|---------|
| 0x00 | `ok` | success |
| 0x01 | `unknown_next_peer` | routing |
| 0x02 | `temporary_relay_failure` | routing |
| 0x03 | `relay_disabled` | policy |
| 0x04 | `node_not_found_at_target_relay` | delivery |
| 0x05 | `target_realm_refused` | policy |
| 0x06 | `loop_detected` | routing |
| 0x07 | `expiry_too_soon` | deadline |
| 0x08 | `upstream_congestion` | capacity |
| 0x09 | `invalid_path_header` | wire |
| 0x0A | `crypto_puzzle_invalid` | identity |
| 0x0B | `realm_not_authoritative_here` | routing |
| 0x0C | `tombstoned` | delivery |
| 0x0D | `payload_too_large` | wire |
| 0x0E | `signature_invalid` | security |
| 0x0F | `unknown_error` | misc |
| **0x10** | `protocol_version_unsupported` | version |
| **0x11** | `capability_missing` | negotiation |
| **0x12** | `realm_not_endorsed` | governance |
| **0x13** | `endorsement_expired` | governance |
| **0x14** | `quota_exceeded` | policy |
| **0x15** | `auth_required` | policy |
| **0x16** | `handshake_timeout` | connection |
| **0x17** | `refresh_failed` | lifecycle (Part 4 §7) |
| **0x18** | `diversity_degraded_refuse` | policy |

Codes 0x80-0xFF reserved for realm-specific extensions (application errors).

---

## 14. Versioning and extension

### 14.1 Version byte

Frame envelope `version` field:
- 0x02 = V2 (this plan's scope).
- 0x03+ = future; incompatible changes.

Peers with major-version mismatch refuse HELLO with `protocol_version_unsupported` (0x10).

### 14.2 Extension policy

Within 0x02:
- **Add new capability bit + new frame types/fields**: safe; older peers ignore via capability negotiation.
- **Add new BOLT#4 error codes in [0x19-0x7F]**: safe; older peers map unknown codes to `unknown_error`.
- **Change payload shape of existing frame**: unsafe; must go through capability gate.
- **Remove field from existing frame**: unsafe; must go through capability gate and deprecation period (6 months).

### 14.3 PKARR record type extension

Adding a new type tag in [0x11+]: safe; custodians that don't understand the tag refuse to STORE (reply `unsupported_type`). Stations that query get `not_found`.

### 14.4 Deprecation

Part 5 §14.3 — foundation announces deprecation ≥6 months before removal. Deprecated features continue to work during window; stations log usage to assist migration.

---

## 15. Worked wire traces

### 15.1 Fresh connection handshake

```
Time   Direction  Frame
─────  ─────────  ─────────────────────────────────────────
0ms    A → B      [QUIC INITIAL + CONNECT]
       CONNECT: node_id=A, realms=[R1,R2], addresses=[...],
                caps=0b...1_1111_1111, puzzle_evidence=SHA256(A),
                endorsements=[R1→A, R2→A], signature=<64>
50ms   B → A      [HELLO]
       HELLO: node_id=B, realms=[R1,R3], caps=0b...1_1110_1111,
              accepted=true, negotiated_capabilities=AND(A,B),
              signature=<64>
80ms   A → B      [SWIM_PING round=0 piggyback=[alive(A)]]
80ms   A → B      [REFRESH phase begins]
        FIND_NODE(key=A.node_id)
        STORE(procedure_ad for A's procedures)
        SUBSCRIBE(aggregated topics for A's realm members)
300ms  both       [CONNECTED state]
```

### 15.2 Cross-realm CALL via source-route

```
Origin A (BE, T0), target Z (PT, T0), path A → BE_T1 → BE_T2 → T3_AMS → PT_T2 → PT_T1 → Z

Time   Hop          Frame
─────  ─────────    ─────────────────────────────
0ms    A            [CALL call_id=UUIDv7 proc=... realm=R42 deadline=now+5000ms
                     source_route=<44+16*5=124 bytes: hops[A..Z]>]
15ms   BE_T1        verify_path_hash ✓; check SWIM(BE_T2)=alive; forward
30ms   BE_T2        verify ✓; SWIM(T3_AMS)=alive; forward
60ms   T3_AMS       verify ✓; SWIM(PT_T2)=alive; forward
90ms   PT_T2        verify ✓; SWIM(PT_T1)=alive; forward
105ms  PT_T1        verify ✓; SWIM(Z)=alive; forward
120ms  Z            current_hop==total_hops-1 ⇒ deliver
                    handler processes; RESULT issued
... reverse path ~120ms ...
240ms  A            RESULT received, payload returned to caller
```

### 15.3 Mid-path failure

```
... as above up to 60ms T3_AMS ...
60ms   T3_AMS       verify ✓; SWIM(PT_T2)=suspect (transient)
                    ⇒ ERROR(code=0x02 temporary_relay_failure,
                            reported_by=T3_AMS, signature=<64>)
... reverse path 60ms ...
120ms  A            ERROR received, retry with path[1] (disjoint)
```

### 15.4 DHT lookup

```
Origin A looking up ProcedureKey K.

Time   Hop      Frame
─────  ───────  ─────────────────────────────
0ms    A        starts 3 disjoint lookups; α=3 per path
0ms    A → P1   FIND_VALUE(K), path=1
0ms    A → P2   FIND_VALUE(K), path=2
0ms    A → P3   FIND_VALUE(K), path=3 (different peers than 1,2)
20ms   P1 → A   NODES(20 closer-to-K peers)
25ms   P2 → A   VALUE(1 record matching K)
30ms   P3 → A   NODES(different set of closer peers)
... continue recursion until all paths converge ...
80ms   A        merged result: 20 distinct records across paths
```

---

## 16. Open questions specific to Part 6

- **O25 (new)** — BERT vs CBOR for frames: do we regret using two encodings? Measure parse cost on RPi 4B in Phase 2; revisit.
- **O26 (new)** — Maximum source-route hops = 8. Too restrictive for Phase 9 onion routing? Revisit then.
- **O27 (new)** — Error code space above 0x7F reserved for realm-extensions; who arbitrates collisions?
- **O28 (new)** — Tombstone envelope shares type tag with tombstoned record or distinct (0x0C)? Currently distinct.
- **O29 (new)** — Anonymous-auth cipher suite for realm-join flows (`auth_required` 0x15 handshakes). Deferred to application.

---

## 17. Success criteria for Part 6

Part 6 is complete when a reader can:

1. Pick any frame (§3-§8) or record (§9) and **encode + sign a concrete instance** by hand or in code.
2. Explain why **CBOR vs BERT** are both used and when each applies (§2.1).
3. Identify the **signing domain string** for any given sig purpose (§10.2).
4. Decode a **source-route header byte dump** (§11) and state per-hop processing rules.
5. State the **capability bits** that gate source-routing + plumtree pubsub (§12).
6. Map any **BOLT#4 error code** (§13) to its cause and reporter.
7. Describe **version extension policy** for adding a new frame type vs modifying existing (§14).
8. Walk through a **handshake wire trace** (§15.1) and a **cross-realm CALL trace** (§15.2).

If any is ambiguous, Part 6 revises before conformance-test authoring (Part 8).

---

*Part 6 closes the wire-level contract. Parts 7–9 remaining: implementation phase schedule (Part 7), verification strategy (Part 8), appendices (Part 9).*
