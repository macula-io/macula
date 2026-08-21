# Macula SDK — Records Guide

**Signed, TTL'd facts in the mesh DHT — the primitive content, RPC
advertisement, and station presence are all built on.**

> **Audience:** applications that need to publish their own signed,
> discoverable fact into the mesh — a capability announcement, a presence
> beacon, a small piece of domain state — and don't want to build a bespoke
> RPC procedure just to hand it out. If what you actually have is an
> immutable blob, see the [Content Guide](../content/CONTENT_GUIDE.md) instead; content
> is built on this same primitive but is addressed by hash, not by signer.

---

## Overview

A **record** is a signed, expiring map stored in the mesh DHT:

```erlang
#{type       => Type,        % uint 1-255 — what kind of fact this is
  key        => SignerPubkey,% 32B Ed25519 public key of whoever asserted it
  version    => Version,     % 16B UUIDv7 — orders successive writes
  created_at => CreatedAtMs,
  expires_at => ExpiresAtMs, % records past this are treated as gone
  payload    => Payload,     % type-specific map — your fields, your names
  signature  => Signature}   % 64B Ed25519, added by sign/2
```

Macula uses this primitive internally for things you already have guides
for — a station's presence (`node_record`), an advertised RPC procedure
(`procedure_advertisement`), a content blob's location (`content_announcement`).
Those built-in types (tag range `0x01`-`0x1F`) have their own typed
constructors in `macula_record` and their own guides — you won't normally
build one by hand. What *this* guide covers is the other half:
`macula_record:envelope/4`, the generic constructor for **your own** record
type, in the reserved range `0x20`-`0xFF`.

```erlang
Payload = #{status => <<"idle">>, since => erlang:system_time(millisecond)},
Record0 = macula_record:envelope(16#20, macula_identity:public(Id), Payload, #{}),
Record  = macula_record:sign(Record0, Id),
ok      = macula:put_record(Pool, Record).
```

Domain code picks its own type tag from the `0x20`-`0xFF` range and its own
payload field names — the envelope only owns `type`/`key`/`version`/
`created_at`/`expires_at`/`payload`/`signature`. Nothing coordinates tag
assignment across applications sharing a realm; pick a tag and keep it
stable for that record shape, the way you'd pick a topic name.

---

## Storing and finding records

```erlang
%% Store (the relay validates the signature; propagates to K-nearest DHT peers)
ok = macula:put_record(Pool, SignedRecord),

%% Fetch by storage key — the first record found there
{ok, Record}   = macula:find_record(Pool, StorageKey),
{error, not_found} = macula:find_record(Pool, SomeOtherKey),

%% Fetch EVERY record at a key — a station serving a shared procedure has
%% N providers, each with their own record under the same key
{ok, Records} = macula:find_records(Pool, StorageKey),

%% Fetch every record of a type currently visible from this pool's stations
{ok, AllOfType} = macula:find_records_by_type(Pool, 16#20),

%% React to new records of a type as they're stored, live
{ok, SubRef} = macula:subscribe_records(Pool, 16#20, fun(Record) ->
    io:format("new record: ~p~n", [Record])
end),
ok = macula:unsubscribe_records(Pool, SubRef).
```

Always verify a record you didn't just build yourself before trusting its
payload — `put_record`/`find_record` move signed bytes, they don't imply the
signer is who you expect:

```erlang
{ok, Record}  = macula:find_record(Pool, StorageKey),
{ok, _Record} = macula_record:verify(Record).   % checks the Ed25519 signature
```

`find_records_by_type/2` and `subscribe_records/3` see only what the pool's
*connected* stations know — each station has its local DHT replicas plus
whatever its peers have gossiped. Aggregating across the whole mesh means
querying multiple stations and deduplicating by storage key yourself; there
is no global index.

---

## Storage keys

`macula_record:storage_key/1` decides where in the DHT a record lands, and
the rule for domain-defined types (`0x20`-`0xFF`) is:

| Envelope has | Storage key |
|---|---|
| no `subject_id` in `Opts` | the signer's own pubkey — **one DHT slot per signer** |
| `subject_id => Sid` (32B) in `Opts` | `SHA-256(<<Type:8, SignerKey/binary, Sid/binary>>)` — **one slot per (signer, subject)** |

Use `subject_id` when one signer needs to publish facts about many different
things — a realm admin signing a license record per licensee, for example.
Without it, a signer's second `put_record/2` for the same type overwrites
their first: there's only one slot for them at that type.

```erlang
Opts0 = #{},
Opts1 = #{subject_id => SomeSubjectPubkeyOr32Bytes},
Record = macula_record:envelope(16#20, SignerPubkey, Payload, Opts1),
```

TTL is also an `Opts` field — `ttl_ms`, defaulting to the envelope's own
default. A record whose `expires_at` has passed is treated as absent by
readers even if it's still physically in the store.

---

## Reference

| Function | Role |
|---|---|
| `macula_record:envelope(Type, Key, Payload, Opts)` | build an unsigned domain record, tag `0x20`-`0xFF` |
| `macula_record:sign(Record, Identity)` | Ed25519-sign the envelope |
| `macula_record:verify(Record)` | check a fetched record's signature before trusting its payload |
| `macula_record:storage_key(Record)` | the DHT key this record will be stored/found under |
| `macula:put_record(Pool, Record)` | store a signed record in the mesh DHT |
| `macula:find_record(Pool, Key)` | fetch the first record at a storage key |
| `macula:find_records(Pool, Key)` | fetch every record at a storage key (multi-provider case) |
| `macula:find_records_by_type(Pool, Type)` | every record of a type visible from this pool's stations |
| `macula:subscribe_records(Pool, Type, Fun)` / `unsubscribe_records/2` | live callback on new records of a type |

Built-in record types (`0x01`-`0x1F`) — station presence, procedure
advertisements, content announcements, realm/org directories, delegation
chains — have their own typed constructors in `macula_record` and are
documented where they're used: [RPC Guide](../rpc/RPC_GUIDE.md),
[Content Guide](../content/CONTENT_GUIDE.md), [Authorization Guide](AUTHORIZATION_GUIDE.md).
