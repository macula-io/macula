# Macula SDK — Records Guide

**Signed, expiring facts in the mesh DHT: the primitive that content, RPC advertisement and station presence are
built on.**

> **Audience:** applications that publish their own signed, discoverable fact into the mesh, such as a capability
> announcement, a presence beacon or a small piece of domain state, without building an RPC procedure to hand it
> out. If what you have is an immutable blob, see the [Content Guide](../content/CONTENT_GUIDE.md) instead: content
> is built on this same primitive but is addressed by hash, not by signer.

---

## Overview

A **record** is a signed object `{key, tbs, signature}` stored in the mesh DHT. `key` is the signer's public key as
carried, and `signature` covers the label `MACULA-PQ-RECORD-V1`, the key and `tbs`, which holds these fields:

```erlang
#{type       => Type,        % 1 to 255: what kind of fact this is
  version    => Version,     % 16-byte UUIDv7: orders one signer's successive writes
  created_at => CreatedAtMs,
  expires_at => ExpiresAtMs, % a record past this is treated as gone
  payload    => Payload,     % the type's fields, with text keys
  subject    => Subject}     % domain types only, and optional: a non-empty binary
```

The key and the signature are ML-DSA-87 in the `pq_pure` profile, and ML-DSA-87 with RSA-PSS in `pq_hybrid`.

Macula uses this primitive for things with guides of their own: a station's presence (`node_record`), an advertised
RPC procedure (`procedure_advertisement`) and a content blob's location (`content_announcement`). Those built-in
types, tags `0x01` to `0x1F`, have their own constructors in `macula_record`. This guide covers the other half:
`macula_record:envelope/3`, the constructor for **your own** record type, tags `0x20` to `0xFF`.

---

## Signing a domain record

Your node's identity key lives in its pool and never leaves it, so the pool signs your record as your node:

```erlang
Payload = #{{text, <<"status">>} => {text, <<"idle">>},
            {text, <<"since">>}  => erlang:system_time(millisecond)},
Record0 = macula_record:envelope(16#20, Payload, #{ttl_ms => 60 * 60 * 1000}),
{ok, Record} = macula_client:sign_domain_record(Pool, Record0),
ok = macula:put_record(Pool, Record).
```

The pool stamps the record with a new version and `created_at`, keeps the lifetime you built it with, and signs it.
Only the record's type, times, payload and subject reach the pool. `sign_domain_record/2` refuses these before it
calls the pool:

| Refusal | When |
|---|---|
| `not_a_domain_type` | the type is outside `0x20` to `0xFF` |
| `invalid_subject` | the subject is not a non-empty binary |
| `lifetime_too_long` | the record would live longer than 7 days |
| `lifetime_reversed` | the record expires at or before its creation |
| `record_too_large` | payload and subject pass 256 KiB together |

A lifetime is never shortened to fit: a longer record is refused. A realm, org or foundation that holds its own key
signs its domain records with `macula_record:sign/2` and that key.

Domain code picks its own type tag and payload field names, and sets its type's payload rules; the envelope owns
only the fields above. Nothing coordinates tag assignment across applications sharing a realm, so pick a tag and keep
it stable for that record shape, as you would a topic name.

To take a record back before it expires, have the pool sign its tombstone, and store that:

```erlang
{ok, Tombstone} = macula_client:withdraw_node_record(Pool, Record, shutdown),
ok = macula:put_record(Pool, Tombstone).
```

The tombstone lands on the record's own storage key, and lives until the record has expired plus the clock tolerance
of 5 minutes.

---

## Storing and finding records

```erlang
%% Store: the station verifies the record and propagates it to the K nearest DHT peers
ok = macula:put_record(Pool, SignedRecord),

%% Fetch by storage key: the first record found there
{ok, Record}       = macula:find_record(Pool, StorageKey),
{error, not_found} = macula:find_record(Pool, SomeOtherKey),

%% Fetch every record at a key: a station serving a shared procedure has
%% N providers, each with their own record under the same key
{ok, Records} = macula:find_records(Pool, StorageKey),

%% Fetch every record of a type currently visible from this pool's stations
{ok, AllOfType} = macula:find_records_by_type(Pool, 16#20),

%% React to new records of a type as they are stored, live
{ok, SubRef} = macula:subscribe_records(Pool, 16#20, fun(Record) ->
    io:format("new record: ~p~n", [Record])
end),
ok = macula:unsubscribe_records(Pool, SubRef).
```

`find_record/2` verifies a record under your node's crypto profile before it returns it, and returns the refusal of
one that does not verify, such as `{error, expired}`. A valid signature says who signed a record, not that the signer
is the one you expect: compare `macula_record:key_id(Record)` with the signer you trust before you act on its payload.
A record you hold as its wire form verifies with `macula_record:verify/2`:

```erlang
{ok, Profile} = macula_crypto_profile:configured(),
{ok, Record}  = macula_record:verify(WireBytes, Profile).
```

`find_records_by_type/2` and `subscribe_records/3` see only what the pool's *connected* stations know: each station
has its local DHT replicas plus whatever its peers have gossiped. Aggregating across the whole mesh means querying
several stations and deduplicating by storage key yourself; there is no global index.

---

## Storage keys

`macula_record:storage_key/1` decides where in the DHT a record lands. For a domain type:

| Record has | Storage key |
|---|---|
| no subject | derived from the signer's key id: **one slot per signer and type** |
| a subject | derived from the signer's key id and the subject: **one slot per signer, type and subject** |

Both are SHA-256 over the label `MACULA-PQ-STORAGE-KEY-V1`, a zero byte, the type and those fields. The signer's key
id is the key id of its key as carried, so a domain record the pool signs lands under your node key's key id, never
in another signer's slot. That key id is not your node_id: your node record and your domain records never share a
slot.

Use a subject when one signer publishes facts about many different things, such as a realm admin signing a license
record per licensee. Without one, a signer's second `put_record/2` for the same type replaces their first: there is
one slot for them at that type. An empty subject is refused, since it would name a slot apart from no subject.

```erlang
Record0 = macula_record:envelope(16#20, Payload, #{subject_id => <<"licensee-42">>}),
```

The lifetime is also an `Opts` field, `ttl_ms`. Without it a domain record lives 48 hours, and it lives at most 7
days. A record whose `expires_at` has passed is treated as absent by readers, even while a store still holds it.

---

## Reference

| Function | Role |
|---|---|
| `macula_record:envelope(Type, Payload, Opts)` | build an unsigned domain record, tag `0x20` to `0xFF`, with `subject_id` and `ttl_ms` in `Opts` |
| `macula_client:sign_domain_record(Pool, Record)` | the pool signs a domain record as your node |
| `macula_client:withdraw_node_record(Pool, Record, Reason)` | the pool signs the tombstone of a record it signed |
| `macula_record:sign(Record, Key)` | a realm, org or foundation signs with its own key |
| `macula_record:verify(Wire, Profile)` | verify a record's wire form under a crypto profile |
| `macula_record:storage_key(Record)` | the DHT key a signed record is stored and found under |
| `macula:put_record(Pool, Record)` | store a signed record in the mesh DHT |
| `macula:find_record(Pool, Key)` | fetch and verify the first record at a storage key |
| `macula:find_records(Pool, Key)` | fetch every record at a storage key |
| `macula:find_records_by_type(Pool, Type)` | every record of a type visible from this pool's stations |
| `macula:subscribe_records(Pool, Type, Fun)` / `unsubscribe_records/2` | live callback on new records of a type |

Built-in record types, tags `0x01` to `0x1F` (station presence, procedure advertisements, content announcements,
realm and org directories, delegation chains), have their own constructors in `macula_record` and are documented
where they are used: [RPC Guide](../rpc/RPC_GUIDE.md), [Content Guide](../content/CONTENT_GUIDE.md) and
[Authorization Guide](AUTHORIZATION_GUIDE.md).
