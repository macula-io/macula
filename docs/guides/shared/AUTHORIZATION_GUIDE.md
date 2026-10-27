# Macula Authorization Guide

This guide covers Macula's identity and authorization primitives: a node's
post-quantum keys, and UCAN capability tokens.

## Overview

Macula's authorization is:

- **Self-sovereign**: identity is the node's own identity key, ML-DSA-87 (with
  an RSA-PSS half in the `pq_hybrid` profile), named by its node_id
- **Cryptographically verifiable**: no network calls needed for validation
- **Capability-based**: fine-grained permissions via UCAN tokens
- **Offline-capable**: all validation happens locally

> **What's actually gated today.** The SDK's only enforced authorization
> point is per-procedure: `macula:advertise/5`'s `auth` opt (and the same
> opt on `macula:advertise_stream/6` for streaming procedures) takes `open`
> (default: serve any identified caller; every request is signed by its
> caller's node key, so "open" is not "anonymous"), `{ucan_required,
> IssuerNodeId}` (a caller must present a valid UCAN issued by the node with
> that node_id and minted for the caller itself, passed through
> `call_station/7`'s `ucan_token` opt), or `{realm_member_required,
> RealmKeyId, RequiredCan}` (a caller must present a UCAN issued by the
> realm's key, likewise minted for the caller itself, carrying a capability
> whose `can` matches `RequiredCan` exactly, which closes the tier gap an
> issuer-only check can't: a realm can mint membership UCANs at more than one
> tier from the same key, e.g. a human-confirmed tier versus a self-service
> device tier, and `RequiredCan` is mandatory so a service names the tier it
> actually needs). Both policies bind a token's audience (`aud`) to the
> wire-authenticated caller, so a token copied from someone else is refused.
> There is no namespace-ownership check on publish/subscribe/call. See
> [Direct-Dial Dual-Trust](#direct-dial-dual-trust) for the one place the
> SDK does enforce something end-to-end: provider authorization against
> squatted advertisements.

---

## Core Concepts

### Node keys and node_ids

A node holds one key per purpose (`macula_node_keys`): an identity key, a
CONNECT key, and on a station a TLS key; a realm, an org and a foundation each
hold a key of their own. In `pq_pure` a key is ML-DSA-87; in `pq_hybrid` an
identity key pairs ML-DSA-87 with RSA-PSS and signs the IETF LAMPS composite
`id-MLDSA87-RSA4096-PSS-SHA512`. ML-DSA is
[`macula-mldsa`](https://crates.io/crates/macula-mldsa), verified against
NIST's ACVP vectors.

A node_id is SHA-256 over the label `MACULA-NODE-ID-V1`, the profile's name
and the identity key as carried. Any other key, a realm's for instance, is
named by its key id, the same hash under the label `MACULA-KEY-ID-V1`.

```erlang
{ok, Key}    = macula_node_keys:generate(identity, pq_pure),
{ok, NodeId} = macula_node_keys:node_id(Key),
{ok, RealmKey} = macula_node_keys:generate(realm, pq_pure),
RealmKeyId   = macula_node_keys:key_id(RealmKey).
```

---

### User Controlled Authorization Networks (UCANs)

**UCAN** (User Controlled Authorization Networks) is a capability-based
authorization system built on JWT (JSON Web Tokens). UCANs enable
**delegation chains** where permissions can be granted and re-delegated
without involving a central authority.

#### UCAN Token Structure

`macula_ucan:create/4` signs a JWT with the issuer's key. Its header's `alg` is
the profile's: RFC 9964's `ML-DSA-87` in `pq_pure`, and `ML-DSA-87-PS384`, the
LAMPS composite, in `pq_hybrid` (JOSE has no name for it yet). Its claims:

| Claim | Description |
|-------|-------------|
| `iss` | **Issuer** - a `did:key` for the issuer's key as carried: multibase base58btc over the multicodec `mldsa-87-pub` (0x1212) in `pq_pure`, or Macula's own key type (0x300087, private use) in `pq_hybrid`, then the key |
| `aud` | **Audience** - the audience's node_id in lowercase hex |
| `exp` | **Expiration** - Unix timestamp in seconds; every token has one, at most ten years (365.25-day years) past now |
| `nbf` | **Not Before** (optional) - token valid only from this time |
| `cap` | **Capabilities** - array of `{with, can}` grants |
| `prf` | **Proofs** (optional) - parent UCANs, for delegation |
| `nnc` | **Nonce** (optional) - for uniqueness |
| `fct` | **Facts** (optional) - metadata |

```erlang
Token = macula_ucan:create(IssuerKey, AudienceNodeId,
                           [#{with => <<"mri:realm:io.example">>, can => <<"read">>}],
                           #{exp => erlang:system_time(second) + 3600}),

{ok, Claims} = macula_ucan:authorize(Token, {ucan_required, IssuerNodeId},
                                     #{caller => AudienceNodeId, profile => pq_pure,
                                       now => erlang:system_time(second)}).
```

`macula_ucan:authorize/3` verifies the signature over the header and payload
exactly as they arrived, never over re-encoded JSON, then the issuer, the
audience, the validity window, and for a membership policy the capability.
It follows no delegation chain yet: a token is authorized only when its own
issuer is the one the policy names.

**Further Reading:**
- [UCAN Specification](https://ucan.xyz/)
- [Fission UCAN Explainer](https://fission.codes/blog/auth-without-backend/)
- [Brooklyn Zelenka's UCAN Paper](https://github.com/ucan-wg/spec)

---

## Certificates removed in 11.0.0

The 10.x certificate form is gone: macula 11.0.0 issues no X.509
certificates (design B1), so `macula_cert` and `macula_cert_system` are
removed, and a provider authorization is only the realm-signed org
directory and the org-signed procedure delegation, carried inside the
provider's `procedure_advertisement` (see
[consumer → provider](#consumer--provider-provider-authorization) below).


## Direct-Dial Dual-Trust

Direct-dial RPC (a consumer resolves a `procedure_advertisement` and dials the
provider's station) collapses the path to one QUIC/TLS session between two
sovereign identities — the natural place for a **mutual** check. Trust is
bidirectional, unlike the one-directional server-authenticates-client of classic
RPC:

- **consumer → provider** — is this the legitimate server of the procedure, not a
  squatter who wrote an advertisement next to the real one?
- **provider → consumer** — should I serve *this* caller at all? Direct-dial makes
  every station a public front door, so the provider decides who it answers.

Both stay compatible with fully-open, permissionless discovery: the discovery
layer is always open, and each endpoint independently chooses what it checks.

### consumer → provider: provider authorization

A procedure with an org namespace, the text before the first `/` of its name,
is served only by a provider that org authorized, and a caller checks that
before it calls. The provider's `procedure_advertisement` carries its
authorization: the realm-signed org directory and the org-signed procedure
delegation that names the provider, checked against the realm key. It is the
only form. 11.0.0 has no certificate form, and an advertisement carrying any
other authorization is refused as `authorization_form_unsupported`. A
procedure without an org namespace carries none, and an advertisement
expires no later than any part of its authorization. A provider publishes
its authorization with `macula_response:advertise_direct/7`'s
`authorization` option.

A caller's pool pins each realm's key when it starts, as
`realm_trust => #{RealmId => RealmKey}` in `macula:connect/2`'s options, and
resolution checks an advertisement only against the key pinned for its realm.
Without that key, the advertisement is never trusted, so writing an
advertisement next to the real one does not make a node the server of an org's
procedure. A realm key never arrives with a request: `realm_trust` on a call
is refused with `{error, {removed_option, realm_trust}}`, as the 10.x options
`verify_cert_chain` and `cert_chain` are.

A caller checks a call's authorization from the advertisement alone and looks
up no tombstone. A delegation its org withdraws is honoured until it expires,
so the caller-side revocation bound for a new call is the delegation's maximum
lifetime, 30 minutes, capped by the record layer for that type
(`PROCEDURE_DELEGATION_MAX_LIFETIME_MS`, D32) in the signer and in the
verifier, plus the 5 minutes of clock tolerance, counted from the realm's
last issue of it. The realm reissues every 10 minutes, and a revoked provider
gets no fresh one. A stream already open to the provider does better: see
[Open streams and revocation](#open-streams-and-revocation).

```erlang
%% consumer side (the check resolution runs on each verified advertisement)
ok = macula_record:verify_authorization(Advertisement,
                                        #{profile => Profile, realm_key => RealmKey},
                                        erlang:system_time(millisecond)).
```

> Note on the realm tag: the 32-byte realm tag is `SHA-256(realm_name)` — a
> keyless label, not a signing key. Trust therefore roots in the realm's
> **key**, which a caller pins, not the tag.

### provider → consumer: UCAN-gated procedures

A bare advertisement serves any *identified* caller (every request is signed by
its caller's node key, so "open" is not "anonymous"). A provider can instead require
a UCAN per procedure via `advertise/5`'s `#{auth => {ucan_required, IssuerNodeId}}` —
a caller presents a `ucan_token` on the CALL (`call_station/7`'s `Opts`), and a
caller without a valid one is refused with a BOLT#4 `unauthorized` code rather
than a timeout. The token is verified offline against the chain the provider
recognises — no live authority in the path.

A valid token is issued by the node the policy names, unexpired, and minted
for the caller that presents it: its `aud` must be the calling node's node_id
in lowercase hex, and `macula_ucan:authorize/3` compares it with the
wire-authenticated caller of the CALL or STREAM_OPEN. A genuine token minted
for someone else is refused like no token at all, so a copied token does not
work for whoever holds the copy.

Managed realms are the first target for this model; the fully-open public realm
keeps discovery permissionless and layers authorization on top only where a
provider opts in.

### provider → consumer: realm-membership-gated procedures

`{ucan_required, IssuerNodeId}` gates a procedure to tokens from exactly one
known node. `{realm_member_required, RealmKeyId, RequiredCan}` gates on
membership in a realm instead — any caller holding a valid UCAN issued by the
realm's key (named by its key id, not the 32-byte realm tag), whose `aud`
names the caller itself and whose capability list carries `RequiredCan`, is
admitted:

```erlang
%% RealmKeyId: the key id of the realm's key (macula_node_keys:key_id/2 over
%% the key the realm publishes), never the 32-byte realm tag used for `-realm` flags.
%% RequiredCan: mandatory -- name the exact tier this procedure needs, since
%% a realm can mint membership UCANs at more than one tier from the same
%% key (a human-confirmed tier and a weaker self-service tier are both
%% "genuine, correctly-signed" tokens; only the capability tells them apart).
Opts = #{auth => {realm_member_required, RealmKeyId, <<"member/email-verified">>}},
ok = macula:advertise(Pool, Realm, <<"private.procedure">>, Handler, Opts).
```

This policy binds the audience the same way `ucan_required` does: a token that
is genuinely realm-signed and unexpired, but minted for a different member, is
refused. What it adds is trust in a realm rather than one issuer, and the tier
check: `RequiredCan` must appear in the token's capabilities.

### Realm rights: roles for call, serve, publish and subscribe (macula#128)

A realm admin gives members roles, and the realm expands each member's roles into the capabilities of the
membership token it issues: `rpc/call` on `mri:proc:<realm>/<procedure>`, `pubsub/publish` and `pubsub/subscribe`
on `mri:topic:<realm>/<topic pattern>`. The serve right travels in the organisation's procedure delegation instead
(`rpc/serve`). Each right is checked where it can be enforced, and each check point holds a different promise.

| Right | Carried by | Checked by | Holds against a dishonest station? |
|---|---|---|---|
| call | the membership token on each CALL | the provider | yes: the provider verifies the chain itself |
| serve | the delegation's `procedures` list | the station at admission, and a caller that dials directly | yes for a caller that checks; a station can still drop |
| publish | the membership token, presented once per connection | the publisher's own station | **no**: see below |
| subscribe | the membership token, presented once per connection | the subscriber's own station | **no**: delivery control, not confidentiality |

**Call.** A provider gates a procedure on the realm's capability with the policy above:

```erlang
Opts = #{auth => {realm_member_required, RealmKeyId, <<"rpc/call">>}},
ok = macula:advertise(Pool, Realm, <<"acme/orders.place">>, Handler, Opts).
```

A member whose role covers `mri:proc:<realm>/acme/orders.place` is served. A member whose role names another
procedure is refused `unauthorized`.

**Serve.** An organisation names what a node may serve in the delegation it signs:

```erlang
Delegation = macula_record:procedure_delegation(OrgKeyId, NodeId,
                                                #{procedures => [<<"acme/orders.place">>, <<"acme/billing/*">>]}).
```

Each entry is an exact procedure of the organisation or a prefix ending in `/*`, which covers whole segments below
it. A delegation names at most 64 entries of at most 512 bytes each. A caller that dials a provider directly checks
the list (`macula_record:verify_serving/3`). A delegation without the list still verifies, and is counted as one that
would be refused.

**Publish and subscribe.** A member presents its membership token to its stations once:

```erlang
ok = macula:present_membership(Pool, Realm, Token, Proofs).
```

The pool presents it on every link. The first SUBSCRIBE or PUBLISH in the realm on each connection carries it
(the per-hop extension `realm.membership`, WIRE_1_0.md section 3.6). The station verifies it once, with the link
peer as its audience, so a token another node presents is refused. It then answers that connection's SUBSCRIBEs and
PUBLISHes from the capabilities it keeps until the token expires. Present the renewed token before the old one ends:
a link already subscribed in the realm sends it at once. A capability must cover the whole subscription:
`orders.eu.*` does not cover `orders.*`. A station that refuses answers with a REGISTER_ACK naming the topic and
`not_authorized`. For a PUBLISH the pool reports it as `_macula.client.registration_refused`.

What these two do not promise:

- Publish is checked at the publisher's own station only. A station that colludes with a member can accept or inject
  readable events on a clear topic, and the stations after it never see the token. Protect content with a sealed
  group, and check publishers at the consumer (`macula:org_member/4,5`).
- Subscribe on a clear topic controls delivery. A station sees every clear event it relays, and any recipient can
  forward one. Only a sealed group keeps a topic's content from those who may not read it.

**Counting first.** Every check runs in a mode per realm, set in the `realm_rights` environment of the `macula`
application:

```erlang
{macula, [{realm_rights, #{default => counting, RealmId => refusing}}]}
```

`off` checks nothing. `counting`, the default, lets everything through and counts what it would refuse.
`refusing` refuses it. Switch a realm to `refusing` once `macula_realm_rights:counters/0` shows no `would_refuse`
for its authorised traffic. A realm in `counting` is open by design: show it as counting, never as protected.
System topics (those starting with `_`, such as `_mesh.node.up`) carry the mesh's own traffic and are exempt.

### provider → consumer: gated streaming procedures

A streaming procedure takes the same policies. Pass `auth` to
`macula:advertise_stream/6` (or in the `Opts` of `macula_streamer:advertise/6`
and `advertise_direct/7`), and a consumer presents its token with
`call_stream/5`'s `ucan_token` opt:

```erlang
Opts = #{auth => {realm_member_required, RealmKeyId, <<"member/email-verified">>}},
ok = macula:advertise_stream(Pool, Realm, <<"private.feed">>, server_stream,
                             Handler, Opts),
{ok, Stream} = macula:call_stream(Pool, Realm, <<"private.feed">>, Args,
                                  #{ucan_token => Token}).
```

The provider first verifies the STREAM_OPEN's signature against its `caller`,
then applies the policy before the handler runs. A refused STREAM_OPEN gets a
STREAM_ERROR with code `unauthorized` on its stream, and the handler never
runs.

### Open streams and revocation

A stream is authorized when it opens, and it can stay open for hours. Since
macula#120 it holds no longer than the authority it was opened under, on both
ends, and a revocation reaches it without waiting for that authority to
expire.

**On the provider's side**, a gated session ends when the token it was opened
with expires (the earliest `exp` of the token and its proofs). A caller keeps
its stream past that by renewing: give `call_stream`'s options a
`ucan_renewal => fun() -> {ok, Token} | _ end`, and the caller's link calls the
provider's `~<node_id>/stream_renew` at a third of the token's remaining life,
once per provider, realm and token source, naming the streams it covers. A
provider-signed `unknown_next_peer` stops the renewal; any other failure
retries on a backoff that stays short of the end. For a
`realm_member_required` session the provider also re-reads the member's
endorsement slot every `standing_recheck_ms` (60 seconds by default, link
option or `macula` application env). The member is the audience of the token
chain's root, the node the realm endorsed, which may have handed its rights on
to the caller (`macula_ucan:root_audience/2`). The realm's tombstone of the
endorsement (`withdrawn`) ends every session of that member and of its
delegates, with a STREAM_ERROR `unauthorized`. Their tokens stay valid to
their `exp`, so the provider's link then holds the member: STREAM_OPENs and
CALLs under that realm's member policy whose chain roots at it are refused
`unauthorized` until the latest end of any token it was served or refused
under, and at least 1 hour and 5 minutes. That floor assumes the realm's
membership tokens live an hour, which is the realm's policy: this library does
not cap a token's life. Only time ends a hold, since a stale replica can still
answer with the endorsement; a member the realm admits again waits it out. The
re-checks run one at a time; one due while the last is still reading is
skipped.

The hold belongs to one station link, not to the provider. A provider has one
link per station it is reached through, and a link that restarts starts with
no hold, so a withdrawn member can open a new session on another of the
provider's station links, or on a link that restarted, and is served there
until that link's own re-check reads the withdrawal: up to one more
`standing_recheck_ms` per station link and per restart.

**On the caller's side**, a stream opened to an org procedure ends when the
provider's authorization ends (`macula_record:authorization_ends_at/3`, the
earlier expiry of the org directory and the delegation), moved forward only
by the provider's renewed advertisements. Every `standing_recheck_ms` the
caller re-reads the provider's delegation slot under the org key that
delegated it (`macula_record:authorization_org_key/3`,
`macula_record:delegation_standing/5`): the org's tombstone of the delegation
aborts the stream `unauthorized` at once. A node's streams to one provider
under one org key share that reading, so they cost one lookup per re-check
however many there are. A shared reading is used only while it is younger than
half the re-check, so no stream acts on one older than half a re-check when
it reads.

Only a tombstone ends a stream early. A failed lookup, an empty slot or an
entry under any other key is no evidence of revocation, so the stream runs to
its end.

The bound, after the DHT replicas hold the tombstone: on the provider's side,
a stream to a withdrawn member ends within one re-check, 60 seconds by
default, plus the lookup that reads it. On the caller's side, a stream to a
provider whose delegation the org tombstoned ends within one and a half
re-checks, 90 seconds by default, plus the lookup: its last answer may be a
shared reading just under half a re-check old, and its next ask is one
re-check later. A withdrawn member that opens again through
another station link, or after a link restarts, is ended within one more
re-check on that link. Without a tombstone, when the realm or org only stops
reissuing, a stream ends when the authority does: at most 1 hour plus 5
minutes of clock tolerance for a member (the life the realm gives its
membership tokens), 35 minutes for a provider.

This covers streams. A subscription is not yet bounded this way: it lasts as
long as its link.

Not covered: `macula:call_stream_station` given an `advertisement` by the app
gets no follower on the caller's side, since no resolution produced it.

---

## Best Practices

### Token lifetime guidelines

| Use Case | Recommended Lifetime |
|----------|---------------------|
| API calls | 1-24 hours |
| Long-term partnerships | Months (narrow scope) |
| Sensitive operations | Always short |

### Security recommendations

1. **Use short-lived tokens** for sensitive operations
2. **Narrow capability scope** — grant only what's needed
3. **Store UCAN tokens securely** (encrypted at rest, treat as credentials)
4. **Protect private keys** — never leave the generating node

---

## Glossary

| Term | Definition |
|------|------------|
| **node_id** | SHA-256 over a node's identity key and profile; names the node |
| **key id** | The same hash of any other key, under its own label; names a realm's key |
| **did:key** | A DID that is a key itself; names a UCAN's issuer |
| **UCAN** | User Controlled Authorization Network - capability-based auth token |
| **Capability** | Permission grant with resource and operation |
| **ML-DSA-87** | FIPS 204 post-quantum signature, the node keys' algorithm |
| **LAMPS composite** | ML-DSA-87 with RSA-PSS, both required; the `pq_hybrid` signature |

---

## References

### Standards

- [UCAN Specification](https://ucan.xyz/) - User Controlled Authorization Networks
- [RFC 7519 - JWT](https://www.rfc-editor.org/rfc/rfc7519) - JSON Web Token specification
- [FIPS 204](https://csrc.nist.gov/pubs/fips/204/final) - ML-DSA
- [RFC 9964](https://www.rfc-editor.org/rfc/rfc9964) - ML-DSA in JOSE and COSE
- [draft-ietf-lamps-pq-composite-sigs](https://datatracker.ietf.org/doc/draft-ietf-lamps-pq-composite-sigs/) - composite ML-DSA
- [The did:key Method](https://w3c-ccg.github.io/did-method-key/) - DIDs that are keys

### Related Guides

- [RPC Guide](../rpc/RPC_GUIDE.md) - direct-dial, `advertise/5`'s `auth` opt, `call_station/7`'s `ucan_token`
- [MRI Guide](MRI_GUIDE.md) - typed, hierarchical resource identifiers
