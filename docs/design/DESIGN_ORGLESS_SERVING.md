# Design: serving without an org (draft for decision)

This exists so an org-less node (an MCP agent, a CLI) can serve a procedure on the macula 12 mesh, which today
only a node holding an org's delegation can do.

Status: **decided by Raf on 2026-09-24: option 4 for the protocol (the self-authorizing `~<hex(node_id)>/<name>`
namespace), and option 1 (rooms over pubsub) now.** Venus and Mercurius. macula-mcp is held on 12 until this is decided
(Raf, 2026-09-24). Code read against macula e3fb3518 and macula-station b2cb203.

## Where things stand

- The record layer already accepts a procedure without an org when it carries no authorization
  (`macula_record:authorization_for(none, undefined, ...)`; `procedure_org/1` is `none` for a name without "/" and
  for `_/...`).
- `no_org_namespace` is enforced in three places: the facade (`macula:signed_provider_advertisement/5`, before
  signing), the station's ADVERTISE admission (`macula_station_peer_observer` `admission/3` → `org_namespace/7`),
  and the station's DHT STORE admission (`macula_dht_admission:advertisement_admission/3` → `org_namespace/6`).
  Any org-less form therefore needs a station release.
- The only authorization form is D25: the realm's org directory plus the org's procedure delegation, verified by a
  caller against the realm key it pins. Orgs and delegations are admitted by a human; nothing is auto-admitted.
- A station keeps one registry entry per (realm, procedure) and routes by it, ignoring the request's target, until
  macula-station#8.

## Options

### 1. Rings and rooms over pubsub (app level, no macula change)

An agent subscribes to one topic per kind of fact and filters by the target in the payload (ids in payloads, not
topics). Publications are signed, so the sender is authenticated.

- Works today. No station or SDK change.
- Loses request/reply: no run-once admission, no signed reply bound to the request, no deadline, and the fan-out
  cost of every subscriber hearing every message.
- Fits rooms and announcements. Fits rings (an agent asking another) only as fire-and-forget messages with an
  app-level correlation id.

### 2. A per-node self-org

The realm signs an org directory for an org owned by each node. Unless a human admits each one, that is
automatic admission, which the gate forbids. **Not recommended.**

### 3. A realm-granted personal delegation at join

Joining a realm is already human-gated (device membership). At that admission the realm also issues a procedure
delegation for the node under a realm-held org (e.g. `member`), so serving rides D25 with no wire, record or
station change.

- No protocol change; the work is in the realm's join flow (Saturnus).
- The delegation covers every procedure under the org, so the procedure must carry the node id until station#8:
  `member/<hex(node)>_<name>`.
- A caller must pin the realm key to accept it, which a bare caller may not do.
- A node serves only in realms that admitted it.

### 4. A self-authorizing namespace (recommended for the protocol)

A procedure whose namespace is the advertiser's own node id, `~<hex(node_id)>/<name>`, is authorized by the
advertisement's own signature: no directory, no delegation.

- It cannot be used to impersonate anyone: only the key behind that node id signs for that namespace.
- It is unique per node, so it does not depend on station#8.
- The caller trusts "node X's procedure", not an org's. That is exactly what an agent calling another agent needs;
  org-scoped trust stays D25's.
- Costs:
  - `macula_record:authorization_for/…`: a clause for org `~` ++ hex(advertiser_node), with no authorization,
    signer = advertiser_node;
  - the same form accepted in both station admissions (a station release). Once macula-station#10 makes ADVERTISE
    admission verify the D25 chain, this is the one form it admits without a chain, checked instead as: the
    namespace is `~` ++ hex of the advertisement's signer;
  - the caller side (`macula:trusted_provider_advertisement`, and macula-go's pool) skips the realm-key check for
    this form only;
  - a D25 amendment;
  - macula-go ports all three pieces (record, pool resolve, Serve) with the same fixtures.

## Recommendation

- **Protocol: option 4.** It is the one form that gives an org-less node a real request/reply service without a
  human per node, and without weakening D25 for org procedures.
- **Now: option 1 for rooms** (and ring messages that do not need a reply), which works on today's fleet.
- Option 3 stays possible later for org-scoped member services; it is not needed for agents.

## Related, found while reading (raised separately)

A station's ADVERTISE admission checks only the form of the authorization (`authorization_form/6`: two
binaries), never the chain, and a direct ADVERTISE replaces the single registry entry for its (realm, procedure)
(`live_advertise/6`). Any node can therefore take over a station's routing for any org's procedure by attaching
junk bytes. Callers refuse replies not signed by their target (`macula_frame:verify_reply/3` → `not_the_target`),
so this is a denial of service, not impersonation. Filed as macula-station#10, for 0.6.3.

## Decision

Raf, 2026-09-24: **(a) option 4 plus rooms over pubsub.**

- The SDK side ships in macula 12.5.0 (`macula_record:own_namespace/1`, shared fixtures in
  `test/fixtures/own_namespace/`); the station side is planned for macula-station 0.6.4 (not released yet). It was planned as: the facade accepts a `~<hex(own node_id)>/<name>` procedure without an
  authorization, and a caller trusts that form only when the advertisement's signer is the node the namespace names.
  Fixtures are shared with macula-go.
- The two station admissions change with Mars. Per-node caps (macula-station#6) apply, so a node cannot flood the
  registry through its own namespace. It ships in or after macula-station 0.6.3, and does not delay 0.6.3's security
  fix (macula-station#10).
- D25 is amended (the wording is reviewed by Raf, as D25 is a decision record).
