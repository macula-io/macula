# Plan: Mesh Topic Authorization with UCAN/DID

**Status:** Planning
**Created:** 2026-01-07
**Updated:** 2026-01-07

## Overview

Implement decentralized authorization for Macula mesh operations using existing UCAN (User Controlled Authorization Networks) and DID (Decentralized Identifiers) infrastructure from `macula-nifs`.

### Problem Statement

Currently, any mesh participant can:
- Declare/announce procedures in any namespace (hijacking risk)
- Publish to any topic (impersonation risk)
- Subscribe to any topic (privacy risk)

### Solution

1. **Namespace ownership** - DID maps to owned namespace (`did:macula:io.macula.rgfaber` owns `io.macula.rgfaber.*`)
2. **Capability grants** - UCAN tokens delegate specific permissions to others
3. **Decentralized validation** - Every node validates locally using cryptography (no central server)

---

## Design Principles

| Principle | Implementation |
|-----------|----------------|
| **Decentralized** | No central auth server - cryptographic validation only |
| **Self-sovereign** | Identity (DID) controlled by owner's keypair |
| **Capability-based** | UCAN tokens grant specific, scoped permissions |
| **Offline-capable** | Tokens self-contained, validation needs no network |
| **Attenuated delegation** | Grants can be re-delegated with narrower scope only |

---

## Existing Infrastructure

### Available in macula-nifs (Ready to Use)

| Module | Purpose | Key Functions |
|--------|---------|---------------|
| `macula_ucan_nif` | UCAN token operations | `create/4`, `verify/2`, `decode/1`, `get_capabilities/1` |
| `macula_did_nif` | DID document operations | `create_document/2`, `parse_did/1`, `is_descendant/2` |
| `macula_crypto_nif` | Ed25519 cryptography | `generate_keypair/0`, `sign/2`, `verify/3` |

### Available in macula (Needs Integration)

| Module | Purpose | Enhancement Needed |
|--------|---------|-------------------|
| `macula_realm_trust` | Realm authentication, TOFU certs | Add UCAN/DID layer |
| `macula_rpc_handler` | RPC call entry | Add auth check at line 458 |
| `macula_pubsub_handler` | Pub/Sub entry | Add auth check at line 417 |
| `macula_gateway_rpc_router` | RPC routing | Add auth check at line 40 |
| `macula_gateway_pubsub_router` | PubSub routing | Add auth check at line 207 |

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           Mesh Message Flow                              │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  Caller                                                    Provider      │
│  ┌──────────────┐                                    ┌──────────────┐   │
│  │ DID:         │                                    │ DID:         │   │
│  │ io.macula.   │                                    │ io.macula.   │   │
│  │ rgfaber      │                                    │ rgfaber      │   │
│  └──────┬───────┘                                    └──────┬───────┘   │
│         │                                                   │           │
│         │  RPC Call: io.macula.rgfaber.get_patient_data    │           │
│         │  + Caller DID (in header)                         │           │
│         │  + UCAN token (if calling others' namespace)      │           │
│         │  + Signature (proves DID ownership)               │           │
│         │                                                   │           │
│         ▼                                                   │           │
│  ┌─────────────────────────────────────────────────────────┐│           │
│  │              macula_authorization.erl                   ││           │
│  │  ┌─────────────────────────────────────────────────┐   ││           │
│  │  │ 1. Extract caller DID from message              │   ││           │
│  │  │ 2. Parse topic/procedure namespace              │   ││           │
│  │  │ 3. Check: caller owns namespace?                │   ││           │
│  │  │    YES → Allow                                  │   ││           │
│  │  │    NO  → Check UCAN token                       │   ││           │
│  │  │          Valid UCAN with capability? → Allow    │   ││           │
│  │  │          No/Invalid UCAN? → Deny                │   ││           │
│  │  └─────────────────────────────────────────────────┘   ││           │
│  └─────────────────────────────────────────────────────────┘│           │
│         │                                                   │           │
│         ▼                                                   ▼           │
│    [Authorized]                                      [Handler invoked]  │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Namespace Ownership Model

### DID to Namespace Mapping

```
DID                              Owns Namespace
───────────────────────────────  ─────────────────────────────
did:macula:io.macula             io.macula.*  (realm root)
did:macula:io.macula.rgfaber     io.macula.rgfaber.*
did:macula:io.macula.ibm         io.macula.ibm.*
did:macula:io.macula.ibm.watson  io.macula.ibm.watson.*
```

### Hierarchical Ownership

```erlang
%% Parent can always access child namespaces
did:macula:io.macula.ibm         → can access io.macula.ibm.watson.*
did:macula:io.macula             → can access io.macula.* (everything in realm)

%% Child cannot access parent or sibling namespaces
did:macula:io.macula.ibm.watson  → CANNOT access io.macula.ibm.other.*
did:macula:io.macula.rgfaber     → CANNOT access io.macula.ibm.*
```

---

## Default Permissions (No UCAN Required)

| Operation | Own Namespace | Parent's Namespace | Others' Namespace | `*.public.*` |
|-----------|---------------|-------------------|-------------------|--------------|
| DECLARE/ANNOUNCE | ✅ | ❌ | ❌ | ❌ |
| CALL | ✅ | ✅ | ❌ needs UCAN | ✅ |
| PUBLISH | ✅ | ❌ | ❌ needs UCAN | ❌ |
| SUBSCRIBE | ✅ | ✅ | ❌ needs UCAN | ✅ |
| DISCOVER | ✅ | ✅ | ✅ (see, not access) | ✅ |

**Public topics**: Any topic containing `.public.` segment is world-readable.

---

## UCAN Capability Format

### For Mesh Operations

```erlang
%% RPC Call capability
#{<<"with">> => <<"io.macula.rgfaber.get_patient_data">>,
  <<"can">> => <<"mesh:call">>}

%% Publish capability
#{<<"with">> => <<"io.macula.rgfaber.events.*">>,
  <<"can">> => <<"mesh:publish">>}

%% Subscribe capability
#{<<"with">> => <<"io.macula.rgfaber.notifications.*">>,
  <<"can">> => <<"mesh:subscribe">>}

%% Wildcard (all operations on namespace)
#{<<"with">> => <<"io.macula.rgfaber.*">>,
  <<"can">> => <<"mesh:*">>}
```

### Example UCAN Token

```
rgfaber grants rgfabers_doctor permission to call get_patient_data:

{
  "iss": "did:macula:io.macula.rgfaber",
  "aud": "did:macula:io.macula.rgfabers_doctor",
  "cap": [
    {"with": "io.macula.rgfaber.get_patient_data", "can": "mesh:call"}
  ],
  "exp": 1738368000,  // 2025-02-01
  "prf": []           // root grant (no parent)
}
```

---

## Protocol Message Extension

### Current Message Headers

```erlang
%% Existing (macula_protocol_types.erl)
-type call_msg() :: #{
    procedure := binary(),
    args := binary(),
    call_id := binary(),
    timeout => integer()
}.
```

### Extended Message Headers

```erlang
%% New fields for authorization
-type call_msg() :: #{
    procedure := binary(),
    args := binary(),
    call_id := binary(),
    timeout => integer(),
    %% NEW: Authorization fields
    caller_did => binary(),        % e.g., <<"did:macula:io.macula.rgfaber">>
    ucan_token => binary(),        % JWT token (if accessing others' namespace)
    signature => binary()          % Ed25519 signature of message
}.
```

---

## Implementation Phases

### Phase 1: Core Authorization Module

**Create:** `src/macula_authorization.erl`

```erlang
-module(macula_authorization).

%% Core authorization checks
-export([
    check_rpc_call/4,
    check_publish/4,
    check_subscribe/3,
    check_announce/3
]).

%% Namespace operations
-export([
    extract_namespace/1,
    check_namespace_ownership/2,
    is_public_topic/1
]).

%% UCAN operations (delegates to macula_ucan_nif)
-export([
    validate_ucan_for_operation/4,
    check_capability_match/3
]).

%% DID operations (delegates to macula_did_nif)
-export([
    resolve_caller_did/1,
    verify_did_signature/3
]).
```

**Key Functions:**

```erlang
%% Main entry point for RPC authorization
check_rpc_call(CallerDID, Procedure, UcanToken, Opts) ->
    Namespace = extract_namespace(Procedure),
    case check_namespace_ownership(CallerDID, Namespace) of
        {ok, owner} ->
            {ok, authorized};
        {ok, ancestor} ->
            {ok, authorized};  % Parent can access child
        {error, not_owner} ->
            %% Check if caller has UCAN grant
            validate_ucan_for_operation(UcanToken, CallerDID, Procedure, <<"mesh:call">>)
    end.

%% Extract namespace from topic/procedure
extract_namespace(<<"io.macula.rgfaber.orders.create">>) ->
    <<"io.macula.rgfaber">>;  % First 3 segments
extract_namespace(Topic) ->
    Parts = binary:split(Topic, <<".">>, [global]),
    case length(Parts) >= 3 of
        true ->
            [A, B, C | _] = Parts,
            <<A/binary, ".", B/binary, ".", C/binary>>;
        false ->
            Topic  % Short topic = namespace itself
    end.

%% Check if DID owns namespace
check_namespace_ownership(CallerDID, Namespace) ->
    {ok, #{identity := Identity}} = macula_did_nif:parse_did(CallerDID),
    IdentityBin = list_to_binary(Identity),
    case IdentityBin =:= Namespace of
        true ->
            {ok, owner};
        false ->
            %% Check if caller is ancestor (parent owns child)
            case is_ancestor(IdentityBin, Namespace) of
                true -> {ok, ancestor};
                false -> {error, not_owner}
            end
    end.
```

**Files to create:**
- [ ] `src/macula_authorization.erl` - Core authorization logic
- [ ] `test/macula_authorization_tests.erl` - Unit tests

---

### Phase 2: Hook Integration

**Modify existing handlers to call authorization:**

#### Hook 1: RPC Call Entry (`macula_rpc_handler.erl:458`)

```erlang
%% Before (line 458)
do_call(not_found, BinaryProcedure, Args, Opts, From, State) ->
    Registry = State#state.service_registry,
    DiscoveryResult = macula_service_registry:discover_service(Registry, BinaryProcedure),
    do_call_discovery(DiscoveryResult, BinaryProcedure, Args, Opts, From, State);

%% After
do_call(not_found, BinaryProcedure, Args, Opts, From, State) ->
    CallerDID = get_caller_did(State, Opts),
    UcanToken = maps:get(ucan_token, Opts, undefined),
    case macula_authorization:check_rpc_call(CallerDID, BinaryProcedure, UcanToken, Opts) of
        {ok, authorized} ->
            Registry = State#state.service_registry,
            DiscoveryResult = macula_service_registry:discover_service(Registry, BinaryProcedure),
            do_call_discovery(DiscoveryResult, BinaryProcedure, Args, Opts, From, State);
        {error, Reason} ->
            gen_server:reply(From, {error, {unauthorized, Reason}}),
            {noreply, State}
    end;
```

#### Hook 2: Gateway RPC Router (`macula_gateway_rpc_router.erl:40`)

```erlang
%% Add authorization check before invoking handler
handle_routed_call(CallMsg, RpcRouteMsg, NodeId, RpcPid, MeshPid) ->
    #{<<"source_node_id">> := SourceNodeId} = RpcRouteMsg,
    CallerDID = extract_did_from_route(RpcRouteMsg),
    Procedure = maps:get(<<"procedure">>, CallMsg),
    UcanToken = maps:get(<<"ucan_token">>, CallMsg, undefined),

    case macula_authorization:check_rpc_call(CallerDID, Procedure, UcanToken, #{}) of
        {ok, authorized} ->
            %% Proceed with handler invocation
            invoke_handler(CallMsg, RpcRouteMsg, NodeId, RpcPid, MeshPid);
        {error, Reason} ->
            send_error_reply(SourceNodeId, {unauthorized, Reason}, RpcRouteMsg, MeshPid)
    end.
```

#### Hook 3: PubSub Publish (`macula_pubsub_handler.erl:417`)

```erlang
do_async_publish(ConnMgrPid, Topic, Data, Opts, State) ->
    CallerDID = get_caller_did(State, Opts),
    UcanToken = maps:get(ucan_token, Opts, undefined),
    case macula_authorization:check_publish(CallerDID, Topic, UcanToken, Opts) of
        {ok, authorized} ->
            %% Proceed with publish
            {MsgId, State2} = next_message_id(State),
            %% ... rest of publish logic
        {error, Reason} ->
            ?LOG_WARNING("Publish denied: ~p", [Reason]),
            {noreply, State}
    end.
```

#### Hook 4: PubSub Delivery (`macula_gateway_pubsub_router.erl:207`)

```erlang
route_to_single_subscriber(Subscriber, Topic, PubMsg, LocalNodeId, Mesh, Clients) ->
    SubscriberDID = get_subscriber_did(Subscriber),
    case macula_authorization:check_subscribe(SubscriberDID, Topic, #{}) of
        {ok, authorized} ->
            %% Proceed with delivery
            route_to_subscriber_impl(Subscriber, Topic, PubMsg, LocalNodeId, Mesh, Clients);
        {error, _Reason} ->
            %% Skip delivery to unauthorized subscriber
            ok
    end.
```

**Files to modify:**
- [ ] `src/macula_rpc_system/macula_rpc_handler.erl` - Add Hook 1
- [ ] `src/macula_gateway_system/macula_gateway_rpc_router.erl` - Add Hook 2
- [ ] `src/macula_pubsub_system/macula_pubsub_handler.erl` - Add Hook 3
- [ ] `src/macula_gateway_system/macula_gateway_pubsub_router.erl` - Add Hook 4

---

### Phase 3: Protocol Message Extension

**Extend message types to carry authorization data:**

```erlang
%% In macula_protocol_types.erl

%% Extended call message
-type call_msg_v2() :: #{
    procedure := binary(),
    args := binary(),
    call_id := binary(),
    timeout => integer(),
    %% Authorization (v0.17+)
    caller_did => binary(),
    ucan_token => binary()
}.

%% Extended publish message
-type publish_msg_v2() :: #{
    topic := binary(),
    payload := binary(),
    qos := 0 | 1 | 2,
    retain := boolean(),
    message_id := binary(),
    %% Authorization (v0.17+)
    publisher_did => binary(),
    ucan_token => binary()
}.
```

**Files to modify:**
- [ ] `src/macula_protocol_system/macula_protocol_types.erl` - Add new types
- [ ] `src/macula_protocol_system/macula_protocol_codec.erl` - Encode/decode new fields

---

### Phase 4: DID Resolution & Caching

**Add DID caching to avoid repeated parsing:**

```erlang
-module(macula_did_cache).

%% Cache DID → parsed info for performance
-export([
    get_or_parse/1,
    invalidate/1,
    clear/0
]).

%% Uses persistent_term for fast reads
get_or_parse(DID) ->
    Key = {macula_did_cache, DID},
    case persistent_term:get(Key, undefined) of
        undefined ->
            {ok, Parsed} = macula_did_nif:parse_did(DID),
            persistent_term:put(Key, Parsed),
            {ok, Parsed};
        Cached ->
            {ok, Cached}
    end.
```

**Files to create:**
- [ ] `src/macula_did_cache.erl` - DID parsing cache

---

### Phase 5: Revocation Support

**Implement revocation via mesh gossip:**

```erlang
-module(macula_ucan_revocation).

%% Revocation list management
-export([
    revoke/2,           % Revoke a UCAN by CID
    is_revoked/1,       % Check if CID is revoked
    broadcast_revocation/2,  % Gossip to mesh
    handle_revocation_gossip/1
]).

%% Local revocation cache (ETS)
%% Entries auto-expire based on original UCAN expiry
```

**Files to create:**
- [ ] `src/macula_ucan_revocation.erl` - Revocation handling

---

### Phase 6: Audit Logging

**Log all authorization decisions:**

```erlang
-module(macula_authorization_audit).

-export([
    log_authorized/3,
    log_denied/4,
    get_audit_log/1
]).

%% Telemetry integration
log_authorized(Operation, CallerDID, Resource) ->
    telemetry:execute(
        [macula, authorization, allowed],
        #{count => 1},
        #{operation => Operation, caller => CallerDID, resource => Resource}
    ).

log_denied(Operation, CallerDID, Resource, Reason) ->
    telemetry:execute(
        [macula, authorization, denied],
        #{count => 1},
        #{operation => Operation, caller => CallerDID, resource => Resource, reason => Reason}
    ).
```

**Files to create:**
- [ ] `src/macula_authorization_audit.erl` - Audit logging

---

## Files Summary

### New Files

| File | Purpose | Phase |
|------|---------|-------|
| `src/macula_authorization.erl` | Core authorization logic | 1 |
| `test/macula_authorization_tests.erl` | Unit tests | 1 |
| `src/macula_did_cache.erl` | DID parsing cache | 4 |
| `src/macula_ucan_revocation.erl` | Revocation handling | 5 |
| `src/macula_authorization_audit.erl` | Audit logging | 6 |

### Modified Files

| File | Change | Phase |
|------|--------|-------|
| `src/macula_rpc_system/macula_rpc_handler.erl` | Add Hook 1 | 2 |
| `src/macula_gateway_system/macula_gateway_rpc_router.erl` | Add Hook 2 | 2 |
| `src/macula_pubsub_system/macula_pubsub_handler.erl` | Add Hook 3 | 2 |
| `src/macula_gateway_system/macula_gateway_pubsub_router.erl` | Add Hook 4 | 2 |
| `src/macula_protocol_system/macula_protocol_types.erl` | Extended message types | 3 |
| `src/macula_protocol_system/macula_protocol_codec.erl` | Encode/decode auth fields | 3 |

---

## Success Criteria

### Phase 1 (Core Module)
- [ ] `macula_authorization.erl` compiles
- [ ] Namespace extraction works correctly
- [ ] Ownership check works for owner, ancestor, non-owner
- [ ] UCAN validation delegates to `macula_ucan_nif`
- [ ] Public topic detection works
- [ ] 20+ unit tests passing

### Phase 2 (Hook Integration)
- [ ] RPC calls to own namespace succeed without UCAN
- [ ] RPC calls to others' namespace fail without UCAN
- [ ] RPC calls to others' namespace succeed with valid UCAN
- [ ] Publish to own namespace succeeds
- [ ] Publish to others' namespace requires UCAN
- [ ] Subscribe delivery respects authorization

### Phase 3 (Protocol Extension)
- [ ] Messages carry caller_did field
- [ ] Messages carry ucan_token field (optional)
- [ ] Backward compatible (old clients still work)

### Phase 4+ (Production Hardening)
- [ ] DID cache improves performance
- [ ] Revocation propagates via mesh gossip
- [ ] Audit logs capture all decisions
- [ ] <1ms overhead for authorization checks

---

## Testing Strategy

### Unit Tests
- Namespace extraction (various topic formats)
- Ownership checking (owner, ancestor, non-owner)
- UCAN capability matching
- Public topic detection

### Integration Tests
- End-to-end RPC with authorization
- End-to-end PubSub with authorization
- Cross-realm isolation
- UCAN delegation chains

### Performance Tests
- Authorization overhead measurement
- DID cache hit rate
- UCAN validation throughput

---

## Design Decisions

### Decision 1: DID embedded in TLS certificate ✅ DECIDED

**Choice:** DID is embedded in Portal-issued TLS certificate (SAN URI field).

**Rationale:**
- Zero extra protocol messages - identity established during TLS handshake
- Cryptographically bound - can't claim DID without matching private key
- Human-readable - `did:macula:io.macula.rgfaber` not `did:key:z6Mk...`
- Portal already issues certs - just add DID field to SAN

**Certificate Structure:**
```
Certificate:
  Subject: CN=io.macula.rgfaber.my-service
  SAN: URI:did:macula:io.macula.rgfaber
  Public Key: Ed25519 (same key as DID uses)
```

**Connection Flow:**
```
1. Client connects with TLS (presents cert)
2. Server extracts DID from cert SAN URI
3. TLS verifies cert signature (proves key ownership)
4. Identity established - no extra messages
5. All subsequent operations use this identity
```

**Portal Changes Required:**
- Add SAN URI field with DID when issuing app certificates
- DID format: `did:macula:{org_identity}` (e.g., `did:macula:io.macula.rgfaber`)

---

### Decision 2: Hybrid UCAN (connection default + per-message override) ✅ DECIDED

**Choice:** Option C - Default UCAN at connection level, with per-message override capability.

**Rationale:**
- Long-lived service connections benefit from cached connection-level UCAN (low overhead)
- Multi-tenant proxies and privilege escalation need per-message flexibility
- Best of both worlds with minimal complexity

**Connection-Level Default:**
```
CONNECT:
  - DID: did:macula:io.macula.rgfabers_doctor (from cert)
  - default_ucan: <token granting io.macula.rgfaber.get_patient_data>

All subsequent calls use this UCAN unless overridden.
```

**Per-Message Override:**
```
CALL io.macula.ibm.special_api:
  - ucan_token: <different token for IBM namespace>

This call uses the override, others still use connection default.
```

**Protocol Extension:**
```erlang
%% CONNECT message (extended)
-type connect_msg() :: #{
    realm := binary(),
    node_id := binary(),
    %% Authorization (v0.17+)
    default_ucan => binary()  % Optional: default UCAN for session
}.

%% CALL message (extended)
-type call_msg() :: #{
    procedure := binary(),
    args := binary(),
    call_id := binary(),
    %% Authorization (v0.17+)
    ucan_token => binary()  % Optional: overrides connection default
}.
```

**UCAN Lifetime Guidelines:**
- Short operations (API calls): 1-24 hours
- Treatment periods (doctor access): days to weeks (e.g., 38 days)
- Long-term relationships: months (with narrow scope)
- Sensitive operations: always short-lived regardless

**Revocation becomes critical for long-lived tokens** - see Decision 4.

---

### Decision 3: No formal refresh mechanism ✅ DECIDED

**Choice:** Option A - Issue tokens for desired lifetime, rely on revocation for early termination.

**Rationale:**
- Simplest approach - no extra infrastructure
- Decentralized - no refresh server needed
- Flexible - issuer chooses appropriate lifetime at grant time
- Revocation handles edge cases (relationship ends early)

**Lifetime Strategy:**
```
Short operations (API calls)     → 1-24 hour tokens
Treatment periods (medical)      → days/weeks tokens (e.g., 38 days)
Long-term partnerships           → months/year tokens (narrow scope)
Sensitive operations             → always short, re-request as needed
```

**If token expires and access still needed:**
- Grantee requests new UCAN from issuer via normal mesh RPC
- No special "refresh" protocol - just a new grant
- Issuer decides whether to re-grant

**No refresh token complexity** - keeps the system simple and decentralized.

---

## Open Questions

1. ~~**How is caller DID established during CONNECT?**~~ → **DECIDED: Embedded in TLS cert**

2. ~~**Should UCAN be required in message or connection-level?**~~ → **DECIDED: Hybrid (Option C)**

3. ~~**How to handle UCAN refresh/renewal?**~~ → **DECIDED: No formal refresh (Option A)**

4. **Revocation gossip protocol?**
   - Use existing PubSub? Special system topic?
   - How to prevent revocation spam?

---

## Dependencies

- `macula_ucan_nif` (macula-nifs) - UCAN token operations
- `macula_did_nif` (macula-nifs) - DID document operations
- `macula_crypto_nif` (macula-nifs) - Ed25519 cryptography

Ensure `macula-nifs` is added as dependency in `rebar.config`:

```erlang
{deps, [
    {macula_nifs, {git, "https://github.com/macula-io/macula-nifs.git", {tag, "v0.1.0"}}}
]}.
```

---

## Version Target

This feature targets **macula v0.17.0** (Mesh Authorization).

Follows existing versioning:
- v0.11.x - Hybrid Trust Model (TLS, TOFU)
- v0.14.x - RPC System
- v0.15.x - PubSub System
- v0.16.x - Registry System
- **v0.17.x - Authorization System** (this plan)
