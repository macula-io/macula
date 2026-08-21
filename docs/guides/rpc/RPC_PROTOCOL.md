# Macula SDK — RPC Protocol

**The raw wire primitives underneath `macula_response` / `macula_request`.**

> **Audience:** building something the supervised wrappers don't fit —
> custom retry logic, observability, an SDK for another language. Most
> applications want the [RPC Guide](RPC_GUIDE.md) instead — it covers the
> same capability via `macula_response`/`macula_request`, with an
> addressable pid, cancel, and mesh facts already wired in.

---

## Advertising a procedure

This is the raw primitive [`macula_response` wraps](RPC_GUIDE.md#supervised-wrappers-macula_response-macula_request).

```erlang
-spec advertise(pool(), realm(), procedure(), Handler, opts()) -> ok | {error, term()}.
```

Build the procedure name via `macula_topic` — inline strings are rejected by
the client validator:

```erlang
ProcApp   = macula_topic:app_hope(Realm, Org, App, Domain, Verb, Version),
ProcOrg   = macula_topic:org_hope(Realm, Org, Domain, Verb, Version),
ProcRealm = macula_topic:realm_hope(Realm, Domain, Verb, Version),
```

Pick a tier by who owns the schema: `app_hope` for an app-specific procedure,
`org_hope` for one an org exposes to itself, `realm_hope` for a realm
authority's own procedure (e.g. `check_health`). See the
[Topic Naming Guide](../shared/TOPIC_NAMING_GUIDE.md) for the full naming convention —
present-tense verbs, no CRUD, IDs in the payload never the name.

`advertise/5` fans out to every link in the pool and registers the handler
for replay on reconnect. `Opts` takes `auth`: `open` (default — serve any
identified caller) or `{ucan_required, Issuer}` (gated — see
[Authorization Guide](../shared/AUTHORIZATION_GUIDE.md)).

```erlang
ok = macula:unadvertise(Pool, Realm, Procedure).
```

Full raw example:

```erlang
Procedure = macula_topic:app_hope(Realm, Org, App, <<"math">>, <<"add">>, 1),

ok = macula:advertise(Pool, Realm, Procedure,
                      fun(#{<<"a">> := A, <<"b">> := B}) -> {ok, A + B} end,
                      #{}),

{ok, 5} = macula:call(Pool, Realm, Procedure, #{<<"a">> => 2, <<"b">> => 3}, 5_000),

ok = macula:unadvertise(Pool, Realm, Procedure).
```

### The handler contract

A handler is `fun((term()) -> term())` or `{Module, Function}`, called as
`Handler(Payload)`. What it returns decides what the caller sees:

| Handler returns | Caller's `call/5` / `call_station/6` sees |
|---|---|
| `{ok, Value}` | `{ok, Value}` — the `{ok, _}` wrapper is stripped and reapplied, so this is the idiomatic Erlang shape |
| any other `Value` | `{ok, Value}` — passed through as-is |
| `{error, Reason}` | `{error, Detail}` — `Detail` is `Reason` verbatim if it was already a binary, otherwise a `~0p` rendering |
| *(crash)* | `{error, {call_error, 16#02, temporary_relay_failure}}` — the crash is logged on the provider's side; the caller sees a generic, retryable code, not the crash reason |

Keep handlers fast — there's no async-reply mechanism for unary RPC; a slow
handler blocks the caller until it returns or the timeout fires. For
multi-chunk or long-lived work, use the [Streaming Guide](../streaming/STREAMING_GUIDE.md)
instead.

---

## Direct-dial: `call_station/6,7`

This is the raw primitive [`macula_request`/`macula_response`'s own
direct-dial wraps](RPC_GUIDE.md#direct-dial-start_link_direct-advertise_direct).

```erlang
-spec call_station(pool(), seed(), realm(), procedure(), term(), timeout_ms()) ->
    {ok, term()} | {error, term()}.
-spec call_station(pool(), seed(), realm(), procedure(), term(), timeout_ms(), opts()) ->
    {ok, term()} | {error, term()}.
%% opts: #{ucan_token => Token,
%%         verify => webpki | none,     %% TLS trust for a fresh dial
%%         expected_node_id => Pubkey,  %% pin app-layer identity to this key
%%         pin_tls_cert => boolean()}   %% also pin the TLS cert itself (default true)
```

`call_station/6` dials a specific station URL directly — reusing an existing
link or opening and monitoring a new one, waiting for the handshake, then
calling through it. One hop, no dependency on your pool's own seed set. Use
it when you already know *which* station's URL to dial.

**Most applications don't need this.** Knowing a procedure's URL up front is
the exception — normally you know the *procedure*, not which station serves
it. [`macula_request:start_link_direct/6,7,8` and
`macula_response:advertise_direct/6,7`](RPC_GUIDE.md#direct-dial-start_link_direct-advertise_direct)
do the resolve, verify, and dial for you, with the right trust model already
wired in.

### What resolution does, if you need it raw

Building something outside the supervised wrappers (custom retry logic,
observability, an SDK for another language)? This is the sequence
`macula_request:start_link_direct` runs internally:

1. Find every `procedure_advertisement` for `Procedure` in the DHT, and keep
   only the ones whose signature verifies — an unsigned or badly-signed
   record is never trusted, however plausible its `serving_station` claim
   looks.
2. Read the first trusted advertisement's `serving_station`, then resolve
   *that* station's own `station_endpoint` record — verified, and its
   signer checked to be exactly the station it claims (not just anyone).
3. Dial the resolved `quic://[Host]:Port` (note the brackets — required for
   the IPv6 hosts most stations advertise) with the TLS certificate itself
   **unpinned** (`pin_tls_cert => false`): a production station's TLS is
   terminated by an unrelated PKI (Let's Encrypt), so pinning the cert's
   own key can never succeed there. Trust instead rests on the
   application-layer CONNECT/HELLO handshake, which independently,
   cryptographically proves the peer holds the private key for the exact
   pubkey step 2 resolved — real trust, just enforced above the TLS layer
   rather than at it.

```erlang
{ok, Records} = macula:find_records(Pool, macula_record:procedure_key(Procedure)),
[Advertisement | _] = [R || R <- Records, {ok, _} =:= macula_record:verify(R)],
#{serving_station := Station} = macula_record:read_procedure_advertisement(Advertisement),

{ok, EndpointRecord} = macula:find_record(Pool, macula_record:station_endpoint_key(Station)),
#{key := Station} = EndpointRecord,          %% signer must be the station itself
{ok, _} = macula_record:verify(EndpointRecord),
#{quic_port := Port, host_advertised := [Host | _]} =
    macula_record:read_station_endpoint(EndpointRecord),
StationUrl = <<"quic://[", Host/binary, "]:", (integer_to_binary(Port))/binary>>,

{ok, Result} = macula:call_station(Pool, StationUrl, Realm, Procedure, Payload, 5_000,
                                   #{expected_node_id => Station,
                                     pin_tls_cert => false, verify => none}).
```

A fourth, **opt-in** check exists for managed realms: pass
`verify_cert_chain => {RealmCaPem, Org}` to
`macula_request:start_link_direct/8` (or `cert_chain => ChainPem` to
`macula_response:advertise_direct/7` on the provider side) to additionally
require the advertisement's embedded X.509 service-cert chain to verify to
the realm CA — proving the *advertiser*, not just the station it names, is
an org/realm-authorized identity. Unmanaged realms have no realm CA to check
against, so this stays opt-in rather than mandatory.

This is the same resolve shape used by [content](../content/CONTENT_PROTOCOL.md)'s
`get_content_station/4,5` and [streaming](../streaming/STREAMING_PROTOCOL.md)'s
raw `call_stream_station/6` — one mechanism, reused across every primitive
pair.

---

## Errors

```erlang
case macula:call(Pool, Realm, Procedure, Payload, Timeout) of
    {ok, Result} ->
        Result;
    {error, timeout} ->
        retry_later;
    {error, {disconnected, Reason}} ->
        %% the link went down mid-call; pending calls on it all fail this way
        retry_later;
    {error, {call_error, Code, Name}} ->
        %% wire-level BOLT#4 error -- see macula_bolt4:is_retryable/1
        maybe_retry(Code, Name);
    {error, Detail} ->
        %% the handler itself returned {error, Detail}
        logger:warning("RPC refused: ~p", [Detail])
end.
```

`{error, no_healthy_station}` (from `call/5`) or `{error, not_connected}`
(from `call_station/6`) means no link has completed its handshake yet — the
pool hasn't connected, or the direct-dial target hasn't finished handshaking
within the deadline.

Wire-level errors carry a BOLT#4 code; `macula_bolt4:is_retryable/1` tells
you whether the *same* path is worth retrying after backoff, or whether you
need a fresh resolve:

| Code | Name | Retry |
|---|---|---|
| `0x01` | `unknown_next_peer` | different path |
| `0x02` | `temporary_relay_failure` | same path, after backoff |
| `0x03` | `relay_disabled` | different path |
| `0x04` | `node_not_found_at_target_relay` | re-resolve and recompute |
| `0x08` | `upstream_congestion` | exponential backoff |
| `0x0A` | `crypto_puzzle_invalid` | drop — do not retry |
| `0x0E` | `signature_invalid` | drop — do not retry |
| `0x0F` | `unknown_error` | a handler's own `{error, Reason}` — see the handler contract above |
| `0x10` | `unauthorized` | application concern — present a valid UCAN, don't retry as-is |

The full table, including codes not relevant to RPC, is in `macula_bolt4`'s
own moduledoc.

---

## See also

- [RPC_GUIDE.md](RPC_GUIDE.md) — the supervised wrappers most applications
  should use instead of these raw primitives.
