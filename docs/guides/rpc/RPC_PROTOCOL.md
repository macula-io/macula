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

| Handler returns | Caller's `call/5` / `call_station/7` sees |
|---|---|
| `{ok, Value}` | `{ok, Value}` — the `{ok, _}` wrapper is stripped and reapplied, so this is the idiomatic Erlang shape |
| any other `Value` | `{ok, Value}` — passed through as-is |
| `{error, Reason}` | `{error, Detail}`: `Detail` is `Reason` itself when it is a binary or a printable charlist, as at most 256 bytes of UTF-8, and otherwise its name, such as `<<"refused">>` for `{refused, Why}`. A reason with neither gives `{error, {call_error, 16#0F, unknown_error}}`. None of a reason's terms leave the provider |
| *(crash)* | `{error, {call_error, 16#02, temporary_relay_failure}}` — the crash is logged on the provider's side; the caller sees a generic, retryable code, not the crash reason |

Text a handler returns in `{error, Text}` is sent to its caller, up to 256 bytes of it.
A handler that answers with an upstream error body or a database message sends those bytes,
so return only text meant for the caller.

Keep handlers fast — there's no async-reply mechanism for unary RPC; a slow
handler blocks the caller until it returns or the timeout fires. For
multi-chunk or long-lived work, use the [Streaming Guide](../streaming/STREAMING_GUIDE.md)
instead.

---

## Direct-dial: `call_station/7,8`

This is the raw primitive [`macula_request`/`macula_response`'s own
direct-dial wraps](RPC_GUIDE.md#direct-dial-start_link_direct-advertise_direct).

```erlang
-spec call_station(pool(), seed(), node_id(), realm(), procedure(), term(), timeout_ms()) ->
    {ok, term()} | {error, term()}.
-spec call_station(pool(), seed(), node_id(), realm(), procedure(), term(), timeout_ms(), opts()) ->
    {ok, term()} | {error, term()}.
%% opts: #{ucan_token => Token,
%%         verify => webpki | none,     %% TLS trust for a fresh dial
%%         expected_node_id => NodeId,  %% the station's node_id
%%         pin_tls_cert => boolean()}   %% also pin the TLS cert itself (default true)
```

`call_station/7` dials a specific station URL directly, reusing an existing
link or opening and monitoring a new one, waiting for the handshake, then
calling through it. The third argument is the target: the node_id of the
provider, from its verified `procedure_advertisement`. The station delivers
the CALL to that provider's connection, and only a reply signed by that
provider completes the call. One hop, no dependency on your pool's own seed
set. Use it when you already know *which* station's URL to dial and which
provider behind it to call.

A pool bounds the links it dials this way. A call to a station that is not
already a link is refused, before anything is dialed, with
`{error, too_many_direct_links}` while the pool holds `max_direct_links` of
them (default 8), and with `{error, new_peer_budget_spent}` once it has
linked to `new_peer_budget` new peers in the last 15 minutes (default 16).
Your configured seeds never count against either. Try another station you
already hold a link to, or retry later. A station URL or map with no text
host, or no port from 1 to 65535, is refused with `{error, unusable_seed}`.
Every refusal is counted in `macula_client:status/1` under `refused_dials`.

**Most applications don't need this.** Knowing a procedure's URL up front is
the exception — normally you know the *procedure*, not which station serves
it. [`macula_request:start_link_direct/6,7,8` and
`macula_response:advertise_direct/6,7`](RPC_GUIDE.md#direct-dial-start_link_direct-advertise_direct)
do the resolve, verify, and dial for you, with the right trust model already
wired in.

### What resolution does, if you need it raw

Building something outside the supervised wrappers (custom retry logic,
observability, an SDK for another language)? This is the sequence
`macula:call/5` and `macula_request:start_link_direct` run internally:

1. Find every `procedure_advertisement` for `Procedure` in `Realm` in the
   DHT. `macula:find_records/2` returns only the records whose signature
   verifies under the node's crypto profile. Keep the ones that advertise
   exactly this procedure in this realm and whose provider authorization
   verifies (`macula_record:verify_authorization/3`): a procedure with an
   org namespace needs an authorization for that org, and a procedure
   without one carries none.
2. Take the provider from a trusted advertisement: the node_id that signed
   it, its `key_id`. Read its `serving_station`, then resolve *that*
   station's own `station_endpoint` record, verified, and its signer
   checked to be exactly the station it claims (not just anyone).
3. Dial the resolved `quic://[Host]:Port` (note the brackets — required for
   the IPv6 hosts most stations advertise) with the TLS certificate itself
   **unpinned** (`pin_tls_cert => false`): a production station's TLS is
   terminated by an unrelated PKI (Let's Encrypt), so pinning the cert's
   own key can never succeed there. Trust instead rests on the
   application-layer CONNECT/HELLO handshake, which proves the peer holds
   the identity key of the exact node_id step 2 resolved.
4. Call the provider through that station, with its node_id as the target.
   The station delivers the CALL to that provider's connection, and only a
   reply the provider signed completes the call.

```erlang
{ok, Profile} = macula_crypto_profile:configured(),
{ok, Records} = macula:find_records(Pool, macula_record:procedure_key(Realm, Procedure)),
Trusted = fun(#{type := Type} = Record) ->
              Type =:= macula_record:type_procedure_advertisement() andalso
                  maps:with([realm_id, procedure],
                            macula_record:read_procedure_advertisement(Record))
                      =:= #{realm_id => Realm, procedure => Procedure} andalso
                  ok =:= macula_record:verify_authorization(
                             Record, #{profile => Profile}, erlang:system_time(millisecond))
          end,
[#{key_id := Provider} = Advertisement | _] = lists:filter(Trusted, Records),
#{serving_station := Station} = macula_record:read_procedure_advertisement(Advertisement),

{ok, EndpointRecord} = macula:find_record(Pool, macula_record:station_endpoint_key(Station)),
#{key_id := Station} = EndpointRecord,          %% signer must be the station itself
#{quic_port := Port, host_advertised := [Host | _]} =
    macula_record:read_station_endpoint(EndpointRecord),
StationUrl = <<"quic://[", Host/binary, "]:", (integer_to_binary(Port))/binary>>,

{ok, Result} = macula:call_station(Pool, StationUrl, Provider, Realm, Procedure, Payload,
                                   5_000, #{expected_node_id => Station,
                                            pin_tls_cert => false, verify => none}).
```

For an org namespaced procedure, add the realm trust you hold to the map
`verify_authorization/3` takes: `realm_key`, the realm key as carried, for the
org directory and the procedure delegation, the only authorization form. An
authorization in any other form, a certificate chain included, is refused as
`authorization_form_unsupported`. Without the realm key, an advertisement for
an org namespaced procedure is never trusted. The supervised wrappers take
the same key as `realm_trust => #{realm_key => RealmKey}` in their options
(`macula_request:start_link_direct/8`). A provider publishes its
authorization with `macula_response:advertise_direct/7`'s `authorization`
option, as `#{org_directory => Wire, procedure_delegation => Wire}`. The 10.x
options `verify_cert_chain` and
`cert_chain` are refused with `{error, {removed_option, Key}}` before anything
is looked up or published: `realm_trust` and `authorization` replace them.

This is the same resolve shape used by [content](../content/CONTENT_PROTOCOL.md)'s
`get_content_station/4,5` and [streaming](../streaming/STREAMING_PROTOCOL.md)'s
raw `call_stream_station/7`: one mechanism, reused across every primitive
pair.

---

## Errors

```erlang
case macula:call(Pool, Realm, Procedure, Payload, TimeoutMs) of
    {ok, Result} ->
        Result;
    {error, timeout} ->
        retry_later;
    {error, {disconnected, _Reason}} ->
        %% the link went down mid-call; pending calls on it all fail this way
        retry_later;
    {error, {unresolved, _Reason}} ->
        %% no trusted advertisement named a provider to call
        retry_later;
    {error, {call_error, unknown_next_peer, undefined}} ->
        %% the station holds no connection to the provider
        retry_later;
    {error, {call_error, Code, Detail}} ->
        %% the provider refused the call with its own code
        logger:warning("RPC refused: ~p ~p", [Code, Detail]);
    {error, Detail} ->
        %% the handler returned {error, Detail}, or the call failed as listed below
        logger:warning("RPC failed: ~p", [Detail])
end.
```

`{error, {unresolved, Reason}}` means resolution found no provider to call:
`procedure_not_advertised`, no advertisement that passed its trust checks,
or `no_healthy_station` when the pool had no connected station to look the
advertisement up through. Once a candidate station has been tried, a failure
before the CALL went out moves on to the next candidate, and the last such
failure is the result. `{error, not_connected}` means no link to the station
completed its handshake within the deadline. `{error, {refused, Reason}}`
means the payload was refused before anything was sent, because no frame
can carry it.

A reply completes a call only if it verifies against the request. A result
or a provider's error must be signed by the provider the call targets, and a
station's error by the station the link is connected to. Any other reply is
counted and dropped, and the call waits for its timeout.

A provider's error code is a binary. The code `handler_error` is the
handler's own `{error, Reason}` and arrives as `{error, Detail}`. Any other
code arrives as `{error, {call_error, Code, Detail}}`, where `Detail` is a
binary, or `undefined` when the provider sent none. The station's one error,
`{error, {call_error, unknown_next_peer, undefined}}`, means it holds no
connection to the provider. The advertisement may be stale, so resolve again
before retrying.

---

## See also

- [RPC_GUIDE.md](RPC_GUIDE.md) — the supervised wrappers most applications
  should use instead of these raw primitives.
