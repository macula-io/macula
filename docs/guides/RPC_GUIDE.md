# Macula SDK — RPC Guide

**Request/response over the mesh: advertise a handler, call a procedure, get a result.**

![Direct-Dial RPC across Two Stations](assets/rpc_two_stations.svg)

> **Audience:** applications that need a request/response call to a specific
> procedure, as opposed to a broadcast ([PubSub Guide](PUBSUB_GUIDE.md)) or an
> open-ended feed ([Streaming Guide](STREAMING_GUIDE.md)).

---

## Overview

A provider advertises a procedure with `macula:advertise/5`. A consumer calls
it with `macula:call/5` — the simple, recommended default: it tries each of
*your own pool's* connected stations in turn, and whichever one answers is
responsible for finding a handler, locally or by forwarding to a peer.

```erlang
Procedure = macula_topic:app_hope(Realm, Org, App, <<"math">>, <<"add">>, 1),

ok = macula:advertise(Pool, Realm, Procedure,
                      fun(#{<<"a">> := A, <<"b">> := B}) -> {ok, A + B} end,
                      #{}),

{ok, 5} = macula:call(Pool, Realm, Procedure, #{<<"a">> => 2, <<"b">> => 3}, 5_000),

ok = macula:unadvertise(Pool, Realm, Procedure).
```

`call/5` doesn't resolve or choose a specific provider — it's "ask any of my
stations to handle this." For **direct-dial** — resolving a specific
provider's station in the DHT and dialing it in one hop, bypassing your own
pool's seeds entirely — use `call_station/6,7`, below.

---

## Advertising a procedure

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
[Topic Naming Guide](TOPIC_NAMING_GUIDE.md) for the full naming convention —
present-tense verbs, no CRUD, IDs in the payload never the name.

`advertise/5` fans out to every link in the pool and registers the handler
for replay on reconnect. `Opts` takes `auth`: `open` (default — serve any
identified caller) or `{ucan_required, Issuer}` (gated — see
[Authorization Guide](AUTHORIZATION_GUIDE.md)).

```erlang
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
multi-chunk or long-lived work, use the [Streaming Guide](STREAMING_GUIDE.md)
instead.

---

## Direct-dial: `call_station/6,7`

```erlang
-spec call_station(pool(), seed(), realm(), procedure(), term(), timeout_ms()) ->
    {ok, term()} | {error, term()}.
-spec call_station(pool(), seed(), realm(), procedure(), term(), timeout_ms(), opts()) ->
    {ok, term()} | {error, term()}.  %% opts: #{ucan_token => Token}
```

`call_station/6` dials a specific station URL directly — reusing an existing
link or opening and monitoring a new one, waiting for the handshake, then
calling through it. One hop, no dependency on your pool's own seed set. Use
it once you've resolved *which* station serves the procedure:

```erlang
%% 1. Resolve the procedure_advertisement from the DHT.
Key = macula_record:procedure_key(Procedure),
{ok, Records} = macula:find_records(Pool, Key),

%% 2. Read it, optionally verifying the provider's cert chain to the realm
%%    CA first -- drops advertisements from a squatter who doesn't chain.
[Advertisement | _] = Records,
#{serving_station := StationNodeId} = macula_record:read_procedure_advertisement(Advertisement),
ok = macula_record:verify_advertisement_cert_chain(RealmCaPem, Advertisement, ExpectedOrg),

%% 3. Resolve that station's dialable endpoint.
EndpointKey = macula_record:station_endpoint_key(StationNodeId),
{ok, EndpointRecord} = macula:find_record(Pool, EndpointKey),
#{quic_port := Port, host_advertised := [Host | _]} =
    macula_record:read_station_endpoint(EndpointRecord),
StationUrl = <<"quic://", Host/binary, ":", (integer_to_binary(Port))/binary>>,

%% 4. Dial it directly.
{ok, Result} = macula:call_station(Pool, StationUrl, Realm, Procedure, Payload, 5_000).
```

This is the same resolve done for [content](CONTENT_GUIDE.md)'s
`find_content_providers/2` and [streaming](STREAMING_GUIDE.md)'s
`call_stream_station/6` — one shape, three call sites. A squatter's
advertisement — signed, but whose cert doesn't chain to the realm CA — is
dropped by step 2 and never dialed.

`call_station/7`'s `Opts` map takes `ucan_token` to present a capability
token to a gated (`{ucan_required, _}`) procedure.

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

## Supervised wrappers: `macula_response` / `macula_request`

`advertise/5`'s handler runs in a transient process spawned per inbound
call, and `call/5` blocks the calling process on its own `gen_server:call`
— neither has an addressable pid you can supervise, monitor, or cancel from
outside. `macula_response` and `macula_request` wrap the same two
primitives as proper OTP behaviours, and publish `rpc.received_v1` /
`rpc.replied_v1` (provider) or `rpc.sent_v1` / `rpc.completed_v1` (consumer)
mesh facts around each call — useful when something else on the mesh wants
to observe RPC traffic, not just participate in it.

Provider side — each inbound call starts one supervised child under a
factory supervisor this module owns:

```erlang
-module(math_service).
-behaviour(macula_response).
-export([init/1, handle_request/2]).

init(_Args) -> {ok, []}.

handle_request(#{<<"a">> := A, <<"b">> := B}, State) ->
    {reply, A + B, State}.
```

```erlang
{ok, _Sup} = macula_response:advertise(Pool, Realm, Procedure,
                                       math_service, []).
```

Consumer side — `start_link/6,7` returns immediately with a pid; the call
itself runs in a linked worker, and the outcome is delivered to
`Module:handle_reply/2`:

```erlang
-module(add_caller).
-behaviour(macula_request).
-export([init/1, handle_reply/2]).

init(Parent) -> {ok, Parent}.

handle_reply(Result, Parent) ->
    Parent ! {add_result, Result},
    {stop, normal, Parent}.
```

```erlang
{ok, Pid} = macula_request:start_link(add_caller, Pool, Realm, Procedure,
                                      #{<<"a">> => 2, <<"b">> => 3},
                                      5_000, self()).

%% cancel before a reply arrives — publishes rpc.completed_v1 with
%% outcome => cancelled
ok = macula_request:cancel(Pid).
```

Embed `macula_request_sup` (a `simple_one_for_one` factory) in your own
supervision tree if you want to enumerate or cancel in-flight requests via
`supervisor:which_children/1` / `terminate_child/2` — that is what backs a
`cancel_*` RPC command in an application built on top of the SDK.

---

## Procedure naming

**See the [Topic Naming Guide](TOPIC_NAMING_GUIDE.md)** — RPC procedures and
pub/sub topics share the same canonical format, built via `macula_topic`,
never inline strings.

---

## See also

- [Streaming Guide](STREAMING_GUIDE.md) — when one request/response isn't
  enough: a live feed, an upload, a duplex session.
- [Authorization Guide](AUTHORIZATION_GUIDE.md) — gating a procedure with
  `{ucan_required, Issuer}` and presenting a UCAN token to call it.
- [Records Guide](RECORDS_GUIDE.md) — the DHT record primitive
  `procedure_advertisement` is built on.
- [`macula_response`](https://hexdocs.pm/macula/macula_response.html) /
  [`macula_request`](https://hexdocs.pm/macula/macula_request.html) —
  supervised, fact-announcing wrappers around `advertise/5` and `call/5`.
