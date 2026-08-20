# Macula SDK

[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-28-brightgreen)](https://www.erlang.org)
[![Hex.pm](https://img.shields.io/hexpm/v/macula.svg)](https://hex.pm/packages/macula)
[![Buy Me A Coffee](https://img.shields.io/badge/Buy%20Me%20A%20Coffee-support-yellow.svg)](https://buymeacoffee.com/rlefever)

<p align="center">
  <img src="assets/logo.svg" width="120" height="120" alt="Macula">
</p>

<p align="center">
  <strong>Erlang/OTP client SDK for the Macula HTTP/3 mesh</strong>
</p>

---

> **Latest — 9.1.0**: OTP 29 readiness (bare `catch` rewritten to
> `try/catch/end`; `macula_record`'s `record()` type renamed `m_record()` —
> a reserved built-in type name in OTP 29) plus a new
> [Records Guide](docs/guides/RECORDS_GUIDE.md) for the raw DHT record API.
> No behavior change. **9.0.0 was breaking**: LAN clustering and
> distribution-over-mesh split into independent concerns —
> `macula_cluster_system/` vs. `macula_dist_system/`. `macula_dist_relay` is
> renamed `macula_dist_pool` (the facade, `join_mesh/1` / `join_dist_relay/1`,
> is unaffected); the `auto_cluster` sys.config option is removed (start
> clustering explicitly via `macula_cluster:start_cluster/1`). See
> [CHANGELOG.md](CHANGELOG.md) for the full history.

## What is Macula?

<p align="center">
  <img src="assets/sdk_architecture.svg" alt="Macula SDK Component and Feature Model" width="100%">
</p>

Macula is an **Erlang/OTP client SDK** for building applications on a mesh of
**stations** — realm-agnostic relays that route over QUIC (HTTP/3) and form a
Kademlia DHT. Your service or daemon connects **outbound** to one or more
stations: no open ports, NAT-friendly, no VPN. It provides:

- **RPC (request/response)** — discover a provider in the DHT, then **dial its
  serving station directly** (one hop), with optional realm-CA trust verification.
- **Pub/Sub** — topic-based event fan-out across stations, with per-publisher
  ordered delivery.
- **Content** — content-addressed sharing and live streaming (MCID).
- **DHT records** — signed, TTL'd records (advertisements, endpoints, more).
- **Erlang distribution over mesh** — `net_adm:ping` across firewalls, no VPN.
- **Identity** — Ed25519 keypairs, UCAN tokens, DID documents (NIF-accelerated).
- **MRI** — typed, hierarchical resource identifiers.
- **Zero-config LAN clustering** — UDP-multicast gossip.

The station (routing, DHT, SWIM, peering) is a separate repo,
[macula-station](https://github.com/macula-io/macula-station); this package is
the client you build against.

---

## Quick Start

Add to `rebar.config`:

```erlang
{deps, [{macula, "~> 9.1"}]}.
```

Or in Elixir `mix.exs`:

```elixir
defp deps do
  [{:macula, "~> 9.1"}]
end
```

<p align="center">
  <img src="assets/connect_flow.svg" alt="SDK Connect Flow" width="100%">
</p>

```erlang
application:ensure_all_started(macula),

%% Connect a pool to one or more stations (seed URLs). The pool owns one
%% QUIC link per seed, reconnecting and replaying subscriptions as needed.
{ok, Pool} = macula:connect([<<"quic://boot.macula.io:443">>], #{}),

%% A realm is a 32-byte tag derived from a name; it scopes every call.
Realm = macula_realm:id(<<"io.example.myapp">>),

%% Subscribe (delivers {macula_event, Ref, Topic, Payload, Meta} to a pid),
{ok, Ref} = macula:subscribe(Pool, Realm, <<"sensors.temperature">>, self()),

%% or subscribe with a callback fun(Topic, Payload, Meta):
{ok, Ref2} = macula:subscribe_callback(
    Pool, Realm, <<"sensors.temperature">>,
    fun(_Topic, Payload, _Meta) -> io:format("~p~n", [Payload]) end),

%% Publish. Entity IDs go in the PAYLOAD, never in the topic.
ok = macula:publish(Pool, Realm, <<"sensors.temperature">>,
                    #{sensor => <<"kitchen">>, value => 23.5}),

%% Advertise an RPC procedure (open to any identified caller here),
ok = macula:advertise(Pool, Realm, <<"math.add">>,
                      fun(#{<<"a">> := A, <<"b">> := B}) -> {ok, A + B} end,
                      #{}),

%% Call it — the SDK resolves the provider and dials its station directly.
{ok, 5} = macula:call(Pool, Realm, <<"math.add">>,
                      #{<<"a">> => 2, <<"b">> => 3}, 5_000).
```

---

## The Four Interaction Patterns

Macula gives you four ways for two parties to interact over the mesh. The
point-to-point ones — RPC, content, and streaming — share one shape:
**resolve in the DHT, then dial the serving station directly** (one hop). RPC
and streaming resolve+dial directly by default (`call_station`,
`call_stream_station`); content sharing reaches a copy via the connected
station's own relay by default and can dial a specific announced host directly
(`find_content_providers` + `call_station`) when that is not enough. Pub/Sub is
the deliberate exception: it fans out *through* the stations, because
broadcasting to many interested parties is a different problem than a two-party
exchange.

### 1. RPC — direct-dial

<p align="center">
  <img src="assets/rpc_two_stations.svg" alt="Direct-Dial RPC across Two Stations" width="100%">
</p>

A provider publishes a signed `procedure_advertisement` naming its serving
station. A consumer resolves it over the DHT, optionally verifies the provider's
realm-issued cert chains to the realm CA (dropping squatters), resolves the
serving station's endpoint, and **dials it directly** for the call. Discovery is
`O(log N)` DHT lookups; the call itself is one hop.

```erlang
%% High-level: the SDK composes resolve -> dial -> call for you.
{ok, Result} = macula:call(Pool, Realm, <<"math.add">>, Payload, 5_000),

%% Low-level: dial a known station URL yourself.
{ok, Result} = macula:call_station(Pool, <<"quic://station-b:443">>,
                                   Realm, <<"math.add">>, Payload, 5_000).
```

### 2. Pub/Sub — fan-out with per-publisher ordering

<p align="center">
  <img src="assets/pubsub_two_stations.svg" alt="PubSub across Two Stations" width="100%">
</p>

Subscription interest gossips across stations; a publish fans out along the
Plumtree eager tree, with lazy IHAVE/GRAFT gossip repairing a missed message.
One subscriber holds one connection and receives from every publisher, wherever
they are. Topics name **event types** — `energy.home.measured`, not
`energy.home.42.measured` — so IDs live in the payload and topics never explode.

Since 8.8, a single publisher's stream is delivered **in order per publisher**.
Pick the contract at subscribe time:

```erlang
%% ordered (default): per-publisher FIFO; a missing seq skipped after a timeout.
{ok, R1} = macula:subscribe(Pool, Realm, Topic, self()),

%% latest_only: newest-wins, drop stale, no head-of-line delay (state snapshots).
{ok, R2} = macula:subscribe(Pool, Realm, Topic, self(), #{delivery => latest_only}),

%% as_arrives: raw arrival order; the consumer orders it itself.
{ok, R3} = macula:subscribe(Pool, Realm, Topic, self(), #{delivery => as_arrives}).
```

See the [PubSub Guide](docs/guides/PUBSUB_GUIDE.md) for the ordering model and
tuning (`order_timeout_ms`, `order_max_buffer`, the `pubsub_gap_skips` telemetry).

### 3. Content Sharing — content-addressed

<p align="center">
  <img src="assets/content_sharing.svg" alt="Content Sharing (MCID)" width="100%">
</p>

Content is addressed by an **MCID** — a hash of the bytes — so any host with the
bytes serves the same MCID and integrity is self-verifying. Content that fits
in one 256 KiB block round-trips as a single block (unchanged since v4.2.7);
larger content is split into chunks and a Merkle-verified manifest,
transparently. Chunked content is announced automatically, so a consumer can
resolve every host serving an MCID and dial a specific one directly. See the
[Content Guide](docs/guides/CONTENT_GUIDE.md).

```erlang
{ok, MCID}  = macula:put_content(Pool, Bytes),        %% any size
{ok, Bytes} = macula:get_content(Pool, MCID),
{ok, Hosts} = macula:find_content_providers(Pool, MCID).
```

### 4. Content Streaming — live QUIC stream

<p align="center">
  <img src="assets/content_streaming.svg" alt="Content Streaming" width="100%">
</p>

Same resolve-then-dial shape, but instead of a finite blob it opens an ordered
QUIC stream: frames flow as produced, paced by QUIC per-stream flow control,
ending when the source stops. Re-resolve on a stall — a listed source can be
dead. Server-push, client-push, and bidirectional modes are supported; see the
[Streaming Guide](docs/guides/STREAMING_GUIDE.md).

```erlang
%% resolve the source (find_records -> read_procedure_advertisement ->
%% station_endpoint, as in RPC), then dial its station directly
{ok, Stream} = macula:call_stream_station(Pool, StationUrl, Realm,
                                          <<"live.feed">>, Request, #{}),
loop_recv(Stream).  %% macula:recv/1 until eof
```

---

## Erlang Distribution Over Mesh

<p align="center">
  <img src="assets/dist_over_mesh.svg" alt="Erlang Distribution Over Relay Mesh" width="100%">
</p>

Full OTP distribution tunneled through the mesh. No VPNs, no open ports.

```erlang
macula:join_mesh(#{
    realm  => <<"io.macula">>,
    relays => [<<"quic://boot.macula.io:443">>],
    site   => #{name => <<"my-site">>, lat => 51.5, lng => -0.1}
}),
net_adm:ping('other@remote-host').  %% => pong
```

## LAN Clustering

Zero-configuration cluster formation over UDP multicast:

```erlang
ok = macula_cluster:start_cluster(#{strategy => gossip,
                                    secret => <<"my_cluster_secret">>}).
```

---

## Identity and Crypto (NIF-accelerated)

<p align="center">
  <img src="assets/identity_crypto.svg" alt="Identity and Crypto Stack" width="100%">
</p>

Rust NIFs with pure-Erlang fallbacks:

```erlang
%% Ed25519 keypair (a #{public := _, private := _} map)
KP  = macula_identity:generate(),
Sig = macula_identity:sign(<<"hello">>, KP),
true = macula_identity:verify(<<"hello">>, Sig, macula_identity:public(KP)),

%% BLAKE3 hashing
Hash = macula_blake3_nif:hash(Data),

%% UCAN capability tokens + DID documents
{ok, Token}   = macula_ucan_nif:create(Issuer, Audience, Caps, PrivKey),
{ok, Payload} = macula_ucan_nif:verify(Token, PubKey).
```

## MRI (Resource Identifiers)

<p align="center">
  <img src="assets/mri_trie_index.svg" alt="MRI Trie Index" width="100%">
</p>

Typed, hierarchical resource addressing: `mri:{type}:{realm}/{path}`

```erlang
{ok, Parsed} = macula_mri:parse(<<"mri:app:io.macula/acme/counter">>),
MRI = macula_mri:new_app(<<"io.macula">>, <<"acme">>, <<"counter">>),

%% Trie index for million-scale hierarchy queries
{ok, Idx}      = macula_mri:build_index(MRIs),
{ok, Children} = macula_mri:index_children(Idx, <<"io.macula">>, [<<"acme">>]).
```

---

## Documentation

| Guide | Description |
|-------|-------------|
| [Connecting](docs/guides/CONNECTING_GUIDE.md) | Pools, seeds, TLS policy, reconnection |
| [PubSub Guide](docs/guides/PUBSUB_GUIDE.md) | Fan-out + per-publisher delivery ordering |
| [Topic Naming](docs/guides/TOPIC_NAMING_GUIDE.md) | Event-type topics, IDs in payloads |
| [RPC Guide](docs/guides/RPC_GUIDE.md) | Direct-dial request/response |
| [Content Guide](docs/guides/CONTENT_GUIDE.md) | Content-addressed blobs (MCID) |
| [Records Guide](docs/guides/RECORDS_GUIDE.md) | Signed, TTL'd facts in the DHT — your own record types |
| [Streaming Guide](docs/guides/STREAMING_GUIDE.md) | Streaming RPC (server / client / bidi) |
| [Distribution Over Mesh](docs/guides/DIST_OVER_MESH_GUIDE.md) | Erlang dist through the mesh |
| [Clustering](docs/guides/CLUSTERING_GUIDE.md) | LAN gossip clustering |
| [Authorization](docs/guides/AUTHORIZATION_GUIDE.md) | DID / UCAN / cert-chain trust |
| [MRI Guide](docs/guides/MRI_GUIDE.md) | Resource identifiers |
| [Development](docs/guides/DEVELOPMENT.md) | Building and testing |
| [Glossary](docs/GLOSSARY.md) | Terminology |

The station server lives in
[macula-station](https://github.com/macula-io/macula-station).

---

## Related Projects

| Project | Description |
|---------|-------------|
| [macula-station](https://github.com/macula-io/macula-station) | The station: DHT, SWIM, routing, peering |
| [macula-realm](https://github.com/macula-io/macula-realm) | Managed-realm identity + certificate authority |
| [macula-mri-khepri](https://github.com/macula-io/macula-mri-khepri) | Distributed MRI persistence (Khepri/Raft) |
| [macula-ecosystem](https://github.com/macula-io/macula-ecosystem) | Documentation hub |

---

## License

Apache 2.0 — see [LICENSE](LICENSE).

---

<p align="center">
  <sub>Built with the BEAM</sub>
</p>
