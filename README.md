# Macula SDK

[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![BEAM](https://img.shields.io/badge/beam-28%2B-A90533?logo=erlang&logoColor=white)](https://www.erlang.org)
[![Hex.pm](https://img.shields.io/hexpm/v/macula.svg)](https://hex.pm/packages/macula)
[![GitHub Sponsors](https://img.shields.io/badge/GitHub%20Sponsors-support-ea4aaa.svg?logo=githubsponsors&logoColor=white)](https://github.com/sponsors/rgfaber)

<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="assets/macula-full-dark.svg">
    <img src="assets/macula-full-light.svg" alt="Macula" width="320">
  </picture>
</p>

<p align="center">
  <strong>Erlang/OTP client SDK for the Macula HTTP/3 mesh</strong>
</p>

---

> **12.0.0: post-quantum key exchange AND post-quantum signatures.**
> Every QUIC link negotiates
> `SecP384r1MLKEM1024`, then `SecP256r1MLKEM768`, and nothing classical,
> from the [`macula-pqc`](https://crates.io/crates/macula-pqc) crate. Every
> signature is ML-DSA-87 on
> [`macula-mldsa`](https://crates.io/crates/macula-mldsa): node keys, UCAN
> tokens, and the self-signed certificate a listener presents, which a dial
> verifies and nothing classical can replace. A station dial is bound end to
> end: the station's identity key signs a binding over its TLS key, the
> client checks it against the certificate that handshake received, and the
> CONNECT proof covers the same certificate.
>
> ⚠ **Erlang distribution over QUIC is the exception**: those dials run no
> connection handshake yet, so they verify that the peer holds its
> certificate's key and nothing about who it is.
>
> **Breaking on the wire:** a node on 11.5.0 or earlier cannot connect to
> this version, in either direction. See [CHANGELOG.md](CHANGELOG.md).

> **Since 10.5.0**: every supervised primitive pair is complete and
> symmetric, each wrapping its raw SDK primitive as an OTP behaviour with a
> `simple_one_for_one` factory supervisor, mesh-visible protocol facts
> (`sharing.*_v1`, `streaming.*_v1`, `rpc.*_v1`) around its own side of the
> operation, and both a pooled and a **direct-dial** (resolve + one-hop
> dial) mode:
> - **RPC** — `macula_request`/`macula_response`, unary call/reply.
> - **Pub/Sub** — `macula_publisher`/`macula_subscriber`, publish and
>   per-publisher-ordered subscribe.
> - **Content sharing** — `macula_feeder`/`macula_download` over
>   `macula:share_content/3,4` and `get_content/3,4`: the node that shares
>   content keeps and serves it (stations only relay), and a fetcher
>   verifies every chunk against the content id it asked for.
> - **Streaming RPC** — `macula_streamer`/`macula_stream_sink`, server /
>   client / bidi modes, with an optional `client_stream` receive loop and
>   terminal-reply callback, and abort-wired cancel.
> - **Push-initiated content transfer** — `macula_pusher`/`macula_upload`
>   push a file at a specific, already-known recipient (rather than sharing
>   it for anyone to fetch by its content id), with
>   the same chunk/hash/verify integrity guarantees, over `client_stream`.
> - **Overlay (HyParView + Plumtree)** — realm-scoped bounded partial
>   views and epidemic broadcast trees, absorbed from the standalone
>   `macula-hyparview`/`macula-plumtree` packages. No supervised wrapper yet
>   — see the [HyParView](docs/guides/overlay/HYPARVIEW_GUIDE.md) and
>   [Plumtree](docs/guides/overlay/PLUMTREE_GUIDE.md) guides.
>
> See [CHANGELOG.md](CHANGELOG.md) for the full version-by-version history.

## What is Macula?

<p align="center">
  <img src="assets/sdk_architecture.svg" alt="Macula SDK Component and Feature Model" width="100%">
</p>

Macula is an **Erlang/OTP client SDK** for building applications on a mesh of
**stations** — realm-agnostic relays that route over QUIC (HTTP/3) and form a
Kademlia DHT. Your service or daemon connects **outbound** to one or more
stations: no open ports, NAT-friendly, no VPN. It provides:

- **RPC (request/response)** — discover a provider in the DHT, then **dial its
  serving station directly** (one hop), with the provider's authorization
  checked against the realm-signed org directory.
- **Pub/Sub** — topic-based event fan-out across stations, with per-publisher
  ordered delivery.
- **Content** — content-addressed sharing and live streaming (MCID).
- **DHT records** — signed, TTL'd records (advertisements, endpoints, more).
- **Erlang distribution over mesh** — `net_adm:ping` across firewalls, no VPN.
- **Identity** — ML-DSA-87 node keys (with an RSA-PSS half under `pq_hybrid`), and UCAN tokens they sign.
- **MRI** — typed, hierarchical resource identifiers.
- **Zero-config LAN clustering** — UDP-multicast gossip.

The station (routing, DHT, SWIM, peering) is a separate repo,
[macula-station](https://github.com/macula-io/macula-station); this package is
the client you build against.

---

## Quick Start

Add to `rebar.config`:

```erlang
{deps, [{macula, "~> 12.0"}]}.
```

Or in Elixir `mix.exs`:

```elixir
defp deps do
  [{:macula, "~> 12.0"}]
end
```

12.0.0 breaks on the wire: a node on 11.5.0 or earlier cannot connect to
it, in either direction, and there is no classical fallback for either key
exchange or authentication. Upgrade every node together.

<p align="center">
  <img src="assets/connect_flow.svg" alt="SDK Connect Flow" width="100%">
</p>

```erlang
%% Every node runs one post-quantum crypto profile, pq_pure
%% or pq_hybrid. The application refuses to start without one.
ok = application:set_env(macula, crypto_profile, pq_pure),
application:ensure_all_started(macula),

%% Connect a pool to one or more stations (seed URLs). The pool owns one
%% QUIC link per seed, reconnecting and replaying subscriptions as needed.
{ok, Pool} = macula:connect([<<"quic://boot.macula.io:443">>], #{}),

%% A realm is a 32-byte tag derived from a name; it scopes every call.
%% Keep the name around too — topics are built from it, not the tag.
RealmName = <<"io.example.myapp">>,
Realm     = macula_realm:id(RealmName),

%% Topics/procedures are built via macula_topic, never hand-typed — a
%% typo becomes a wrong VALUE your own tests catch, not two strings
%% silently drifting apart. Facts (pub/sub) are past tense; hopes (RPC)
%% are present tense. See docs/guides/shared/TOPIC_NAMING_GUIDE.md.
Topic     = macula_topic:app_fact(RealmName, <<"example">>, <<"myapp">>,
                                  <<"sensors">>, <<"temperature_measured">>, 1),
Procedure = macula_topic:app_hope(RealmName, <<"example">>, <<"myapp">>,
                                  <<"math">>, <<"add">>, 1),

%% Subscribe (delivers {macula_event, Ref, Topic, Payload, Meta} to a pid),
{ok, Ref} = macula:subscribe(Pool, Realm, Topic, self()),

%% or subscribe with a callback fun(Topic, Payload, Meta):
{ok, Ref2} = macula:subscribe_callback(
    Pool, Realm, Topic,
    fun(_Topic, Payload, _Meta) -> io:format("~p~n", [Payload]) end),

%% Publish. Entity IDs go in the PAYLOAD, never in the topic.
ok = macula:publish(Pool, Realm, Topic,
                    #{sensor => <<"kitchen">>, value => 23.5}),

%% Advertise an RPC procedure (open to any identified caller here),
ok = macula:advertise(Pool, Realm, Procedure,
                      fun(#{<<"a">> := A, <<"b">> := B}) -> {ok, A + B} end,
                      #{}),

%% Call it — the SDK resolves the provider and dials its station directly.
{ok, 5} = macula:call(Pool, Realm, Procedure,
                      #{<<"a">> => 2, <<"b">> => 3}, 5_000).
```

---

## Identity and Crypto (NIF-accelerated)

<p align="center">
  <img src="assets/identity_crypto.svg" alt="Identity and Crypto Stack" width="100%">
</p>

A node holds one key per purpose in its crypto profile. In `pq_pure` a key
is ML-DSA-87; in `pq_hybrid` an identity key pairs ML-DSA-87 with RSA-PSS and
signs the IETF LAMPS composite `id-MLDSA87-RSA4096-PSS-SHA512`. ML-DSA is
[`macula-mldsa`](https://crates.io/crates/macula-mldsa), verified against
NIST's ACVP vectors, in a Rust NIF with no Erlang fallback, and new keys are
stored as their 32-byte seed. The node_id is SHA-256 over the identity key.

```erlang
{ok, Key}    = macula_node_keys:generate(identity, pq_pure),
{ok, NodeId} = macula_node_keys:node_id(Key),
Sig  = macula_node_keys:sign(<<"hello">>, Key),
true = macula_node_keys:verify(<<"hello">>, Sig, macula_node_keys:public_key(Key), pq_pure),
ok   = macula_node_keys:save("identity.key", Key),

%% BLAKE3 hashing
Hash = macula_blake3_nif:hash(<<"hello">>).
```

UCAN capability tokens (`macula_ucan`) are signed by node keys too, with
the profile's `alg`: `ML-DSA-87` in `pq_pure` and `ML-DSA-87-PS384`, the
LAMPS composite, in `pq_hybrid`. A token names its issuer by `did:key` and
its audience by node_id, and an EdDSA token is refused (see the
[Authorization guide](docs/guides/shared/AUTHORIZATION_GUIDE.md)).

---

## Documentation

| Guide | Description |
|-------|-------------|
| [Connecting](docs/guides/shared/CONNECTING_GUIDE.md) | Pools, seeds, expected identities, reconnection |
| [PubSub Guide](docs/guides/pubsub/PUBSUB_GUIDE.md) | Fan-out + per-publisher delivery ordering |
| [PubSub Protocol](docs/guides/pubsub/PUBSUB_PROTOCOL.md) | Raw `subscribe`/`publish` primitives |
| [Topic Naming](docs/guides/shared/TOPIC_NAMING_GUIDE.md) | Event-type topics, IDs in payloads |
| [RPC Guide](docs/guides/rpc/RPC_GUIDE.md) | Direct-dial request/response |
| [RPC Protocol](docs/guides/rpc/RPC_PROTOCOL.md) | Raw `advertise`/`call` primitives, error codes |
| [Content Guide](docs/guides/content/CONTENT_GUIDE.md) | Content-addressed blobs (MCID), push/upload |
| [Content Protocol](docs/guides/content/CONTENT_PROTOCOL.md) | The content id, the announcement, the content procedure, the fetch and its bounds |
| [Records Guide](docs/guides/shared/RECORDS_GUIDE.md) | Signed, TTL'd facts in the DHT — your own record types |
| [Streaming Guide](docs/guides/streaming/STREAMING_GUIDE.md) | Streaming RPC (server / client / bidi) |
| [Streaming Protocol](docs/guides/streaming/STREAMING_PROTOCOL.md) | Raw `call_stream`/`advertise_stream` primitives |
| [HyParView Guide](docs/guides/overlay/HYPARVIEW_GUIDE.md) | Bounded partial-view realm membership |
| [Plumtree Guide](docs/guides/overlay/PLUMTREE_GUIDE.md) | Epidemic broadcast trees, realm PubSub, OR-Set CRDT |
| [Distribution Over Mesh](docs/guides/DIST_OVER_MESH_GUIDE.md) | Erlang dist through the mesh |
| [Clustering](docs/guides/CLUSTERING_GUIDE.md) | LAN gossip clustering |
| [Authorization](docs/guides/shared/AUTHORIZATION_GUIDE.md) | Node keys, UCAN, provider authorization |
| [MRI Guide](docs/guides/shared/MRI_GUIDE.md) | Resource identifiers |
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
