# Macula SDK - RPC Guide

**Request/response over the mesh: DHT discovery, then a direct dial.**

![Direct-Dial RPC across Two Stations](assets/rpc_two_stations.svg)

---

## Overview

Macula provides decentralized RPC (remote procedure call). A provider advertises
a procedure as a signed `procedure_advertisement` in the Kademlia DHT, naming the
station it is reachable through (`serving_station`). A caller resolves that record
`O(log N)` in the DHT and then **dials the serving station directly** for the
call — one hop, point-to-point over QUIC, not relayed across the mesh.

From the SDK perspective: advertise a handler, call a procedure, get a result.
`macula:call/5` composes the whole resolve → dial → call path for you;
`macula:call_station/6` dials a station URL you already know.

> **Note on the model.** Earlier Macula relayed every CALL through the mesh
> ("nodes never connect directly"). That was a stopgap; discovery was always
> meant to resolve a record and then dial the provider's station directly. The
> direct-dial path is the current default. See
> [macula-station `DESIGN_DIRECT_DIAL_DISCOVERY.md`](https://github.com/macula-io/macula-station).

---

## Advertising a Procedure

Build the procedure name via `macula_topic` — inline strings are rejected by the client validator.

```erlang
%% Advertise an app-tier procedure
Procedure = macula_topic:app_hope(Realm, Org, App,
                                  <<"math">>, <<"add">>, 1),
{ok, Ref} = macula:advertise(Client, Procedure, fun(#{a := A, b := B}) ->
    {ok, #{result => A + B}}
end).

%% Advertise a realm-tier procedure (e.g. realm authority's check_health)
HealthProc = macula_topic:realm_hope(Realm, <<"auth">>, <<"check_health">>, 1),
{ok, Ref} = macula:advertise(Client, HealthProc, fun(_args) ->
    {ok, #{status => <<"ok">>}}
end, #{
    description => <<"Realm health check">>
}).

%% Stop advertising
ok = macula:unadvertise(Client, Ref).
```

### What Happens Under the Hood

1. SDK sends `register_procedure` message to connected relay
2. Relay registers the handler in gproc (local lookup)
3. Relay stores procedure in Kademlia DHT (cross-relay discovery)
4. Other relays can now find this procedure via DHT

---

## Calling a Procedure

```erlang
%% Synchronous call with default timeout (5s)
Procedure = macula_topic:app_hope(Realm, Org, App, <<"math">>, <<"add">>, 1),
{ok, #{result := 5}} = macula:call(Client, Procedure, #{a => 2, b => 3}).

%% Call with explicit timeout
WeatherProc = macula_topic:app_hope(Realm, Org, App,
                                    <<"weather">>, <<"get_current">>, 1),
{ok, Weather} = macula:call(Client, WeatherProc,
    #{location => <<"berlin">>}, 10000).  %% 10 second timeout
```

### Call Flow

```
Your Node                    Your Relay              Remote Relay
─────────                    ──────────              ────────────
call(math.add, Args)
  └─► CALL message ──────► local gproc lookup
                              ├─ found locally? ──► invoke handler ──► REPLY
                              └─ not found?
                                  └─► DHT find_procedure
                                        └─► found on remote relay
                                             └─► forward via peering ──► invoke ──► REPLY
                                                                                      │
  ◄── REPLY ◄──────────────────────────────────────────────────────────────────────────┘
```

---

## Error Handling

```erlang
case macula:call(Client, Procedure, Args, Timeout) of
    {ok, Result} ->
        %% Success
        Result;
    {error, no_handler} ->
        %% No node has registered this procedure (local)
        not_available;
    {error, procedure_not_found} ->
        %% No node found via DHT either
        not_available;
    {error, timeout} ->
        %% Handler didn't respond in time
        retry_later;
    {error, cross_relay_error} ->
        %% Remote relay forwarding failed
        retry_later;
    {error, Reason} ->
        logger:warning("RPC failed: ~p", [Reason])
end.
```

---

## Procedure Naming Conventions

**See [TOPIC_NAMING_GUIDE.md](TOPIC_NAMING_GUIDE.md) — RPC procedures and pub/sub topics share the same canonical 5-segment format.**

Quick summary:
- Procedure names are mesh topics with present-tense names: `{realm}/{publisher}/{publisher}/{domain}/{verb_subject}_v{N}`
- Pick a tier: `realm_hope` (realm authority), `org_hope` (org-spanning), `app_hope` (app-specific)
- Build via `macula_topic` — never inline strings
- IDs in args, never in procedure name
- No CRUD verbs

Examples:
- App: `io.macula/beam-campus/hecate/math/add_v1`
- Realm: `io.macula/_realm/_realm/auth/check_health_v1`

---

## Best Practices

1. **Return `{ok, Result}` from handlers** -- consistent with Erlang conventions
2. **Keep handlers fast** -- spawn workers for heavy computation, reply later
3. **Set appropriate timeouts** -- default 5s is often too short for external APIs
4. **Handle all error cases** -- `no_handler`, `timeout`, `cross_relay_error`
5. **Idempotent procedures** -- calls may be retried on timeout
6. **One procedure per handler** -- each advertise registers one procedure name

---

## Provider Selection

When multiple nodes advertise the same procedure, the relay selects a provider:

- **Local first** -- handlers on the same relay are preferred (zero network hop)
- **DHT discovery** -- if not local, relay queries Kademlia DHT for remote providers
- **Peer fallback** -- if DHT miss, relay tries each peered relay sequentially

---

## How It Works (Relay Side)

For relay operators and contributors: RPC routing, DHT procedure store,
cross-relay call forwarding, and provider selection are documented in
macula-station.
