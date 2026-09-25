# Macula SDK — RPC Guide

**Request/response over the mesh: advertise a handler, call a procedure, get a result.**

![Direct-Dial RPC across Two Stations](assets/rpc_two_stations.svg)

> **Audience:** applications that need a request/response call to a specific
> procedure, as opposed to a broadcast ([PubSub Guide](../pubsub/PUBSUB_GUIDE.md)) or an
> open-ended feed ([Streaming Guide](../streaming/STREAMING_GUIDE.md)). Building
> something the wrapper below doesn't fit (custom retry logic, observability,
> an SDK for another language)? See [RPC_PROTOCOL.md](RPC_PROTOCOL.md) for the
> raw primitives this guide is built on.

---

## Overview

A provider **advertises** a procedure with a handler; a consumer **calls** it
by name and gets a result back — `macula_response` and `macula_request`, an
addressable pid you can monitor and cancel, with `rpc.*_v1` mesh facts
around every call.

Calling means resolving the procedure's provider and calling it in one hop.
The provider's signed `procedure_advertisement` in the DHT names its node_id
and the station it is connected to, and the call goes to that provider
through that station, bypassing your own pool's seeds entirely. A provider
is callable once its advertisement is published.
`macula_request:start_link_direct/6,7,8`, below, also takes options for that
resolution, such as the realm trust an org's procedures are checked against.

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

### Direct-dial: `start_link_direct` / `advertise_direct`

The counterparts to `start_link/6,7` and `advertise/5,6` above, with the
same callback modules and the same behaviour. `start_link/6,7` already
resolves and dials the provider's station directly, through `macula:call/5`;
`start_link_direct` also takes options for that resolution.
`advertise_direct` publishes the advertisement a caller resolves. See
[RPC_PROTOCOL.md](RPC_PROTOCOL.md) for the full trust-model writeup
(signature verification, station-endpoint resolution, why the TLS cert
itself stays unpinned).

Provider — `advertise_direct/6,7` does everything `advertise/5,6` does, and
additionally publishes a signed `procedure_advertisement` naming this pool's
connected station as the server, so a direct-dial consumer can find it:

```erlang
%% NodeIdentity must be the node identity key this pool was started with: a
%% caller targets the node_id that signed the advertisement, and the station
%% knows this pool's connection by that node_id.
{ok, _Sup} = macula_response:advertise_direct(Pool, Realm, Procedure,
                                              math_service, [], NodeIdentity).
```

A provider linked to several stations registers at every one of them unless
it names some: `#{stations => [StationNodeId]}` in the options registers the
procedure only on the links to those stations, and the direct-dial record then
names the first of them the pool is connected to. A station the pool has no
link to is refused as `{error, {station_not_linked, StationNodeId}}`, and
nothing is registered.

```erlang
{ok, _Sup} = macula_response:advertise_direct(Pool, Realm, Procedure,
                                              math_service, [], NodeIdentity,
                                              #{stations => [StationNodeId]}).
```

Consumer — `start_link_direct/6,7,8` resolves the advertisement, resolves
and verifies the serving station's endpoint, and dials it in one hop:

```erlang
{ok, Pid} = macula_request:start_link_direct(add_caller, Pool, Realm, Procedure,
                                             #{<<"a">> => 2, <<"b">> => 3},
                                             5_000, self()).
```

Resolve failures are distinguishable from call failures:
`{error, {unresolved, Reason}}` means nobody has advertised the procedure via
direct-dial yet (or the DHT record hasn't replicated to your station), not
that the call itself failed. Requires the provider to have advertised via
`advertise_direct/6,7`, not plain `advertise/5,6` — a plain advertise
publishes no discoverable record.

For a procedure with an org namespace, resolution also checks the provider's
authorization, the org directory and the procedure delegation, against the
realm key your pool pinned for the realm when it started, from
`macula:connect/2`'s `realm_trust => #{RealmId => RealmKey}`. The provider publishes its
authorization with `macula_response:advertise_direct/7`'s `authorization`
option. 11.0.0 has no certificate form. Without the realm key, an
advertisement for an org namespaced procedure is never trusted. The 10.x
options `verify_cert_chain` and
`cert_chain`, and `realm_trust` on a call, are refused with
`{error, {removed_option, Key}}`. See
[RPC_PROTOCOL.md](RPC_PROTOCOL.md#what-resolution-does-if-you-need-it-raw).

---

## Errors

`handle_reply/2`'s `Result` is the same `{ok, Value} | {error, Reason}` shape
a raw `call/5` returns — the wrapper doesn't change the outcome, only how you
receive it:

```erlang
handle_reply({ok, Value}, Parent) ->
    Parent ! {add_result, Value},
    {stop, normal, Parent};
handle_reply({error, {call_error, Code, Detail}}, Parent) ->
    %% refused with a code: see RPC_PROTOCOL.md's Errors section
    logger:warning("RPC refused: ~p ~p", [Code, Detail]),
    {stop, normal, Parent};
handle_reply({error, Detail}, Parent) ->
    %% the handler returned {error, Detail}, or the call failed
    logger:warning("RPC failed: ~p", [Detail]),
    {stop, normal, Parent}.
```

`{error, Detail}` is the handler answering with an error. `Detail` is the
handler's own text when it returned a binary or a printable charlist, up to
256 bytes of it, and otherwise the name of its reason, such as
`<<"refused">>`. A handler's error text is sent to its caller as it is, so a
provider returns only text meant for the caller.

`{error, {call_error, Code, Detail}}` is a refusal with a code. It is either
the provider's own code, a binary, or `unknown_next_peer` from the station
when it holds no connection to the provider. See
[RPC_PROTOCOL.md](RPC_PROTOCOL.md#errors) for every error a call returns.

---

## Procedure naming

**See the [Topic Naming Guide](../shared/TOPIC_NAMING_GUIDE.md)** — RPC procedures and
pub/sub topics share the same canonical format, built via `macula_topic`,
never inline strings.

---

## See also

- [RPC_PROTOCOL.md](RPC_PROTOCOL.md) — the raw primitives underneath, every
  error a call returns, direct-dial trust-model internals.
- [Streaming Guide](../streaming/STREAMING_GUIDE.md) — when one request/response isn't
  enough: a live feed, an upload, a duplex session.
- [Authorization Guide](../shared/AUTHORIZATION_GUIDE.md) — gating a procedure with
  `{ucan_required, Issuer}` and calling it with a UCAN token minted for the
  calling identity.
- [Records Guide](../shared/RECORDS_GUIDE.md) — the DHT record primitive
  `procedure_advertisement` is built on.
- [`macula_response`](https://hexdocs.pm/macula/macula_response.html) /
  [`macula_request`](https://hexdocs.pm/macula/macula_request.html) —
  supervised, fact-announcing wrappers around `advertise/5` and `call/5`.
