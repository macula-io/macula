# DESIGN: Sealed by default

**This exists so a call or stream between Macula nodes is sealed end to end unless its operator explicitly opts out,
so a relaying station never sees the payload.**

| | |
|---|---|
| Kind | BUILD that changes a security default, with its threat model first (macula#135). It makes no new public claim: the claim "stations relay payloads they cannot read" waits for the fleet measurement (§8, D11) |
| Extends | `DESIGN_E2E_PAYLOAD_CONFIDENTIALITY.md` (§8.1, §8.2, Amendment A1), `DESIGN_E2E_SEALED_PUBSUB.md`, `DESIGN_E2E_SEAL_REPORT.md` |
| Replaces | that design's decision §12.2 (`preferred` as the default) and A1's `kem_advertise` switch |
| Decided | Raf, 2026-10-27: **required** by default; clear only by explicit opt-out; no backward-compatibility mode |
| Written against | macula 17.1.1 (432aff2f), macula-go v0.28.0 (b82930b), macula-rust v0.12.0 (e22027f), macula-ts v0.31.0, macula-py v0.6.0, macula-dotnet v0.9.0-rc.5, macula-php v0.9.0-rc.6 (the four bindings pin libmacula from macula-go v0.27.0) |

---

## 1. Where we are

The sealing mechanism is built and cross-tested in every SDK. Only the defaults keep it off. Seven SDKs come down to
**three implementations**: Erlang `macula`, `macula-go` (whose C ABI, libmacula, the TypeScript, Python, .NET and PHP
bindings wrap) and `macula-rust` (with its own UniFFI crate). A binding decides nothing about sealing itself, except
the defaults it writes into its own parameters.

| What decides | Erlang macula | macula-go / libmacula | macula-rust | Bindings |
|---|---|---|---|---|
| Provider names a KEM key | app env `kem_advertise`, default `disabled` (`macula.erl:1324`) | `pool.Opts.KEMAdvertise`, false (`pool/pool.go:109`) | `pool::Opts.kem_advertise`, false (`pool.rs:220`) | every binding writes 0/false itself (ts `pool.ts:233`, py `pool.py:483`, .NET `Pool.cs:37`, PHP `Pool.php:36`) |
| Advertise mode | `off \| preferred \| required`, default `preferred` (`macula.erl:1309`) | same, zero value `Preferred` (`stationlink/confidential.go:23`) | same, `#[default] Preferred` (`station_link/confidential.rs:38`) | inherited; PHP writes `Preferred` itself (`Pool.php`, serve and serveStream) |
| Call and stream mode | `preferred \| required` (+ `off` on explicit targets), default `preferred` (`macula.erl:550`, `macula_direct_dial.erl:285`) | `Preferred \| Required`, `Off` refused (`pool/call.go:51,122`) | same (`pool/call.rs:52,782`) | inherited; PHP writes `Preferred` itself (`Pool.php:116,143`) |
| Keyless advertisement under the default | called in the clear (`sealed_by/2`) | called in the clear (`call.go:320`) | called in the clear (`call.rs:123`) | inherited |
| Start-up clear window | `keyed_since` + 5 min + 5 min = **10 min** (`macula_station_link.erl:3556`) | `clearAllowed`, 10 min (`confidential.go:85`) | `clear_allowed`, 10 min (`confidential.rs:369`) | inherited |
| Explicit target, no option | refused `no_signed_state` (`macula.erl:553`) | refused `no_signed_state` (`confidential.go:216`) | refused `NoSignedState`, but a link-level `Seal::Clear` to a keyed provider is not checked (`confidential.rs:66`) | not exposed |
| D27 content | fetch `confidential => off` (`macula_content_fetch.erl:319`); **serve inherits the default** (`macula_content_sharer.erl`, `advertise_stream`) | serve `ConfidentialOff`, fetch `Clear` (`pool/content.go:231,560`) | fetch `Seal::Clear`; **serve inherits the default** (`pool/content.rs:257`) | inherited |
| Pubsub, no group named | clear; refused `group_held` under a held prefix | same (`pubsub.go:249`) | same (`pubsub.rs:128`) | inherited |
| A refused clear caller | provider ERROR `sealed_required`, **no log, no count** | same, no log or metric | same, no log (no log crate) | `ProviderError` code `sealed_required` |

The KEM switch was there for a station floor: every station had to accept `sealed` and the keyed advertisement
first (A1). That floor is behind us since the wire 1.0 flag day. All six stations run it.

## 2. Threat model

This design keeps T1 to T5 of `DESIGN_E2E_PAYLOAD_CONFIDENTIALITY.md` §2 and their mechanisms unchanged. It changes
**for whom** T1 is defeated: today only toward providers whose operator turned two switches; afterwards for every
provider whose operator did not turn one off.

**Added, and defeated by this design:**

- **T6, clear by omission.** A developer forgets an option, so the payload is relayed in the clear and nothing tells
  anyone. Today that is the default path in every SDK, and five of the fleet's providers seal. Defeated: with no
  option, a provider names its key and refuses clear, and a caller refuses a provider that names none (§3).
- **T7, the start-up downgrade.** For ten minutes after a provider starts naming its key, it still answers clear calls.
  Inside that window a station can serve a caller the provider's previous keyless advertisement: it is still within
  its lifetime and validly signed, so a `preferred` caller sends in the clear and the provider answers. **Defeated
  for `required` callers only**, which refuse a keyless advertisement whatever its age. Removing the window (§4) stops
  the provider from answering a clear request; it cannot stop a `preferred` caller from sending one, and the relaying
  station reads that request's payload before the provider refuses it (below, "Still not protected").
- **T8, a silent opt-out.** An operator turns sealing off and nobody on the path notices. Not defeated, but
  **made visible**: an opt-out must be a written option on both ends (§3), is logged when it takes effect, and is
  counted (§7).

**Still not protected**, stated so no text claims otherwise (D11). §9 of the base design lists the full set. These
rows matter to this change:

- **A provider that opted out.** Its payloads are clear to every station, by its operator's decision.
- **A `preferred` caller's request after a provider flips from `off` to `required`.** For the provider's last keyless
  advertisement's lifetime plus the clock tolerance (5 + 5 minutes), a `preferred` caller holding it, or served it by
  a station, sends its request in the clear. The provider refuses it, but the stations on the path have read it. The
  caller's operator accepted that by choosing `preferred` (§3).
- **Open pubsub topics.** An open topic has no recipient key, and SUBSCRIBE is unchecked: anyone may subscribe (§5).
- **D27 content.** It is public to every station through its announcement (§6).
- **A station's own control procedures** (`find_record`, `put_record`, `find_records`, `find_records_by_type`,
  `_dht.budget`, `_relay.ping`). The station is their endpoint over a direct post-quantum link, not a relay. The
  records they move are public by design.
- **A request's `token` and `proofs`**, which stay clear beside `sealed` in scheme 1. That is unchanged.

## 3. Point 1: the default, and the shape of the opt-out

**Provider (advertise, serve, serve stream).**

- The `kem_advertise` switch is **deleted** in every SDK, and so is every binding's `kemAdvertise` parameter. A
  provider always holds a KEM keyring.
- Advertise modes become **`required` (the default) and `off`**:
  - `required` names the key and refuses every clear CALL and STREAM_OPEN, from the first advertisement on.
  - `off` names no key and serves clear only: the opt-out.
  - **`preferred` is deleted on the advertise side.** With no clear window, a keyed provider that still takes clear
    calls has no caller that needs it: every caller seals to a named key.
- `kem_advertise_disabled` disappears with the switch.

**Caller (call, stream, explicit target).**

- Call modes become **`required` (the default) and `preferred`** (the opt-out). The caller default ships one
  release after the provider default (§8, release B):
  - `required` seals to the key the target's verified advertisement names. A keyless advertisement fails the call
    with the new reason `provider_serves_clear` (§7), which is distinct from `no_kem_key` (no usable key, or none
    found).
  - `preferred` seals to a named key and calls a keyless provider in the clear. That is today's default, now an
    explicit choice. The name stays, so no SDK renames an enum value that its interop vectors carry.
- **An explicit target always needs signed state.** Erlang's `confidential => off` on `call_station` and
  `call_stream_station` is deleted, and so is Rust's unchecked link-level `Seal::Clear` toward a non-station target.
  Under either mode an explicit target seals from, or is called clear because of, an advertisement that the target
  signed (passed in, or resolved). `no_signed_state` remains for none found.
- After a `sealed_required` refusal, the caller re-resolves once, as A1 does for `sealed_refused`. If the fresh
  advertisement names a key, it seals under a new request_id; otherwise the call fails. This only ever upgrades.

**Clear needs both ends.** A provider's `off` is reached in the clear only by callers that chose `preferred`. So every
clear exchange is two written decisions, one per operator, and neither end can push the other into clear. A default
caller of an opted-out provider gets a refusal naming the fix (§7), not a silent downgrade. The provider's opt-out
does not decide the caller's confidentiality for it.

Considered and not taken: a default caller that follows the provider (seal when keyed, clear when keyless). Under
universal keying a keyless advertisement would be an authentic opt-out, since a station cannot strip a signed key.
But that makes the provider's operator the only one deciding whether the caller's payload is relayed in the clear,
which is T8 for the caller. And a caller following the provider follows a stale keyless advertisement too, so after
every `off` to `required` flip it would leak requests by default (§4), not only by opt-out.

## 4. Point 2: the start-up clear window is removed

- Deleted: Erlang `keyed_since` (`macula_client:keyed_since/2`, the `keyed_since` spec field, the `clear_by_spec/1`
  clause), Go `Offer.KeyedSince` and `clearAllowed`'s window, Rust `keyed_since_ms` and `clear_allowed`'s window. What
  remains is one rule: a keyed provider never answers a clear CALL or STREAM_OPEN.
- **Flipping a provider from `off` to `required`.** Its last keyless advertisement lives up to 5 min plus the 5 min
  clock tolerance at the callers that hold it.
  - A `required` caller already refused the keyless advertisement: nothing of it goes out in the clear.
  - A `preferred` caller holding that advertisement sends its request in the clear. The provider answers
    `sealed_required`, the caller re-resolves (§3) and seals. **That one clear request per caller is read by the
    stations on its path** (§2, "Still not protected"); no provider answer is ever clear.
- **Flipping from `required` to `off`.** Sealed calls get `sealed_refused` ("opens no sealed payload"). A `required`
  caller fails, which is correct. A `preferred` caller re-resolves, finds the keyless advertisement and calls clear,
  as both operators chose.
- A restart under `required` loses the in-memory keyring (A1). Callers get `sealed_refused` naming the new key and
  reseal, as today. Nothing falls to clear.

## 5. Point 3: what "on by default" can mean for publish/subscribe

**What it cannot mean.** An open topic has no recipient to seal to. A publisher does not know its subscribers, and
SUBSCRIBE is unsigned and unchecked, so whoever subscribes receives the event. Sealing an event to "everyone" protects
nothing. **Only a group can be sealed.** A group is an org's topic prefix with a distributor
(`DESIGN_E2E_SEALED_PUBSUB.md`).

**What it means here:**

1. **Publishing and subscribing state their confidentiality, with no default.** `publish` and `subscribe` take either
   `group => Prefix` (sealed, as today) or `open => 1` (clear, the opt-out). A publish or subscribe naming neither is
   refused `{confidentiality, no_group}` before anything is sent. That error names both options.
2. **A distributor's seal policy defaults to `required`.** In Erlang it is a mandatory start option today
   (`macula_group_keys:start_link/1`); it becomes optional with that default, and Go and Rust match it.
   `preferred` and `off` stay explicit choices. A `required` group refuses clear events
   at receipt, as it does today (`group.go:134`, `group.rs:259`, the Erlang keyring), and counts them.
3. **What stays clear, stated plainly:** every event published `open => 1`, under any topic. The mesh's public topics
   (`agents.lobby`, presence, the station and realm announcement facts) are open by design, and each of their
   publishers states `open => 1` in code.
4. **`open => 1` under a prefix this node holds as a group is refused `group_held`**, for subscribe as for publish
   today, so `open` never reaches past a group's policy at the node that holds it.
5. **The SDKs' own publishes and subscribes state their mode too.** Today every internal fact goes through the
   default publish path: in Erlang the download, feeder, lifetime announcer, publisher, pusher, request, response,
   streamer and stream sink modules all call `macula:publish/4` with no group. Each internal publish and subscribe in
   each of the three implementations writes `open => 1` (or names its group), and a test proves a default pool's
   internal facts still flow. macula-station publishes through the Erlang SDK, so its rebuild on that release writes
   `open => 1` on its announcement publishes (§8).
6. Unchanged: SUBSCRIBE frames stay readable (stations route on the topic). Topic names, publishers and epoch key ids
   stay visible (base design §9).

This is the most expensive point in the design: every publish and subscribe call site in every service and client
gains one option. That cost is the point. Each clear event becomes a decision someone wrote down.

## 6. Point 4: D27 content transfers stay clear, as a built-in, explicit opt-out

A content announcement is a public DHT record. Every station stores and serves it, and anyone may list them
(`find_records_by_type`). A fetch trusts no realm by design, and the sharer answers anyone who holds the MCID. So any
station on a fetch's path can fetch the same content itself. **Sealing the transfer would hide nothing from T1.** It
would only hide which MCID a node asked for, which is traffic analysis, and register row #18 covers that, not this
design. Sealing it would make "content is sealed" a claim that does not hold.

So:

- Content serve and fetch use the opt-out explicitly in all three implementations: the content procedure is
  advertised `off` and fetched `preferred` against that keyless advertisement. Go already serves `ConfidentialOff`.
  **Erlang's sharer and Rust's content serve inherit the default today**, so the new default would key them and
  refuse every clear fetch. Both must pass `off` explicitly; that is part of this build, not a follow-up.
- The guides state it in one sentence: D27 content is public, and private bytes go over an application's own sealed
  call or stream. That is unchanged from today.
- Considered and not taken: putting the sharer's KEM key in the signed content announcement. It is doable (the
  record's `payload_ok` has no key-count rule, so current stations would store the pair), but it buys metadata
  hiding only, for a record change in three implementations. It can reopen under row #18.

## 7. Point 5: what the operators see

**The refused caller sees a reason with its fix**, in every SDK, with the same reason names across ABI and FFI:

| Reason | When | Message names |
|---|---|---|
| `provider_serves_clear` (new) | a `required` caller resolved only a keyless advertisement | the provider node and procedure, and "call with `preferred` to accept clear" |
| `sealed_required` (provider ERROR, existing) | a clear call or open reached a keyed provider | after the one re-resolve of §3, "the provider seals; this caller sent clear" |
| `no_kem_key`, `key_mismatch`, `no_signed_state` (existing) | unchanged | unchanged |
| `no_group` (new) | publish or subscribe naming neither `group` nor `open` | both options |

**The provider sees its refusals and its clear traffic.** None of the three implementations logs or counts either
today. Each gains:

- a counter per `(realm, procedure)` of **clear requests refused** and of **clear requests served**. They are read the
  way each SDK exposes counts today: Erlang telemetry plus a `counts` function as `macula_group_keys:counts/1` does,
  Go and Rust a pool method, the bindings through the ABI or FFI.
- a warning log when a clear request is refused, **rate-limited per procedure, with no per-caller state**: at most
  one line per (realm, procedure) per minute, naming the last refused caller and how many refusals the line covers.
  Any node can be a caller, so a limiter keyed by caller would grow without bound (memory exhaustion on the
  provider). The limiter's state is bounded by the procedures the provider advertises, and a test floods refusals
  from many callers and checks that the state stays at one entry per procedure;
- a warning log once when a procedure is advertised `off`: "serving <procedure> in the clear (confidential => off)".

The served-clear counter is what the fleet measurement in §8 reads: **zero served clear requests on every procedure
not advertised `off`**. The keyless advertisements on the fleet are the opt-out list.

## 8. Point 5: rollout order

Compatibility across the transition is a concern of order, not of code (no compat mode).

- An old caller (`preferred`) and a new provider (keyed, `required`): the call is sealed and works.
- A new caller (`required`) and an old provider (keyless): it fails, `provider_serves_clear`.

So providers must be keyed before any caller defaults to `required`. Most services are both, and the fleet rolls box
by box, canary first, so "rebuild providers first" cannot be ordered from the build list: a rebuilt service on the
canary would become a `required` caller of providers that have not rolled, and live paths would fail, the realm's
first. **Each implementation therefore ships the change in two releases, provider default first:**

- **Release A, the provider default.** Everything in §3 to §7 **except the caller default**: the keyring is always
  on, `kem_advertise` and advertise-side `preferred` are gone, there is no clear window, publish and subscribe need
  `group` or `open`, content is `off`, and the counters and logs are in. The caller default stays `preferred` in
  release A, and `provider_serves_clear` exists but is reached only by a caller that writes `required`.
- **Release B, the caller default.** It flips the caller default to `required`. It ships only after the fleet has
  rolled release A everywhere and the measurement below shows that every keyless advertisement left on the fleet is
  an explicit `off`.

This is ordering, not a compatibility mode: release B deletes the interim default, and nothing negotiates. Between
the two releases a default caller is a `preferred` caller, so T7 (§2) holds for default callers only from release B
on, and the docs of release A say so.

1. **Release A of the three implementations, in house order:** Erlang `macula` (major), then `macula-go` and
   libmacula, then `macula-rust` and its FFI crate. Default-pinning tests are rewritten (the inventory lists them per
   repo). The cross-SDK interop jobs (`sealed-interop.yml`, `scripts/interop/*`) gain: default provider to default
   caller seals; a `required` caller to an `off` provider is refused `provider_serves_clear`; `preferred` to `off` is
   clear and counted served-clear; a clear request to a default provider is refused and counted.
2. **The four bindings** re-pin to libmacula's release A. They delete their `kemAdvertise` parameters and PHP's
   written `Preferred` provider defaults, and add `open` to publish and subscribe. Today they pin v0.27.0 against
   macula-go v0.28.0, so the re-pin also covers that gap.
3. **Services and macula-station are rebuilt on released SDKs only**, in any order: under release A no rebuilt
   service refuses an unrolled one. Each rebuild writes `open => 1` on its public topics and `off` only where an
   operator decides it, each one listed in the service's PR. Before the first rebuild, each service is checked for an
   Erlang explicit-target `confidential => off`, which release A deletes.
4. **The fleet, canary first**, through macula-fleet digest pins (macula-fleet#14). Then measure: the keyless
   advertisements on the fleet (`find_records_by_type` for `procedure_advertisement`), each matched to a written
   `off`, and the served-clear counters at zero on every keyed procedure.
5. **Release B** of the three implementations and the bindings, then the same rebuilds and the same canary-first roll.
   Measured again; that reading moves the register rows "Payload sealing for calls and streams" and "Sealed
   publish/subscribe", and Saturnus is copied.

The stations relay `sealed` already and need no change for it. Their own control procedures stay clear (§2), and
the SDKs' internal calls to them must not take the release B caller default: each implementation's build checks that
those calls state their mode, with a test that a default pool still reads and writes DHT records.

## 9. Done when

- In each of the three implementations and the four bindings, a release seals calls and streams by default, refuses
  clear unless opted out at both ends, has no `kem_advertise` and no clear window, and requires `group` or `open` on
  publish and subscribe. The interop matrix proves it across SDKs.
- Measured on the fleet: zero clear requests served on any procedure not advertised `off`, and the opt-out list is
  published with the measurement.
- The register rows move on that measurement. Until then no public text claims stations cannot read payloads (D11).

## 10. Work, once approved (BUILD, sized)

| # | Package | Size |
|---|---|---|
| 1 | Erlang macula release A: §3 (provider side), §4, §5 (internal publishes included), §6 (sharer `off`), §7, tests | M |
| 2 | macula-go + libmacula release A: the same, plus the ABI reason names and the `open` option | M |
| 3 | macula-rust + FFI release A: the same, plus closing the unchecked `Seal::Clear` | M |
| 4 | Four bindings: re-pin, delete `kemAdvertise`, `open`, new reasons | S each |
| 5 | Interop matrix cases (four) in the existing jobs | S |
| 6 | Services and macula-station: rebuild, `open` on public topics, opt-outs listed | S each |
| 7 | Fleet roll, canary first, and the keyless-advertisement measurement | S |
| 8 | Release B in the three implementations and the bindings, rebuilds, roll, and the served-clear measurement | S each |

## Gate history

- **Round 1 (Uranus, Opus), four required, all folded:**
  - "nothing goes out in the clear" was false for `preferred` callers after an `off` to `required` flip: T7 is now
    defeated for `required` callers only, and the exposure is listed under "Still not protected" (§2, §4);
  - the SDKs' own publishes would all be refused `no_group`: they now state their mode, with a test (§5.5);
  - "providers before callers" could not be ordered on a fleet of services that are both, rolled box by box: the
    change now ships in two releases, the provider default first (§8);
  - the per-caller log limiter grew without bound: it is now per procedure, with a test of its bound (§7).
  - Observations taken: `open` under a held prefix is refused `group_held` (§5.4); the stale-advertisement leak is a
    further reason the follow-the-provider default was not taken (§3).
