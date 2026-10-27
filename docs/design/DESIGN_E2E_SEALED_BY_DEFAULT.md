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
| Explicit target, no option | refused `no_signed_state` (`macula.erl:553`) | refused `no_signed_state` (`confidential.go:216`), but a link-level `Clear: true` on `Call` or `OpenStream` skips that check (`confidential.go:205-218`) | refused `NoSignedState`, but a link-level `Seal::Clear` to a keyed provider is not checked (`confidential.rs:66`) | not exposed |
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
  its lifetime and validly signed, so a `preferred` caller sends in the clear and the provider answers. The window
  exists only for providers advertised `preferred`, which §3 deletes. **Defeated
  for `required` callers only**, which refuse a keyless advertisement whatever its age. Removing the window (§4) stops
  the provider from answering a clear request; it cannot stop a `preferred` caller from sending one, and the relaying
  station reads that request's payload before the provider refuses it (below, "Still not protected").
- **T8, a silent opt-out.** An operator turns sealing off and nobody on the path notices. Not defeated, but
  **made visible**: an opt-out must be a written option on both ends (§3), and the provider logs and counts it (§7).
  A caller's `preferred` is visible only in its code.

**Still not protected**, stated so no text claims otherwise (D11). §9 of the base design lists the full set. These
rows matter to this change:

- **A provider that opted out.** Its payloads are clear to every station, by its operator's decision.
- **A `preferred` caller's requests after a provider flips from `off` to `required`.** While the caller still holds
  the provider's last keyless advertisement (a remembered candidate up to 5 minutes, a stale DHT record up to 5 + 5
  minutes), each call attempt goes out in the clear. The provider refuses it, but the stations on the path have read
  it, and then reseals to the key the refusal names, so the call itself succeeds (§3). The caller's operator
  accepted that by choosing `preferred`. Until release B (§8) every caller is `preferred` by default, so release A's
  rollout exposes each caller's first request to each newly keyed provider this way. No call fails.
- **Open pubsub topics.** An open topic has no recipient key, and SUBSCRIBE is unchecked: anyone may subscribe (§5).
- **D27 content.** It is public to every station through its announcement (§6).
- **A station's own control procedures** (`find_record`, `put_record`, `find_records`, `find_records_by_type`,
  `_dht.budget`, `_relay.ping`). The station is their endpoint over a direct post-quantum link, not a relay. The
  records they move are public by design. In Erlang the SDK reaches them as DHT frames, not CALLs
  (`macula_station_link.erl:912`), so no call default touches them there.
- **A request's `token` and `proofs`**, which stay clear beside `sealed` in scheme 1. That is unchanged.

## 3. Point 1: the default, and the shape of the opt-out

**Provider (advertise, serve, serve stream).**

- The `kem_advertise` switch is **deleted** in every SDK, and so is every binding's `kemAdvertise` parameter. A
  provider always holds a KEM keyring.
- Advertise modes become **`required` (the default) and `off`**:
  - `required` names the key and refuses every clear CALL and STREAM_OPEN, from the first advertisement on.
  - `off` names no key and takes clear requests: the opt-out. Because the keyring is per node and a sealed request
    is opened before any per-procedure check (`macula_station_link.erl:3532`, Go `serve.go` `openRequest`), an
    `off` procedure **still opens and answers a sealed request** from a caller that holds the node's key from an
    earlier advertisement. That only ever adds sealing, and it is kept.
  - **`preferred` is deleted on the advertise side.** With no clear window, a keyed provider that still takes clear
    calls has no caller that needs it: every caller seals to a named key.
- `kem_advertise_disabled` disappears with the switch.
- **Every advertise path takes the default**, including the spec-less `macula_client:advertise/4` that
  `macula_dist_pool.erl:105` uses. Today it is always served clear, and `macula:call` cannot resolve it; release A
  makes it `required` like every other advertise.

**Caller (call, stream, explicit target).**

- Call modes become **`required` (the default) and `preferred`** (the opt-out). The caller default ships one
  release after the provider default (§8, release B):
  - `required` seals to the key the target's verified advertisement names. A keyless advertisement fails the call
    with the new reason `provider_serves_clear` (§7), which is distinct from `no_kem_key` (no usable key, or none
    found).
  - `preferred` seals to a named key and calls a keyless provider in the clear. That is today's default, now an
    explicit choice. The name stays, so no SDK renames an enum value that its interop vectors carry.
- **An explicit target always needs signed state.** Three unchecked clear paths are deleted: Erlang's
  `confidential => off` on `call_station` and `call_stream_station`, Go's link-level `Clear: true` on `Call` and
  `OpenStream`, and Rust's link-level `Seal::Clear`, each toward a non-station target. A link call is clear only
  when it carries a verified advertisement that the target signed and that names no key. Under either mode an
  explicit target seals from, or is called clear because of, such an advertisement (passed in, or resolved).
  `no_signed_state` remains for none found.
- **A `sealed_required` refusal carries the provider's key, and the caller seals to it** (release A0, §8):
  - **Where the key travels.** A keyed provider's `sealed_required` ERROR (to a CALL) and its STREAM_ERROR (to a
    STREAM_OPEN) carry the KEM key it holds now, as carried (`macula_seal:key_as_carried/1`, 1568 or 1665 bytes), as
    bytes in a **non-critical extension `seal.kem_key`** of the reply's and the stream frame's signed tbs
    (`docs/design/WIRE_1_0.md` §3.2). It cannot go in the text fields: ERROR `detail` and STREAM_ERROR `message`
    are capped at 256 bytes in all three implementations, and STREAM_ERROR has no `detail`. Those fields stay text
    for people. A reader that predates A0 ignores a non-critical extension and counts it
    (`macula_frame.erl:3630-3676`), so a pre-A0 caller still reads an A0 provider's refusal as today.
    `test/vectors/E2E_SEAL_V1.md` gains that refusal per profile, so every SDK reads one shape.
  - **Why it can be trusted.** The refusal is signed by the provider's identity key (`MACULA-PQ-REPLY-V1`,
    `responded_by` = signer = target), and bound to the request by `request_id` and `request_hash` (D25). So the key
    is the target's own, and fresh: the same node, with the same authorization, that the caller already chose.
  - **Where the reseal runs: in the link, under every caller.** The clear call and the clear open of each link
    (Erlang `macula_station_link` call/9 and its stream open, Go `Link.Call` and `Link.OpenStream`, the Rust
    equivalents) handle the refusal. Direct dial, pools and explicit-target callers (`call_station`,
    `dos_calls.erl:77`) all get it. The link checks the key's length against its profile, then seals the same
    payload once more, to that key, under a **new request_id**. It hands the key up in the seal report
    (`DESIGN_E2E_SEAL_REPORT.md`).
  - **What the pool remembers.** The pool writes that key into the candidate it remembered for that provider (Erlang
    `macula_direct_dial`'s remembered answer and `macula_client:resolved_candidate/3`, and the Go and Rust
    equivalents). A head-start candidate that answers is not re-remembered today (`macula_direct_dial.erl:1098-1121`),
    so the write is its own step. In `call_seal`'s precedence, a key learnt from a signed refusal wins over the same
    candidate's signed keyless advertisement for the advertisement's remaining lifetime, so the next call seals at
    once.
  - **Clear streams that already wrote.** A reopen runs only before the owner's first write (`seq_out` 0:
    `macula_stream.erl:997-1004`, Go `pool/call.go:697` and `stationlink/stream_seal.go:137`, Rust
    `station_link/stream.rs:138`), and STREAM_OPEN has no ack. So a **clear** open (only a clear one) holds the
    owner's writes until the provider's first frame, one round trip, queued in order as `forward_to_peer/2` already
    queues them. A refusal then reopens sealed and sends the held writes under the new open. A sealed open is
    unchanged.
  - **Bounds.** The reseal runs at most once per call, and nothing answers a `sealed_required` with clear. A call can
    now go out at most **three** times: clear, resealed to the refusal's key, and resealed once more on a
    `sealed_refused` (A1). That replaces A1's "at most twice". A refusal without the extension, or with a key of the
    wrong length (a pre-A0 provider, or a faulty one), surfaces as the provider's own `sealed_required`, unchanged.
  - Why not re-resolve, as A1 does for `sealed_refused`? A lookup can still return the provider's stale keyless
    advertisement, for up to 5 + 5 minutes. The signed refusal cannot be stale, and it costs no lookup.
  This only ever upgrades.

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
- **Flipping a provider from `off` to `required`.** Callers may still hold its last keyless advertisement: up to 5
  minutes as a remembered candidate, and up to 5 + 5 minutes from a stale DHT record.
  - A `required` caller already refused the keyless advertisement: nothing of it goes out in the clear.
  - A `preferred` caller on release A0 or later that holds it sends one request in the clear and gets
    `sealed_required` with the key. It seals to that key, and the call succeeds. Its remembered candidate now
    carries the key. A stale record resolved again later can repeat that once per call attempt that resolves it,
    until the keyless advertisement has expired everywhere. **The call never fails, but each such first request
    is read by the stations on its path** (§2, "Still not protected"). No provider answer is ever clear.
- **Flipping from `required` to `off`.** The node keeps its keyring, so sealed calls to the remembered key still open
  and are answered (§3). Once the keyless advertisement reaches a caller, a `required` caller fails with
  `provider_serves_clear`, which is correct, and a `preferred` caller calls clear, as both operators chose.
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
4. **`open => 1` under a prefix this node holds as a group is refused `group_held`**, for subscribe as for publish,
   so `open` never reaches past a group's policy at the node that holds it. Today all three implementations refuse
   a held prefix on publish only (Go `pubsub.go:252`), so the subscribe refusal is new in each.
5. **The SDKs' publishes and subscribes state their mode too, in two cases:**
   - **The SDK's own facts**, such as the lifetime announcer's presence facts, are public mesh plumbing. They write
     `open => 1`.
   - **Helpers that publish on application topics on the application's behalf**: in Erlang, request, response,
     pusher, streamer, stream sink, download, upload (`macula_upload.erl:177`, its `fact_publish` default) and
     feeder, and their Go and Rust equivalents. The helper must not choose for the application: hard-coding
     `open => 1` there would be T6 done by the SDK itself, and would be refused `group_held`, or relayed clear, on an
     org's prefix. Each helper's start options take `group` or `open`, and a helper started with neither is refused
     `no_group`.
   - A test proves a default pool's own facts still flow, and one proves that a helper started with neither option is
     refused. macula-station publishes through the Erlang SDK, so its rebuild writes `open => 1` on its
     announcement publishes (§8).
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

- Content serve and fetch use the opt-out explicitly in all three implementations, through the explicit-target
  rule of §3:
  - The content procedure is advertised `off`. Go already serves `ConfidentialOff`; **Erlang's sharer and Rust's
    content serve inherit the default today**, so the new default would key them and refuse every clear fetch. Both
    pass `off` explicitly, as part of this build.
  - The fetch resolves the sharer's own-namespace `procedure_advertisement` for the announced procedure
    (`find_records`), verifies it under the node's profile, and checks that its `advertiser_node` is the
    announcement's `announcer_node`. It then hands that advertisement to the link call, which is clear because the
    advertisement names no key. This trusts no realm: the advertisement is signed by the very node the fetch dials.
  - It replaces the unchecked clear the fetches use today (Erlang `confidential => off`, `macula_content_fetch.erl:319`;
    Go `Clear: true`, `pool/content.go:560`; Rust `Seal::Clear`, `pool/content.rs:635`), which §3 deletes. A sharer
    that names a key is sealed to, since sealing only ever adds.
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
| `sealed_required` (provider ERROR or STREAM_ERROR, existing; from release A0 its signed `seal.kem_key` extension carries the provider's key) | a clear call or open reached a keyed provider | only when the reseal of §3 is impossible (no `seal.kem_key`, or a key of the wrong length, as from a pre-A0 provider), unchanged from today |
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

- A caller that already holds a provider's keyed advertisement seals to it, whatever its SDK, and the call works.
- A caller holding the provider's earlier keyless candidate sends clear and is refused `sealed_required`. Only a
  caller with §3's reseal recovers from that.
- A `required` caller and a keyless provider: it fails, `provider_serves_clear`.

Two hazards follow from these.

- **Providers must be keyed before any caller defaults to `required`.** Most services are both, and the fleet rolls
  box by box, canary first, so "rebuild providers first" cannot be ordered from the build list. A rebuilt service on
  the canary would become a `required` caller of providers that have not rolled, and live paths would fail, the
  realm's first.
- **Callers must handle `sealed_required` before any provider requires sealing.** A caller on today's SDK
  remembers a provider's keyless candidate (up to 5 minutes) and may resolve its stale keyless record (up to 5 + 5).
  It sends clear, and today it ends the call on `sealed_required` (`failure_scope`, the provider-scope clause at `macula_station_link.erl:800`),
  re-resolving only on `sealed_refused` (`macula_direct_dial.erl:892-896`). Every provider roll would then break
  every unrolled caller for 5 to 10 minutes. Security must never cause downtime, so this is ordered away, not
  accepted.

**Each implementation therefore ships the change in three releases, caller handling first:**

- **Release A0, the caller's reseal.** §3's `sealed_required` handling on both sides: a keyed provider's refusal
  carries its key, and a caller reseals to it and replaces its remembered candidate's key. Nothing else changes:
  defaults stay as they are today. A0 is rolled to the whole fleet, and every service and client is rebuilt on it,
  before any release A provider runs anywhere.
- **Release A, the provider default.** Everything else in §3 to §7 **except the caller default**: the keyring is always
  on, `kem_advertise` and advertise-side `preferred` are gone, there is no clear window, publish and subscribe need
  `group` or `open`, content is `off`, and the counters and logs are in. The caller default stays `preferred` in
  release A, and `provider_serves_clear` exists but is reached only by a caller that writes `required`.
- **Release B, the caller default.** It flips the caller default to `required`. It ships only after the fleet has
  rolled release A everywhere and the measurement below shows that every keyless advertisement left on the fleet is
  an explicit `off`.

This is ordering, not a compatibility mode: no release negotiates, and release B deletes the interim caller
default. Between releases A and B a default caller is a `preferred` caller, so T7 (§2) holds for default callers
only from release B on, and the docs of release A say so.

1. **Release A0 of the three implementations, in house order:** Erlang `macula`, then `macula-go` and libmacula,
   then `macula-rust` and its FFI crate. Then the four bindings re-pin to libmacula A0. Interop gains: a clear
   call and a clear open to a keyed provider are refused `sealed_required` carrying the key, and every SDK's caller
   reseals to it and gets the answer; a clear client stream that wrote before the refusal delivers its writes under
   the sealed reopen; an explicit-target clear call reseals; and a pre-A0 caller still reads an A0 provider's
   refusal.
2. **Every service, macula-station and every client we ship is rebuilt on A0 and rolled, canary first**, through
   macula-fleet digest pins (macula-fleet#14). Gate to the next step: every fleet digest is an A0-or-later build, and
   the caller-only clients we publish (macula-mcp, the CLIs) have an A0 release. A client outside our releases on an
   older SDK is not protected from the break; nothing of that kind runs in production.
3. **Release A of the three implementations**, in the same order. Default-pinning tests are rewritten (the inventory
   lists them per repo). The interop jobs (`sealed-interop.yml`, `scripts/interop/*`) gain: default provider to
   default caller seals; a `required` caller to an `off` provider is refused `provider_serves_clear`; `preferred` to
   `off` is clear and counted served-clear; a clear request to a default provider is refused, counted and resealed.
4. **The four bindings** re-pin to libmacula's release A. They delete their `kemAdvertise` parameters and PHP's
   written `Preferred` provider defaults, and add `open` to publish and subscribe. Today they pin v0.27.0 against
   macula-go v0.28.0, so the A0 re-pin already closes that gap.
5. **Services and macula-station are rebuilt on release A**, in any order. Every caller on the fleet already
   reseals (step 2), and no release A caller is `required` by default, so no rebuilt provider breaks an unrolled
   caller, and no rebuilt caller breaks an unrolled provider. Each rebuild writes `open => 1` on its public topics
   and `off` only where an operator decides it, each one listed in the service's PR. Before the first rebuild, each
   service is checked for an Erlang explicit-target `confidential => off`, which release A deletes
   (`macula-station` `dos_campaign/src/dos_calls.erl:77` is one).
6. **The fleet, canary first.** Then measure: the keyless advertisements on the fleet (`find_records_by_type` for
   `procedure_advertisement`), each matched to a written `off`. After §4, served-clear on a keyed procedure is zero
   by construction, so this matching is the measurement's real content.
7. **Release B** of the three implementations and the bindings, then the same rebuilds and the same canary-first roll.
   Measured again; that reading moves the register rows "Payload sealing for calls and streams" and "Sealed
   publish/subscribe", and Saturnus is copied.

The stations relay `sealed` already and need no change for it. Their own control procedures stay clear (§2). In
Erlang the SDK reaches them as DHT frames, not CALLs; in Go and Rust each build checks that any CALL to them states
its mode, with a test that a default pool on release B still reads and writes DHT records.

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
| 1 | Release A0 in Erlang macula, macula-go + libmacula, macula-rust + FFI: `sealed_required` carries the key in `seal.kem_key`; the link reseals (calls, opens, held clear-stream writes) and reports the key; the pool replaces its remembered candidate's key; vectors and interop cases | M each |
| 2 | Bindings re-pin to A0; every service, station and shipped client rebuilt on A0 and rolled, canary first | S each |
| 3 | Erlang macula release A: §3 (provider side, every advertise path, the explicit-target rule), §4, §5 (SDK facts and helpers), §6 (sharer `off`, fetch from the sharer's advertisement), §7, tests | M |
| 4 | macula-go + libmacula release A: the same, closing link-level `Clear: true`, plus the ABI reason names and the `open` option | M |
| 5 | macula-rust + FFI release A: the same, closing link-level `Seal::Clear` and moving the content fetch onto §6's advertisement | M |
| 6 | Four bindings: re-pin, delete `kemAdvertise`, `open`, new reasons | S each |
| 7 | Interop matrix cases (four) in the existing jobs | S |
| 8 | Services and macula-station: rebuild, `open` on public topics and helpers, opt-outs listed | S each |
| 9 | Fleet roll, canary first, and the keyless-advertisement measurement | S |
| 10 | Release B in the three implementations and the bindings, rebuilds, roll, and the measurement again | S each |

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
- **Round 2 (Fable, run by Uranus), seven required, all folded; the Supervisor ruled finding 1 must be ordered
  away, not stated as a window:**
  - release A would break every unrolled caller for 5 to 10 minutes per provider roll: `sealed_required` now
    carries the provider's signed key, the caller reseals to it, and that ships as release A0, rolled everywhere
    before any provider requires sealing (§3, §8);
  - Go's link-level `Clear: true` was as unchecked as Rust's, and the content fetch had no mechanism: both closed,
    with the fetch moved onto the sharer's own advertisement (§1, §3, §6, §10);
  - the SDK's application-topic helpers must not hard-code `open`: they take `group` or `open` from the
    application (§5.5);
  - bugs: §1's Go explicit-target row; `off` still opens sealed requests, so §3 and §4 were wrong; §4's "one clear
    request per caller"; `macula_upload.erl` missing from §5.5.
  - Observations taken: T7 exists only under advertise-`preferred`; the measurement's real content (§8.6); T8 is
    logged provider-side only; the spec-less `advertise/4` takes the default (§3); Erlang control procedures are DHT
    frames (§2, §8); the subscribe `group_held` refusal is new (§5.4); `dos_calls.erl:77` (§8.5).
- **Round 3 (Uranus diff-only, Fable diff-only on the A0 hunk), four required, folded on Raf's GO:**
  - the key does not fit ERROR `detail` or STREAM_ERROR `message` (256-byte text): it travels in a signed
    non-critical extension `seal.kem_key` (§3);
  - a clear stream that already wrote could not reopen: a clear open holds writes until the provider's first frame
    (§3);
  - the reseal sat in direct dial only: it is in the link's clear call and open, under every caller (§3);
  - §3 and §7 disagreed on the error without a usable key: the provider's own `sealed_required` surfaces (§3).
  - Observations folded: the `:800` citation (§8); a call goes out at most three times (§3); the head-start
    candidate's key write and its precedence in `call_seal` (§3).
