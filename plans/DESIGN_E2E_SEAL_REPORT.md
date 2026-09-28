# DESIGN: The caller's seal report

**This exists so a caller can tell, from the SDK and not from a policy it set, whether the exchange that produced its
result was sealed, and to which key.**

| | |
|---|---|
| Kind | BUILD (an API shape; it makes no new claim about the mesh) |
| Extends | `DESIGN_E2E_PAYLOAD_CONFIDENTIALITY.md` §5, §8, Amendment A1 |
| Ships in | macula 13.1.0 (Erlang), macula-go 0.19.0 and libmacula 0.19.0, then the bindings on libmacula |
| Status | Agreed with Mercurius (2026-09-28); Fable round 1 answered, back to Mercurius for the changed §3 and §4 |
| Written against | macula v13.0.1 (92137b94), macula-go v0.18.1 (afbb4d7) |

---

## 1. Why

Today a caller that must know its call went sealed sets `confidential => required`. That's a policy: it refuses the
call when no key is named, but a `preferred` caller, the default, can't tell afterwards which way its call went. The
report answers that one question after the fact, for calls and streams, in one shape in every SDK.

## 2. What the report says, and what it does not

The report states that **the mechanism ran on this exchange**: the request went out sealed to a named KEM key, and the
answer the caller received was opened under that key. It is not a guarantee about anything beyond that exchange (D11):
not that the provider keeps the payload secret, not that the key is uncompromised, not that anything else was sealed.
No name or doc may say "confidential", "secure" or "private" about it. It says `sealed`.

| Field | Type | Present | Meaning |
|---|---|---|---|
| `sealed` | integer 0 or 1 | always | 1: the request that produced the returned result was sealed and its answer opened under the same key. 0: that request went in the clear. |
| `provider` | 32 bytes | always | The node_id the request was addressed to, its target. For a sealed result it is also the reply's `responded_by`, which opening the reply binds; for a clear result it is the target, not a claim about who answered. |
| `seal_key_id` | 8 bytes | when `sealed` is 1 | The id of the KEM key the request was sealed to (`kem_key_id` as the advertisement names it). |

No booleans cross a wire or the C ABI: `sealed` is 0 or 1 everywhere, including in Erlang, so no SDK translates it.

## 3. What it means in each case

- **A result.** A sealed call accepts only a result opened under its request's key (§5.1, macula's
  `answer_of/3`), so `sealed` is 1 exactly when the returned result was opened. A clear call's result reports 0.
- **After a reseal (A1).** The report describes the exchange that produced the result, never the first attempt. A call
  refused `sealed_refused` and resealed reports the **reseal's** key id. The provider is the same node: a reseal goes
  to the provider that refused, and a pool never moves a sent call to another candidate (macula `failure_scope/1`,
  macula-go 0.18.1), so no report can name one provider while the result came from another.
- **An error.** There is no report: an error is not a result, and its shape doesn't change. Callers that need to know
  whether a failure was a clear refusal already have it: relay errors and pre-open refusals come back as their own
  kinds (§8.3).
- **A stream.** A stream's report **settles** when the first provider frame is **opened under the stream's key**, and
  not before: that is the one event that shows the provider opened the STREAM_OPEN, and after it no reseal can happen
  (A1). A clear stream settles at its first provider frame. Until then the report is refused as `not_settled`, never
  guessed from the open. **A stream that ends before it settles has no report**, whatever ended it (a clear refusal, a
  failed reseal, the caller closing it): as with a call's error, there is no result to report on. So a stream refused
  `sealed_refused` whose reseal found no key never reports the key it was first sealed to.
- **A caller's report only.** The report is the caller's evidence. The provider side of a stream has none: asked on a
  served stream, it is refused (Erlang `{error, not_a_caller}`, Go `ErrNotACaller`, the cabi kind `not_a_caller`).

## 4. Erlang (macula 13.1.0)

**Calls.** An opt-in option, so no 13.x caller's return changes:

```erlang
macula:call(Pool, Realm, Procedure, Payload, TimeoutMs, #{report => true})
  -> {ok, Result, #{sealed := 0 | 1, provider := <<_:256>>, seal_key_id => <<_:64>>}}
   | {error, Reason}                                  %% unchanged
```

`report` is an Erlang API option, not a wire field, so it takes a boolean. Without it, or with `false`, the return is
`{ok, Result}`, as today. `call/6`'s spec gains `report => boolean()`, and `macula_direct_dial`'s option checks gain a
`report_option/1` beside `confidential_option/1` and `provider_option/1`: any other value is
`{error, {invalid_option, report}}`, before anything is sent.

Where it is built, from the link up (macula main b51202a8):

1. **The link.** `macula_station_link:call_answered/7` is the one clause that holds both the verified answer and the
   pending call's `Seal` (`{Keys, SealRequest}`, with `Keys` carrying `key_id`, or `clear`). The report is built
   there and handed to `completed/5`, which does not see `Seal` today. The link replies `{ok, Result, Report}`
   **only when the call asked for it**, so the pending entry (`{From, TRef, Request, Seal}` today) must carry the flag.
   Widening it touches `seal_redacted/1` (the `format_status` redaction) and the tests' `state_field_index/1`.
2. **The flag's path down.** Neither lower hop has a slot for it today: `macula_client:call_station/11` takes the
   link options as `maps:with([expected_node_id], Opts)` and a `Seal` guarded to `clear | {sealed_to, Key}`, and
   `macula_station_link:call/8` guards `Seal` the same way. Both gain an explicit argument for it (a
   `call_station/12` and a `call/9`), rather than widening `Seal`, whose guard is a confidentiality check and stays
   exactly as it is.
3. **The facade the pool dials through.** `macula_direct_dial:default_dial_io/0` makes `fun macula:call_station/8`
   the pool's station call, so the pool's own path runs through the explicit-target facade. `call_station/8`
   therefore **honours** `report => true` (validated by the same `report_option/1`), passes it to
   `call_station/12`, and returns `{ok, Result, Report}` when asked. `policy_opts/2` adds it to what
   `call_work/6` hands `CallStation`.
4. **Direct dial.** `call_work/6` returns what the link replied. `resealed/7` passes anything but its two
   `{error, {sealed_refused, _}}` clauses through, so a resealed call returns the second call's reply and reports its
   key. `settled/6`, `outcome/1`, `remembering/6`, `sent_or_not/1` and the `dial_io` type's `call_station` return
   (`{ok, term()} | {error, term()}`) are written against two shapes today: each must take `{ok, Result, Report}` as
   the answer it is (remembered, sent), and dialyzer will name any that don't.

**Why Erlang's explicit target reports and Go's doesn't.** In Erlang the pool dials through `call_station/8`, so
refusing the option there would need an internal detour whose only purpose is to hide a working option; honouring it
costs nothing and gives an explicit caller the same evidence. Go's pool doesn't route through its explicit
`stationlink.Link.Call`, so this note doesn't add the report there. Go could add it later without changing its shape:
the difference is scope, not design.

**Streams.** A query, not a return change: `macula:stream_report(Stream) -> {ok, Report} | {error, not_settled} |
{error, not_a_caller}`, answered by the stream process from `#state.seal` (`key_id`) and `#state.open` (the
target). The stream state gains a `settled` marker, set in `peer_event/2`'s sealed clause when `opened/5` succeeds
(the clause that already clears `reopen`), and at the first provider frame of a clear stream. The report is
`not_settled` until the marker is set, including after the stream has ended.

## 5. Go (macula-go 0.19.0)

**Calls.** `Pool.Call`'s signature stays. A second method returns the report:

```go
// in package stationlink, beside Confidentiality
type Report struct {
    Sealed    int      // 0 or 1
    Provider  [32]byte
    SealKeyID [8]byte  // zero when Sealed is 0
}

// in package pool
func (p *Pool) CallReport(ctx context.Context, c Call) (cbor.Value, stationlink.Report, error)
```

`Report` lives in `stationlink`, because `pool` imports `stationlink` and a stream's report comes from
`stationlink.Stream`; `pool` uses that one type, so Go has one shape, not two same-named types.

`callAt` knows the key it sealed to, including after a reseal, so the report is built where the result returns.
`stationlink.Link.Call` is an explicit target's call: its caller chose `SealTo` or `Clear` itself, and this note
gives it no report (§4 says why Erlang differs).

**Streams.** `func (s *stationlink.Stream) Report() (Report, error)`: `ErrNotSettled` until a provider frame has
been opened under the stream's key (a flag set when `streamSeal.opened` succeeds; Go clears `reopen` only on a
reseal, so `reopen` can't serve as the marker), or has arrived on a clear stream; `ErrNotACaller` on a served stream.

## 6. The C ABI (libmacula 0.19.0) and the bindings

**Calls.** `"report": 1` in `macula_pool_call_opts`'s options turns the reply from the result JSON into an envelope:

```json
{"result": <the result, as today>, "sealed": 1, "provider": "<node_id hex>", "seal_key_id": "<16 hex>"}
```

`seal_key_id` is absent when `sealed` is 0. Without `"report"`, or with `"report": 0`, the reply is unchanged. Any
other value is `invalid_argument`. Today's decoder is strict (`callOptionsOf`, unknown fields refused), so no 0.18
caller can already be passing it. The same options decoder serves `macula_pool_open_stream_opts`: there `"report"`
is `invalid_argument`, since a stream reports through `macula_stream_report`.

**Streams.** `char *macula_stream_report(macula_handle stream, char **err_out)` returns
`{"sealed", "provider", "seal_key_id"}`, or fails with the error kind `not_settled` or `not_a_caller`. Both are new
kinds in the fixed error table (`cabi/abierror.go`, `CONTRACT.md` "Errors"), so this is a contract minor bump.

Python, .NET and TypeScript expose the same three fields, with their own names for the method, when they ship on
libmacula 0.19.0.

## 7. Names that sealed events will reuse (package 5)

A sealed event (§6) carries its epoch's key id and its publisher. When a subscriber is told whether an event was
sealed, it uses **the same names**: `sealed` (0 or 1) and `seal_key_id` (the epoch id, 8 bytes), with `publisher` in
place of `provider`. `seal_key_id` is deliberately not `kem_key_id`: an epoch key isn't a KEM key, and one name
serves both. This note fixes only the names. The event side is built with package 5.

## 8. Tests

- **Vectors aren't needed.** The report holds no new bytes: it names what the call already sealed.
- **Each SDK, red first:** a keyed provider reports 1 with its key id; a keyless one reports 0 and no key id; a call
  refused `sealed_refused` and resealed reports the reseal's key id; an error returns no report; a stream reports
  `not_settled` before its first opened provider frame and settles after it; a stream refused `sealed_refused` whose
  reseal finds no key stays `not_settled` after it ends; a served stream is `not_a_caller`.
- **Across SDKs:** `scripts/interop/sealed.sh` asserts the report both ways (Erlang calling Go, Go calling Erlang),
  both profiles, and macula-go#10 moves that into CI.

## 9. Decided in review (Mercurius, 2026-09-28)

1. The option on `call/6` returning `{ok, Result, Report}`, not a separate `call_report/6`: one function, and the
   return changes only when asked.
2. No report on an error, even one the provider sealed: "report" means "about a result", and no error shape changes.
3. A dedicated `macula:stream_report/1`, not a key in `macula_stream:info/1`, so `not_settled` is a return.
4. `sealed` is 0 or 1 inside Erlang too: one value in every SDK beats the Erlang idiom here.
5. `call_station/8` honours `report`, because it is the pool's own path (`default_dial_io/0`); Go's explicit
   `stationlink.Link.Call` gets none in this note, which is scope, not design.

## 10. Fable round 1 (2026-09-28), answered

1. §4 contradicted itself (the pool dials through `call_station/8`), and the flag had no slot at
   `macula_client:call_station/11` or `macula_station_link:call/8`: now §4 steps 2 and 3.
2. The stream rule could report `sealed` 1 for a key nothing was opened under (a failed reseal ends the stream with
   `reopen` cleared), and never settled a stream ended otherwise: now it settles only on an opened provider frame,
   and an unsettled stream has no report (§3, §4, §5).
3. Go's `Report` can't be returned from `stationlink` if `pool` declares it: now it lives in `stationlink` (§5).
