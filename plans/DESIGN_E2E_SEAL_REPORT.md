# DESIGN: The caller's seal report

**This exists so a caller can tell, from the SDK and not from a policy it set, whether the exchange that produced its
result was sealed, and to which key.**

| | |
|---|---|
| Kind | BUILD (an API shape; it makes no new claim about the mesh) |
| Extends | `DESIGN_E2E_PAYLOAD_CONFIDENTIALITY.md` §5, §8, Amendment A1 |
| Ships in | macula 13.1.0 (Erlang), macula-go 0.19.0 and libmacula 0.19.0, then the bindings on libmacula |
| Status | Agreed with Mercurius (2026-09-28); one Fable round next |
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
- **A stream.** A stream's seal is fixed once the provider's first frame arrives: until then the stream may reseal once
  (A1, macula `reopen`, macula-go `Stream.reopen`), and after it, it can't. So a stream's report **settles** at its
  first received frame, or at its end, whichever comes first. Asked before that, the report is refused as
  `not_settled`, never guessed from the open.

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
   **only when the call asked for it**, so no other `call_station` caller's return changes.
2. **The station call.** The option rides down as `report => true` in the station-call options, which
   `macula_direct_dial:policy_opts/2` composes for `call_work/6`, through `macula_client:call_station/11` to the
   link.
3. **Direct dial.** `call_work/6` returns what the link replied. `resealed/7` passes anything but its two
   `{error, {sealed_refused, _}}` clauses through, so a resealed call returns the second call's reply and reports its
   key. `settled/6`, `sent_or_not/1` and the `remember_resolved` path are written against `{ok, _}` and
   `{error, _}` today: each clause must be checked to pass `{ok, Result, Report}` through untouched and to treat it
   as the answer it is (remembered, sent).

**An explicit target gets no report**, in Erlang as in Go (§5): `call_station/8`'s caller chose the seal itself
(`advertisement`, `confidential => required` or `off`), so it already knows. `report` there is
`{invalid_option, report}`.

**Streams.** A query, not a return change: `macula:stream_report(Stream) -> {ok, Report} | {error, not_settled}`,
answered by the stream process from `#state.seal` (`key_id`) and `#state.open` (the target). It is `not_settled`
while `reopen` or `reopening` is set and no provider frame has arrived.

## 5. Go (macula-go 0.19.0)

**Calls.** `Pool.Call`'s signature stays. A second method returns the report:

```go
func (p *Pool) CallReport(ctx context.Context, c Call) (cbor.Value, Report, error)

type Report struct {
    Sealed    int      // 0 or 1
    Provider  [32]byte
    SealKeyID [8]byte  // zero when Sealed is 0
}
```

`callAt` knows the key it sealed to, including after a reseal, so the report is built where the result returns.
`stationlink.Link.Call` is an explicit target's call: its caller chose `SealTo` or `Clear` itself, so it gets no
report.

**Streams.** `func (s *stationlink.Stream) Report() (Report, error)`, with `stationlink.ErrNotSettled` while
`s.reopen` is set and no provider frame has arrived.

## 6. The C ABI (libmacula 0.19.0) and the bindings

**Calls.** `"report": 1` in `macula_pool_call_opts`'s options turns the reply from the result JSON into an envelope:

```json
{"result": <the result, as today>, "sealed": 1, "provider": "<node_id hex>", "seal_key_id": "<16 hex>"}
```

`seal_key_id` is absent when `sealed` is 0. Without `"report"`, or with `"report": 0`, the reply is unchanged. Any
other value is `invalid_argument`.

**Streams.** `char *macula_stream_report(macula_handle stream, char **err_out)` returns
`{"sealed", "provider", "seal_key_id"}`, or fails with the error kind `not_settled`.

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
  `not_settled` before its first frame and settles after it.
- **Across SDKs:** `scripts/interop/sealed.sh` asserts the report both ways (Erlang calling Go, Go calling Erlang),
  both profiles, and macula-go#10 moves that into CI.

## 9. Decided in review (Mercurius, 2026-09-28)

1. The option on `call/6` returning `{ok, Result, Report}`, not a separate `call_report/6`: one function, and the
   return changes only when asked.
2. No report on an error, even one the provider sealed: "report" means "about a result", and no error shape changes.
3. A dedicated `macula:stream_report/1`, not a key in `macula_stream:info/1`, so `not_settled` is a return.
4. `sealed` is 0 or 1 inside Erlang too: one value in every SDK beats the Erlang idiom here.
