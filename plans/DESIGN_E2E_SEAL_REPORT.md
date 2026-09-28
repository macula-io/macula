# DESIGN: The caller's seal report

**This exists so a caller can tell, from the SDK and not from a policy it set, whether the exchange that produced its
result was sealed, and to which key.**

| | |
|---|---|
| Kind | BUILD (an API shape; it makes no new claim about the mesh) |
| Extends | `DESIGN_E2E_PAYLOAD_CONFIDENTIALITY.md` §5, §8, Amendment A1 |
| Ships in | macula 13.1.0 (Erlang), macula-go 0.19.0 and libmacula 0.19.0, then the bindings on libmacula |
| Status | Draft (Venus, 2026-09-28), for Mercurius's review, then one Fable round |
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
| `provider` | 32 bytes | always | The node_id of the provider that answered: the request's target. |
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

`report` is an Erlang API option, not a wire field, so it takes `true`. Without it the return is `{ok, Result}`, as
today. Where it hooks: since 13.0.1 a pending call keeps its seal beside its request
(`#state.pending`, `{From, TRef, Request, Seal}` in `macula_station_link`), and `Seal` is `clear` or
`{Keys, SealRequest}` with the key id in `Keys`. `completed/5` has what it needs; the report is built there and
carried up through `macula_direct_dial:call_work/6` with the provider it called. The reseal in `resealed/7` returns
the second call's outcome, so its seal is the one reported, with no extra bookkeeping.

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

## 9. For the reviewer

1. `{ok, Result, Report}` for Erlang, or `{ok, Result}` with the report in a second return from a separate function
   (`call_report/6`)? This note picks the option, which keeps one function and changes the return only when asked.
2. Should an error that a provider sealed (its handler's ERROR, opened under the key) carry a report? This note says
   no: it keeps "report" meaning "about a result" and changes no error shape.
3. `macula:stream_report/1` versus adding the report to `macula_stream:info/1`'s map. This note picks a dedicated
   function, so `not_settled` is a return, not a missing key.
