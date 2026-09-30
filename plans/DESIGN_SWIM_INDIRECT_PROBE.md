# SWIM indirect probe (PING-REQ)

This exists so a station never declares a live peer suspect because one direct
path to it was slow or lossy (macula#59, follow-up to macula-station#19).

Status: draft for Raf's decision. BUILD, not a claim.

## What changes

One new frame type, `swim_ping_req`, and one new capability bit,
`CAP_SWIM_INDIRECT`. The forwarded answer reuses `swim_ack`. Nothing else on
the wire changes.

## The frame

`swim_ping_req`, sent by requester A to helper B: "ping `target` for me".

| Field | Rule | Meaning |
|---|---|---|
| `round` | non-negative integer | A's probe round; B echoes it in the forwarded ACK |
| `target` | 32-byte key | the node id A wants probed |

It has the common frame header (`version`, `frame_type`) like every frame, and
no piggyback. `received_rules(swim_ping_req)` and the field table carry exactly
these two fields; any other field refuses the frame, as for every type.

**Signer.** It is a control frame: it joins `?NEIGHBOUR_SIGNED`, so on a v4
`pq_hybrid` link it is neighbour-signed by A like `swim_ping`, and on v5 it
rides the session-authenticated link (no per-frame signature, D17 and
DESIGN_NEIGHBOUR_CHANNEL_BINDING). On v4 `pq_pure` no control frame carries a
signature, and PING-REQ is exactly as authenticated as `swim_ping` there (the
TLS-bound link). The sender never signs it itself.

**Replay bound.** The same as every control frame: on v4 `pq_hybrid` the
neighbour signature binds the connection and the next receive `seq`; on v5 and
v4 `pq_pure` the link is bound to its TLS session. A PING-REQ cannot be
replayed on another link or twice on the same one. `round` is only
correlation, not a freshness check.

## The ACK relay path

1. A's direct PING to T (round R) times out.
2. A sends `swim_ping_req{round = R, target = T}` to up to k helpers.
3. Helper B checks T is in its own SWIM membership with a live conn, and is
   neither B itself nor A. If not, it drops the request (no answer). If so, it
   sends T an ordinary `swim_ping` of B's own round R_B and records
   `R_B -> {A, R, T}` in a **relays** map, separate from B's own probes. A
   relay's timer expiring only drops the entry: **a relay never suspects T**,
   so a requester cannot steer B's own verdict on T by asking it to probe T
   often (Fable, required 2). An ACK for a relay round is forwarded, and may
   touch T alive at B, as any ACK from T does.
4. T answers B with its usual `swim_ack{round = R_B, responder = T}`.
5. B forwards `swim_ack{round = R, responder = T}` to A on the A-B link.
6. A reads an ACK whose `responder` (T) differs from the link it came on (B) as
   a relayed ACK for T. A records `{B, T} => R` for each PING-REQ it sends,
   until the indirect window ends. A relayed ACK with a matching entry ends
   round R's probe exactly as a direct ACK from T would; **a relayed ACK with
   no entry is dropped** (Fable, required 1). Otherwise any member could keep
   a dead T alive everywhere by sending `swim_ack{responder = T}` unasked.

T sees only an ordinary PING from B. A relayed ACK is B's authenticated claim
that T answered B, which is the SWIM trust model: A already trusts B as a
member. T's own ACK is neighbour-signed to B, not end to end, so it cannot be
passed through as proof from T.

The late-ACK refutation of macula-station#19 stays **direct only**: an ACK
refutes T's suspicion when it comes on T's own link (responder = link peer).

No NACK. Lifeguard's NACK (B says "T did not answer me") helps A judge its own
health; it is not needed for the invariant and is left out.

## k, timeouts, bounds

| Setting | Value | Why |
|---|---|---|
| k | 3 (config `swim.indirect_k`) | SWIM paper default; the fleet has 6 stations |
| Helpers | members that are `alive`, have a live conn, and set `CAP_SWIM_INDIRECT`; never T itself | only nodes that understand the frame |
| Indirect window | 2 x `ping_timeout_ms` (1 s by default) | A-B-T-B-A is two round trips |
| B's ping to T | `ping_timeout_ms` | B's own direct probe |
| B's relays | at most one per (A, T), at most 4 per requester, at most 16 in flight; excess dropped | a requester cannot turn B into an amplifier or fill every slot; B only pings members it already probes |

On a direct timeout A sends the station-only retry (macula-station#19) and the
PING-REQs at the same moment; any ACK, direct or relayed, before the window
ends keeps T alive. A with no eligible helper behaves exactly as today.

## Old SDKs and old stations

A node that does not know a frame type refuses it at decode, and
`macula_peering_conn` then **closes the connection** with `malformed_frame`.
So an unknown `swim_ping_req` would not be ignored, it would cut the link.

Therefore the frame is gated, not tolerated:

- `CAP_SWIM_INDIRECT = 16#0000_0000_0000_0002`. A node sets it in the
  capabilities it declares in CONNECT/HELLO only when it can read
  `swim_ping_req` and act as a helper. Protocol bits come from code: the
  station masks them out of any configured `capabilities` value, so an older
  station configured with `2` cannot declare a frame it cannot read.
- A sends `swim_ping_req` only to a peer whose `peer_capabilities` has the bit.
  An old node never sets it, so it never receives the frame.
- The forwarded answer is a `swim_ack`, which every version already reads, and
  B only sends it to an A that asked, so A knows the frame.
- v4 and v5 links behave the same: the gate is the capability bit, not the
  handshake version.

Old nodes stay exactly as they are; they are just never picked as helpers and
never ask for help. A mixed fleet works, with fewer helpers.

## Order

1. **macula 13.4.0** (minor): the frame builder `swim_ping_req/1`, its received
   rules and field table entry, `?NEIGHBOUR_SIGNED`, the `CAP_SWIM_INDIRECT`
   define, and cross-language vectors (`test/vectors/swim_ping_req_v1.json`:
   encode, decode, and a refused extra field). Tag via CI.
2. **macula-station**: set the bit, the requester and helper sides in
   `macula_swim`, config `swim.indirect_k`. The red test is the issue's: in the
   station harness, T is reachable from B and C but every A-T SWIM frame is
   dropped; A must not suspect T. It fails today (the direct retry cannot
   help when the path is gone).
3. **SDKs (Venus)**: SDKs that decode SWIM frames learn `swim_ping_req` from
   the vectors. None has to implement the helper side; without the bit none
   is ever sent the frame.

Fable reviews the frame and the helper bounds before the macula tag
(wire-format and amplification surface).

## Not in scope

NACK, Lifeguard's local health multiplier, membership gossip.
