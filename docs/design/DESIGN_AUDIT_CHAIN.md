# The audit chain: entries, checkpoints and a verifier

This exists so an operator can show, after an incident, what a station or a provider refused, served or reconfigured and
when — and so that an intruder who later deletes, edits or cuts short that record is caught by anyone holding the
anchors.

It is WP1 of `macula#175`; the threat model is that issue's body and is not repeated here. This document is the format:
what an entry is, what a checkpoint signs, what an export looks like, and what the verifier refuses. The station stream,
the provider record and the fleet's anchor destinations are later work packages and are named only where the format has
to accommodate them.

## What an entry holds

An entry is a deterministic-CBOR map with text keys — the same encoding the records and frames use, so there is one
encoder to trust and nothing is re-encoded when a signature or a hash is checked.

| key | meaning |
|---|---|
| `v` | the chain format version, 1 |
| `n` | the entry's sequence number, 1, 2, 3, … with no gaps |
| `k` | the event kind, a business verb: `handshake_refused`, `store_refused`, `call_refused`, `limits_changed`, `key_rotated`, `checkpoint_overdue`, `events_dropped` |
| `t` | the node's own clock, seconds since the epoch, as a hint and never as evidence |
| `s` | the node id the transport verified, where one was verified |
| `r` | the reason, where the kind has one |
| `d` | a digest of the request, taken over the signed or sealed bytes and never over plaintext |
| `c` | how many identical events this entry stands for (a coalesced flood), 1 unless coalesced |
| `p` | the previous entry's hash, empty for the first entry of a segment |

There are no payloads. Sealed traffic stays sealed, and an entry never carries what it cannot carry safely.

**The chain link.** `p` is the SHA-384 of the previous entry's own bytes. An entry's bytes are the CBOR of its fields
WITHOUT a `h` key; its hash is the SHA-384 of exactly those bytes. So a caller can check one entry without trusting the
encoder to reproduce anything: the bytes in hand are hashed as received.

SHA-384 rather than SHA-256 because the record outlives the ids it names and the `pq_pure` profile is CNSA 2.0, which
names SHA-384; the ids the SDK hashes with SHA-256 are short-lived by comparison.

## What a checkpoint signs

A checkpoint is the claim that everything up to a point is fixed:

| key | meaning |
|---|---|
| `v` | the chain format version |
| `node` | the node id whose chain this is |
| `cs` | the checkpoint's own sequence number, 1, 2, 3, … |
| `n` | how many entries the chain had when this checkpoint was taken |
| `head` | the hash of entry `n` |
| `prev` | the previous checkpoint's hash, empty for the first |
| `i` | the interval, in seconds, at which this node promises the next checkpoint |

It is signed with the node's identity key through `macula_signed_object:sign/3` under the label
`MACULA-AUDIT-CHECKPOINT-V1`, the key travelling with the signature: a verifier holding nothing but the export and the
anchors must be able to check it, so the checkpoint may not assume a key it was not given. The label is a signing domain of its own: a checkpoint signature cannot be read as a
transport signature, and a signature made under a transport label cannot be read as a checkpoint. The object carries
`alg` and is verified under the verifier's profile, exactly as a record or a frame is.

A checkpoint's own hash is the SHA-384 of the canonical CBOR of its fields, and `prev` links checkpoints the way `p`
links entries — which is what makes a fork visible: two checkpoints claiming the same `cs` cannot both extend the same
`prev`.

**A checkpoint only ever covers what is durable.** `macula_audit_chain:checkpoint/3` asks the chain's sink to `sync`
first and refuses to sign if that fails. A crash after anchoring therefore cannot lose an anchored tail, and an accident
cannot be made to look like tampering.

## What an export looks like

An export is JSON lines, so a SIEM can take it in without any Macula code. Each line is one of three shapes, in this
order:

1. `{"chain": {"v": 1, "node": "<hex>", "segment": 1, "sealed": false}}` — the header, once.
2. `{"entry": "<hex of the entry's exact CBOR bytes>", "n": 3, "k": "handshake_refused", "t": 1760000000,
   "s": "<hex>", "r": "...", "d": "<hex>", "c": 1, "p": "<hex>", "h": "<hex>"}` — one per entry.
3. `{"checkpoint": "<hex of the signed object's CBOR bytes>", "cs": 2, "n": 40, "head": "<hex>", "prev": "<hex>"}`
   — one per checkpoint.

**The bytes are the authority.** The verifier hashes `entry` as received and checks that every readable field beside it
decodes from those bytes to the same value. A line whose fields disagree with its bytes is refused, in both directions:
editing a readable field leaves the bytes claiming something else, and editing the bytes leaves the hash broken. The
human-readable half can never say something the signed half does not.

**The order is strict, and one export is one segment.** The verifier reads the header once, then the entries in
ascending `n`, then the checkpoints in ascending `cs`. A second header, an entry or a checkpoint out of that order, or
one repeated is refused as malformed rather than sorted into place — sorting is how a spliced export would pass. An
export carries one segment of one chain because `segment` and `sealed` are facts about the segment, and a reader that
stitches two of them together would be reading a chain nobody wrote.

**A checkpoint line's readable fields are decoration.** The verifier reads a checkpoint's `cs`, `n`, `head` and `prev`
from the signed object's own bytes and never from the line beside it: editing them changes nothing that is believed.
The encoder writes them from the object, so a normal export always agrees.

**`sealed` is the exporter's claim, and the verifier tests it.** `false` says the chain was still being written when the
export was taken: the verifier judges it by its anchors and reports `overdue` when the newest anchor is older than the
interval the last checkpoint promised. `true` says the chain ended because the node's identity key rotated: the verifier
then requires an anchor for the last checkpoint's sequence number and hash, and reports `{sealed_without_anchor, Cs}`
otherwise. Writing `false` buys an exporter nothing — an ended chain whose last anchor is stale reads `overdue`, which
is the same verdict under another name, and one whose anchor is current is inside the window L1 already states.

## What the verifier refuses

`macula_audit_verifier:verify/3` takes the export, the anchors, and options, and answers one of three things:

- `{ok, Report}` — the chain is intact and its checkpoints are current.
- `{overdue, Report}` — the chain is intact and its checkpoints have stopped: the last anchor is older than the
  interval the node promised. This is deliberately not `ok`: a chain nobody is watching is not a chain that passed.
- `{error, Findings}` — the chain is broken, with the FIRST bad place reported, not a count of everything after it.

The findings, and the test that produces each:

| finding | what was done |
|---|---|
| `{entry_hash_mismatch, Seq}` | an entry's bytes were edited |
| `{fields_bytes_mismatch, Seq}` | the readable fields disagree with the bytes |
| `{sequence_gap, Expected, Found}` | an entry was deleted from the middle |
| `{prev_hash_mismatch, Seq}` | an entry was reordered, or its link rewritten |
| `{truncated_after_checkpoint, Cs, N}` | entries after the last checkpoint were cut, or entries before it removed |
| `{anchor_mismatch, Cs}` | the chain was rewritten after an anchor: the checkpoint no longer matches what a destination holds |
| `{checkpoint_signature_invalid, Cs}` | a checkpoint was forged, or signed under another label |
| `{checkpoint_not_extending, Cs}` | a checkpoint does not extend the previous one (a fork in the verifier) |
| `{checkpoint_head_mismatch, Cs, N}` | a signed checkpoint's head is not the hash of entry `N` in the export — the entries were rewritten consistently, so their own links are intact and only the checkpoint signed before the rewrite disagrees. This is the rewrite-after-anchor case the verifier catches with no anchor at all, **and it rests entirely on the key binding below**: without it, anyone could rewrite the chain and re-sign the checkpoint with their own key, and this finding would never fire |
| `{checkpoint_key_mismatch, Cs}` | a checkpoint's signing key is not the node it names, or it names a node other than the export's. The signed object carries its own key, so `macula_node_keys:node_id/2` of that key must equal the checkpoint's `node` field, and that field must equal the export header's. The same check runs at receipt (`macula_audit_checkpoint:received/2` refuses a checkpoint of another node) |
| `{unsealed_tail, Cs, N}` | a chain exported as sealed whose last checkpoint covers fewer entries than the export holds: the seal does not reach the tail, so nothing anchored speaks for it |
| `{checkpoint_replayed, Cs}` | an old checkpoint was replayed: the sequence went backwards, or a `cs` repeated with a different hash |
| `{sealed_without_anchor, Cs}` | the chain ends because the node's key rotated, with no anchor sealing the last checkpoint |

`macula_audit_checkpoint:received/2` is the same fork and replay check at RECEIPT, for a destination holding anchors: it
is handed the checkpoint it just received and the last one it holds for that node, and refuses a checkpoint that does
not extend it.

**The interval that judges an overdue chain is the one the newest anchor names.** The verifier takes the anchor with the
newest `received_at`, finds the checkpoint that anchor names, and uses THAT checkpoint's `i`. Reading the interval from
the last checkpoint in the export instead would let a holder of the node's key append an unanchored checkpoint promising
a long interval and read as watched while nothing had been anchored for weeks.

**A seal covers the whole export.** `sealed: true` says the chain ended; it is only true if the last checkpoint covers
every entry the export holds (`n` equal to the entry count). An entry written after the last checkpoint is not sealed by
anything, so a chain claiming to have ended is refused with `{unsealed_tail, Cs, N}`. What no verifier can see is a tail
that was CUT: with the tail removed, the export claims to have ended earlier, and only an anchor of a later checkpoint —
which a sealed chain does not have — could show otherwise.

A sealed export therefore proves completeness only up to its last anchored checkpoint. The seal closes the tail the
exporter still had; only an anchor the destination holds says that tail was the real one, so everything the export holds
after its last anchored checkpoint rests on the node's own word.

**The anchors, and the only clock in the verifier.** An anchor is what a destination holds for one node:
`#{cs, n, head, prev, hash, received_at}` — the checkpoint's sequence number, its entry count, the head and previous
hash it carries, its own hash, and the destination's receipt time in seconds. `received_at` is the only time the
verifier uses, and it is the destination's clock, never the node's. Nothing in the export supplies a time: an entry's
`t` is a hint a node writes about itself, and a line that tried to state when a checkpoint was received would be a
claim by the party being checked. An export that does not parse at all is not a finding — nothing was verified — and is
answered `{error, [{malformed_export, Reason}]}`.

## What this does not claim

The chain proves the record was not changed after it was anchored; it does not prove the node told the truth, and it
cannot stop a deletion — only show one. Time is the anchor destination's receipt clock, never the node's. Everything
after the last anchor can be rewritten for one interval, and that interval is only as useful as somebody noticing an
overdue anchor.
