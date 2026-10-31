# Sealed key files at rest (MACULA-SEALED-KEY-V1)

**Status:** design of record for WP-K1 of macula-io/macula-realm#176.
**Applies to:** the Erlang SDK (this repository) first; Go and Rust follow in WP-K2
against the same vectors.

This exists so that a copy of a key file is not a working identity. A key file
today is the key, in the clear (`macula-node-key-v1`): anyone who copies a
backup, a data directory or a mis-mounted volume holds a working node, station
or realm identity (attack X1 of #176). Sealed, the copy is ciphertext, and the
thing that opens it lives off that file's volume and outside the backup.

## 1. What it is

A sealed key file carries the same key bytes as a plaintext one, encrypted with
AES-256-GCM under a key-encryption key (KEK) the deployment supplies, and
names both the key inside and the KEK it was sealed to. Sealing changes no
identity: the node id, the key id and every signature stay exactly what they
were. Nothing expires, no peer sees any difference, and a service does not go
down to be sealed.

## 2. The file

Byte layout, all integers big-endian:

| Offset | Bytes | Field |
|---|---|---|
| 0 | 21 | the magic `MACULA-SEALED-KEY-V1` and a NUL: `4d 41 43 55 4c 41 2d 53 45 41 4c 45 44 2d 4b 45 59 2d 56 31 00` |
| 21 | 1 | format version, `1` |
| 22 | 1 | purpose tag, as the plaintext format's: 1 identity, 2 connect, 3 tls, 4 realm, 5 org, 6 foundation |
| 23 | 1 | profile tag, as the plaintext format's: 1 `pq_pure`, 2 `pq_hybrid` |
| 24 | 32 | the key id of the key inside: a node's `node_id` for an identity key, its key id for every other purpose |
| 56 | 32 | the KEK's id (section 3) |
| 88 | 12 | the nonce |
| 100 | n | the ciphertext of the plaintext key file, **followed by** its 16-byte GCM tag |

The header is bytes 0 to 99 — everything before the ciphertext — and it is the
whole of the additional authenticated data. The version byte is part of it, so
a reader that ever learns a second version is told so by the tag rather than by
a length it happens to dislike.

Why each field is where it is:

- **The key id inside** lets an operator tell two sealed files apart without a
  KEK, and it makes a file whose header and content disagree be refused by name:
  a header naming one key over another key's ciphertext fails on the key id
  before a single signature is made under the wrong identity. **It does not, on
  its own, refuse one node's whole sealed file moved over another's.** The
  reader compares the header's key id with the key inside the same file, not
  with a key the caller expected; two boxes that share a KEK accept each
  other's files, and a caller that must refuse that passes the id it expects
  (`#{key_id => Expected}` to `load/4`, section 4). Per-box KEKs are what make
  that substitution impossible without the caller asking (section 6).
- **The KEK's id** says which KEK this file was sealed to, so a box rotated to
  a new KEK refuses with "wrong KEK" instead of "tampered" — a refusal an
  operator can act on.
- **The purpose and profile** repeat what the plaintext key file already says.
  They are here so a store that lists key files can label them without a KEK,
  and the reader refuses a file whose header and content disagree.
- **The nonce is in the header, not beside the ciphertext**, so that the AEAD's
  additional data covers it: a swapped nonce fails the tag.

## 3. The KEK, and its id

The KEK is 32 bytes, from one source in version 1: a file whose path the
deployment gives the service. The Erlang implementation takes it from the
`key_kek_file` application setting, else from `MACULA_KEY_KEK_FILE` in the
process environment; a caller that holds a path passes it explicitly
(`#{kek_file => Path}`).

Its id is `SHA-256("MACULA-KEK-ID-V1" || 0x00 || KEK)`. Labelled and separated
from a key id the same way `MACULA-KEY-ID-V1` is, so a KEK's id can never be
mistaken for a key's.

A KEK is refused when:

| Refusal | When |
|---|---|
| `{kek, not_configured}` | no path is given and neither setting names one |
| `{kek, Path, Reason}` | the file will not open, for the file system's reason |
| `{kek, not_owner_only}` | group or others can read it |
| `{kek, same_filesystem}` | its `st_dev` equals the sealed key's: a whole-disk copy would then carry both |
| `{kek, wrong_length}` | it is not 32 bytes |

## 4. Loading a key file

`load/4` reads a file and decides from its first bytes:

- **Sealed** (`MACULA-SEALED-KEY-V1`): the KEK is read and checked, the header's
  KEK id is compared with the supplied KEK's, the tag is verified with the
  header as additional data, the plaintext is decoded, and the header's key id,
  purpose and profile must agree with the key that came out. Every refusal
  names which of those steps failed.
- **Plaintext** (`macula-node-key-v1`): the mode decides.

| Mode | A plaintext file |
|---|---|
| `accept` (the default, and the first release's) | loads, and the load is counted and logged at `notice` |
| `sealed_required` | refused as `plaintext_key_refused` |

A caller that knows which key it must get passes it: `#{key_id => Expected}`,
and a file holding any other key is refused as `{key_id_mismatch, Expected}`,
sealed or plaintext. Without it, the reader can only catch a file whose header
and content disagree, not a whole sealed file of another node's — see section 6.

The count is a `counters` ref in `persistent_term` (`plaintext_loads/0`),
created when the module is loaded, so no process owns it and it cannot go back
to zero when a short-lived loader exits. The fleet check reads it: a box that
has gone through WP-K3 must report zero, and drift shows as a failing check
rather than as a quietly unsealed key.

## 5. Sealing a file in place

`seal_file/4` converts one file, and is the only writer of the format:

1. read and check the plaintext key (a file that is already sealed is refused,
   `already_sealed`);
2. seal it with the KEK and a fresh nonce;
3. write a temporary file in the same directory, `0600`, and `fsync` it;
4. read it back and unseal it, and check the key id is the one that went in;
5. rename it over the old name.

The plaintext file is replaced only after step 4 passes. A crash before the
rename leaves the plaintext and a stray temporary, never a half-sealed key.

`save/2` and a first ground identity write their temporary and rename it
without an `fsync`, which predates this format and is left as it was: what it
can lose in a hard crash is a freshly minted key that no peer has seen yet. It
is noted for WP-K3, where the fleet's write paths are weighed together.

A KEK that is configured seals a key **whatever the mode**. Steps 2 to 4 leave
a box in `accept` for as long as it takes to confirm the sealed load, and in
that window a statement issuer rewrites connect and TLS keys and a node that
has none grinds a new identity: all of those are sealed too, so the flip at
step 4 cannot refuse a file the box itself wrote. A key file is written in
plaintext only when no KEK is configured at all.

**A KEK that is configured and unusable stops the write.** If the KEK path is
set but the file is on the key's own device, readable by group or others, or
not 32 bytes, then a node that has to grind its first identity fails to start,
in `accept` as much as in `sealed_required`. That is deliberate: the
alternative is writing a plaintext key on a box whose operator has said where
the KEK is, and the refusal names the KEK file and what is wrong with it. The
fix is the KEK, not the mode.

## 6. The order on a box, and what we do not claim

1. Wire the KEK into the service.
2. Seal.
3. Confirm the sealed load.
4. Then set `sealed_required`.

For the realm and organisation keys, the restore drill from escrow comes before
step 4 (WP-K3).

**Not claimed.** A live intruder who can read the process holds the key. On a
single-volume box without a TPM the KEK's host secret can sit on the same disk,
so only copies of the data directory and the backup are closed there; the
`same_filesystem` refusal is about the running box, not about the disk image.
A desktop key with no KEK path configured is still plaintext. Hardware custody
of the realm key is #142's later step.

**Not claimed: that a sealed file is the *right* file.** Nothing here detects a
replay or a rollback. An older sealed file of the same key — one taken before a
rotation, or restored from a backup — opens exactly as the current one does,
because the header carries no counter, time or generation. Nor is cross-node
substitution caught by the file alone: the reader compares the header's key id
with the key inside that same file, so if two boxes share one KEK, one box's
sealed file opened on the other yields the other's identity without a word.
**KEKs are therefore per box, one per box, and sharing one between boxes is a
misconfiguration, not a supported arrangement.** A caller that must refuse a
substituted file in its own right passes `#{key_id => Expected}` to `load/4`,
and a caller that must refuse a rolled-back one has to keep that record
somewhere this format does not: a generation is a follow-up with its own issue.

**Not claimed about a file that has been sealed.** The rename replaces the
name, not the blocks: the plaintext a key file held before sealing stays in
those blocks until the filesystem reuses them, and no step here overwrites it.
A backup or a snapshot taken before the seal still holds the plaintext, so
sealing a box does not seal its history — the age backup and any other copy
have to be dealt with as copies. The directory itself is not synced after the
rename; the sealed file is synced before it, so a crash costs the rename, not
the file.


## 7. Vectors

`test/vectors/sealed_key_v1.json` is the shared contract for the three
implementations. It carries two vectors, each with a KEK, the KEK's id, a
nonce, the header, the plaintext key file and the sealed bytes they produce,
written once by `scripts/generate-sealed-key-vectors.sh` (key generation is
random, so a run makes new keys and the committed file is the vector). One is
an identity key in `pq_pure`, whose key id is the `node_id` derivation; the
other is an `org` key in `pq_pure`, whose key id is the `MACULA-KEY-ID-V1`
derivation. Between them an implementation can tell the two derivations apart,
and the separate `kek_id` and `header` fields let a label or an offset mistake
show without a full open.

A conforming reader unseals a vector's bytes to a key of that vector's key id;
a conforming writer seals the vector's plaintext with the vector's nonce to
those exact bytes. From the same inputs every implementation also runs the same
refusals: another KEK (`wrong_kek`), a byte flipped at `flipped_byte_offset`
(`not_authentic`), a file shorter than its header (`bad_file`), and the
plaintext file itself under `sealed_required`.

## 8. Out of version 1

Passphrase-derived KEKs, the macOS Keychain and the Linux keyring on desktops,
and Windows DPAPI (the Rust SDK keeps the Credential Manager it already has).
Desktop sources are a follow-up with their own issue.

Two things version 1 fixes that a passphrase KEK would force open: the KEK is
used **raw** as the AES-256 key, with no KDF, and its id is a **plain,
unsalted** SHA-256 of it. Both are right for 32 random bytes from a file and
both are wrong for anything a person chooses, so a version 2 that derives a KEK
from a passphrase must derive the key and the id from the same KDF output, and
the id must cover the derivation's parameters.
