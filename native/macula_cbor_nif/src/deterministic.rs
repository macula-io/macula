//! Deterministic CBOR (RFC 8949 §4.2.1) encode/decode, mirroring
//! `macula_record_cbor.erl` byte-for-byte. This is a SEPARATE code path
//! from `nif_pack`/`nif_unpack` above — those go through
//! `ciborium::value::Value`, a generic and non-deterministic
//! representation (no canonical map-key order, no forced integer/float
//! widths). This module bypasses `ciborium` entirely and operates
//! directly on `rustler::Term`, because the value model the wire
//! protocol actually needs is CBOR-specific:
//!
//!   - non-negative integer  -> uint (major 0), minimal-length encoding
//!   - negative integer      -> major 1, encoded count = `-1 - N`
//!   - binary                -> byte string (major 2)
//!   - `{text, Binary}`      -> UTF-8 text string (major 3), bytes used
//!                              AS-IS, no UTF-8 validation (matches the
//!                              Erlang encoder exactly)
//!   - atom (not `null`)     -> UTF-8 text string (major 3) via the
//!                              atom's own utf8 name — ENCODE ONLY;
//!                              decode never produces a bare atom here
//!   - list                  -> array (major 4)
//!   - map                   -> map (major 5), keys sorted by the
//!                              bytewise order of their OWN encoded
//!                              bytes
//!   - atom `null`           -> simple null (major 7, 0xF6)
//!   - float                 -> ALWAYS binary64 (major 7, AI 27) on
//!                              encode; decode accepts binary16/32/64
//!
//! Every function here is on the hot path for untrusted,
//! network-received bytes (`nif_unpack_deterministic` decodes bytes a
//! remote peer sent). It must never panic: a malformed or truncated
//! frame has to come back as `Err`, not crash the NIF (and therefore
//! the whole BEAM VM). No `unwrap`/`expect`/slice-index-that-can-panic
//! on attacker-controlled data anywhere in this file — every bounds
//! check is explicit.

use rustler::types::tuple::get_tuple;
use rustler::{Binary, Encoder, Env, NifResult, OwnedBinary, Term, TermType};

mod atoms {
    rustler::atoms! {
        text,
        null,
    }
}

// ==================================================================
// Encode: Erlang Term -> deterministic CBOR bytes
// ==================================================================

pub fn encode<'a>(env: Env<'a>, term: Term<'a>) -> NifResult<Binary<'a>> {
    let mut buf: Vec<u8> = Vec::with_capacity(64);
    encode_value(env, term, &mut buf)?;
    let mut out = OwnedBinary::new(buf.len()).ok_or_else(|| {
        rustler::Error::RaiseTerm(Box::new("cbor: failed to allocate output binary"))
    })?;
    out.as_mut_slice().copy_from_slice(&buf);
    Ok(out.release(env))
}

fn encode_value<'a>(env: Env<'a>, term: Term<'a>, out: &mut Vec<u8>) -> NifResult<()> {
    match term.get_type() {
        TermType::Integer | TermType::Float => encode_number(term, out),
        TermType::Binary => {
            let bin: Binary = term.decode()?;
            encode_head(2, bin.as_slice().len() as u64, out);
            out.extend_from_slice(bin.as_slice());
            Ok(())
        }
        TermType::List => {
            let items: Vec<Term> = term.decode().map_err(|_| {
                rustler::Error::RaiseTerm(Box::new("cbor: improper list not encodable"))
            })?;
            encode_head(4, items.len() as u64, out);
            for item in items {
                encode_value(env, item, out)?;
            }
            Ok(())
        }
        TermType::Tuple => encode_tuple(env, term, out),
        TermType::Map => encode_map(env, term, out),
        TermType::Atom => encode_atom(env, term, out),
        _ => Err(rustler::Error::RaiseTerm(Box::new(
            "cbor: unsupported term type (pid/ref/port/fun/bitstring)",
        ))),
    }
}

fn encode_number(term: Term, out: &mut Vec<u8>) -> NifResult<()> {
    // rustler's numeric decoders are exact and tag-based, not coercive:
    // an integer term never succeeds `decode::<f64>()` (there is no
    // BEAM "integer-valued float" — a term is tagged as one or the
    // other at construction), so trying the integer decoders first is
    // sufficient to disambiguate; a term that fails both must be a
    // float. Integers are tried as u64/i128 rather than f64 first,
    // since decoding a large integer as f64 would silently lose
    // precision instead of erroring.
    if let Ok(u) = term.decode::<u64>() {
        encode_head(0, u, out);
        return Ok(());
    }
    // i128 (unconditionally available in rustler 0.34, not gated
    // behind the `big_integer` feature) covers the full negative range
    // `is_encodable_int/1` declares — down to `-(MAX_UINT64 + 1)` —
    // which plain i64 does not (i64::MIN is only -2^63, one bit short
    // of the declared -2^64 floor).
    if let Ok(i) = term.decode::<i128>() {
        if i < 0 {
            let count = -1i128 - i; // i in -(2^64)..=-1 -> count in 0..=2^64-1
            if let Ok(count_u64) = u64::try_from(count) {
                encode_head(1, count_u64, out);
                return Ok(());
            }
        }
        // i >= 0 here only if it exceeds u64::MAX (the u64 branch above
        // would have already caught it otherwise) — falls through to
        // the bignum-error case below, matching `is_encodable_int/1`'s
        // upper bound.
    }
    if let Ok(f) = term.decode::<f64>() {
        out.push(0xFB); // major 7, AI 27 = <<7:3,27:5>>
        out.extend_from_slice(&f.to_be_bytes());
        return Ok(());
    }
    // Outside the declared i64/u64-plus-sign-extension range, or a
    // true bignum beyond even i128. `macula_record_cbor:encode/1` also
    // has no matching clause for a value this large, so an explicit
    // error here matches its behavior rather than an unbounded
    // conversion or a panic.
    Err(rustler::Error::RaiseTerm(Box::new(
        "cbor: integer outside encodable range",
    )))
}

fn encode_atom<'a>(env: Env<'a>, term: Term<'a>, out: &mut Vec<u8>) -> NifResult<()> {
    if term == atoms::null().to_term(env) {
        out.push(0xF6); // major 7, AI 22
        return Ok(());
    }
    let s: String = term.atom_to_string()?;
    let bytes = s.as_bytes();
    encode_head(3, bytes.len() as u64, out);
    out.extend_from_slice(bytes);
    Ok(())
}

fn encode_tuple<'a>(env: Env<'a>, term: Term<'a>, out: &mut Vec<u8>) -> NifResult<()> {
    let elems = get_tuple(term)?;
    if elems.len() == 2 && elems[0].get_type() == TermType::Atom && elems[0] == atoms::text().to_term(env)
    {
        let bin: Binary = elems[1].decode().map_err(|_| {
            rustler::Error::RaiseTerm(Box::new("cbor: {text, _} second element must be a binary"))
        })?;
        encode_head(3, bin.as_slice().len() as u64, out);
        out.extend_from_slice(bin.as_slice());
        return Ok(());
    }
    // `macula_record_cbor:encode/1` has no clause for a bare tuple that
    // isn't `{text, Binary}` — matching that with an explicit error.
    Err(rustler::Error::RaiseTerm(Box::new(
        "cbor: tuple is not {text, Binary}, not encodable",
    )))
}

fn encode_map<'a>(env: Env<'a>, term: Term<'a>, out: &mut Vec<u8>) -> NifResult<()> {
    let iter = rustler::types::map::MapIterator::new(term)
        .ok_or_else(|| rustler::Error::RaiseTerm(Box::new("cbor: failed to iterate map")))?;
    let mut pairs: Vec<(Vec<u8>, Vec<u8>)> = Vec::new();
    for (k, v) in iter {
        let mut kbuf = Vec::with_capacity(16);
        encode_value(env, k, &mut kbuf)?;
        let mut vbuf = Vec::with_capacity(16);
        encode_value(env, v, &mut vbuf)?;
        pairs.push((kbuf, vbuf));
    }
    // Bytewise lexicographic order of the encoded key, shorter-is-
    // smaller-when-a-prefix — exactly `Vec<u8>`'s own `Ord`, and
    // exactly what Erlang's `lists:sort/1` does for binaries.
    pairs.sort_by(|a, b| a.0.cmp(&b.0));
    encode_head(5, pairs.len() as u64, out);
    for (k, v) in pairs {
        out.extend_from_slice(&k);
        out.extend_from_slice(&v);
    }
    Ok(())
}

// ==================================================================
// Decode: deterministic CBOR bytes -> Erlang Term
//
// Parses untrusted, network-received bytes. Every read is bounds-
// checked before it happens; nothing here may panic on malformed or
// truncated input. `macula_record_cbor:decode/1` crashes (badmatch /
// no matching function clause) on anything it can't parse, and the
// SDK relies on that: `macula_frame:decode_cbor/2` wraps the call in
// try/catch and turns ANY exception into `{error, bad_frame}`. This
// NIF cannot rely on a catch — a panic here would take down the whole
// BEAM VM — so every one of those "no matching clause" cases becomes
// an explicit `Err` instead.
// ==================================================================

fn err(msg: &'static str) -> rustler::Error {
    rustler::Error::RaiseTerm(Box::new(msg))
}

// Recursive-descent nesting limit. Without it, a list-of-one-list-of-
// one-list... encodes extreme nesting in one byte per level (0x81
// repeated), and this decoder is plain recursive descent -- a crafted
// frame well under the 16MB cap can overflow the stack and crash the
// whole process (this runs as a NIF directly on a BEAM scheduler
// thread, so it takes the whole VM down, not just one connection).
// Confirmed empirically: a chain of 10,000 nested one-element arrays
// (10,001 bytes) segfaulted this NIF before this limit existed. No real
// macula wire value nests remotely this deep.
const MAX_NESTING_DEPTH: usize = 128;

pub fn decode<'a>(env: Env<'a>, bytes: Binary<'a>) -> NifResult<Term<'a>> {
    let len = bytes.as_slice().len();
    // Nobody consumes the top-level value's own canonical bytes -- start
    // with need_canon=false (see decode_one's doc for what that skips).
    let (term, _canon, pos) = decode_one(env, bytes, 0, 0, false)?;
    if pos != len {
        // `macula_record_cbor:decode/1` requires `{V, <<>>} = decode_one(Bin)`
        // — trailing bytes after the top-level value is a badmatch there.
        return Err(err("cbor: trailing bytes after top-level value"));
    }
    Ok(term)
}

fn need(buf: &[u8], pos: usize, n: usize) -> NifResult<()> {
    if pos.checked_add(n).map(|end| end <= buf.len()) == Some(true) {
        Ok(())
    } else {
        Err(err("cbor: truncated input"))
    }
}

// Decodes one value and — only when `need_canon` is true — its own
// canonical (deterministic-CBOR) bytes, built bottom-up as decoding
// proceeds rather than re-derived by a separate `encode_value` call
// afterward. `decode_map` needs a key's canonical bytes as its dedup
// identity (see that function's doc); re-encoding a key from scratch at
// every ancestor level (an earlier version of this fix) costs O(depth x
// key size) for a map nested inside another map's key, which a deep
// enough chain turns back into real, measured seconds of pre-auth CPU.
// Building bytes bottom-up instead means each value is encoded exactly
// once, then only ever concatenated (arrays) or sorted+concatenated
// (maps, matching `encode_map`'s own key-sort rule) by its ancestors.
//
// `need_canon` exists because computing these bytes for every value
// regardless of whether anything ever reads them is itself a real,
// separate cost: most decoded values are never used as a map key
// anywhere, and a large value that never touches one shouldn't pay for
// canon-bytes it doesn't need. `decode_map` is the only caller that
// ever passes different values for its two child decodes: always `true`
// for a key (dedup needs it unconditionally) and its own `need_canon`
// for a value (only needed if this whole map is itself nested inside
// some ancestor's key).
//
// `orig` (the whole input binary, threaded through every recursive call)
// is what makes `make_subbinary` possible below: byte-string and
// text-string decoding take a zero-copy reference into `orig` instead of
// allocating a fresh `OwnedBinary` and copying — the same thing Erlang's
// own `<<B:Len/binary, Rest/binary>> = R` pattern match does for a refc
// binary. Measured: without this, native decode was *slower* than the
// pure-Erlang reference implementation (15-43% depending on payload
// shape) specifically because it was paying a real allocation+copy on
// every binary/text field where Erlang pays neither.
fn decode_one<'a>(
    env: Env<'a>,
    orig: Binary<'a>,
    pos: usize,
    depth: usize,
    need_canon: bool,
) -> NifResult<(Term<'a>, Vec<u8>, usize)> {
    if depth > MAX_NESTING_DEPTH {
        return Err(err("cbor: nesting exceeds 128 levels"));
    }
    let buf = orig.as_slice();
    need(buf, pos, 1)?;
    let byte0 = buf[pos];
    let major = byte0 >> 5;
    let ai = byte0 & 0x1F;

    if major == 7 {
        let (term, next) = decode_major7(env, buf, pos, ai)?;
        return scalar_canon(env, term, next, need_canon);
    }

    let (n, next) = decode_count(buf, pos + 1, ai)?;
    match major {
        0 => scalar_canon(env, n.encode(env), next, need_canon),
        1 => {
            // actual value = -1 - n. n fits in u64 (up to 2^64-1), so
            // -1-n can be as low as -2^64, below i64::MIN — i128's
            // Encoder (unconditionally available in rustler 0.34)
            // handles this correctly, falling back to Erlang's bignum
            // external term format for anything i64 can't hold.
            let neg: i128 = -1i128 - n as i128;
            scalar_canon(env, neg.encode(env), next, need_canon)
        }
        2 => {
            let len = n as usize;
            need(buf, next, len)?;
            let sub = orig
                .make_subbinary(next, len)
                .map_err(|_| err("cbor: failed to slice binary"))?;
            scalar_canon(env, sub.to_term(env), next + len, need_canon)
        }
        3 => {
            let len = n as usize;
            need(buf, next, len)?;
            let sub = orig
                .make_subbinary(next, len)
                .map_err(|_| err("cbor: failed to slice binary"))?;
            let tuple = (atoms::text().to_term(env), sub.to_term(env));
            scalar_canon(env, tuple.encode(env), next + len, need_canon)
        }
        4 => decode_array(env, orig, next, n, depth + 1, need_canon),
        5 => decode_map(env, orig, next, n, depth + 1, need_canon),
        _ => Err(err("cbor: major type 6 (tags) not supported")),
    }
}

// A scalar's own canonical bytes, computed via `encode_value` — or
// skipped (an empty `Vec`) when nothing will ever read them, see
// `decode_one`'s `need_canon` doc. Cheap regardless of where in a
// nested structure it's called from: a scalar's canonical bytes are
// just its own `encode_value` output, never a recursive re-derivation
// of a child's bytes the way `decode_array`/`decode_map` build theirs.
fn scalar_canon<'a>(
    env: Env<'a>,
    term: Term<'a>,
    next: usize,
    need_canon: bool,
) -> NifResult<(Term<'a>, Vec<u8>, usize)> {
    if !need_canon {
        return Ok((term, Vec::new(), next));
    }
    let mut canon = Vec::with_capacity(16);
    encode_value(env, term, &mut canon)?;
    Ok((term, canon, next))
}

fn decode_count(buf: &[u8], pos: usize, ai: u8) -> NifResult<(u64, usize)> {
    match ai {
        0..=23 => Ok((ai as u64, pos)),
        24 => {
            need(buf, pos, 1)?;
            Ok((buf[pos] as u64, pos + 1))
        }
        25 => {
            need(buf, pos, 2)?;
            let mut b = [0u8; 2];
            b.copy_from_slice(&buf[pos..pos + 2]);
            Ok((u16::from_be_bytes(b) as u64, pos + 2))
        }
        26 => {
            need(buf, pos, 4)?;
            let mut b = [0u8; 4];
            b.copy_from_slice(&buf[pos..pos + 4]);
            Ok((u32::from_be_bytes(b) as u64, pos + 4))
        }
        27 => {
            need(buf, pos, 8)?;
            let mut b = [0u8; 8];
            b.copy_from_slice(&buf[pos..pos + 8]);
            Ok((u64::from_be_bytes(b), pos + 8))
        }
        _ => Err(err("cbor: additional info 28-31 not supported")),
    }
}

fn decode_major7<'a>(env: Env<'a>, buf: &[u8], pos: usize, ai: u8) -> NifResult<(Term<'a>, usize)> {
    match ai {
        22 => Ok((atoms::null().to_term(env), pos + 1)),
        25 => {
            need(buf, pos + 1, 2)?;
            let mut b = [0u8; 2];
            b.copy_from_slice(&buf[pos + 1..pos + 3]);
            let f = half_to_f64(u16::from_be_bytes(b))?;
            Ok((f.encode(env), pos + 3))
        }
        26 => {
            need(buf, pos + 1, 4)?;
            let mut b = [0u8; 4];
            b.copy_from_slice(&buf[pos + 1..pos + 5]);
            let f = f32::from_be_bytes(b) as f64;
            // NaN/Infinity have no BEAM float representation (same
            // reason half_to_f64 rejects binary16 exp=31 below) — reject
            // here too, not just for binary16. Found via adversarial
            // review: `f.encode(env)` on a non-finite value still
            // "succeeds" (BEAM has no validity check at construction),
            // producing an invalid float term that only crashes later
            // when something actually inspects it — `encode_value` does
            // exactly that when this term is used as a map key (for its
            // canonical-bytes dedup identity), aborting the whole VM
            // with an ERTS assertion failure, uncatchable by try/catch.
            // Confirmed empirically pre-fix: a NaN-float64 map key
            // reliably aborts the process (`tag_val_def()` assertion,
            // SIGABRT) instead of the clean `{error, badarg}` a NaN
            // value (not key) already got.
            if !f.is_finite() {
                return Err(err("cbor: float32 NaN/infinity not representable"));
            }
            Ok((f.encode(env), pos + 5))
        }
        27 => {
            need(buf, pos + 1, 8)?;
            let mut b = [0u8; 8];
            b.copy_from_slice(&buf[pos + 1..pos + 9]);
            let f = f64::from_be_bytes(b);
            // See the ai=26 arm above — same non-finite rejection, same
            // reason.
            if !f.is_finite() {
                return Err(err("cbor: float64 NaN/infinity not representable"));
            }
            Ok((f.encode(env), pos + 9))
        }
        // `macula_record_cbor:decode_one/1` has no clause for any other
        // major-7 additional info (booleans at AI 20/21, undefined at
        // AI 23, etc.) — this codec's value model has no representation
        // for them, matching that with an explicit error.
        _ => Err(err("cbor: unsupported major-7 additional info")),
    }
}

fn decode_array<'a>(
    env: Env<'a>,
    orig: Binary<'a>,
    mut pos: usize,
    count: u64,
    depth: usize,
    need_canon: bool,
) -> NifResult<(Term<'a>, Vec<u8>, usize)> {
    let mut items: Vec<Term<'a>> = Vec::with_capacity(count.min(1024) as usize);
    let mut canon = Vec::new();
    if need_canon {
        encode_head(4, count, &mut canon);
    }
    for _ in 0..count {
        let (item, item_canon, next) = decode_one(env, orig, pos, depth, need_canon)?;
        if need_canon {
            canon.extend_from_slice(&item_canon);
        }
        items.push(item);
        pos = next;
    }
    Ok((items.encode(env), canon, pos))
}

// Duplicate keys overwrite (last write wins), matching Erlang's
// `Acc#{K => V}` in `decode_map/3` exactly — not an error.
//
// Dedup is keyed on each key's own canonical bytes — computed once, by
// `decode_one`, as part of decoding that key (see its doc) — not a
// linear `Term` equality scan: a linear scan makes this function O(n^2)
// in the key count, a pre-auth algorithmic-complexity DoS reachable
// during frame decode, before any signature check. Measured on the
// equivalent Rust pattern (macula-rust, same decoder shape): a map with
// 80,000 distinct keys, 348KB on the wire (well under the 16MB frame
// cap), took 52.3s to decode, scaling quadratically.
//
// An earlier version of this fix looked up each key by calling
// `encode_value` on it fresh, per entry, instead of reusing the bytes
// `decode_one` already built while decoding that same key. That's sound
// for a flat map (fixed the case above), but reintroduces unbounded
// work for a map whose KEY is itself a large nested structure:
// re-encoding a key from scratch at every ancestor level costs O(depth x
// key size), and a 128-level chain of single-entry maps
// (MAX_NESTING_DEPTH) each keyed by a large blob turns back into real
// seconds of pre-auth CPU on a frame still under the size cap — the same
// bug macula-rust's own adversarial review caught in its first attempt
// at this exact fix. Building canonical bytes bottom-up instead (this
// version) bounds it to O(depth x size), the same bound
// MAX_NESTING_DEPTH already enforces elsewhere — not O(total input size)
// regardless of nesting shape.
//
// One documented behavioral change from a literal `Term`-equality scan:
// two float keys holding bit-identical NaN payloads now dedupe (they
// never did under Term's PartialEq, since NaN != NaN); no real macula
// wire value uses a float as a map key.
fn decode_map<'a>(
    env: Env<'a>,
    orig: Binary<'a>,
    mut pos: usize,
    count: u64,
    depth: usize,
    need_canon: bool,
) -> NifResult<(Term<'a>, Vec<u8>, usize)> {
    let capacity = count.min(1024) as usize;
    let mut pairs: Vec<(Term<'a>, Term<'a>)> = Vec::with_capacity(capacity);
    // Owns each distinct key's canonical bytes -> its slot index into
    // `pairs`/`vals_canon`.
    let mut index_of_key: std::collections::HashMap<Vec<u8>, usize> =
        std::collections::HashMap::with_capacity(capacity);
    // Per-slot VALUE canon, kept in step with `pairs` (same index, same
    // last-write-wins updates) — only populated when `need_canon`, since
    // a key's canon (owned by `index_of_key` above) is the only one ever
    // needed just to make dedup itself work.
    let mut vals_canon: Vec<Vec<u8>> = Vec::with_capacity(if need_canon { capacity } else { 0 });
    for _ in 0..count {
        // A key ALWAYS needs its canon bytes — that's the dedup identity
        // itself, independent of whether this map's OWN canon bytes
        // (built below) are ever going to be read by anything.
        let (k, key_canon, next1) = decode_one(env, orig, pos, depth, true)?;
        let (v, val_canon, next2) = decode_one(env, orig, next1, depth, need_canon)?;
        pos = next2;
        use std::collections::hash_map::Entry;
        match index_of_key.entry(key_canon) {
            Entry::Occupied(e) => {
                let i = *e.get();
                pairs[i].1 = v;
                if need_canon {
                    vals_canon[i] = val_canon;
                }
            }
            Entry::Vacant(e) => {
                e.insert(pairs.len());
                pairs.push((k, v));
                if need_canon {
                    vals_canon.push(val_canon);
                }
            }
        }
    }
    let keys: Vec<Term<'a>> = pairs.iter().map(|(k, _)| *k).collect();
    let vals: Vec<Term<'a>> = pairs.iter().map(|(_, v)| *v).collect();
    let map_term = Term::map_from_arrays(env, &keys, &vals)
        .map_err(|_| err("cbor: map_from_arrays failed"))?;
    if !need_canon {
        return Ok((map_term, Vec::new(), pos));
    }
    // Matches `encode_map`'s own rule exactly: sort entries by the key's
    // encoded bytes, plain lexicographic `Ord` on `Vec<u8>`.
    let mut order: Vec<(&Vec<u8>, usize)> = index_of_key.iter().map(|(k, &i)| (k, i)).collect();
    order.sort_by(|a, b| a.0.cmp(b.0));
    let mut canon = Vec::new();
    encode_head(5, order.len() as u64, &mut canon);
    for (k, i) in order {
        canon.extend_from_slice(k);
        canon.extend_from_slice(&vals_canon[i]);
    }
    Ok((map_term, canon, pos))
}

// IEEE 754 binary16 -> f64, mirroring `macula_record_cbor:half_to_float/1`
// exactly: subnormals (Exp=0) and normals (1<=Exp<=30) use the same
// arithmetic; Exp=31 (NaN/infinity) has no Erlang float representation
// and is an explicit error here (Erlang: no matching clause).
fn half_to_f64(half: u16) -> NifResult<f64> {
    let sign: f64 = if (half >> 15) & 1 == 1 { -1.0 } else { 1.0 };
    let exp = (half >> 10) & 0x1F;
    let frac = (half & 0x3FF) as f64;
    match exp {
        0 => Ok(sign * 2f64.powi(-14) * (frac / 1024.0)),
        1..=30 => Ok(sign * 2f64.powi(exp as i32 - 15) * (1.0 + frac / 1024.0)),
        _ => Err(err("cbor: half-float NaN/infinity not representable")),
    }
}

fn encode_head(major_type: u8, n: u64, out: &mut Vec<u8>) {
    if n <= 23 {
        out.push((major_type << 5) | (n as u8));
    } else if n <= 0xFF {
        out.push((major_type << 5) | 24);
        out.push(n as u8);
    } else if n <= 0xFFFF {
        out.push((major_type << 5) | 25);
        out.extend_from_slice(&(n as u16).to_be_bytes());
    } else if n <= 0xFFFF_FFFF {
        out.push((major_type << 5) | 26);
        out.extend_from_slice(&(n as u32).to_be_bytes());
    } else {
        out.push((major_type << 5) | 27);
        out.extend_from_slice(&n.to_be_bytes());
    }
}
