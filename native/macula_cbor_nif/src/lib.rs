//! CBOR pack/unpack NIF for Macula mesh wire protocol.
//!
//! Replaces the pure-Erlang `msgpack` hex package as the per-frame
//! serialization layer. CBOR (RFC 8949) is the long-term wire format
//! for Macula because:
//!
//! - It composes with UCAN, DID, COSE, and IPLD which the platform
//!   already uses for identity and auth.
//! - It has deterministic encoding rules (RFC 8949 §4.2.1) which
//!   matter for signed payloads.
//! - It is an IETF standard with stable governance.
//!
//! Wire-format conversion (Erlang term <-> CBOR Value):
//!
//!   Atom              -> Text string  (atoms are stringly-typed in BEAM
//!                                       wire protocols; we lose atom-vs-binary
//!                                       distinction across the wire, same as
//!                                       msgpack did)
//!   Binary            -> Byte string  (preserved as bytes; readers expecting
//!                                       a binary still get a binary)
//!   Integer           -> Integer     (CBOR uint or negative-int as appropriate)
//!   Float             -> Float
//!   Boolean (atom)    -> Bool        (true/false)
//!   Atom 'undefined'  -> Null
//!   Atom 'nil'        -> Null
//!   List              -> Array
//!   Tuple             -> Array        (BEAM tuples lose their tuple-ness;
//!                                       use lists when arity matters)
//!   Map               -> Map          (keys can be any encodable term)
//!
//! Decoding is symmetric except text strings always come back as binaries
//! (the encoder cannot reliably round-trip atoms — same constraint as msgpack).

use ciborium::value::Value;
use rustler::types::atom::Atom as ErlAtom;
use rustler::types::map::MapIterator;
use rustler::{Binary, Encoder, Env, NifResult, OwnedBinary, Term, TermType};

mod atoms {
    rustler::atoms! {
        ok,
        error,
        // Common BEAM atoms we round-trip with semantic meaning.
        nil,
        undefined,
        true_ = "true",
        false_ = "false",
    }
}

#[rustler::nif]
fn nif_pack<'a>(env: Env<'a>, term: Term<'a>) -> NifResult<Binary<'a>> {
    let value = term_to_value(term)?;
    let mut buf: Vec<u8> = Vec::with_capacity(64);
    ciborium::ser::into_writer(&value, &mut buf)
        .map_err(|e| rustler::Error::Term(Box::new(format!("cbor encode: {}", e))))?;
    let mut out = OwnedBinary::new(buf.len()).ok_or_else(|| {
        rustler::Error::Term(Box::new("cbor: failed to allocate output binary"))
    })?;
    out.as_mut_slice().copy_from_slice(&buf);
    Ok(out.release(env))
}

#[rustler::nif]
fn nif_unpack<'a>(env: Env<'a>, bytes: Binary<'a>) -> NifResult<(ErlAtom, Term<'a>)> {
    let value: Value = match ciborium::de::from_reader(bytes.as_slice()) {
        Ok(v) => v,
        Err(e) => {
            let err = format!("{}", e);
            let err_term = err.as_str().encode(env);
            return Ok((atoms::error(), err_term));
        }
    };
    let term = value_to_term(env, &value)?;
    Ok((atoms::ok(), term))
}

// ---------- Encode: Erlang Term -> ciborium Value ----------

fn term_to_value(term: Term) -> Result<Value, rustler::Error> {
    match term.get_type() {
        TermType::Atom => atom_to_value(term),
        TermType::Binary => {
            let bin: Binary = term.decode()?;
            Ok(Value::Bytes(bin.as_slice().to_vec()))
        }
        TermType::Integer => {
            // Try i128 first (handles both small and very large ints up to 64-bit safely).
            if let Ok(i) = term.decode::<i64>() {
                Ok(Value::Integer(i.into()))
            } else if let Ok(u) = term.decode::<u64>() {
                Ok(Value::Integer(u.into()))
            } else {
                Err(rustler::Error::Term(Box::new(
                    "cbor: integer outside i64/u64 range not supported",
                )))
            }
        }
        TermType::Float => {
            let f: f64 = term.decode()?;
            Ok(Value::Float(f))
        }
        TermType::List => {
            let list: Vec<Term> = term.decode()?;
            let mut arr = Vec::with_capacity(list.len());
            for item in list {
                arr.push(term_to_value(item)?);
            }
            Ok(Value::Array(arr))
        }
        TermType::Tuple => {
            // Tuples lose tuple-ness — encode as array. Document at module level.
            let tup = rustler::types::tuple::get_tuple(term)?;
            let mut arr = Vec::with_capacity(tup.len());
            for item in tup {
                arr.push(term_to_value(item)?);
            }
            Ok(Value::Array(arr))
        }
        TermType::Map => {
            let mut entries: Vec<(Value, Value)> = Vec::new();
            let iter = MapIterator::new(term).ok_or_else(|| {
                rustler::Error::Term(Box::new("cbor: failed to iterate map"))
            })?;
            for (k, v) in iter {
                entries.push((term_to_value(k)?, term_to_value(v)?));
            }
            Ok(Value::Map(entries))
        }
        _ => Err(rustler::Error::Term(Box::new(
            "cbor: unsupported term type (pid/ref/port/fun)",
        ))),
    }
}

fn atom_to_value(term: Term) -> Result<Value, rustler::Error> {
    if term == atoms::true_().to_term(term.get_env()) {
        return Ok(Value::Bool(true));
    }
    if term == atoms::false_().to_term(term.get_env()) {
        return Ok(Value::Bool(false));
    }
    if term == atoms::nil().to_term(term.get_env())
        || term == atoms::undefined().to_term(term.get_env())
    {
        return Ok(Value::Null);
    }
    // Other atoms: encode as text string. Lossy on round-trip (decoder
    // returns binary), matching msgpack-era semantics.
    let s: String = term.atom_to_string()?;
    Ok(Value::Text(s))
}

// ---------- Decode: ciborium Value -> Erlang Term ----------

fn value_to_term<'a>(env: Env<'a>, value: &Value) -> NifResult<Term<'a>> {
    match value {
        Value::Null => Ok(atoms::nil().to_term(env)),
        Value::Bool(b) => Ok(b.encode(env)),
        Value::Integer(i) => {
            // ciborium::value::Integer: try to fit into i64, then u64.
            if let Ok(n) = i64::try_from(*i) {
                Ok(n.encode(env))
            } else if let Ok(n) = u64::try_from(*i) {
                Ok(n.encode(env))
            } else {
                Err(rustler::Error::Term(Box::new(
                    "cbor: integer too large for i64/u64 decode",
                )))
            }
        }
        Value::Float(f) => Ok(f.encode(env)),
        Value::Text(s) => {
            // Text strings always come back as binaries (lossy round-trip
            // on atoms — same as msgpack-era behavior).
            let bytes = s.as_bytes();
            let mut out = OwnedBinary::new(bytes.len()).ok_or_else(|| {
                rustler::Error::Term(Box::new("cbor: failed to allocate text->binary"))
            })?;
            out.as_mut_slice().copy_from_slice(bytes);
            Ok(out.release(env).to_term(env))
        }
        Value::Bytes(b) => {
            let mut out = OwnedBinary::new(b.len()).ok_or_else(|| {
                rustler::Error::Term(Box::new("cbor: failed to allocate bytes->binary"))
            })?;
            out.as_mut_slice().copy_from_slice(b);
            Ok(out.release(env).to_term(env))
        }
        Value::Array(arr) => {
            let mut items = Vec::with_capacity(arr.len());
            for v in arr {
                items.push(value_to_term(env, v)?);
            }
            Ok(items.encode(env))
        }
        Value::Map(entries) => {
            let mut keys: Vec<Term> = Vec::with_capacity(entries.len());
            let mut vals: Vec<Term> = Vec::with_capacity(entries.len());
            for (k, v) in entries {
                keys.push(value_to_term(env, k)?);
                vals.push(value_to_term(env, v)?);
            }
            Term::map_from_arrays(env, &keys, &vals)
                .map_err(|_| rustler::Error::Term(Box::new("cbor: map_from_arrays failed")))
        }
        Value::Tag(_, inner) => {
            // Tags are CBOR's typed-data extension. We don't surface them
            // as tuples; pass through the inner value. If/when callers need
            // tag inspection, expand this branch.
            value_to_term(env, inner)
        }
        _ => Err(rustler::Error::Term(Box::new(
            "cbor: unsupported value variant",
        ))),
    }
}

rustler::init!("macula_cbor_nif");
