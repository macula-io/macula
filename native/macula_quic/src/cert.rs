//! A station's TLS certificate: self-signed ML-DSA-87 on its TLS key, made by
//! `macula-pqc` (plan decision D12).
//!
//! The TLS key is a node key of purpose `tls`, kept as its 32-byte seed (D6),
//! and used for nothing but the TLS handshake. The certificate wraps it and
//! nothing else: no chain, no authority, since none issues ML-DSA
//! certificates. A client does not trust a station by its certificate. It
//! trusts the station's handshake signature under the certificate's key
//! (`config::client_tls_config`), and the station's identity through the
//! binding its identity key makes over this TLS key, checked in the
//! connection handshake.

use rustler::{Binary, Encoder, Env, NifResult, OwnedBinary, Term};
use rustls::pki_types::{CertificateDer, PrivatePkcs8KeyDer};

use crate::atoms;

/// A self-signed ML-DSA-87 certificate for the key `seed` derives, naming
/// `sans`, and that key as PKCS#8 in RFC 9881's seed form. Each SAN that
/// parses as an IP address is an IP address SAN, any other a DNS name.
pub fn generate_self_signed(
    seed: &[u8],
    sans: &[String],
) -> Result<(CertificateDer<'static>, PrivatePkcs8KeyDer<'static>), String> {
    let seed: &[u8; 32] = seed
        .try_into()
        .map_err(|_| format!("a TLS key seed is 32 bytes, got {}", seed.len()))?;
    macula_pqc::self_signed_certificate(seed, sans.to_vec())
        .map_err(|e| format!("self-signed certificate: {}", e))
}

// ─────────────────────────────────────────────────────────────────
// NIF: generate_self_signed_cert(Seed, Sans) ->
//       {ok, {CertDer, KeyDer}} | {error, Reason}
// ─────────────────────────────────────────────────────────────────

/// Sans is passed as a single comma-joined binary on the Erlang side: a flat
/// string sidesteps rustler's list-of-binary decoding. Seed is a
/// `rustler::Binary` because rustler's `Vec<u8>` decoder takes a list, not
/// a binary.
#[rustler::nif(schedule = "DirtyCpu")]
pub fn nif_generate_self_signed_cert<'a>(
    env: Env<'a>,
    seed: Binary<'a>,
    sans_csv: Binary<'a>,
) -> NifResult<Term<'a>> {
    let sans: Vec<String> = std::str::from_utf8(sans_csv.as_slice())
        .unwrap_or_default()
        .split(',')
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .collect();
    match generate_self_signed(seed.as_slice(), &sans) {
        Ok((certificate, key)) => Ok((
            atoms::ok(),
            (
                der_binary(env, &certificate),
                der_binary(env, key.secret_pkcs8_der()),
            ),
        )
            .encode(env)),
        Err(e) => Ok((atoms::error(), e).encode(env)),
    }
}

fn der_binary<'a>(env: Env<'a>, der: &[u8]) -> Binary<'a> {
    let mut binary = OwnedBinary::new(der.len()).expect("allocate a DER binary");
    binary.as_mut_slice().copy_from_slice(der);
    binary.release(env)
}
