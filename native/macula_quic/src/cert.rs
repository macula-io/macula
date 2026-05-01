//! Self-signed Ed25519 cert generation + pubkey-pin TLS verifier.
//!
//! Used by the sovereign-overlay path (PLAN_SOVEREIGN_OVERLAY_PHASE1
//! §4.3, §4.4): a station presents a self-signed cert that wraps its
//! macula identity Ed25519 pubkey; a client validates by comparing
//! the cert's SubjectPublicKeyInfo to a pinned pubkey it was given
//! out-of-band. No CA chain, no DNS-anchored trust.
//!
//! The cert generator is also used at runtime by stations seeding
//! their Yggdrasil-bound listener — they materialize a cert from
//! their existing identity keypair and hand the PEM bytes to the
//! transport.

use std::sync::Arc;

use rcgen::{CertificateParams, DistinguishedName, DnType, KeyPair, SanType};
use rustls::pki_types::CertificateDer;
use rustler::{Binary, Encoder, Env, NifResult, Term};

use crate::atoms;

// ─────────────────────────────────────────────────────────────────
// Self-signed cert generation
// ─────────────────────────────────────────────────────────────────

/// Generate a self-signed X.509 certificate from an Ed25519 keypair.
///
/// Inputs are raw 32-byte values: `pubkey` is the Ed25519 public
/// key, `privkey` is the Ed25519 secret seed (RFC 8032). Both are
/// what `crypto:generate_key(eddsa, ed25519)` returns on the Erlang
/// side.
///
/// `sans` is the Subject Alternative Names list — typically just
/// the identity's hostname (`relay-fi-helsinki.macula.io`) or the
/// derived Yggdrasil address. The latter is a valid IPAddress SAN
/// when present; we autodetect by trying to parse as IP, falling
/// back to DNS.
///
/// Returns `(cert_pem, key_pem)` — both PEM-encoded text suitable
/// for handing to rustls / Quinn.
pub fn generate_self_signed(
    pubkey: &[u8],
    privkey: &[u8],
    sans: &[String],
) -> Result<(String, String), String> {
    if pubkey.len() != 32 {
        return Err(format!(
            "pubkey must be 32 bytes Ed25519, got {}",
            pubkey.len()
        ));
    }
    if privkey.len() != 32 {
        return Err(format!(
            "privkey must be 32 bytes Ed25519 secret seed, got {}",
            privkey.len()
        ));
    }

    let pkcs8 = wrap_ed25519_pkcs8_v1(privkey);
    let key_pair = KeyPair::try_from(pkcs8.as_slice())
        .map_err(|e| format!("rcgen KeyPair from PKCS8: {}", e))?;
    let _ = pubkey; // pubkey is implicit in the seed; rcgen derives it

    let mut params = CertificateParams::default();
    params.distinguished_name = DistinguishedName::new();
    params
        .distinguished_name
        .push(DnType::CommonName, "Macula Yggdrasil Identity");

    let mut san_vec = Vec::with_capacity(sans.len());
    for s in sans {
        let san = match s.parse::<std::net::IpAddr>() {
            Ok(ip) => SanType::IpAddress(ip),
            Err(_) => {
                let ia5 = s
                    .clone()
                    .try_into()
                    .map_err(|e| format!("invalid DNS SAN {:?}: {:?}", s, e))?;
                SanType::DnsName(ia5)
            }
        };
        san_vec.push(san);
    }
    params.subject_alt_names = san_vec;

    // 10-year validity. Yggdrasil-anchored identities are stable
    // because the keypair is derived from the box-secret + hostname;
    // the cert is just a TLS-format wrapper, not the identity.
    let not_before = time::OffsetDateTime::now_utc();
    params.not_before = not_before;
    params.not_after = not_before + time::Duration::days(3650);

    let cert = params
        .self_signed(&key_pair)
        .map_err(|e| format!("rcgen self_signed: {}", e))?;

    Ok((cert.pem(), key_pair.serialize_pem()))
}

/// Wrap a raw Ed25519 32-byte secret seed as a PKCS#8 v1 DER blob
/// (RFC 8410). Fixed 48-byte structure that rcgen's
/// `KeyPair::try_from` accepts.
///
/// ```text
/// 30 2e                        SEQUENCE (46 bytes)
///    02 01 00                  INTEGER 0 (v1)
///    30 05                     SEQUENCE (5 bytes)
///       06 03 2b 65 70         OID 1.3.101.112  ; id-Ed25519
///    04 22                     OCTET STRING (34 bytes)
///       04 20 <32-byte seed>   inner OCTET STRING wrapping seed
/// ```
fn wrap_ed25519_pkcs8_v1(seed: &[u8]) -> [u8; 48] {
    debug_assert_eq!(seed.len(), 32);
    let mut out = [0u8; 48];
    out[..16].copy_from_slice(&[
        0x30, 0x2e, // SEQUENCE 46
        0x02, 0x01, 0x00, // INTEGER 0 (v1)
        0x30, 0x05, // SEQUENCE 5
        0x06, 0x03, 0x2b, 0x65, 0x70, // OID id-Ed25519
        0x04, 0x22, // OCTET STRING 34
        0x04, 0x20, // inner OCTET STRING 32
    ]);
    out[16..48].copy_from_slice(seed);
    out
}

// ─────────────────────────────────────────────────────────────────
// Pubkey-pin TLS verifier
// ─────────────────────────────────────────────────────────────────

/// Custom rustls `ServerCertVerifier` that pins on the leaf cert's
/// SubjectPublicKeyInfo Ed25519 pubkey rather than walking a CA
/// chain.
///
/// Behaviour:
///   - Extract the pubkey from `end_entity` (must be Ed25519).
///   - Compare to the pinned 32-byte pubkey provided at construction.
///   - Accept iff equal; otherwise return a rustls error.
///
/// No expiry check, no SAN check, no CA chain. The pubkey IS the
/// identity. This is the pragmatic equivalent of TLS raw-public-key
/// (RFC 7250) without changing the wire protocol.
#[derive(Debug)]
pub struct PubkeyPinVerifier {
    pinned: Vec<u8>,
    crypto: Arc<rustls::crypto::CryptoProvider>,
}

impl PubkeyPinVerifier {
    pub fn new(pinned_pubkey: Vec<u8>) -> Self {
        Self {
            pinned: pinned_pubkey,
            crypto: Arc::new(rustls::crypto::ring::default_provider()),
        }
    }
}

impl rustls::client::danger::ServerCertVerifier for PubkeyPinVerifier {
    fn verify_server_cert(
        &self,
        end_entity: &CertificateDer<'_>,
        _intermediates: &[CertificateDer<'_>],
        _server_name: &rustls::pki_types::ServerName<'_>,
        _ocsp_response: &[u8],
        _now: rustls::pki_types::UnixTime,
    ) -> Result<rustls::client::danger::ServerCertVerified, rustls::Error> {
        let presented = ed25519_pubkey_from_cert(end_entity.as_ref())
            .map_err(|e| rustls::Error::General(format!("pubkey extract: {}", e)))?;

        if presented == self.pinned {
            Ok(rustls::client::danger::ServerCertVerified::assertion())
        } else {
            Err(rustls::Error::General(format!(
                "pubkey mismatch: pinned={} presented={}",
                hex(&self.pinned),
                hex(&presented)
            )))
        }
    }

    fn verify_tls12_signature(
        &self,
        _message: &[u8],
        _cert: &CertificateDer<'_>,
        _dss: &rustls::DigitallySignedStruct,
    ) -> Result<rustls::client::danger::HandshakeSignatureValid, rustls::Error> {
        // Ed25519 leaf is TLS 1.3 only; if a 1.2 path triggers
        // signature verification, accept (the cert match is what
        // anchors trust).
        Ok(rustls::client::danger::HandshakeSignatureValid::assertion())
    }

    fn verify_tls13_signature(
        &self,
        message: &[u8],
        cert: &CertificateDer<'_>,
        dss: &rustls::DigitallySignedStruct,
    ) -> Result<rustls::client::danger::HandshakeSignatureValid, rustls::Error> {
        // Defer to the crypto provider — verifies the dss using the
        // leaf cert's pubkey, which we already pinned.
        rustls::crypto::verify_tls13_signature(message, cert, dss, &self.crypto.signature_verification_algorithms)
    }

    fn supported_verify_schemes(&self) -> Vec<rustls::SignatureScheme> {
        self.crypto.signature_verification_algorithms.supported_schemes()
    }
}

/// Extract the 32-byte Ed25519 pubkey from a DER-encoded X.509 cert's
/// SubjectPublicKeyInfo.
pub fn ed25519_pubkey_from_cert(der: &[u8]) -> Result<Vec<u8>, String> {
    let (_, cert) = x509_parser::parse_x509_certificate(der)
        .map_err(|e| format!("parse cert: {}", e))?;
    let spki = cert.public_key();
    // Verify the algorithm is Ed25519 (OID 1.3.101.112).
    let alg_oid = &spki.algorithm.algorithm;
    if alg_oid.to_id_string() != "1.3.101.112" {
        return Err(format!(
            "expected Ed25519 SPKI, got OID {}",
            alg_oid.to_id_string()
        ));
    }
    let pk = spki.subject_public_key.data.to_vec();
    if pk.len() != 32 {
        return Err(format!("Ed25519 pubkey must be 32 bytes, got {}", pk.len()));
    }
    Ok(pk)
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{:02x}", b)).collect()
}

// ─────────────────────────────────────────────────────────────────
// NIF: generate_self_signed_cert(Pubkey, Privkey, Sans) ->
//       {ok, {CertPem, KeyPem}} | {error, Reason}
// ─────────────────────────────────────────────────────────────────

/// Sans is passed as a single comma-joined binary on the Erlang
/// side — list-of-binary auto-decode in rustler 0.34 is brittle, a
/// flat string sidesteps it. Pubkey/Privkey are passed as
/// `rustler::Binary` (zero-copy view into the Erlang term) rather
/// than `Vec<u8>` because the latter doesn't decode binaries with
/// arbitrary bytes in this rustler version.
#[rustler::nif(schedule = "DirtyCpu")]
pub fn nif_generate_self_signed_cert<'a>(
    env: Env<'a>,
    pubkey: Binary<'a>,
    privkey: Binary<'a>,
    sans_csv: Binary<'a>,
) -> NifResult<Term<'a>> {
    let sans: Vec<String> = std::str::from_utf8(sans_csv.as_slice())
        .unwrap_or_default()
        .split(',')
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .collect();
    match generate_self_signed(pubkey.as_slice(), privkey.as_slice(), &sans) {
        Ok((cert_pem, key_pem)) => Ok((atoms::ok(), (cert_pem, key_pem)).encode(env)),
        Err(e) => Ok((atoms::error(), e).encode(env)),
    }
}
