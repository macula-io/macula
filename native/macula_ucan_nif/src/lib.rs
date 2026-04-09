//! Macula UCAN NIF operations.
//!
//! This module provides high-performance UCAN (User Controlled Authorization Networks)
//! token operations for the Macula mesh:
//!
//! - Token creation with Ed25519 signing
//! - Token verification (signature and expiration)
//! - JWT-like encoding/decoding (header.payload.signature)
//! - Token CID (Content ID) generation for proof chains
//!
//! UCAN tokens are self-contained, delegatable capability tokens that enable
//! decentralized authorization without requiring a central authority.

use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine};
use ed25519_dalek::{Signature, SigningKey, Signer, VerifyingKey, Verifier};
use rustler::{Atom, Binary, Env, NifResult, OwnedBinary};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::time::{SystemTime, UNIX_EPOCH};

mod atoms {
    rustler::atoms! {
        ok,
        error,
        invalid_token,
        invalid_signature,
        invalid_private_key,
        invalid_public_key,
        expired,
        not_yet_valid,
        malformed_json,
    }
}

/// UCAN Header
#[derive(Debug, Serialize, Deserialize)]
struct UcanHeader {
    alg: String,       // "EdDSA"
    typ: String,       // "JWT"
    ucv: String,       // UCAN version "0.10.0"
}

/// UCAN Payload
#[derive(Debug, Serialize, Deserialize)]
struct UcanPayload {
    iss: String,                       // Issuer DID
    aud: String,                       // Audience DID
    #[serde(skip_serializing_if = "Option::is_none")]
    exp: Option<u64>,                  // Expiration (unix timestamp)
    #[serde(skip_serializing_if = "Option::is_none")]
    nbf: Option<u64>,                  // Not before (unix timestamp)
    #[serde(skip_serializing_if = "Option::is_none")]
    nnc: Option<String>,               // Nonce (for uniqueness)
    cap: serde_json::Value,            // Capabilities (array of {with, can})
    #[serde(skip_serializing_if = "Option::is_none")]
    fct: Option<serde_json::Value>,    // Facts (metadata)
    prf: Vec<String>,                  // Proof chain (CIDs of parent UCANs)
}

/// Create a new UCAN token.
///
/// Arguments:
/// - issuer: Issuer DID (binary string)
/// - audience: Audience DID (binary string)
/// - capabilities_json: JSON array of capabilities [{with, can}, ...]
/// - private_key: 32-byte Ed25519 private key
/// - opts_json: JSON object with optional fields (exp, nbf, nnc, fct, prf)
///
/// Returns:
/// - `{ok, Token}` where Token is the encoded JWT string
/// - `{error, Reason}` on failure
#[rustler::nif]
fn nif_create<'a>(
    env: Env<'a>,
    issuer: Binary,
    audience: Binary,
    capabilities_json: Binary,
    private_key: Binary,
    opts_json: Binary,
) -> NifResult<(Atom, Binary<'a>)> {
    // Parse capabilities
    let capabilities: serde_json::Value = match serde_json::from_slice(capabilities_json.as_slice()) {
        Ok(v) => v,
        Err(_) => return result_error(env, atoms::malformed_json()),
    };

    // Parse options
    let opts: serde_json::Value = match serde_json::from_slice(opts_json.as_slice()) {
        Ok(v) => v,
        Err(_) => serde_json::json!({}),
    };

    // Validate private key
    if private_key.len() != 32 {
        return result_error(env, atoms::invalid_private_key());
    }

    let key_bytes: [u8; 32] = match private_key.as_slice().try_into() {
        Ok(b) => b,
        Err(_) => return result_error(env, atoms::invalid_private_key()),
    };

    let signing_key = SigningKey::from_bytes(&key_bytes);

    // Build header
    let header = UcanHeader {
        alg: "EdDSA".to_string(),
        typ: "JWT".to_string(),
        ucv: "0.10.0".to_string(),
    };

    // Build payload
    let payload = UcanPayload {
        iss: String::from_utf8_lossy(issuer.as_slice()).to_string(),
        aud: String::from_utf8_lossy(audience.as_slice()).to_string(),
        exp: opts.get("exp").and_then(|v| v.as_u64()),
        nbf: opts.get("nbf").and_then(|v| v.as_u64()),
        nnc: opts.get("nnc").and_then(|v| v.as_str().map(String::from)),
        cap: capabilities,
        fct: opts.get("fct").cloned(),
        prf: opts.get("prf")
            .and_then(|v| v.as_array())
            .map(|arr| arr.iter().filter_map(|v| v.as_str().map(String::from)).collect())
            .unwrap_or_default(),
    };

    // Encode header and payload
    let header_json = match serde_json::to_string(&header) {
        Ok(s) => s,
        Err(_) => return result_error(env, atoms::malformed_json()),
    };

    let payload_json = match serde_json::to_string(&payload) {
        Ok(s) => s,
        Err(_) => return result_error(env, atoms::malformed_json()),
    };

    let header_b64 = URL_SAFE_NO_PAD.encode(header_json.as_bytes());
    let payload_b64 = URL_SAFE_NO_PAD.encode(payload_json.as_bytes());

    // Sign header.payload
    let signing_input = format!("{}.{}", header_b64, payload_b64);
    let signature = signing_key.sign(signing_input.as_bytes());
    let signature_b64 = URL_SAFE_NO_PAD.encode(signature.to_bytes());

    // Combine into JWT format
    let token = format!("{}.{}.{}", header_b64, payload_b64, signature_b64);

    // Return token
    let mut output = OwnedBinary::new(token.len()).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(token.as_bytes());

    Ok((atoms::ok(), output.release(env)))
}

/// Verify a UCAN token.
///
/// Arguments:
/// - token: The encoded JWT token (binary string)
/// - public_key: 32-byte Ed25519 public key of the issuer
///
/// Returns:
/// - `{ok, PayloadJson}` if valid, with the decoded payload
/// - `{error, invalid_signature}` if signature doesn't match
/// - `{error, expired}` if token is expired
/// - `{error, not_yet_valid}` if token is not yet valid
/// - `{error, invalid_token}` if token is malformed
#[rustler::nif]
fn nif_verify<'a>(env: Env<'a>, token: Binary, public_key: Binary) -> NifResult<(Atom, Binary<'a>)> {
    let token_str = match std::str::from_utf8(token.as_slice()) {
        Ok(s) => s,
        Err(_) => return result_error(env, atoms::invalid_token()),
    };

    // Split token into parts
    let parts: Vec<&str> = token_str.split('.').collect();
    if parts.len() != 3 {
        return result_error(env, atoms::invalid_token());
    }

    let header_b64 = parts[0];
    let payload_b64 = parts[1];
    let signature_b64 = parts[2];

    // Decode and parse payload
    let payload_bytes = match URL_SAFE_NO_PAD.decode(payload_b64) {
        Ok(b) => b,
        Err(_) => return result_error(env, atoms::invalid_token()),
    };

    let payload: UcanPayload = match serde_json::from_slice(&payload_bytes) {
        Ok(p) => p,
        Err(_) => return result_error(env, atoms::invalid_token()),
    };

    // Check expiration
    if let Some(exp) = payload.exp {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        if now > exp {
            return result_error(env, atoms::expired());
        }
    }

    // Check not-before
    if let Some(nbf) = payload.nbf {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        if now < nbf {
            return result_error(env, atoms::not_yet_valid());
        }
    }

    // Verify signature
    if public_key.len() != 32 {
        return result_error(env, atoms::invalid_public_key());
    }

    let pk_bytes: [u8; 32] = match public_key.as_slice().try_into() {
        Ok(b) => b,
        Err(_) => return result_error(env, atoms::invalid_public_key()),
    };

    let verifying_key = match VerifyingKey::from_bytes(&pk_bytes) {
        Ok(k) => k,
        Err(_) => return result_error(env, atoms::invalid_public_key()),
    };

    let sig_bytes = match URL_SAFE_NO_PAD.decode(signature_b64) {
        Ok(b) => b,
        Err(_) => return result_error(env, atoms::invalid_signature()),
    };

    if sig_bytes.len() != 64 {
        return result_error(env, atoms::invalid_signature());
    }

    let sig_array: [u8; 64] = match sig_bytes.try_into() {
        Ok(a) => a,
        Err(_) => return result_error(env, atoms::invalid_signature()),
    };

    let signature = Signature::from_bytes(&sig_array);
    let signing_input = format!("{}.{}", header_b64, payload_b64);

    if verifying_key.verify(signing_input.as_bytes(), &signature).is_err() {
        return result_error(env, atoms::invalid_signature());
    }

    // Return payload as JSON
    let payload_json = match serde_json::to_string(&payload) {
        Ok(s) => s,
        Err(_) => return result_error(env, atoms::invalid_token()),
    };

    let mut output = OwnedBinary::new(payload_json.len()).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(payload_json.as_bytes());

    Ok((atoms::ok(), output.release(env)))
}

/// Decode a UCAN token without verification.
///
/// Arguments:
/// - token: The encoded JWT token (binary string)
///
/// Returns:
/// - `{ok, PayloadJson}` with the decoded payload (unverified!)
/// - `{error, invalid_token}` if token is malformed
#[rustler::nif]
fn nif_decode<'a>(env: Env<'a>, token: Binary) -> NifResult<(Atom, Binary<'a>)> {
    let token_str = match std::str::from_utf8(token.as_slice()) {
        Ok(s) => s,
        Err(_) => return result_error(env, atoms::invalid_token()),
    };

    let parts: Vec<&str> = token_str.split('.').collect();
    if parts.len() != 3 {
        return result_error(env, atoms::invalid_token());
    }

    let payload_bytes = match URL_SAFE_NO_PAD.decode(parts[1]) {
        Ok(b) => b,
        Err(_) => return result_error(env, atoms::invalid_token()),
    };

    // Validate it's valid JSON
    let _: serde_json::Value = match serde_json::from_slice(&payload_bytes) {
        Ok(v) => v,
        Err(_) => return result_error(env, atoms::invalid_token()),
    };

    let mut output = OwnedBinary::new(payload_bytes.len()).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(&payload_bytes);

    Ok((atoms::ok(), output.release(env)))
}

/// Compute the CID (Content ID) of a UCAN token.
///
/// The CID is a SHA-256 hash of the token, encoded as URL-safe base64.
/// Used for proof chains.
///
/// Arguments:
/// - token: The encoded JWT token (binary string)
///
/// Returns:
/// - CID as URL-safe base64 binary string
#[rustler::nif]
fn nif_compute_cid<'a>(env: Env<'a>, token: Binary) -> NifResult<Binary<'a>> {
    let mut hasher = Sha256::new();
    hasher.update(token.as_slice());
    let hash = hasher.finalize();

    let cid = URL_SAFE_NO_PAD.encode(hash);

    let mut output = OwnedBinary::new(cid.len()).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(cid.as_bytes());

    Ok(output.release(env))
}

/// Extract the issuer DID from a UCAN token.
///
/// Arguments:
/// - token: The encoded JWT token (binary string)
///
/// Returns:
/// - `{ok, IssuerDid}` binary string
/// - `{error, invalid_token}` if malformed
#[rustler::nif]
fn nif_get_issuer<'a>(env: Env<'a>, token: Binary) -> NifResult<(Atom, Binary<'a>)> {
    let token_str = match std::str::from_utf8(token.as_slice()) {
        Ok(s) => s,
        Err(_) => return result_error(env, atoms::invalid_token()),
    };

    let parts: Vec<&str> = token_str.split('.').collect();
    if parts.len() != 3 {
        return result_error(env, atoms::invalid_token());
    }

    let payload_bytes = match URL_SAFE_NO_PAD.decode(parts[1]) {
        Ok(b) => b,
        Err(_) => return result_error(env, atoms::invalid_token()),
    };

    let payload: UcanPayload = match serde_json::from_slice(&payload_bytes) {
        Ok(p) => p,
        Err(_) => return result_error(env, atoms::invalid_token()),
    };

    let iss_bytes = payload.iss.as_bytes();
    let mut output = OwnedBinary::new(iss_bytes.len()).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(iss_bytes);

    Ok((atoms::ok(), output.release(env)))
}

/// Extract the audience DID from a UCAN token.
///
/// Arguments:
/// - token: The encoded JWT token (binary string)
///
/// Returns:
/// - `{ok, AudienceDid}` binary string
/// - `{error, invalid_token}` if malformed
#[rustler::nif]
fn nif_get_audience<'a>(env: Env<'a>, token: Binary) -> NifResult<(Atom, Binary<'a>)> {
    let token_str = match std::str::from_utf8(token.as_slice()) {
        Ok(s) => s,
        Err(_) => return result_error(env, atoms::invalid_token()),
    };

    let parts: Vec<&str> = token_str.split('.').collect();
    if parts.len() != 3 {
        return result_error(env, atoms::invalid_token());
    }

    let payload_bytes = match URL_SAFE_NO_PAD.decode(parts[1]) {
        Ok(b) => b,
        Err(_) => return result_error(env, atoms::invalid_token()),
    };

    let payload: UcanPayload = match serde_json::from_slice(&payload_bytes) {
        Ok(p) => p,
        Err(_) => return result_error(env, atoms::invalid_token()),
    };

    let aud_bytes = payload.aud.as_bytes();
    let mut output = OwnedBinary::new(aud_bytes.len()).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(aud_bytes);

    Ok((atoms::ok(), output.release(env)))
}

// Helper to return error tuple with empty binary
fn result_error<'a>(env: Env<'a>, reason: Atom) -> NifResult<(Atom, Binary<'a>)> {
    let empty = OwnedBinary::new(0).ok_or(rustler::Error::Term(Box::new("alloc")))?;
    Ok((reason, empty.release(env)))
}

rustler::init!("macula_ucan_nif");
