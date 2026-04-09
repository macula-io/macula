//! Macula Cryptographic NIF operations.
//!
//! This module provides high-performance implementations of:
//! - Ed25519 key generation, signing, and verification
//! - BLAKE3 hashing (primary algorithm for content-addressed storage)
//! - SHA-256 hashing
//! - Base64 encoding/decoding (URL-safe)
//! - Constant-time secure comparison
//!
//! These NIFs provide the cryptographic foundation for UCAN tokens,
//! DID operations, and content-addressed storage in the Macula mesh.

use ed25519_dalek::{Signature, SigningKey, VerifyingKey, Signer, Verifier};
use rand::rngs::OsRng;
use rustler::{Atom, Binary, Env, NifResult, OwnedBinary};
use sha2::{Digest, Sha256};

mod atoms {
    rustler::atoms! {
        ok,
        error,
        invalid_signature,
        invalid_public_key,
        invalid_private_key,
        invalid_key_length,
    }
}

/// Generate a new Ed25519 keypair.
///
/// Returns:
/// - `{ok, {PublicKey, PrivateKey}}` where both are 32-byte binaries
///   Note: PrivateKey is the seed (32 bytes), not the full secret key (64 bytes)
#[rustler::nif]
fn nif_generate_keypair<'a>(env: Env<'a>) -> NifResult<(Atom, (Binary<'a>, Binary<'a>))> {
    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);
    let verifying_key = signing_key.verifying_key();

    // Get the 32-byte seed (private key)
    let private_key_bytes = signing_key.to_bytes();
    let public_key_bytes = verifying_key.to_bytes();

    // Create output binaries
    let mut pub_out = OwnedBinary::new(32).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary for public key",
    )))?;
    pub_out.as_mut_slice().copy_from_slice(&public_key_bytes);

    let mut priv_out = OwnedBinary::new(32).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary for private key",
    )))?;
    priv_out.as_mut_slice().copy_from_slice(&private_key_bytes);

    Ok((atoms::ok(), (pub_out.release(env), priv_out.release(env))))
}

/// Sign a message with an Ed25519 private key.
///
/// Arguments:
/// - message: The message to sign (binary)
/// - private_key: The 32-byte Ed25519 private key seed (binary)
///
/// Returns:
/// - `{ok, Signature}` where Signature is a 64-byte binary
/// - `{error, invalid_private_key}` if the key is invalid
#[rustler::nif]
fn nif_sign<'a>(env: Env<'a>, message: Binary, private_key: Binary) -> NifResult<(Atom, Binary<'a>)> {
    // Validate key length
    if private_key.len() != 32 {
        let empty = OwnedBinary::new(0).ok_or(rustler::Error::Term(Box::new(
            "Failed to allocate binary",
        )))?;
        return Ok((atoms::invalid_private_key(), empty.release(env)));
    }

    // Parse private key
    let key_bytes: [u8; 32] = match private_key.as_slice().try_into() {
        Ok(bytes) => bytes,
        Err(_) => {
            let empty = OwnedBinary::new(0).ok_or(rustler::Error::Term(Box::new(
                "Failed to allocate binary",
            )))?;
            return Ok((atoms::invalid_private_key(), empty.release(env)));
        }
    };

    let signing_key = SigningKey::from_bytes(&key_bytes);
    let signature = signing_key.sign(message.as_slice());

    // Create output binary
    let mut sig_out = OwnedBinary::new(64).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary for signature",
    )))?;
    sig_out.as_mut_slice().copy_from_slice(&signature.to_bytes());

    Ok((atoms::ok(), sig_out.release(env)))
}

/// Verify an Ed25519 signature.
///
/// Arguments:
/// - message: The message that was signed (binary)
/// - signature: The 64-byte Ed25519 signature (binary)
/// - public_key: The 32-byte Ed25519 public key (binary)
///
/// Returns:
/// - `true` if signature is valid
/// - `false` if signature is invalid
#[rustler::nif]
fn nif_verify(message: Binary, signature: Binary, public_key: Binary) -> bool {
    // Validate input lengths
    if signature.len() != 64 {
        return false;
    }
    if public_key.len() != 32 {
        return false;
    }

    // Parse public key
    let pk_bytes: [u8; 32] = match public_key.as_slice().try_into() {
        Ok(bytes) => bytes,
        Err(_) => return false,
    };

    let verifying_key = match VerifyingKey::from_bytes(&pk_bytes) {
        Ok(key) => key,
        Err(_) => return false,
    };

    // Parse signature
    let sig_bytes: [u8; 64] = match signature.as_slice().try_into() {
        Ok(bytes) => bytes,
        Err(_) => return false,
    };

    let sig = Signature::from_bytes(&sig_bytes);

    // Verify signature
    verifying_key.verify(message.as_slice(), &sig).is_ok()
}

/// Compute SHA-256 hash of data.
///
/// Arguments:
/// - data: The data to hash (binary)
///
/// Returns:
/// - 32-byte SHA-256 hash (binary)
#[rustler::nif]
fn nif_sha256<'a>(env: Env<'a>, data: Binary) -> NifResult<Binary<'a>> {
    let mut hasher = Sha256::new();
    hasher.update(data.as_slice());
    let result = hasher.finalize();

    let mut output = OwnedBinary::new(32).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(&result);

    Ok(output.release(env))
}

/// Compute BLAKE3 hash of data.
///
/// BLAKE3 is a cryptographic hash function that is:
/// - Much faster than SHA-256 (especially on modern CPUs)
/// - Parallelizable for large inputs
/// - Secure (based on BLAKE2 and ChaCha)
///
/// Arguments:
/// - data: The data to hash (binary)
///
/// Returns:
/// - 32-byte BLAKE3 hash (binary)
#[rustler::nif]
fn nif_blake3<'a>(env: Env<'a>, data: Binary) -> NifResult<Binary<'a>> {
    let hash = blake3::hash(data.as_slice());

    let mut output = OwnedBinary::new(32).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(hash.as_bytes());

    Ok(output.release(env))
}

/// Compute BLAKE3 hash of multiple chunks (streaming).
///
/// This is optimized for content-addressed storage where data
/// is processed in chunks. The hasher maintains internal state
/// across all chunks.
///
/// Arguments:
/// - chunks: List of binaries to hash
///
/// Returns:
/// - 32-byte BLAKE3 hash (binary)
#[rustler::nif]
fn nif_blake3_streaming<'a>(env: Env<'a>, chunks: Vec<Binary>) -> NifResult<Binary<'a>> {
    let mut hasher = blake3::Hasher::new();
    for chunk in chunks {
        hasher.update(chunk.as_slice());
    }
    let hash = hasher.finalize();

    let mut output = OwnedBinary::new(32).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(hash.as_bytes());

    Ok(output.release(env))
}

/// Verify that data matches a BLAKE3 hash.
///
/// Arguments:
/// - data: The data to verify (binary)
/// - expected_hash: The expected 32-byte BLAKE3 hash (binary)
///
/// Returns:
/// - `true` if the hash matches
/// - `false` if the hash doesn't match or expected_hash is wrong length
#[rustler::nif]
fn nif_blake3_verify(data: Binary, expected_hash: Binary) -> bool {
    if expected_hash.len() != 32 {
        return false;
    }
    let computed = blake3::hash(data.as_slice());
    computed.as_bytes() == expected_hash.as_slice()
}

/// Compute BLAKE3 hash and encode as hex string.
///
/// Optimized for debugging and logging - combines hash + hex encode
/// in a single NIF call.
///
/// Arguments:
/// - data: The data to hash (binary)
///
/// Returns:
/// - Hex-encoded BLAKE3 hash (64-character binary string)
#[rustler::nif]
fn nif_blake3_hex<'a>(env: Env<'a>, data: Binary) -> NifResult<Binary<'a>> {
    let hash = blake3::hash(data.as_slice());
    let hex = hash.to_hex();

    let mut output = OwnedBinary::new(64).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(hex.as_bytes());

    Ok(output.release(env))
}

/// Compute SHA-256 hash and encode as URL-safe base64 (no padding).
///
/// This is optimized for token CID generation - combines hash + encode
/// in a single NIF call to avoid intermediate allocations.
///
/// Arguments:
/// - data: The data to hash (binary)
///
/// Returns:
/// - URL-safe base64-encoded SHA-256 hash (binary string)
#[rustler::nif]
fn nif_sha256_base64<'a>(env: Env<'a>, data: Binary) -> NifResult<Binary<'a>> {
    use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine};

    // Hash the data
    let mut hasher = Sha256::new();
    hasher.update(data.as_slice());
    let hash = hasher.finalize();

    // Encode as URL-safe base64 (no padding)
    let encoded = URL_SAFE_NO_PAD.encode(hash);

    // Create output binary
    let mut output = OwnedBinary::new(encoded.len()).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(encoded.as_bytes());

    Ok(output.release(env))
}

/// Base64 URL-safe encode without padding.
///
/// Arguments:
/// - data: The data to encode (binary)
///
/// Returns:
/// - URL-safe base64-encoded data (binary string)
#[rustler::nif]
fn nif_base64_encode<'a>(env: Env<'a>, data: Binary) -> NifResult<Binary<'a>> {
    use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine};

    let encoded = URL_SAFE_NO_PAD.encode(data.as_slice());

    let mut output = OwnedBinary::new(encoded.len()).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(encoded.as_bytes());

    Ok(output.release(env))
}

/// Base64 URL-safe decode.
///
/// Arguments:
/// - data: The base64-encoded data (binary string)
///
/// Returns:
/// - `{ok, Binary}` on success
/// - `{error, invalid_base64}` on failure
#[rustler::nif]
fn nif_base64_decode<'a>(env: Env<'a>, data: Binary) -> NifResult<(Atom, Binary<'a>)> {
    use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine};

    match URL_SAFE_NO_PAD.decode(data.as_slice()) {
        Ok(decoded) => {
            let mut output =
                OwnedBinary::new(decoded.len()).ok_or(rustler::Error::Term(Box::new(
                    "Failed to allocate binary",
                )))?;
            output.as_mut_slice().copy_from_slice(&decoded);
            Ok((atoms::ok(), output.release(env)))
        }
        Err(_) => {
            // Return empty binary for error case
            let output = OwnedBinary::new(0).ok_or(rustler::Error::Term(Box::new(
                "Failed to allocate binary",
            )))?;
            Ok((atoms::error(), output.release(env)))
        }
    }
}

/// Constant-time comparison of two binaries.
///
/// This is important for security - prevents timing attacks when comparing
/// signatures, hashes, or tokens.
///
/// Arguments:
/// - a: First binary
/// - b: Second binary
///
/// Returns:
/// - `true` if equal (constant time)
/// - `false` if not equal (constant time)
#[rustler::nif]
fn nif_secure_compare(a: Binary, b: Binary) -> bool {
    if a.len() != b.len() {
        return false;
    }

    // Constant-time comparison
    let mut result: u8 = 0;
    for (x, y) in a.as_slice().iter().zip(b.as_slice().iter()) {
        result |= x ^ y;
    }
    result == 0
}

rustler::init!("macula_crypto_nif");
