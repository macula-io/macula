//! Macula DID NIF operations.
//!
//! This module provides Decentralized Identifier (DID) operations for the Macula mesh:
//!
//! - DID Document creation with Ed25519 verification methods
//! - DID resolution (local cache lookup)
//! - Controller verification
//! - DID-to-public-key extraction
//!
//! DID Format: `did:macula:<identity>`
//! - Realm: `did:macula:io.macula`
//! - Org: `did:macula:io.macula.{org}`
//! - App: `did:macula:io.macula.{org}.{app}`

use ed25519_dalek::VerifyingKey;
use rustler::{Atom, Binary, Env, NifResult, OwnedBinary};
use serde::{Deserialize, Serialize};

mod atoms {
    rustler::atoms! {
        ok,
        error,
        invalid_did,
        invalid_public_key,
        invalid_document,
        not_found,
        controller_mismatch,
        malformed_json,
    }
}

/// DID Document verification method
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct VerificationMethod {
    id: String,
    #[serde(rename = "type")]
    method_type: String,
    controller: String,
    public_key_multibase: String,
}

/// DID Document (W3C DID Core compatible)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct DidDocument {
    #[serde(rename = "@context")]
    context: Vec<String>,
    id: String,
    controller: Option<String>,
    verification_method: Vec<VerificationMethod>,
    authentication: Vec<String>,
    assertion_method: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    capability_invocation: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    capability_delegation: Option<Vec<String>>,
}

/// Create a new DID Document.
///
/// Arguments:
/// - did: The DID string (e.g., "did:macula:io.macula.acme")
/// - public_key: 32-byte Ed25519 public key
///
/// Returns:
/// - `{ok, DocumentJson}` - JSON-encoded DID document
/// - `{error, Reason}` on failure
#[rustler::nif]
fn nif_create_document<'a>(
    env: Env<'a>,
    did: Binary,
    public_key: Binary,
) -> NifResult<(Atom, Binary<'a>)> {
    let did_str = match std::str::from_utf8(did.as_slice()) {
        Ok(s) => s,
        Err(_) => return result_error(env, atoms::invalid_did()),
    };

    // Validate DID format
    if !did_str.starts_with("did:macula:") {
        return result_error(env, atoms::invalid_did());
    }

    // Validate public key
    if public_key.len() != 32 {
        return result_error(env, atoms::invalid_public_key());
    }

    let pk_bytes: [u8; 32] = match public_key.as_slice().try_into() {
        Ok(b) => b,
        Err(_) => return result_error(env, atoms::invalid_public_key()),
    };

    // Validate it's a valid Ed25519 public key
    if VerifyingKey::from_bytes(&pk_bytes).is_err() {
        return result_error(env, atoms::invalid_public_key());
    }

    // Encode public key as multibase (base58btc with 'z' prefix)
    // For Ed25519 public keys, we use the multicodec prefix 0xed01
    let mut multicodec_key = vec![0xed, 0x01];
    multicodec_key.extend_from_slice(&pk_bytes);
    let multibase_key = format!("z{}", bs58::encode(&multicodec_key).into_string());

    // Create key ID
    let key_id = format!("{}#key-1", did_str);

    // Determine controller (parent in hierarchy)
    let controller = get_parent_did(did_str);

    // Build DID Document
    let doc = DidDocument {
        context: vec!["https://www.w3.org/ns/did/v1".to_string()],
        id: did_str.to_string(),
        controller,
        verification_method: vec![VerificationMethod {
            id: key_id.clone(),
            method_type: "Ed25519VerificationKey2020".to_string(),
            controller: did_str.to_string(),
            public_key_multibase: multibase_key,
        }],
        authentication: vec![key_id.clone()],
        assertion_method: vec![key_id.clone()],
        capability_invocation: Some(vec![key_id.clone()]),
        capability_delegation: Some(vec![key_id]),
    };

    // Serialize to JSON
    let json = match serde_json::to_string(&doc) {
        Ok(j) => j,
        Err(_) => return result_error(env, atoms::malformed_json()),
    };

    let mut output = OwnedBinary::new(json.len()).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(json.as_bytes());

    Ok((atoms::ok(), output.release(env)))
}

/// Parse a DID Document from JSON.
///
/// Arguments:
/// - document_json: JSON-encoded DID document
///
/// Returns:
/// - `{ok, DocumentJson}` - validated and re-serialized document
/// - `{error, invalid_document}` if parsing fails
#[rustler::nif]
fn nif_parse_document<'a>(env: Env<'a>, document_json: Binary) -> NifResult<(Atom, Binary<'a>)> {
    let json_str = match std::str::from_utf8(document_json.as_slice()) {
        Ok(s) => s,
        Err(_) => return result_error(env, atoms::invalid_document()),
    };

    let _doc: DidDocument = match serde_json::from_str(json_str) {
        Ok(d) => d,
        Err(_) => return result_error(env, atoms::invalid_document()),
    };

    // Return the original JSON (validated)
    let mut output = OwnedBinary::new(document_json.len()).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output
        .as_mut_slice()
        .copy_from_slice(document_json.as_slice());

    Ok((atoms::ok(), output.release(env)))
}

/// Extract the public key from a DID Document.
///
/// Arguments:
/// - document_json: JSON-encoded DID document
///
/// Returns:
/// - `{ok, PublicKey}` - 32-byte Ed25519 public key
/// - `{error, Reason}` on failure
#[rustler::nif]
fn nif_extract_public_key<'a>(
    env: Env<'a>,
    document_json: Binary,
) -> NifResult<(Atom, Binary<'a>)> {
    let json_str = match std::str::from_utf8(document_json.as_slice()) {
        Ok(s) => s,
        Err(_) => return result_error(env, atoms::invalid_document()),
    };

    let doc: DidDocument = match serde_json::from_str(json_str) {
        Ok(d) => d,
        Err(_) => return result_error(env, atoms::invalid_document()),
    };

    // Get the first verification method
    let vm = match doc.verification_method.first() {
        Some(v) => v,
        None => return result_error(env, atoms::invalid_document()),
    };

    // Decode multibase public key
    let multibase_key = &vm.public_key_multibase;
    if !multibase_key.starts_with('z') {
        return result_error(env, atoms::invalid_public_key());
    }

    let decoded = match bs58::decode(&multibase_key[1..]).into_vec() {
        Ok(d) => d,
        Err(_) => return result_error(env, atoms::invalid_public_key()),
    };

    // Check multicodec prefix (0xed01 for Ed25519)
    if decoded.len() < 34 || decoded[0] != 0xed || decoded[1] != 0x01 {
        return result_error(env, atoms::invalid_public_key());
    }

    let pk_bytes = &decoded[2..34];

    let mut output = OwnedBinary::new(32).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(pk_bytes);

    Ok((atoms::ok(), output.release(env)))
}

/// Get the DID from a DID Document.
///
/// Arguments:
/// - document_json: JSON-encoded DID document
///
/// Returns:
/// - `{ok, Did}` - DID string
/// - `{error, invalid_document}` if parsing fails
#[rustler::nif]
fn nif_get_did<'a>(env: Env<'a>, document_json: Binary) -> NifResult<(Atom, Binary<'a>)> {
    let json_str = match std::str::from_utf8(document_json.as_slice()) {
        Ok(s) => s,
        Err(_) => return result_error(env, atoms::invalid_document()),
    };

    let doc: DidDocument = match serde_json::from_str(json_str) {
        Ok(d) => d,
        Err(_) => return result_error(env, atoms::invalid_document()),
    };

    let did = doc.id.as_bytes();
    let mut output = OwnedBinary::new(did.len()).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(did);

    Ok((atoms::ok(), output.release(env)))
}

/// Get the controller DID from a DID Document.
///
/// Arguments:
/// - document_json: JSON-encoded DID document
///
/// Returns:
/// - `{ok, ControllerDid}` - Controller DID string (or self if no controller)
/// - `{error, invalid_document}` if parsing fails
#[rustler::nif]
fn nif_get_controller<'a>(env: Env<'a>, document_json: Binary) -> NifResult<(Atom, Binary<'a>)> {
    let json_str = match std::str::from_utf8(document_json.as_slice()) {
        Ok(s) => s,
        Err(_) => return result_error(env, atoms::invalid_document()),
    };

    let doc: DidDocument = match serde_json::from_str(json_str) {
        Ok(d) => d,
        Err(_) => return result_error(env, atoms::invalid_document()),
    };

    let controller = doc.controller.unwrap_or(doc.id);
    let controller_bytes = controller.as_bytes();

    let mut output = OwnedBinary::new(controller_bytes.len()).ok_or(rustler::Error::Term(
        Box::new("Failed to allocate binary"),
    ))?;
    output.as_mut_slice().copy_from_slice(controller_bytes);

    Ok((atoms::ok(), output.release(env)))
}

/// Verify that a DID Document is controlled by the expected controller.
///
/// Arguments:
/// - document_json: JSON-encoded DID document
/// - expected_controller: The expected controller DID
///
/// Returns:
/// - `ok` if controller matches
/// - `{error, controller_mismatch}` if controller doesn't match
/// - `{error, invalid_document}` if parsing fails
#[rustler::nif]
fn nif_verify_controller(document_json: Binary, expected_controller: Binary) -> Atom {
    let json_str = match std::str::from_utf8(document_json.as_slice()) {
        Ok(s) => s,
        Err(_) => return atoms::invalid_document(),
    };

    let expected = match std::str::from_utf8(expected_controller.as_slice()) {
        Ok(s) => s,
        Err(_) => return atoms::invalid_did(),
    };

    let doc: DidDocument = match serde_json::from_str(json_str) {
        Ok(d) => d,
        Err(_) => return atoms::invalid_document(),
    };

    let actual_controller = doc.controller.as_ref().unwrap_or(&doc.id);

    if actual_controller == expected {
        atoms::ok()
    } else {
        atoms::controller_mismatch()
    }
}

/// Parse a DID string and extract its components.
///
/// Arguments:
/// - did: The DID string (e.g., "did:macula:io.macula.acme.myapp")
///
/// Returns:
/// - `{ok, ComponentsJson}` - JSON object with method, identity, and parts
/// - `{error, invalid_did}` if parsing fails
#[rustler::nif]
fn nif_parse_did<'a>(env: Env<'a>, did: Binary) -> NifResult<(Atom, Binary<'a>)> {
    let did_str = match std::str::from_utf8(did.as_slice()) {
        Ok(s) => s,
        Err(_) => return result_error(env, atoms::invalid_did()),
    };

    // Parse DID format: did:method:identity
    let parts: Vec<&str> = did_str.split(':').collect();
    if parts.len() != 3 || parts[0] != "did" || parts[1] != "macula" {
        return result_error(env, atoms::invalid_did());
    }

    let identity = parts[2];
    let identity_parts: Vec<&str> = identity.split('.').collect();

    let result = serde_json::json!({
        "method": "macula",
        "identity": identity,
        "parts": identity_parts,
        "depth": identity_parts.len()
    });

    let json = result.to_string();
    let mut output = OwnedBinary::new(json.len()).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(json.as_bytes());

    Ok((atoms::ok(), output.release(env)))
}

/// Check if one DID is a descendant of another.
///
/// Arguments:
/// - child_did: The potential child DID
/// - parent_did: The potential parent DID
///
/// Returns:
/// - `true` if child is a descendant of parent
/// - `false` otherwise
#[rustler::nif]
fn nif_is_descendant(child_did: Binary, parent_did: Binary) -> bool {
    let child = match std::str::from_utf8(child_did.as_slice()) {
        Ok(s) => s,
        Err(_) => return false,
    };

    let parent = match std::str::from_utf8(parent_did.as_slice()) {
        Ok(s) => s,
        Err(_) => return false,
    };

    // Extract identities from DIDs
    let child_identity = match child.strip_prefix("did:macula:") {
        Some(i) => i,
        None => return false,
    };

    let parent_identity = match parent.strip_prefix("did:macula:") {
        Some(i) => i,
        None => return false,
    };

    // Child must start with parent identity + "."
    if child_identity == parent_identity {
        return false; // Same DID, not descendant
    }

    child_identity.starts_with(&format!("{}.", parent_identity))
}

// Helper: Get parent DID from a DID
fn get_parent_did(did: &str) -> Option<String> {
    let identity = did.strip_prefix("did:macula:")?;
    let parts: Vec<&str> = identity.split('.').collect();

    if parts.len() <= 2 {
        // Root realm (io.macula) has no parent
        None
    } else {
        // Remove last part to get parent
        let parent_parts = &parts[..parts.len() - 1];
        Some(format!("did:macula:{}", parent_parts.join(".")))
    }
}

// Helper to return error tuple with empty binary
fn result_error<'a>(env: Env<'a>, reason: Atom) -> NifResult<(Atom, Binary<'a>)> {
    let empty = OwnedBinary::new(0).ok_or(rustler::Error::Term(Box::new("alloc")))?;
    Ok((reason, empty.release(env)))
}

rustler::init!("macula_did_nif");
