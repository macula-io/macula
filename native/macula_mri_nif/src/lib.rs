//! Macula MRI (Resource Identifier) NIF operations.
//!
//! This module provides high-performance implementations of:
//! - MRI parsing (single-pass binary parsing)
//! - Realm and segment validation (efficient character validation)
//! - Path segment operations (single-allocation joining)
//! - Trie-based hierarchy queries (children/descendants)
//! - Persistent trie index with O(d) queries for million-scale deployments
//! - Builtin type lookup (hash table)
//!
//! These NIFs provide significant speedups for MRI operations which are
//! on the hot path for service discovery, authorization, and routing.
//!
//! ## Trie Index
//!
//! For large-scale deployments (millions of MRIs), use the persistent index:
//!
//! ```erlang
//! %% Build index once
//! {ok, Index} = macula_mri_nif:build_path_index(AllMRIs),
//!
//! %% O(d) queries instead of O(n)
//! Children = macula_mri_nif:index_find_children(Index, Realm, Path),
//! Descendants = macula_mri_nif:index_find_descendants(Index, Realm, Path),
//!
//! %% Dynamic updates for device churn
//! ok = macula_mri_nif:index_insert(Index, Realm, Path, MRI),
//! ok = macula_mri_nif:index_remove(Index, Realm, Path),
//! ```

use rustler::{Binary, Encoder, Env, NifResult, OwnedBinary, ResourceArc, Term};
use std::collections::{HashMap, HashSet};
use std::sync::RwLock;

mod atoms {
    rustler::atoms! {
        ok,
        error,
        invalid_format,
        invalid_realm,
        invalid_segment,
        invalid_type,
        invalid_index,
        not_found,
        mri,
        // MRI components
        type_,  // 'type' is reserved in Rust
        realm,
        path,
        // Builtin types
        realm_type,
        org,
        user,
        app,
        service,
        artifact,
        license,
        cert,
        key,
        topic,
        proc,
        content,
        device,
        cluster,
        location,
        zone,
        network,
        model,
        dataset,
        config,
        class,
        taxonomy,
    }
}

// ============================================================================
// Trie Index Data Structures
// ============================================================================

/// A node in the MRI path trie.
///
/// Each node represents a path segment and can have:
/// - Children: Map of segment name -> child node
/// - MRI: The full MRI string if this node is a leaf/endpoint
#[derive(Debug, Default)]
struct TrieNode {
    children: HashMap<String, TrieNode>,
    /// The full MRI binary stored at this path (if any)
    mri: Option<Vec<u8>>,
}

impl TrieNode {
    fn new() -> Self {
        Self::default()
    }

    /// Insert a path into the trie, storing the MRI at the leaf.
    fn insert(&mut self, path: &[String], mri: Vec<u8>) {
        if path.is_empty() {
            self.mri = Some(mri);
            return;
        }

        let child = self.children
            .entry(path[0].clone())
            .or_insert_with(TrieNode::new);
        child.insert(&path[1..], mri);
    }

    /// Remove a path from the trie.
    /// Returns true if the path was found and removed.
    fn remove(&mut self, path: &[String]) -> bool {
        if path.is_empty() {
            if self.mri.is_some() {
                self.mri = None;
                return true;
            }
            return false;
        }

        if let Some(child) = self.children.get_mut(&path[0]) {
            let removed = child.remove(&path[1..]);
            // Clean up empty nodes
            if child.is_empty() {
                self.children.remove(&path[0]);
            }
            return removed;
        }
        false
    }

    /// Check if this node has no children and no MRI.
    fn is_empty(&self) -> bool {
        self.children.is_empty() && self.mri.is_none()
    }

    /// Find direct children of a path (nodes exactly one level deeper).
    fn find_children(&self, path: &[String]) -> Vec<&[u8]> {
        // Navigate to the target node
        let node = match self.navigate(path) {
            Some(n) => n,
            None => return Vec::new(),
        };

        // Collect MRIs from immediate children
        let mut results = Vec::new();
        for child in node.children.values() {
            if let Some(ref mri) = child.mri {
                results.push(mri.as_slice());
            }
        }
        results
    }

    /// Find all descendants of a path (all nodes deeper in the tree).
    fn find_descendants(&self, path: &[String]) -> Vec<&[u8]> {
        // Navigate to the target node
        let node = match self.navigate(path) {
            Some(n) => n,
            None => return Vec::new(),
        };

        // Collect all MRIs from subtree
        let mut results = Vec::new();
        node.collect_descendants(&mut results);
        results
    }

    /// Navigate to a node at the given path.
    fn navigate(&self, path: &[String]) -> Option<&TrieNode> {
        if path.is_empty() {
            return Some(self);
        }

        self.children.get(&path[0])
            .and_then(|child| child.navigate(&path[1..]))
    }

    /// Recursively collect all MRIs in this subtree (excluding self).
    fn collect_descendants<'a>(&'a self, results: &mut Vec<&'a [u8]>) {
        for child in self.children.values() {
            if let Some(ref mri) = child.mri {
                results.push(mri.as_slice());
            }
            child.collect_descendants(results);
        }
    }

    /// Count all MRIs in this subtree (including self).
    #[allow(dead_code)]
    fn count(&self) -> usize {
        let self_count = if self.mri.is_some() { 1 } else { 0 };
        let child_count: usize = self.children.values().map(|c| c.count()).sum();
        self_count + child_count
    }
}

/// The MRI Index - a collection of tries, one per realm.
///
/// Using separate tries per realm provides:
/// - Natural isolation between realms
/// - Faster queries (smaller tries)
/// - Simpler realm-scoped operations
#[derive(Debug, Default)]
struct MriIndex {
    /// Map of realm -> trie root for that realm
    realms: HashMap<String, TrieNode>,
    /// Total count of MRIs across all realms
    total_count: usize,
}

impl MriIndex {
    fn new() -> Self {
        Self::default()
    }

    /// Insert an MRI into the index.
    fn insert(&mut self, realm: &str, path: Vec<String>, mri: Vec<u8>) {
        let trie = self.realms
            .entry(realm.to_string())
            .or_insert_with(TrieNode::new);
        trie.insert(&path, mri);
        self.total_count += 1;
    }

    /// Remove an MRI from the index.
    fn remove(&mut self, realm: &str, path: &[String]) -> bool {
        if let Some(trie) = self.realms.get_mut(realm) {
            if trie.remove(path) {
                self.total_count -= 1;
                // Clean up empty realm tries
                if trie.is_empty() {
                    self.realms.remove(realm);
                }
                return true;
            }
        }
        false
    }

    /// Find direct children of a path in a realm.
    fn find_children(&self, realm: &str, path: &[String]) -> Vec<&[u8]> {
        self.realms.get(realm)
            .map(|trie| trie.find_children(path))
            .unwrap_or_default()
    }

    /// Find all descendants of a path in a realm.
    fn find_descendants(&self, realm: &str, path: &[String]) -> Vec<&[u8]> {
        self.realms.get(realm)
            .map(|trie| trie.find_descendants(path))
            .unwrap_or_default()
    }

    /// Get total count of MRIs in the index.
    fn len(&self) -> usize {
        self.total_count
    }
}

/// Resource wrapper for the MRI index.
///
/// This allows the index to be passed to/from Erlang as an opaque reference.
/// RwLock provides concurrent read access with exclusive write access.
pub struct MriIndexResource(RwLock<MriIndex>);

impl MriIndexResource {
    fn new() -> Self {
        Self(RwLock::new(MriIndex::new()))
    }
}

// Builtin MRI types - stored in a HashSet for O(1) lookup
lazy_static::lazy_static! {
    static ref BUILTIN_TYPES: HashSet<&'static str> = {
        let mut set = HashSet::new();
        set.insert("realm");
        set.insert("org");
        set.insert("user");
        set.insert("app");
        set.insert("service");
        set.insert("artifact");
        set.insert("license");
        set.insert("cert");
        set.insert("key");
        set.insert("topic");
        set.insert("proc");
        set.insert("content");
        set.insert("device");
        set.insert("cluster");
        set.insert("location");
        set.insert("zone");
        set.insert("network");
        set.insert("model");
        set.insert("dataset");
        set.insert("config");
        set.insert("class");
        set.insert("taxonomy");
        set
    };
}

/// Parse an MRI string into its components.
///
/// Format: `mri:{type}:{realm}` or `mri:{type}:{realm}/{path...}`
///
/// Returns:
/// - `{ok, {Type, Realm, PathList}}` on success (Erlang side wraps in map)
/// - `{error, invalid_format}` on failure
#[rustler::nif]
fn nif_parse_mri<'a>(env: Env<'a>, mri: Binary) -> NifResult<Term<'a>> {
    let mri_str = match std::str::from_utf8(mri.as_slice()) {
        Ok(s) => s,
        Err(_) => return Ok((atoms::error(), atoms::invalid_format()).encode(env)),
    };

    // Must start with "mri:"
    if !mri_str.starts_with("mri:") {
        return Ok((atoms::error(), atoms::invalid_format()).encode(env));
    }

    let rest = &mri_str[4..]; // Skip "mri:"

    // Find type (up to next ":")
    let type_end = match rest.find(':') {
        Some(pos) => pos,
        None => return Ok((atoms::error(), atoms::invalid_format()).encode(env)),
    };

    let type_str = &rest[..type_end];
    if type_str.is_empty() {
        return Ok((atoms::error(), atoms::invalid_format()).encode(env));
    }

    let after_type = &rest[type_end + 1..]; // Skip type and ":"

    // Split realm and path at first "/"
    let (realm_str, path_str) = match after_type.find('/') {
        Some(pos) => (&after_type[..pos], Some(&after_type[pos + 1..])),
        None => (after_type, None),
    };

    // Check for empty realm (missing required component = invalid_format)
    if realm_str.is_empty() {
        return Ok((atoms::error(), atoms::invalid_format()).encode(env));
    }

    // Validate realm format (must have valid chars AND contain at least one dot)
    if !is_valid_realm_chars(realm_str) || !realm_str.contains('.') {
        return Ok((atoms::error(), atoms::invalid_realm()).encode(env));
    }

    // Parse path segments
    let path_segments: Vec<&str> = match path_str {
        Some(p) if !p.is_empty() => p.split('/').collect(),
        _ => Vec::new(),
    };

    // Validate path segments
    for seg in &path_segments {
        if !is_valid_segment_chars(seg) {
            return Ok((atoms::error(), atoms::invalid_segment()).encode(env));
        }
    }

    // Convert strings to binaries
    let type_bin = string_to_binary(env, type_str)?;
    let realm_bin = string_to_binary(env, realm_str)?;

    // Convert path segments to list of binaries
    let path_terms: Vec<Binary> = path_segments
        .iter()
        .map(|s| string_to_binary(env, s))
        .collect::<Result<Vec<_>, _>>()?;

    // Return as tuple: {ok, {Type, Realm, PathList}}
    // Erlang side will convert to map
    Ok((atoms::ok(), (type_bin, realm_bin, path_terms)).encode(env))
}

/// Validate realm format.
///
/// Valid realm: reverse domain notation (e.g., "io.macula.example")
/// Characters: a-z, 0-9, dots (.) only
/// Must contain at least one dot.
///
/// Returns: `true` if valid, `false` otherwise
#[rustler::nif]
fn nif_validate_realm_format(realm: Binary) -> bool {
    match std::str::from_utf8(realm.as_slice()) {
        Ok(s) => is_valid_realm_chars(s) && s.contains('.'),
        Err(_) => false,
    }
}

/// Validate segment characters.
///
/// Valid segment: a-z, 0-9, hyphen (-), underscore (_)
///
/// Returns: `true` if valid, `false` otherwise
#[rustler::nif]
fn nif_validate_segment_chars(segment: Binary) -> bool {
    match std::str::from_utf8(segment.as_slice()) {
        Ok(s) => is_valid_segment_chars(s),
        Err(_) => false,
    }
}

/// Check if a type is a builtin MRI type.
///
/// Uses O(1) hash table lookup instead of 22 pattern matches.
///
/// Returns: `true` if builtin, `false` otherwise
#[rustler::nif]
fn nif_is_builtin_type(type_bin: Binary) -> bool {
    match std::str::from_utf8(type_bin.as_slice()) {
        Ok(s) => BUILTIN_TYPES.contains(s),
        Err(_) => false,
    }
}

/// Join path segments into a single path binary.
///
/// Single-allocation implementation - calculates total size first,
/// then writes all segments with "/" separators.
///
/// Returns: Joined path binary (e.g., "foo/bar/baz")
#[rustler::nif]
fn nif_join_path_segments<'a>(env: Env<'a>, segments: Vec<Binary>) -> NifResult<Binary<'a>> {
    if segments.is_empty() {
        let empty = OwnedBinary::new(0).ok_or(rustler::Error::Term(Box::new(
            "Failed to allocate binary",
        )))?;
        return Ok(empty.release(env));
    }

    // Calculate total size
    let total_size: usize = segments.iter().map(|s| s.len()).sum::<usize>()
        + segments.len().saturating_sub(1); // separators

    let mut output = OwnedBinary::new(total_size).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;

    let mut offset = 0;
    for (i, segment) in segments.iter().enumerate() {
        if i > 0 {
            output.as_mut_slice()[offset] = b'/';
            offset += 1;
        }
        output.as_mut_slice()[offset..offset + segment.len()].copy_from_slice(segment.as_slice());
        offset += segment.len();
    }

    Ok(output.release(env))
}

/// Format an MRI from components.
///
/// Takes type, realm, and path segments, returns formatted MRI string.
///
/// Returns: `{ok, MRI}` or `{error, Reason}`
#[rustler::nif]
fn nif_format_mri<'a>(
    env: Env<'a>,
    type_bin: Binary,
    realm_bin: Binary,
    path_segments: Vec<Binary>,
) -> NifResult<Term<'a>> {
    let type_str = match std::str::from_utf8(type_bin.as_slice()) {
        Ok(s) => s,
        Err(_) => return Ok((atoms::error(), atoms::invalid_type()).encode(env)),
    };

    let realm_str = match std::str::from_utf8(realm_bin.as_slice()) {
        Ok(s) => s,
        Err(_) => return Ok((atoms::error(), atoms::invalid_realm()).encode(env)),
    };

    // Calculate size: "mri:" + type + ":" + realm + "/" + segments
    let mut size = 4 + type_str.len() + 1 + realm_str.len();
    for seg in &path_segments {
        size += 1 + seg.len(); // "/" + segment
    }

    let mut output = OwnedBinary::new(size).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;

    let mut offset = 0;
    let slice = output.as_mut_slice();

    // Write "mri:"
    slice[offset..offset + 4].copy_from_slice(b"mri:");
    offset += 4;

    // Write type
    slice[offset..offset + type_str.len()].copy_from_slice(type_str.as_bytes());
    offset += type_str.len();

    // Write ":"
    slice[offset] = b':';
    offset += 1;

    // Write realm
    slice[offset..offset + realm_str.len()].copy_from_slice(realm_str.as_bytes());
    offset += realm_str.len();

    // Write path segments
    for seg in &path_segments {
        slice[offset] = b'/';
        offset += 1;
        slice[offset..offset + seg.len()].copy_from_slice(seg.as_slice());
        offset += seg.len();
    }

    Ok((atoms::ok(), output.release(env)).encode(env))
}

/// Find children of a parent MRI from a list of MRIs.
///
/// Children are MRIs that:
/// - Have the same realm as parent
/// - Have path that starts with parent's path
/// - Have exactly one more path segment than parent
///
/// This is O(n) but with very efficient prefix matching in Rust.
///
/// Arguments:
/// - parent_realm: The parent's realm binary
/// - parent_path: The parent's path segments (list of binaries)
/// - all_mris: List of {Realm, Path, MRI} tuples to search
///
/// Returns: List of matching MRI binaries
#[rustler::nif]
#[allow(unused_variables)]
fn nif_find_children<'a>(
    env: Env<'a>,
    parent_realm: Binary,
    parent_path: Vec<Binary>,
    all_mris: Vec<(Binary<'a>, Vec<Binary<'a>>, Binary<'a>)>,
) -> NifResult<Vec<Binary<'a>>> {
    let parent_depth = parent_path.len();
    let mut results: Vec<Binary<'a>> = Vec::new();

    for (realm, path, mri) in all_mris {
        // Check realm match
        if realm.as_slice() != parent_realm.as_slice() {
            continue;
        }

        // Check depth (must be exactly parent_depth + 1)
        if path.len() != parent_depth + 1 {
            continue;
        }

        // Check prefix match
        let mut prefix_matches = true;
        for (i, parent_seg) in parent_path.iter().enumerate() {
            if path[i].as_slice() != parent_seg.as_slice() {
                prefix_matches = false;
                break;
            }
        }

        if prefix_matches {
            results.push(mri);
        }
    }

    Ok(results)
}

/// Find descendants of a parent MRI from a list of MRIs.
///
/// Descendants are MRIs that:
/// - Have the same realm as parent
/// - Have path that starts with parent's path
/// - Have at least one more path segment than parent
///
/// This is O(n) but with very efficient prefix matching in Rust.
///
/// Arguments:
/// - parent_realm: The parent's realm binary
/// - parent_path: The parent's path segments (list of binaries)
/// - all_mris: List of {Realm, Path, MRI} tuples to search
///
/// Returns: List of matching MRI binaries
#[rustler::nif]
#[allow(unused_variables)]
fn nif_find_descendants<'a>(
    env: Env<'a>,
    parent_realm: Binary,
    parent_path: Vec<Binary>,
    all_mris: Vec<(Binary<'a>, Vec<Binary<'a>>, Binary<'a>)>,
) -> NifResult<Vec<Binary<'a>>> {
    let parent_depth = parent_path.len();
    let mut results: Vec<Binary<'a>> = Vec::new();

    for (realm, path, mri) in all_mris {
        // Check realm match
        if realm.as_slice() != parent_realm.as_slice() {
            continue;
        }

        // Check depth (must be > parent_depth)
        if path.len() <= parent_depth {
            continue;
        }

        // Check prefix match
        let mut prefix_matches = true;
        for (i, parent_seg) in parent_path.iter().enumerate() {
            if path[i].as_slice() != parent_seg.as_slice() {
                prefix_matches = false;
                break;
            }
        }

        if prefix_matches {
            results.push(mri);
        }
    }

    Ok(results)
}

/// Build a trie index from a list of MRIs and return a handle.
///
/// This creates an in-memory trie structure that can be used for
/// efficient O(d) hierarchy queries, where d is the path depth.
///
/// For million-scale deployments, this provides orders of magnitude
/// faster queries compared to O(n) list scanning.
///
/// Arguments:
/// - mris: List of {Realm, Path, MRI} tuples
///
/// Returns: `{ok, IndexHandle}` where IndexHandle is an opaque reference
#[rustler::nif]
fn nif_build_path_index<'a>(
    env: Env<'a>,
    mris: Vec<(Binary<'a>, Vec<Binary<'a>>, Binary<'a>)>,
) -> NifResult<Term<'a>> {
    let resource = MriIndexResource::new();

    {
        let mut index = resource.0.write().map_err(|_| {
            rustler::Error::Term(Box::new("Failed to acquire write lock"))
        })?;

        for (realm_bin, path_bins, mri_bin) in mris {
            // Convert realm to string
            let realm = match std::str::from_utf8(realm_bin.as_slice()) {
                Ok(s) => s,
                Err(_) => continue, // Skip invalid entries
            };

            // Convert path segments to strings
            let path: Vec<String> = path_bins
                .iter()
                .filter_map(|b| std::str::from_utf8(b.as_slice()).ok().map(String::from))
                .collect();

            // Store the full MRI binary
            let mri = mri_bin.as_slice().to_vec();

            index.insert(realm, path, mri);
        }
    }

    let arc = ResourceArc::new(resource);
    Ok((atoms::ok(), arc).encode(env))
}

/// Find direct children using a trie index.
///
/// O(d) complexity where d is the path depth, vs O(n) for list scanning.
///
/// Arguments:
/// - index: Index handle from build_path_index/1
/// - realm: Realm binary to search within
/// - path: Parent path segments
///
/// Returns: List of MRI binaries for direct children
#[rustler::nif]
fn nif_index_find_children<'a>(
    env: Env<'a>,
    index: ResourceArc<MriIndexResource>,
    realm_bin: Binary,
    path_bins: Vec<Binary>,
) -> NifResult<Term<'a>> {
    let realm = match std::str::from_utf8(realm_bin.as_slice()) {
        Ok(s) => s,
        Err(_) => return Ok((atoms::error(), atoms::invalid_realm()).encode(env)),
    };

    let path: Vec<String> = path_bins
        .iter()
        .filter_map(|b| std::str::from_utf8(b.as_slice()).ok().map(String::from))
        .collect();

    let guard = index.0.read().map_err(|_| {
        rustler::Error::Term(Box::new("Failed to acquire read lock"))
    })?;

    let results = guard.find_children(realm, &path);

    // Convert to list of binaries
    let binaries: Result<Vec<Binary>, _> = results
        .iter()
        .map(|mri| bytes_to_binary(env, mri))
        .collect();

    match binaries {
        Ok(bins) => Ok((atoms::ok(), bins).encode(env)),
        Err(e) => Err(e),
    }
}

/// Find all descendants using a trie index.
///
/// O(d + m) complexity where d is path depth and m is number of descendants,
/// vs O(n) for list scanning where n is total MRIs.
///
/// Arguments:
/// - index: Index handle from build_path_index/1
/// - realm: Realm binary to search within
/// - path: Parent path segments
///
/// Returns: List of MRI binaries for all descendants
#[rustler::nif]
fn nif_index_find_descendants<'a>(
    env: Env<'a>,
    index: ResourceArc<MriIndexResource>,
    realm_bin: Binary,
    path_bins: Vec<Binary>,
) -> NifResult<Term<'a>> {
    let realm = match std::str::from_utf8(realm_bin.as_slice()) {
        Ok(s) => s,
        Err(_) => return Ok((atoms::error(), atoms::invalid_realm()).encode(env)),
    };

    let path: Vec<String> = path_bins
        .iter()
        .filter_map(|b| std::str::from_utf8(b.as_slice()).ok().map(String::from))
        .collect();

    let guard = index.0.read().map_err(|_| {
        rustler::Error::Term(Box::new("Failed to acquire read lock"))
    })?;

    let results = guard.find_descendants(realm, &path);

    // Convert to list of binaries
    let binaries: Result<Vec<Binary>, _> = results
        .iter()
        .map(|mri| bytes_to_binary(env, mri))
        .collect();

    match binaries {
        Ok(bins) => Ok((atoms::ok(), bins).encode(env)),
        Err(e) => Err(e),
    }
}

/// Insert a single MRI into an existing index.
///
/// Use this for dynamic updates when devices connect/disconnect.
///
/// Arguments:
/// - index: Index handle from build_path_index/1
/// - realm: Realm binary
/// - path: Path segments
/// - mri: Full MRI binary to store
///
/// Returns: `ok` or `{error, Reason}`
#[rustler::nif]
fn nif_index_insert<'a>(
    env: Env<'a>,
    index: ResourceArc<MriIndexResource>,
    realm_bin: Binary,
    path_bins: Vec<Binary>,
    mri_bin: Binary,
) -> NifResult<Term<'a>> {
    let realm = match std::str::from_utf8(realm_bin.as_slice()) {
        Ok(s) => s,
        Err(_) => return Ok((atoms::error(), atoms::invalid_realm()).encode(env)),
    };

    let path: Vec<String> = path_bins
        .iter()
        .filter_map(|b| std::str::from_utf8(b.as_slice()).ok().map(String::from))
        .collect();

    let mri = mri_bin.as_slice().to_vec();

    let mut guard = index.0.write().map_err(|_| {
        rustler::Error::Term(Box::new("Failed to acquire write lock"))
    })?;

    guard.insert(realm, path, mri);

    Ok(atoms::ok().encode(env))
}

/// Remove a single MRI from an existing index.
///
/// Use this for dynamic updates when devices disconnect.
///
/// Arguments:
/// - index: Index handle from build_path_index/1
/// - realm: Realm binary
/// - path: Path segments to remove
///
/// Returns: `ok` if removed, `{error, not_found}` if path not in index
#[rustler::nif]
fn nif_index_remove<'a>(
    env: Env<'a>,
    index: ResourceArc<MriIndexResource>,
    realm_bin: Binary,
    path_bins: Vec<Binary>,
) -> NifResult<Term<'a>> {
    let realm = match std::str::from_utf8(realm_bin.as_slice()) {
        Ok(s) => s,
        Err(_) => return Ok((atoms::error(), atoms::invalid_realm()).encode(env)),
    };

    let path: Vec<String> = path_bins
        .iter()
        .filter_map(|b| std::str::from_utf8(b.as_slice()).ok().map(String::from))
        .collect();

    let mut guard = index.0.write().map_err(|_| {
        rustler::Error::Term(Box::new("Failed to acquire write lock"))
    })?;

    if guard.remove(realm, &path) {
        Ok(atoms::ok().encode(env))
    } else {
        Ok((atoms::error(), atoms::not_found()).encode(env))
    }
}

/// Get the number of MRIs in an index.
///
/// Arguments:
/// - index: Index handle from build_path_index/1
///
/// Returns: `{ok, Count}`
#[rustler::nif]
fn nif_index_size<'a>(
    env: Env<'a>,
    index: ResourceArc<MriIndexResource>,
) -> NifResult<Term<'a>> {
    let guard = index.0.read().map_err(|_| {
        rustler::Error::Term(Box::new("Failed to acquire read lock"))
    })?;

    Ok((atoms::ok(), guard.len()).encode(env))
}

// ============================================================================
// Helper functions
// ============================================================================

/// Check if string contains only valid realm characters (a-z, 0-9, .)
fn is_valid_realm_chars(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    s.bytes().all(|c| {
        (c >= b'a' && c <= b'z') || (c >= b'0' && c <= b'9') || c == b'.'
    })
}

/// Check if string contains only valid segment characters (a-z, 0-9, -, _)
fn is_valid_segment_chars(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    s.bytes().all(|c| {
        (c >= b'a' && c <= b'z') || (c >= b'0' && c <= b'9') || c == b'-' || c == b'_'
    })
}

/// Convert a string slice to an OwnedBinary
fn string_to_binary<'a>(env: Env<'a>, s: &str) -> NifResult<Binary<'a>> {
    let mut output = OwnedBinary::new(s.len()).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(s.as_bytes());
    Ok(output.release(env))
}

/// Convert a byte slice to an OwnedBinary
fn bytes_to_binary<'a>(env: Env<'a>, bytes: &[u8]) -> NifResult<Binary<'a>> {
    let mut output = OwnedBinary::new(bytes.len()).ok_or(rustler::Error::Term(Box::new(
        "Failed to allocate binary",
    )))?;
    output.as_mut_slice().copy_from_slice(bytes);
    Ok(output.release(env))
}

rustler::init!("macula_mri_nif", load = on_load);

fn on_load(env: Env, _info: Term) -> bool {
    let _ = rustler::resource!(MriIndexResource, env);
    true
}
