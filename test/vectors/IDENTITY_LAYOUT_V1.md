# Stored identity keys, by name and profile: the vectors

The contract every Macula SDK implements for the identity a program uses when it is given no key (macula#76). A node
identity belongs to one program running under one user account. The reference implementation is
`macula_node_keys:stored_identity/2`, `identity_dir/0` and `identity_path/3`, and `macula_client`'s connect, which logs
the key it uses. `identity_layout_v1.json` holds the vectors, and `test/macula_identity_layout_vectors_tests.erl` runs
every one of them on each run.

## The layout

Keys are stored per user account under the identity directory, `identity` in the platform's per-user data directory
(`~/.local/share/macula/identity` on Linux), one file per name and profile:

    <identity_dir>/<name>.<profile>.key

- `<name>` is `default` unless the program names itself. A name matches `name_pattern`: 1 to 64 lowercase ASCII
  letters, digits, `-` and `_`, starting with a letter or digit. Any other is refused before any file is touched.
- `<profile>` is the security profile the key is for, `pq_pure` or `pq_hybrid`. A key works in one profile only, so a
  program in both profiles is two nodes, with a key and a node_id for each.
- A connect with no key loads that file, or generates the key (puzzle solved) and stores it there the first time. A
  file that exists and will not load is refused and never replaced. Two first starts at once end with one key.
- An application may still pass its own key; then no file is read.
- **Every connect logs the key file it uses (or that the key was supplied) and the node_id.** A program that switches
  profile or name becomes another node, and grants pinned to the old node_id stop matching with no error; the log line
  is what shows it.

## The old key

The single `identity.key` of earlier releases lives next to the identity directory (`old_key_file`, relative to the
identity directory's parent). It is moved to `default.<its profile>.key`, where its profile is the one it loads in, and
is never read in place as a fallback. The move only ever creates (link, then remove the old name):

| `old_key` | `in_place` | `outcome` |
|---|---|---|
| a key of the node's profile, or of the other | nothing | `moved`: the old name is gone, the bytes are at the new name |
| a key | the same file | `moved`: a move cut short is finished |
| a key | another key | `place_taken`: refused, naming both paths; both files stay |
| not a key | nothing | refused with the load's reason (`bad_key_file`), naming the old path; it stays |

## What an SDK must pass

1. For each of `paths`, the file for `name` and `profile` is `file`, relative to the identity directory's parent.
2. Each of `refused_names` is refused, and nothing is created.
3. For each of `old_key_cases`, set up the old key and what is in place as described, run the stored-identity lookup
   for `default` in the node's profile, and reach `outcome`.
