# Own-namespace fixtures, shared by the SDKs

Signed procedure advertisements in a node's own namespace, `~<node_id>/<name>` (D25 item 6, revised
2026-09-24), and the verdicts every SDK must reach on them. A procedure in a node's own namespace carries no
authorization. It is accepted only when the 64 lowercase hex characters after `~` are the advertisement's
`advertiser_node`, which verifying the record binds to its signer.

- `pq_pure/` and `pq_hybrid/`: one advertisement per case, the record's wire form, signed by a throwaway
  identity of that profile.
- `verdicts.json`: for each file, its profile, its procedure, the time to evaluate it at (`now_ms`, since
  advertisements expire), and the two expected verdicts:
  - `own_namespace`: `macula_record:own_namespace/1`, the rule the station's two admissions also apply;
  - `verify_authorization`: `macula_record:verify_authorization/3` with no realm key, what a caller decides.
  A verdict is `ok` or the refusal's name.

| Case | own_namespace | verify_authorization |
|---|---|---|
| `own_ok` | ok | ok |
| `own_other_node`: another node's hex | not_own_namespace | not_own_namespace |
| `own_with_authorization`: an authorization attached | authorization_not_allowed | authorization_not_allowed |
| `own_uppercase_hex` | malformed | malformed |
| `own_short_hex`: 62 characters | malformed | malformed |
| `org_without_chain`: `acme/ring`, no chain | not_own_namespace | no_authorization |

`test/macula_own_namespace_fixtures_tests.erl` holds macula to every verdict; macula-go runs the same files.
Regenerate with `scripts/generate-own-namespace-fixtures.sh` after `rebar3 compile`: new identities and new
signatures, the same cases and verdicts.
