# CLAUDE.md - Macula Project Guidelines

**Current Version**: v0.44.1 (April 2026)

---

## CRITICAL USER SENSITIVITIES

These are non-negotiable requirements. Violations will result in rejection:

### 1. NO TODO STUBS - Code Must Be Functional
- **NEVER** leave TODO comments with stub implementations
- **NEVER** write placeholder code that "will be implemented later"
- If a feature cannot be fully implemented, either:
  - Implement it completely, OR
  - Don't add the code at all - discuss with user first
- Every exported function MUST work as documented

### 2. Documentation Diagrams Must Be Proper
- **NEVER** use ASCII art diagrams in edoc comments (they break XML parsing)
- **ALWAYS** create proper SVG diagrams and link them from documentation
- Store SVG diagrams in `doc/diagrams/` directory
- Use `@image` edoc tag or HTML `<img>` tags to embed

### 3. Verify ALL Links Recursively
- Before any documentation commit, verify ALL links work
- Check internal links (to other modules, files, sections)
- Check external links (hex docs, external sites)
- Use automated tools: `rebar3 edoc` must pass with 0 warnings
- Broken links are unacceptable - they waste user time

### 4. No Half-Measures
- Features are either complete or not present
- No "partial implementations" or "basic support"
- Every commit should leave codebase in working state

---

See [CHANGELOG.md](CHANGELOG.md) for version history.

---

## Architecture Overview

Macula is a **federated relay mesh**. Nodes connect outbound to relays over QUIC.
Relays route pub/sub, RPC, and Erlang distribution traffic.

Key documentation:
- `docs/guides/DIST_OVER_MESH_GUIDE.md` - Erlang distribution over relay mesh
- `docs/guides/CLUSTERING_GUIDE.md` - Cluster formation
- `docs/GLOSSARY.md` - Complete terminology reference
- `docs/operator/TLS_GUIDE.md` - TLS configuration

### Distribution Modes

| Mode | When | How |
|------|------|-----|
| **Direct QUIC** | Nodes can reach each other | `-proto_dist macula` |
| **Relay mesh** | Nodes behind NAT/firewalls | `MACULA_DIST_MODE=relay` |

### `*_system` Module Convention

All `*_system` modules are **supervisors only**. They contain child specifications and supervision strategy. They do NOT contain business logic, message handling, or state management.

---

## Docker Build Best Practices

**Problem**: Docker build cache can use stale layers even after source code changes, leading to outdated code in the image.

**Solution**: Always prune build cache and rebuild from scratch when testing code changes:

```bash
docker builder prune -af
docker compose -f docker-compose.multi-node-test.yml build --no-cache
```

Don't waste time inspecting Docker images to verify code or trusting cached builds after changes. Just prune and rebuild from scratch.

## Testing Workflow

1. Make code changes
2. Compile: `rebar3 compile`
3. Prune Docker cache: `docker builder prune -af`
4. Rebuild image: `docker compose -f <compose-file> build --no-cache`
5. Run tests: `docker compose -f <compose-file> up`

## Coding Guidelines

Follow **Idiomatic Erlang** principles for all code:

### Core Principles
- **Avoid deep nesting** - Keep nesting to 1-2 levels maximum
- **Avoid `if` and `cond`** - Use pattern matching instead
- **Prefer multiple function clauses** over deep nesting
- **Prefer declarative style** over imperative style
- **Prefer guards** over `case` clauses
- **Avoid `try..catch`** - Not idiomatic Erlang (use pattern matching on return values)

### Examples

**Bad: Using `if` and deep nesting**
```erlang
process_message(Msg, State) ->
    if
        is_binary(Msg) ->
            case decode_message(Msg) of
                {ok, Data} ->
                    if Data#data.type == request -> handle_request(Data, State);
                       true -> {error, unknown_type}
                    end;
                {error, Reason} -> {error, Reason}
            end;
        true -> {error, invalid_message}
    end.
```

**Good: Multiple function clauses with pattern matching**
```erlang
process_message(Msg, State) when is_binary(Msg) ->
    case decode_message(Msg) of
        {ok, Data} -> handle_decoded_message(Data, State);
        {error, Reason} -> {error, Reason}
    end;
process_message(_Msg, _State) ->
    {error, invalid_message}.

handle_decoded_message(#data{type = request} = Data, State) ->
    handle_request(Data, State);
handle_decoded_message(#data{type = response} = Data, State) ->
    handle_response(Data, State);
handle_decoded_message(_Data, _State) ->
    {error, unknown_type}.
```

### When `case` is Acceptable
Use `case` when you need to pattern match on a function result:

```erlang
process_request(Request, State) ->
    case validate_request(Request) of
        {ok, ValidRequest} -> handle_valid_request(ValidRequest, State);
        {error, Reason} -> {error, Reason}
    end.
```

### Key Takeaways
1. **Let it crash** - Don't catch errors unless you can handle them meaningfully
2. **Pattern match early** - Use function clause selection instead of conditionals
3. **Use guards liberally** - They're more readable than `case` or `if`
4. **Keep functions small** - Each function should do one thing
5. **Declarative > Imperative** - Express what you want, not how to get it

## Version Management

### Version Numbering

| Change Type | Version Bump | Example |
|-------------|--------------|---------|
| Breaking API change | MAJOR | v0.x.x -> v1.0.0 |
| New feature (backwards compatible) | MINOR | v0.42.x -> v0.43.0 |
| Bug fix, documentation, tests | PATCH | v0.44.0 -> v0.44.1 |

### After Completing Work - ALWAYS Update

1. **CLAUDE.md** - Add new version row, update "Current Version" in header
2. **ROADMAP.md** - Mark completed items, add new files/tests
3. **Sync docs** - `docs/GLOSSARY.md`, `docs/operator/*.md`, ADRs as needed

## Bash Scripting Guidelines

### NEVER Use HEREDOC

**CRITICAL**: Do NOT use heredoc syntax (cat <<'EOF' ... EOF) in bash commands or git commit messages.

**Bad:**
```bash
git commit -m "$(cat <<'EOF'
Commit message here.
EOF
)"
```

**Good:**
```bash
git commit -m "Commit message here."
```

**Rules:**
1. Use direct strings with proper quoting (single or double quotes)
2. Use -F flag for git commit if message is in a file
3. Prefer shell scripts over complex one-liners
4. Keep bash commands simple and readable

---

## Support

If you find this project valuable, consider supporting its development:

https://buymeacoffee.com/rlefever
