# Refactoring: macula_client → macula_client

## Rationale

The current naming `macula_client` is confusing when used in application code:
```erlang
macula_client:call(...)      % SDK calling? Doesn't make sense
```

The correct naming should be:
```erlang
macula_client:call(...)   % Client making a call - clear!
```

**Industry Standard**: Client libraries are named `*_client` (e.g., `httpc`, `gun_client`, `pgsql_client`)

## Changes Overview

### 1. Directory Rename
```bash
apps/macula_client/ → apps/macula_client/
```

### 2. Module Renames
All modules prefixed with `macula_client_*` become `macula_client_*`:
- `macula_client.erl` → `macula_client.erl`
- `macula_client_app.erl` → `macula_client_app.erl`
- `macula_client_sup.erl` → `macula_client_sup.erl`
- `macula_client_connection.erl` → `macula_client_connection.erl`
- etc.

### 3. Application Name
- `.app.src`: `{application, macula_client, ...}` → `{application, macula_client, ...}`

### 4. Version Bump
- Current: `0.3.4`
- New: `0.4.0` (breaking change)

## Files to Update

### Core Files
- [ ] `apps/macula_client/` → `apps/macula_client/`
- [ ] `apps/macula_client/src/macula_client*.erl` → `macula_client*.erl`
- [ ] `apps/macula_client/src/macula_client.app.src` → `macula_client.app.src`
- [ ] `apps/macula_client/include/macula_client*.hrl` → `macula_client*.hrl`

### Configuration Files
- [ ] Root `rebar.config` - No changes needed (umbrella doesn't reference sub-apps)
- [ ] `apps/macula_client/rebar.config` - Update if it references itself

### Documentation
- [ ] `README.md` - Update all references
- [ ] `architecture/*.md` - Update code examples
- [ ] `docs/*.md` - Update API references
- [ ] `examples/` - Update example code

### Dependencies
Check which apps depend on macula_client:
- [ ] `apps/macula/` (umbrella app)
- [ ] `apps/macula_gateway/` (might use for testing)
- [ ] Other apps

## Step-by-Step Execution Plan

### Step 1: Backup and Branch
```bash
cd /home/rl/work/github.com/macula-io/macula
git checkout -b refactor/rename-sdk-to-client
git status  # Ensure clean working tree
```

### Step 2: Rename Directory
```bash
mv apps/macula_client apps/macula_client
```

### Step 3: Rename Files
```bash
cd apps/macula_client/src
for file in macula_client*.erl; do
  git mv "$file" "${file/macula_client/macula_client}"
done

cd ../include
for file in macula_client*.hrl; do
  git mv "$file" "${file/macula_client/macula_client}"
done
```

### Step 4: Update File Contents
```bash
# Update all .erl files
find apps/macula_client -name "*.erl" -type f -exec sed -i 's/macula_client/macula_client/g' {} +

# Update all .hrl files
find apps/macula_client -name "*.hrl" -type f -exec sed -i 's/macula_client/macula_client/g' {} +

# Update .app.src
sed -i 's/macula_client/macula_client/g' apps/macula_client/src/macula_client.app.src

# Update version to 0.4.0
sed -i 's/0\.3\.4/0.4.0/g' apps/macula_client/src/macula_client.app.src
```

### Step 5: Update Dependencies
```bash
# Find all files that reference macula_client in other apps
grep -r "macula_client" apps/macula*/src/*.erl apps/macula*/rebar.config

# Update each file found
```

### Step 6: Update Documentation
```bash
# Update README
sed -i 's/macula_client/macula_client/g' README.md

# Update architecture docs
find architecture -name "*.md" -type f -exec sed -i 's/macula_client/macula_client/g' {} +

# Update other docs
find docs -name "*.md" -type f -exec sed -i 's/macula_client/macula_client/g' {} +
```

### Step 7: Update Examples
```bash
find examples -name "*.erl" -type f -exec sed -i 's/macula_client/macula_client/g' {} +
```

### Step 8: Test Compilation
```bash
rebar3 clean
rebar3 compile
rebar3 eunit
```

### Step 9: Git Commit
```bash
git add -A
git status  # Review changes

git commit -m "Refactor: Rename macula_client to macula_client

Breaking change: Module name changed from macula_client to macula_client
for better clarity and industry standard naming conventions.

Changes:
- Renamed directory: apps/macula_client → apps/macula_client
- Renamed all modules: macula_client_* → macula_client_*
- Updated .app.src: application name and version (0.3.4 → 0.4.0)
- Updated all documentation and examples
- Updated all internal references

Migration path for users:
- Change all calls from :macula_client to :macula_client
- Update mix.exs/rebar.config: {:macula, \"~> 0.4.0\"}

See MIGRATION_0.3_TO_0.4.md for detailed migration guide.
"
```

### Step 10: Create Migration Guide
Create `MIGRATION_0.3_TO_0.4.md` with instructions for users.

## Testing Checklist

- [ ] `rebar3 compile` succeeds
- [ ] `rebar3 eunit` passes all tests
- [ ] Examples compile and run
- [ ] Documentation builds without warnings
- [ ] Can publish to hex.pm as version 0.4.0

## Breaking Changes for Users

### Before (0.3.x)
```erlang
macula_client:start_link(Config).
macula_client:call(Client, Procedure, Args).
macula_client:subscribe(Client, Topic, Handler).
```

### After (0.4.0)
```erlang
macula_client:start_link(Config).
macula_client:call(Client, Procedure, Args).
macula_client:subscribe(Client, Topic, Handler).
```

### Elixir (via mix.exs)
```elixir
# Before
{:macula, "~> 0.3.4"}
:macula_client.call(...)

# After
{:macula, "~> 0.4.0"}
:macula_client.call(...)
```

## Rollout Plan

1. **Complete refactoring** on `refactor/rename-sdk-to-client` branch
2. **Test thoroughly** - ensure all compilation and tests pass
3. **Merge to main** via PR with detailed description
4. **Publish 0.4.0** to hex.pm
5. **Update macula-console** to use 0.4.0
6. **Deprecate 0.3.x** - add notice in hex.pm docs

## Post-Refactoring

- [ ] Update GitHub README badges
- [ ] Update hex.pm package description
- [ ] Announce breaking change in release notes
- [ ] Update any external documentation or blog posts

---

**Status**: Ready to execute
**Estimated Time**: 30 minutes
**Risk**: Low (pre-1.0 version, internal change)
