# Refactoring Ready: macula_client → macula_client

## ✅ Preparation Complete

All refactoring preparation is complete and ready to execute.

### What's Been Prepared

1. **Refactoring Plan** (`REFACTORING_SDK_TO_CLIENT.md`)
   - Comprehensive step-by-step plan
   - Rationale and breaking changes documented
   - Migration path for users

2. **Automated Script** (`scripts/refactor-sdk-to-client.sh`)
   - Renames directory: `apps/macula_client` → `apps/macula_client`
   - Renames all files: `macula_client_*.erl` → `macula_client_*.erl`
   - Updates all module references in code
   - Updates `.app.src` application name
   - Updates version: `0.3.4` → `0.4.0`
   - Updates root `rebar.config`
   - Updates umbrella app dependencies
   - Updates all documentation
   - Updates all examples
   - Checks for remaining references

3. **Branch Created**
   - Branch: `refactor/rename-sdk-to-client`
   - Ready for changes

### Files That Will Be Changed

**Renamed:**
- `apps/macula_client/` → `apps/macula_client/`
- `macula_client.erl` → `macula_client.erl`
- `macula_client_app.erl` → `macula_client_app.erl`
- `macula_client_sup.erl` → `macula_client_sup.erl`
- `macula_client_client.erl` → `macula_client_client.erl`
- All test files: `macula_client_*_tests.erl` → `macula_client_*_tests.erl`

**Modified Content:**
- `apps/macula_client/src/*.erl` - Module names updated
- `apps/macula_client/src/macula_client.app.src` - Application name + version
- `apps/macula/src/macula.app.src` - Dependency reference
- `rebar.config` - Release configuration
- `README.md` - All references
- `architecture/*.md` - All code examples
- `docs/*.md` - API references
- `examples/*.erl` - Example code

### To Execute

Simply run:
```bash
cd /home/rl/work/github.com/macula-io/macula
./scripts/refactor-sdk-to-client.sh
```

### After Execution

1. **Review changes**:
   ```bash
   git status
   git diff --staged
   ```

2. **Test compilation**:
   ```bash
   rebar3 clean
   rebar3 compile
   ```

3. **Run tests**:
   ```bash
   rebar3 eunit
   ```

4. **Commit**:
   ```bash
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

   See MIGRATION_0.3_TO_0.4.md for detailed migration guide."
   ```

5. **Update macula-console**:
   ```elixir
   # In apps/macula_console/mix.exs
   {:macula, "~> 0.4.0"}

   # In application code
   :macula_client.call(...)  # instead of :macula_client.call(...)
   ```

### Breaking Change Summary

**Before (0.3.x)**:
```erlang
macula_client:start_link(Config).
macula_client:call(Client, Procedure, Args).
macula_client:subscribe(Client, Topic, Handler).
```

**After (0.4.0)**:
```erlang
macula_client:start_link(Config).
macula_client:call(Client, Procedure, Args).
macula_client:subscribe(Client, Topic, Handler).
```

### Risk Assessment

- **Risk Level**: Low
- **Reason**: Pre-1.0 version, internal API change, no external dependencies yet
- **Impact**: Only affects macula-console (which we control)
- **Rollback**: Easy (just revert the commit)

### Ready to Proceed?

Execute the script when ready. The entire process takes ~30 seconds.

---

**Status**: ✅ Ready to execute
**Estimated Time**: 30 seconds
**Next Action**: Run `./scripts/refactor-sdk-to-client.sh`
