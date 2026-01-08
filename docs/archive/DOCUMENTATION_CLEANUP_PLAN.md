# Documentation Cleanup Plan for v0.6.0

**Date:** 2025-11-15
**Priority:** HIGH (affects first impressions and professional appearance)
**Estimated Time:** 2-3 hours

---

## Problem Statement

**Current State:**
- 56 markdown files in project root (extremely cluttered)
- README.md has badge rendering issues
- Many broken internal links
- Poor professional appearance on GitHub

**Impact:**
- Confusing first impression for new users
- Difficult to find relevant documentation
- Unprofessional appearance
- Poor UX for contributors

---

## Goals

1. ✅ Clean, professional GitHub landing page
2. ✅ Well-organized documentation structure
3. ✅ All links working correctly
4. ✅ Easy navigation for users and contributors
5. ✅ Maintain only essential files in root

---

## Proposed Structure

```
macula/
├── README.md                    # Clean landing page
├── CHANGELOG.md                 # Version history
├── LICENSE                      # Apache 2.0
├── HELLO_WORLD.md              # User-facing quick start
├── CLAUDE.md                    # Internal (keep for development)
├──
├── docs/
│   ├── README.md               # Documentation index
│   ├── KADEMLIA_DHT_ARCHITECTURE.md
│   ├── NOMENCLATURE_PROPOSAL_CONNECTION_TO_PEER.md
│   ├── PEER_VS_CONNECTION_ANALYSIS.md
│   ├── PEER_CONNECTION_SEPARATION_PLAN.md
│   ├── V0.6.0_RELEASE_SUMMARY.md
│   ├── QUIC_TLS_GATEWAY_SETUP.md
│   │
│   ├── development/            # Development guides
│   │   ├── DOCKER_BUILD_REFERENCE.md
│   │   ├── HEX_PUBLICATION_GUIDE.md
│   │   ├── MDNS_SETUP.md
│   │   └── TESTING.md
│   │
│   ├── planning/               # Feature planning
│   │   ├── IMPLEMENTATION_PLAN.md
│   │   ├── MULTI_NODE_TESTING_PLAN.md
│   │   └── PUBSUB_HANDLER_REFACTORING_PLAN.md
│   │
│   ├── archive/                # Historical/completed work
│   │   ├── ARCHITECTURE_ANALYSIS_NOVEMBER_2025.md
│   │   ├── CODE_QUALITY_*.md
│   │   ├── REFACTORING_*.md
│   │   ├── PERFORMANCE_*.md
│   │   └── ... (40+ files)
│   │
│   └── sessions/               # Development session summaries
│       └── SESSION_SUMMARY_*.md
│
└── architecture/               # Architecture docs (keep existing)
    └── ... (57 existing files)
```

---

## Files to Move

### Keep in Root (4 files)
- ✅ `README.md`
- ✅ `CHANGELOG.md`
- ✅ `HELLO_WORLD.md`
- ✅ `LICENSE`
- ⚠️ `CLAUDE.md` (internal, consider .gitignore)

### Move to docs/ (11 files - active docs)
Current docs/ files stay, new ones added:
- `KADEMLIA_DHT_ARCHITECTURE.md` (already there)
- `NOMENCLATURE_PROPOSAL_CONNECTION_TO_PEER.md` (already there)
- `PEER_VS_CONNECTION_ANALYSIS.md` (already there)
- `PEER_CONNECTION_SEPARATION_PLAN.md` (already there)
- `V0.6.0_RELEASE_SUMMARY.md` (already there)
- `QUIC_TLS_GATEWAY_SETUP.md` (move from root if exists)

### Move to docs/development/ (4 files)
- `DOCKER_BUILD_REFERENCE.md`
- `HEX_PUBLICATION_GUIDE.md`
- `MDNS_SETUP.md`
- `TESTING.md`

### Move to docs/planning/ (3 files)
- `IMPLEMENTATION_PLAN.md`
- `MULTI_NODE_TESTING_PLAN.md`
- `PUBSUB_HANDLER_REFACTORING_PLAN.md`

### Move to docs/sessions/ (2 files)
- `SESSION_SUMMARY_2025_01_10.md`
- Any other session summaries

### Move to docs/archive/ (~40 files)
All historical analysis, refactoring, and status documents:
- `ARCHITECTURE_ANALYSIS_NOVEMBER_2025.md`
- `CODEBASE_REVIEW_*.md` (3 files)
- `CODE_QUALITY_*.md` (5 files)
- `CODE_REVIEW_REPORT.md`
- `CHANGELOG_RPC_REPLY.md`
- `DHT_SERVICE_ADVERTISEMENT_STATUS.md`
- `FAILOVER_IMPLEMENTATION.md`
- `FINISHING_TOUCHES.md`
- `GATEWAY_*.md` (4 files)
- `macula_scaling_analysis.md`
- `MIGRATION_0.3_TO_0.4.md`
- `MULTI_*.md` (8 files)
- `OPTIMIZATION_MASTER_INDEX.md`
- `PERFORMANCE_*.md` (3 files)
- `PROVIDER_SELECTION_IMPLEMENTATION.md`
- `REFACTORING_*.md` (6 files)
- `RPC_ENHANCEMENT_SUMMARY.md`
- `SCALING_*.md` (2 files)
- `SIMPLIFICATION_RECOMMENDATIONS.md`
- `STREAM_*.md` (3 files)
- `TEST_*.md` (3 files)
- `TRY_CATCH_ANALYSIS.md`

---

## README.md Fixes

### Issue 1: Badge Rendering
**Problem:** Badges inside `<div>` tag break markdown rendering

**Current (BROKEN):**
```markdown
<div align="center">
  <img src="assets/macula-logo-color.svg" alt="Macula Logo" width="500"/>

  <h1>Macula HTTP/3 Mesh</h1>
  <p><em>A distributed platform for decentralized applications</em></p>

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-26+-brightgreen)](https://www.erlang.org)

</div>
```

**Fixed:**
```markdown
<div align="center">
  <img src="assets/macula-logo-color.svg" alt="Macula Logo" width="500"/>

  <h1>Macula HTTP/3 Mesh</h1>
  <p><em>A distributed platform for decentralized applications</em></p>
</div>

<p align="center">
  <a href="LICENSE"><img src="https://img.shields.io/badge/License-Apache%202.0-blue.svg" alt="License"/></a>
  <a href="https://www.erlang.org"><img src="https://img.shields.io/badge/Erlang%2FOTP-26+-brightgreen" alt="Erlang/OTP"/></a>
</p>

---
```

### Issue 2: Broken Links
Check and fix all internal documentation links:
- `[Hello World Tutorial](HELLO_WORLD.md)` ✅ (stays in root)
- `[Quick Start Guide](architecture/macula_http3_mesh_quick_start.md)` ✅ (correct path)
- `[Project Structure](architecture/MACULA_PROJECT_STRUCTURE.md)` ✅ (correct path)
- `[Memory Management](architecture/memory_management/README.md)` ✅ (correct path)
- Any links to moved files need updating

---

## Link Validation

### Script to Check All Links
```bash
#!/bin/bash
# docs/scripts/validate-links.sh

echo "=== CHECKING MARKDOWN LINKS ==="

# Find all markdown files
find . -name "*.md" -not -path "./_build/*" -not -path "./doc/*" | while read file; do
  echo "Checking: $file"

  # Extract markdown links [text](path)
  grep -oP '\[.*?\]\(\K[^)]+' "$file" | while read link; do
    # Skip external URLs
    if [[ $link =~ ^https?:// ]]; then
      continue
    fi

    # Check if file exists
    target_dir=$(dirname "$file")
    target_file="$target_dir/$link"

    if [ ! -f "$target_file" ] && [ ! -d "$target_file" ]; then
      echo "  ❌ BROKEN: $link (referenced in $file)"
    fi
  done
done
```

---

## Documentation Index

### Create docs/README.md
```markdown
# Macula Documentation Index

**Welcome to the Macula HTTP/3 Mesh documentation!**

## Getting Started
- [Quick Start Guide](../HELLO_WORLD.md)
- [Architecture Overview](../architecture/macula_http3_mesh_root.md)
- [Project Structure](../architecture/MACULA_PROJECT_STRUCTURE.md)

## Architecture
- [Kademlia DHT](KADEMLIA_DHT_ARCHITECTURE.md)
- [QUIC/TLS Setup](QUIC_TLS_GATEWAY_SETUP.md)
- [Memory Management](../architecture/memory_management/README.md)

## Development
- [Testing Guide](development/TESTING.md)
- [Docker Build Reference](development/DOCKER_BUILD_REFERENCE.md)
- [Hex Publication](development/HEX_PUBLICATION_GUIDE.md)
- [mDNS Setup](development/MDNS_SETUP.md)

## Planning & Design
- [Implementation Plan](planning/IMPLEMENTATION_PLAN.md)
- [Multi-Node Testing Plan](planning/MULTI_NODE_TESTING_PLAN.md)
- [Peer-Connection Separation](PEER_CONNECTION_SEPARATION_PLAN.md)

## Archive
Historical analysis and completed work: [archive/](archive/)

## Sessions
Development session summaries: [sessions/](sessions/)
```

---

## Implementation Steps

### Phase 1: Backup (5 min)
```bash
cd /home/rl/work/github.com/macula-io/macula
git status  # Check for uncommitted changes
git add .
git commit -m "Backup before documentation cleanup"
```

### Phase 2: Create Structure (5 min)
```bash
mkdir -p docs/development
mkdir -p docs/planning
mkdir -p docs/archive
mkdir -p docs/sessions
mkdir -p docs/scripts
```

### Phase 3: Move Files (10 min)
```bash
chmod +x /tmp/cleanup_docs.sh
/tmp/cleanup_docs.sh
```

### Phase 4: Fix README.md (10 min)
- Fix badge rendering (move outside `<div>`)
- Verify all links still work
- Test markdown rendering locally

### Phase 5: Create Index (15 min)
- Write `docs/README.md` with navigation
- Add `docs/scripts/validate-links.sh`
- Run link validation

### Phase 6: Update rebar.config (10 min)
Update ex_doc extras paths:
```erlang
{ex_doc, [
    {extras, [
        {"README.md", #{title => "Overview"}},
        {"HELLO_WORLD.md", #{title => "Hello World"}},
        {"CHANGELOG.md", #{title => "Changelog"}},
        {"LICENSE", #{title => "License"}},
        {"docs/KADEMLIA_DHT_ARCHITECTURE.md", #{title => "Kademlia DHT"}},
        {"architecture/MACULA_PROJECT_STRUCTURE.md", #{title => "Project Structure"}},
        {"architecture/macula_http3_mesh_root.md", #{title => "Architecture"}},
        {"architecture/macula_http3_mesh_quick_start.md", #{title => "Quick Start"}},
        {"architecture/macula_http3_mesh_hello_world.md", #{title => "Tutorial"}},
        {"architecture/macula_http3_mesh_roadmap.md", #{title => "Roadmap"}},
        {"architecture/DOCUMENTATION_STATUS.md", #{title => "Documentation Status"}},
        {"architecture/macula_http3_mesh_contributing.md", #{title => "Contributing"}}
    ]},
    {main, "readme"},
    {source_url, "https://github.com/macula-io/macula"},
    {prefix_ref_vsn_with_v, false}
]}.
```

### Phase 7: Test (20 min)
```bash
# Test documentation build
rebar3 ex_doc

# Check for warnings
# Verify all links work
# Test GitHub rendering (push to test branch)
```

### Phase 8: Commit (10 min)
```bash
git add .
git commit -m "docs: Reorganize documentation structure for v0.6.0

- Move 40+ historical docs to docs/archive/
- Organize active docs into docs/, docs/development/, docs/planning/
- Fix README.md badge rendering
- Create docs/README.md navigation index
- Validate and fix broken links
- Update rebar.config ex_doc configuration

Fixes #xxx (professional documentation structure)
"
```

---

## Success Criteria

- [ ] Root directory has ≤5 .md files
- [ ] README.md badges render correctly on GitHub
- [ ] All internal links work
- [ ] docs/README.md provides clear navigation
- [ ] `rebar3 ex_doc` builds without warnings
- [ ] GitHub repository looks professional
- [ ] Easy to find relevant documentation

---

## Rollback Plan

If something goes wrong:
```bash
git reset --hard HEAD~1  # Undo last commit
# Or:
git checkout <previous-commit-hash>
```

All moved files are tracked by git, so nothing is lost.

---

## Next Steps After Cleanup

1. Update CONTRIBUTING.md to reference new structure
2. Add .github/ISSUE_TEMPLATE/ with documentation guidelines
3. Consider adding docs/CONTRIBUTING_DOCS.md for documentation standards
4. Add GitHub Actions workflow to validate links on PR

---

**Recommendation:** Execute this plan as a separate focused session before v0.6.0 publication.

**Estimated Total Time:** 2-3 hours (including testing)
