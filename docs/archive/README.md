# Documentation Archive

> **Status:** Historical reference only
> **Last Updated:** 2025-11-28

This directory contains historical documentation that has been superseded, completed, or is maintained for reference purposes only.

---

## Directory Structure

```
archive/
├── development/     # Development guides and testing docs
├── migration/       # Migration guides between versions
├── planning/        # Historical planning documents
├── sessions/        # Development session summaries
└── versions/        # Version-specific release notes
```

---

## Categories

### Code Reviews & Analysis

Historical code quality analysis and review documents:

| Document | Description |
|----------|-------------|
| [CODE_REVIEW_REPORT.md](CODE_REVIEW_REPORT.md) | Comprehensive code quality analysis |
| [CODE_QUALITY_ANALYSIS.md](CODE_QUALITY_ANALYSIS.md) | Phase 1 quality analysis |
| [CODE_QUALITY_ANALYSIS_PHASE2.md](CODE_QUALITY_ANALYSIS_PHASE2.md) | Phase 2 quality analysis |
| [CODEBASE_REVIEW_2025-01-16.md](CODEBASE_REVIEW_2025-01-16.md) | January 2025 review |
| [ARCHITECTURE_ANALYSIS_NOVEMBER_2025.md](ARCHITECTURE_ANALYSIS_NOVEMBER_2025.md) | November 2025 architecture review |

### Refactoring History

Documents tracking major refactoring efforts:

| Document | Description |
|----------|-------------|
| [REFACTORING_PHASE2_COMPLETE.md](REFACTORING_PHASE2_COMPLETE.md) | Phase 2 completion summary |
| [GATEWAY_REFACTORING_V0.7.9.md](GATEWAY_REFACTORING_V0.7.9.md) | Gateway module extraction |
| [NOMENCLATURE_PROPOSAL_CONNECTION_TO_PEER.md](NOMENCLATURE_PROPOSAL_CONNECTION_TO_PEER.md) | v0.7.0 naming convention change |
| [PEER_CONNECTION_SEPARATION_PLAN.md](PEER_CONNECTION_SEPARATION_PLAN.md) | Module separation planning |

### Performance Analysis

Historical performance investigations:

| Document | Description |
|----------|-------------|
| [PERFORMANCE_ANALYSIS.md](PERFORMANCE_ANALYSIS.md) | Comprehensive performance study |
| [PERFORMANCE_FINDINGS_SUMMARY.md](PERFORMANCE_FINDINGS_SUMMARY.md) | Key findings summary |
| [macula_scaling_analysis.md](macula_scaling_analysis.md) | Scaling characteristics |
| [OPTIMIZATION_MASTER_INDEX.md](OPTIMIZATION_MASTER_INDEX.md) | Optimization tracking |

### Bug Fixes & Investigations

Documented bug investigations and fixes:

| Document | Description |
|----------|-------------|
| [BUG_PUBSUB_DHT_LOOKUP.md](BUG_PUBSUB_DHT_LOOKUP.md) | PubSub DHT lookup bug |
| [GATEWAY_STARTUP_INVESTIGATION.md](GATEWAY_STARTUP_INVESTIGATION.md) | Startup issues |
| [STREAM_ACCEPTANCE_ROOT_CAUSE.md](STREAM_ACCEPTANCE_ROOT_CAUSE.md) | Stream acceptance analysis |
| [TRY_CATCH_ANALYSIS.md](TRY_CATCH_ANALYSIS.md) | Error handling patterns |

### Implementation History

Historical implementation documentation:

| Document | Description |
|----------|-------------|
| [MULTI_HOP_RPC_ANALYSIS.md](MULTI_HOP_RPC_ANALYSIS.md) | Multi-hop RPC design |
| [MULTI_ENDPOINT_RPC_IMPLEMENTATION.md](MULTI_ENDPOINT_RPC_IMPLEMENTATION.md) | Multi-endpoint support |
| [FAILOVER_IMPLEMENTATION.md](FAILOVER_IMPLEMENTATION.md) | Failover mechanisms |
| [GATEWAY_TLS_IMPLEMENTATION.md](GATEWAY_TLS_IMPLEMENTATION.md) | TLS setup history |

### Testing History

Test results and coverage tracking:

| Document | Description |
|----------|-------------|
| [TEST_RESULTS.md](TEST_RESULTS.md) | Historical test results |
| [TEST_COVERAGE_STATUS.md](TEST_COVERAGE_STATUS.md) | Coverage tracking |
| [MULTI_NODE_TEST_RESULTS.md](MULTI_NODE_TEST_RESULTS.md) | Multi-node testing |

---

## Subdirectories

### development/

Development and testing guides:
- `TESTING.md` - Test suite documentation
- `HEX_PUBLICATION_GUIDE.md` - Hex.pm publishing process

### migration/

Version migration guides:
- `BONDY_TO_MACULA_*.md` - Migration from Bondy/WAMP architecture

### planning/

Historical planning documents (project phases, feature planning)

### sessions/

Development session summaries documenting decisions and progress:
- `SESSION_SUMMARY_*.md` - Daily/weekly session notes

### versions/

Version-specific release documentation

---

## Usage Notes

**These documents are for historical reference only.**

- Do not update archived documents
- Check the main `docs/` directory for current documentation
- Some links in archived documents may be broken (pointing to moved/deleted files)
- Code examples may reference deprecated APIs

---

## See Also

- [Current Documentation](../README.md)
- [Architecture Docs](../../architecture/)
- [Changelog](../../CHANGELOG.md)

