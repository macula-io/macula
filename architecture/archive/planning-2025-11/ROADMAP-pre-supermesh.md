# ROADMAP-pre-supermesh.md

> **Archived:** 2025-12-01
> **Superseded by:** `architecture/ROADMAP.md` (v3.0 - SuperMesh Architecture)

This document preserves the state of the roadmap before the December 2025 SuperMesh architecture refinement.

## What Changed

The following architectural decisions were revised:

| Before | After | Rationale |
|--------|-------|-----------|
| reckon_db dependency for Platform Layer | CRDTs + Gossip (internal) | No external dependency, simpler |
| Raft consensus for coordination | CRDTs (leaderless) | No quorum management, partition tolerant |
| MuC (Macula Micro Center) | Macula Cluster | Deployment-agnostic, not K8s-specific |
| BSN (Bootstrap Replica Node) | Seed Node | Just discoverable peer, no Raft |
| Single mesh | SuperMesh (Cluster federation) | Cross-org connectivity via Bridge Nodes |
| BEAM-only | Protocol Gateway | HTTP/3 API for non-BEAM clients |

## Terminology Evolution

| Term | Status | Notes |
|------|--------|-------|
| MuC | Deprecated | Originally "Macula Micro Center" - too K8s-specific |
| Macula Cell | Deprecated | Replaced by "Macula Cluster" (Dec 2025) - "Cell" implies atomic/singular |
| **Macula Cluster** | Current | Better conveys "group of cooperating nodes" |
| Realm | Unchanged | Virtual namespace spanning Clusters (orthogonal to Cluster) |

## Key Additions

1. **Bridge Nodes** - Cross-Cluster connectivity
2. **Federated Registry** - Secure app distribution
3. **Protocol Gateway** - HTTP/3 API
4. **Cluster Controller** - App deployment management

## Conceptual Clarification

**Fractal mesh hierarchy:**
```
Cluster (Home)           ←── Smallest unit (1-10 nodes)
    └─► Street Mesh
            └─► Neighborhood Mesh
                    └─► City Mesh
                            └─► Province Mesh
                                    └─► Country Mesh
                                            └─► Region Mesh
                                                    └─► Global Mesh
```

- **Cluster** = Smallest unit: local deployment (home, office, edge). 1-10 nodes.
- **SuperMesh** = Any federation level above Cluster. Fractal - nests at any scale.
- **Realm** = Virtual namespace (like DNS domain). Spans the entire hierarchy.

**Orthogonal concepts:**
- A Cluster can host multiple Realms (multi-tenancy)
- A Realm can span the entire mesh hierarchy (geo-distribution)
- Each level of SuperMesh can have its own policies and trust boundaries

## See Also

- Current roadmap: `architecture/ROADMAP.md`
- Original 107KB roadmap: `ROADMAP.md` in this directory
- Platform vision (superseded): `PLATFORM_VISION.md` in this directory
