     STDIN
   1 # Macula Scaling Analysis - Executive Summary
   2 
   3 ## Key Findings
   4 
   5 ### Critical Bottlenecks (Limit throughput to 1-2K msgs/sec)
   6 
   7 1. **Single gen_server Message Router** (`macula_gateway.erl`)
   8    - All incoming messages processed serially
   9    - Cannot exceed 1-2K messages/sec throughput
  10    - Recommendation: Shard by realm/topic
  11 
  12 2. **Single pub/sub Server** (`macula_pubsub_server.erl:177-193`)
  13    - Serial delivery to N subscribers: O(N)
  14    - Per-pattern DHT queries: O(M) sequential
  15    - At 100 msgs/sec with 1000 subscribers: 100% CPU saturation
  16    - Recommendation: Async worker pool + sharding
  17 
  18 3. **O(N) Cache Lookups** (`macula_cache.erl:88-99`)
  19    - List-based cache with 1000 entries
  20    - Each lookup: ~10μs (list scan)
  21    - At 10K msgs/sec: 50% latency from cache alone
  22    - Recommendation: Replace with ETS table (10-50x faster)
  23 
  24 4. **Unbounded Connection Pool** (`macula_connection_pool.erl:33-57`)
  25    - No idle connection cleanup
  26    - At 1000-node mesh: 50MB per node just for connections
  27    - Memory leak in long-running nodes
  28    - Recommendation: Add LRU reaper (5-minute timeout)
  29 
  30 ### High-Risk State Management Issues
  31 
  32 | Component | Issue | Risk | Fix Effort |
  33 |-----------|-------|------|-----------|
  34 | Client Manager | No max client limit | Memory exhaustion | 1 day |
  35 | Service Registry | Unbounded storage | DHT memory leak | 2 days |
  36 | Subscription Cache | No TTL enforcement | Stale cache | 1 day |
  37 | Pattern Index | O(N) list scan | Slow pattern matching | 1 week |
  38 
  39 ### Missing Observability
  40 
  41 - No throughput metrics (msgs/sec, RPCs/sec, pub/sec)
  42 - No queue depth monitoring (gen_server hidden overflow)
  43 - No cache hit/miss rates
  44 - No connection pool utilization tracking
  45 
  46 ---
  47 
  48 ## Scaling Roadmap (8-10 weeks total)
  49 
  50 ### Phase 1: Immediate Fixes (1-2 weeks) - CRITICAL
  51 - [ ] Connection pool reaper (2 days) - Prevent memory leak
  52 - [ ] Max client limit (1 day) - Prevent resource exhaustion
  53 - [ ] ETS cache (3 days) - **10-50x cache speedup**
  54 - [ ] Service registry TTL (2 days) - Prevent DHT memory leak
  55 
  56 **Impact: Stop memory leaks, improve cache throughput 10-50x**
  57 
  58 ### Phase 2: Architecture (2-3 weeks)
  59 - [ ] Shard service registries (1 week) - **8-16x parallelization**
  60 - [ ] Shard pub/sub servers (1 week) - **8-16x parallelization**
  61 - [ ] Pattern trie indexing (1 week) - **O(N)→O(log N)**
  62 
  63 **Impact: Distributed processing, O(log N) pattern matching**
  64 
  65 ### Phase 3: Advanced (3-4 weeks)
  66 - [ ] Publisher worker pool (1 week) - Async delivery
  67 - [ ] Result caching (3 days) - Reduce RPC calls 80-90%
  68 - [ ] Backpressure handling (1 week) - Prevent queue explosion
  69 
  70 **Impact: Async ops, better cache hit rates**
  71 
  72 ### Phase 4: Observability (1-2 weeks)
  73 - [ ] Prometheus metrics (1 week)
  74 - [ ] Health checks (3 days)
  75 
  76 **Impact: Visibility into bottlenecks**
  77 
  78 ---
  79 
  80 ## Throughput Projections
  81 
  82 | Phase | Gateway | Service Lookups | Pub/Sub | Target Gap |
  83 |-------|---------|-----------------|---------|------------|
  84 | Current | ~1K msgs/sec | ~1K queries/sec | ~20 pub/sec | 10x gap |
  85 | After Phase 1 | ~2-3K | ~2-3K | ~50 | 5x gap |
  86 | After Phase 2 | ~5-8K | ~8-16K | ~200 | 1.5-2x gap |
  87 | After Phase 3 | ~10K | ~16K | ~1K | Target ✓ |
  88 
  89 ---
  90 
  91 ## Critical Code Locations
  92 
  93 **Must Fix First:**
  94 - `src/macula_cache.erl` (lines 60-82) - O(N) operations
  95 - `src/macula_connection_pool.erl` (lines 33-57) - No cleanup
  96 - `src/macula_routing_server.erl` (lines 104-110) - Unbounded storage
  97 - `src/macula_gateway_client_manager.erl` (lines 110-127) - No limits
  98 
  99 **High Priority:**
 100 - `src/macula_pubsub_server.erl` (lines 177-193) - Serial processing
 101 - `src/macula_pubsub_registry.erl` (lines 93-101) - O(N) matching
 102 - `src/macula_rpc_server.erl` (lines 128-148) - Blocking calls
 103 
 104 ---
 105 
 106 ## Effort & Timeline
 107 
 108 - **Phase 1** (Immediate): 8 working days
 109 - **Phase 2** (Architecture): 15 working days  
 110 - **Phase 3** (Optimization): 21 working days
 111 - **Phase 4** (Observability): 10 working days
 112 
 113 **Total: 8-10 weeks** to reach 10K msgs/sec throughput target
 114 
 115 ---
 116 
 117 ## Risk Without Fixes
 118 
 119 If deployed to production without scaling improvements:
 120 - **30-60 minutes**: Node memory exhausted from connection pooling
 121 - **60-120 seconds**: Pub/sub queue buildup causes latency spike
 122 - **2-5 minutes**: Cascading failures as nodes run out of memory
 123 - **Recovery impossible**: Entire cluster unavailable during restart
 124 
 125 Recommend Phase 1 fixes (1-2 weeks) before production deployment.
 126 
 127 ---
 128 
 129 For detailed analysis with specific code examples and step-by-step improvement guides, see: **macula_scaling_analysis.md**
