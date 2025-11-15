     STDIN
   1 # NAT Traversal Strategy - Quick Reference
   2 
   3 **Full Details**: See [NAT_TRAVERSAL_ROADMAP.md](NAT_TRAVERSAL_ROADMAP.md)
   4 
   5 ---
   6 
   7 ## Problem
   8 
   9 Edge peers behind NAT/firewalls cannot accept incoming connections → all traffic must relay through gateway (bottleneck).
  10 
  11 ---
  12 
  13 ## Solution: 3-Phase Approach
  14 
  15 ### Phase 1: Gateway Relay (v0.7.x - CURRENT) ✅
  16 - **Works**: 100% (everywhere)
  17 - **Direct P2P**: 0%
  18 - **Infrastructure**: Gateway only
  19 - **Status**: Implemented
  20 
  21 ### Phase 2: Hole Punching (v0.8.0 - Q2 2025) 🎯
  22 - **Works**: 100% (80% direct + 20% relay fallback)
  23 - **Direct P2P**: 80% (cone NAT)
  24 - **Infrastructure**: Gateway coordination
  25 - **Timeline**: 6-8 weeks
  26 - **New Modules**:
  27   - `macula_nat_discovery.erl`
  28   - `macula_hole_punch.erl`
  29   - `macula_connection_upgrade.erl`
  30 
  31 ### Phase 3: STUN/TURN (v0.9.0 - Q4 2025) ⭐
  32 - **Works**: 100% (95% direct + 5% relay fallback)
  33 - **Direct P2P**: 95% (all NAT types)
  34 - **Infrastructure**: Gateway + STUN + TURN servers
  35 - **Timeline**: 8-10 weeks
  36 - **New Modules**:
  37   - `macula_stun_client.erl`
  38   - `macula_turn_client.erl`
  39   - `macula_ice_agent.erl`
  40   - `macula_candidate_exchange.erl`
  41   - `macula_connection_strategy.erl`
  42 
  43 ---
  44 
  45 ## Technology Decision: WebRTC Principles (NOT WebTransport)
  46 
  47 ### ❌ WebTransport - REJECTED
  48 - **Why**: Client→Server protocol, NOT peer-to-peer
  49 - **Problem**: Doesn't solve NAT traversal
  50 - **Use Case**: Browsers connecting to servers (not our use case)
  51 
  52 ### ✅ WebRTC NAT Traversal - ADOPTED
  53 - **Why**: Peer-to-peer NAT traversal (ICE/STUN/TURN)
  54 - **Success**: 95%+ connection rate in production
  55 - **Proven**: Billions of WebRTC calls daily
  56 - **Adaptable**: Can use QUIC transport (WebRTC uses DTLS)
  57 
  58 ---
  59 
  60 ## Benefits by Version
  61 
  62 | Metric | v0.7.x | v0.8.0 | v0.9.0 |
  63 |--------|--------|--------|--------|
  64 | **Direct P2P** | 0% | 80% | 95% |
  65 | **Gateway Load** | 100% | 20% | 5% |
  66 | **Latency** | 2 hops | 1 hop | 1 hop |
  67 | **Reliability** | 100% | 100% | 100% |
  68 | **Complexity** | Low | Medium | High |
  69 
  70 ---
  71 
  72 ## Cost Impact
  73 
  74 - **v0.7.x**: Gateway bandwidth = 100%
  75 - **v0.8.0**: Gateway bandwidth = 20% (80% reduction)
  76 - **v0.9.0**: Gateway bandwidth = 5% (95% reduction) + STUN/TURN costs ($25-50/mo)
  77 
  78 **ROI**: v0.8.0 pays for itself immediately. v0.9.0 worth it for symmetric NAT support.
  79 
  80 ---
  81 
  82 ## Migration Path
  83 
  84 - **v0.7.x → v0.8.0**: Automatic, no config changes
  85 - **v0.8.0 → v0.9.0**: Add STUN/TURN server config
  86 - **Backward Compatible**: Old clients keep using relay
  87 
  88 ---
  89 
  90 ## Next Steps
  91 
  92 1. ✅ Complete v0.7.2 (nested case refactoring, DNS fix)
  93 2. 📋 Review and approve NAT_TRAVERSAL_ROADMAP.md
  94 3. 🚀 Begin v0.8.0 implementation (hole punching)
  95 
  96 ---
  97 
  98 **Document**: Quick Reference  
  99 **Full Details**: [NAT_TRAVERSAL_ROADMAP.md](NAT_TRAVERSAL_ROADMAP.md)  
 100 **Last Updated**: Jan 2025
