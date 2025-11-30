     STDIN
   1 # NAT Traversal & P2P Connectivity Roadmap
   2 
   3 **Target Versions**: v0.8.0 (Hole Punching), v0.9.0 (STUN/TURN)  
   4 **Goal**: Enable true peer-to-peer connectivity for edge nodes behind NAT/firewalls  
   5 **Status**: Planning Phase (as of v0.7.2)
   6 
   7 ---
   8 
   9 ## Problem Statement
  10 
  11 **Current Architecture (v0.7.x)**:
  12 - Edge peers behind residential/corporate NAT cannot accept incoming connections
  13 - All communication must relay through gateway (bottleneck)
  14 - Gateway becomes single point of failure and bandwidth constraint
  15 
  16 **Target Architecture**:
  17 - Direct peer-to-peer QUIC connections where possible (80-95% of cases)
  18 - Graceful fallback to gateway relay when direct connection fails
  19 - Distributed mesh without central bottleneck
  20 
  21 ---
  22 
  23 ## Technology Evaluation
  24 
  25 ### ❌ WebTransport (REJECTED)
  26 
  27 **Why NOT WebTransport?**
  28 - WebTransport is a **client→server** protocol, not **peer↔peer**
  29 - Built on HTTP/3/QUIC but designed for browser-to-server communication
  30 - Does NOT solve NAT traversal problem (peers behind NAT still can't accept incoming connections)
  31 - Useful for browser clients connecting to servers, but Macula needs **symmetric P2P**
  32 
  33 **Verdict**: WebTransport is irrelevant for our P2P mesh architecture.
  34 
  35 ### ✅ WebRTC NAT Traversal Principles (ADOPTED)
  36 
  37 **What we're borrowing from WebRTC**:
  38 - ICE (Interactive Connectivity Establishment) - try multiple paths
  39 - STUN (Session Traversal Utilities for NAT) - discover public IP/port
  40 - TURN (Traversal Using Relays around NAT) - relay as fallback
  41 - Hole punching techniques
  42 
  43 **Why this works**:
  44 - Battle-tested by billions of WebRTC calls
  45 - 95%+ NAT traversal success rate with TURN fallback
  46 - Can reuse our existing QUIC transport (WebRTC uses DTLS, we use QUIC)
  47 - Multiple Erlang STUN/TURN libraries available
  48 
  49 ---
  50 
  51 ## Phased Rollout Strategy
  52 
  53 ### Phase 1: Gateway Relay (v0.7.x - CURRENT) ✅
  54 
  55 **Status**: Implemented  
  56 **Success Rate**: 100% (always works)  
  57 **Architecture**:
  58 ```
  59 Edge Peer A (NAT) → Gateway → Edge Peer B (NAT)
  60 ```
  61 
  62 **Characteristics**:
  63 - ✅ Works everywhere (no NAT issues)
  64 - ✅ Simple, reliable
  65 - ⚠️ Gateway bandwidth bottleneck
  66 - ⚠️ Gateway is single point of failure
  67 - ⚠️ Higher latency (two hops)
  68 
  69 **No changes needed** - this remains as the universal fallback.
  70 
  71 ---
  72 
  73 ### Phase 2: Opportunistic Hole Punching (v0.8.0 - NEXT) 🎯
  74 
  75 **Target Release**: Q2 2025  
  76 **Success Rate**: 80% direct + 20% fallback to relay = 100% total  
  77 **Timeline**: 6-8 weeks development
  78 
  79 #### Architecture
  80 
  81 ```
  82                    Gateway (Coordinator)
  83                          ↓
  84 Edge Peer A (NAT) ←─────────────→ Edge Peer B (NAT)
  85    (Relay fallback if hole punch fails)
  86 ```
  87 
  88 **Strategy**: Attempt direct connection while keeping relay as fallback.
  89 
  90 #### New Modules to Create
  91 
  92 ##### 1. `macula_nat_discovery.erl` (NEW)
  93 **Purpose**: Detect peer's public IP/port and NAT type  
  94 **Responsibilities**:
  95 - Query gateway for peer's observed public address
  96 - Detect NAT type (cone, symmetric, etc.)
  97 - Cache results for reuse
  98 
  99 **API**:
 100 ```erlang
 101 -module(macula_nat_discovery).
 102 
 103 %% Discover my public address as seen by gateway
 104 -spec discover_public_address(GatewayPid :: pid()) -> 
 105     {ok, {inet:ip_address(), inet:port_number()}} | {error, term()}.
 106 
 107 %% Detect NAT type (full cone, restricted cone, port-restricted, symmetric)
 108 -spec detect_nat_type(GatewayPid :: pid()) -> 
 109     {ok, nat_type()} | {error, term()}.
 110 
 111 %% Types
 112 -type nat_type() :: full_cone | restricted_cone | port_restricted | symmetric | unknown.
 113 ```
 114 
 115 **Implementation Notes**:
 116 - Use gateway's observed address from QUIC connection
 117 - Perform STUN-like behavior discovery (test port consistency)
 118 - Cache results (NAT type doesn't change frequently)
 119 
 120 ##### 2. `macula_hole_punch.erl` (NEW)
 121 **Purpose**: Coordinate simultaneous connection attempts for hole punching  
 122 **Responsibilities**:
 123 - Exchange addresses via gateway
 124 - Initiate simultaneous QUIC connections
 125 - Detect success/failure
 126 - Upgrade relay to direct connection
 127 
 128 **API**:
 129 ```erlang
 130 -module(macula_hole_punch).
 131 
 132 %% Attempt hole punching with remote peer
 133 -spec attempt_punch(
 134     RemotePeerId :: binary(),
 135     RelayConn :: pid(),
 136     GatewayPid :: pid()
 137 ) -> {ok, DirectConn :: pid()} | {error, Reason :: term()}.
 138 
 139 %% Coordinate simultaneous connection
 140 -spec coordinate_simultaneous_open(
 141     MyPublicAddr :: {inet:ip_address(), inet:port_number()},
 142     TheirPublicAddr :: {inet:ip_address(), inet:port_number()}
 143 ) -> {ok, quicer:connection_handle()} | {error, term()}.
 144 ```
 145 
 146 **Implementation Notes**:
 147 - Send "punch request" via relay
 148 - Both peers attempt QUIC connection at same time
 149 - Timeout after 5 seconds, fallback to relay
 150 - Handle race conditions (both peers dialing simultaneously)
 151 
 152 ##### 3. `macula_connection_upgrade.erl` (NEW)
 153 **Purpose**: Upgrade existing relay connection to direct P2P  
 154 **Responsibilities**:
 155 - Migrate active streams from relay to direct connection
 156 - Handle graceful transition (no message loss)
 157 - Update routing tables
 158 
 159 **API**:
 160 ```erlang
 161 -module(macula_connection_upgrade).
 162 
 163 %% Upgrade relay connection to direct
 164 -spec upgrade_to_direct(
 165     RelayConn :: pid(),
 166     DirectConn :: pid()
 167 ) -> ok | {error, term()}.
 168 
 169 %% Migrate streams from old to new connection
 170 -spec migrate_streams(
 171     OldConn :: pid(),
 172     NewConn :: pid()
 173 ) -> ok | {error, term()}.
 174 ```
 175 
 176 **Implementation Notes**:
 177 - Pause new streams on relay
 178 - Transfer in-flight messages
 179 - Close relay connection gracefully
 180 - Update `macula_gateway_mesh` connection pool
 181 
 182 ##### 4. `macula_gateway` Extensions (MODIFY)
 183 **New Responsibilities**:
 184 - Track peer public addresses (observed from QUIC handshake)
 185 - Coordinate hole punching attempts
 186 - Relay address exchange messages
 187 
 188 **New Functions**:
 189 ```erlang
 190 %% In macula_gateway.erl
 191 
 192 %% Get peer's public address as observed by gateway
 193 -spec get_peer_public_address(PeerId :: binary()) -> 
 194     {ok, {inet:ip_address(), inet:port_number()}} | {error, not_found}.
 195 
 196 %% Coordinate hole punch between two peers
 197 -spec coordinate_hole_punch(PeerA :: binary(), PeerB :: binary()) -> 
 198     ok | {error, term()}.
 199 ```
 200 
 201 **Implementation Notes**:
 202 - Store peer addresses in state: `#{peer_id => {ip, port}}`
 203 - Add message type `{hole_punch_coordinate, PeerAAddr, PeerBAddr}`
 204 - Send addresses to both peers via relay
 205 
 206 ##### 5. `macula_peer` Extensions (MODIFY)
 207 **New Responsibilities**:
 208 - Attempt hole punching before falling back to relay
 209 - Monitor connection quality (direct vs relay)
 210 - Report metrics
 211 
 212 **New Functions**:
 213 ```erlang
 214 %% In macula_peer.erl
 215 
 216 %% Establish connection with hole punching
 217 -spec connect_with_nat_traversal(
 218     PeerAddr :: binary(),
 219     Opts :: map()
 220 ) -> {ok, pid()} | {error, term()}.
 221 
 222 %% Get connection statistics
 223 -spec get_connection_stats(Pid :: pid()) -> #{
 224     connection_type => direct | relay,
 225     latency_ms => non_neg_integer(),
 226     bytes_sent => non_neg_integer(),
 227     bytes_received => non_neg_integer()
 228 }.
 229 ```
 230 
 231 #### Configuration Changes
 232 
 233 **New sys.config Options**:
 234 ```erlang
 235 {macula, [
 236     %% NAT traversal settings
 237     {nat_traversal, #{
 238         enabled => true,
 239         hole_punch_timeout => 5000,  % ms
 240         prefer_direct => true,        % try direct before relay
 241         fallback_to_relay => true     % always fallback if direct fails
 242     }},
 243     
 244     %% Connection strategy
 245     {connection_strategy, opportunistic}  % always_relay | opportunistic | direct_only
 246 ]}
 247 ```
 248 
 249 #### Testing Strategy
 250 
 251 **New Test Modules**:
 252 1. `test/macula_nat_discovery_tests.erl` - Address discovery
 253 2. `test/macula_hole_punch_tests.erl` - Hole punching logic
 254 3. `test/macula_connection_upgrade_tests.erl` - Connection migration
 255 
 256 **Integration Tests**:
 257 - Multi-node Docker setup with simulated NAT
 258 - Test direct connection success/failure
 259 - Test relay fallback
 260 - Test connection upgrade
 261 
 262 **Docker Test Setup**:
 263 ```yaml
 264 # docker-compose.nat-test.yml
 265 services:
 266   gateway:
 267     image: macula:0.8.0
 268     ports:
 269       - "9443:9443"
 270   
 271   peer1:
 272     image: macula:0.8.0
 273     # Simulated NAT (no port forwarding)
 274     network_mode: "bridge"
 275   
 276   peer2:
 277     image: macula:0.8.0
 278     # Simulated NAT (no port forwarding)
 279     network_mode: "bridge"
 280 ```
 281 
 282 #### Success Metrics (v0.8.0)
 283 
 284 - ✅ 80%+ direct P2P connections (non-symmetric NAT)
 285 - ✅ 100% connectivity (with relay fallback)
 286 - ✅ <100ms upgrade time (relay → direct)
 287 - ✅ No message loss during upgrade
 288 - ✅ 50%+ reduction in gateway bandwidth usage
 289 
 290 ---
 291 
 292 ### Phase 3: STUN/TURN Infrastructure (v0.9.0 - FUTURE) ⭐
 293 
 294 **Target Release**: Q4 2025  
 295 **Success Rate**: 95%+ direct + 5% relay = 100% total  
 296 **Timeline**: 8-10 weeks development
 297 
 298 #### Architecture
 299 
 300 ```
 301           STUN Server (Address Discovery)
 302                  ↓
 303 Edge Peer A → Direct P2P ← Edge Peer B
 304                  ↓
 305           TURN Relay (Symmetric NAT Fallback)
 306                  ↓
 307             Gateway (Final Fallback)
 308 ```
 309 
 310 **Strategy**: Full ICE-like candidate exchange with STUN/TURN.
 311 
 312 #### New Infrastructure Services
 313 
 314 ##### 1. STUN Server (EXTERNAL SERVICE)
 315 **Purpose**: Help peers discover their public IP/port  
 316 **Technology**: `coturn` (open source, Erlang-compatible)  
 317 **Deployment**: 
 318 - Hosted service: `stun.macula.io:3478`
 319 - Protocol: STUN over UDP
 320 - Cost: Low (minimal bandwidth)
 321 
 322 **Configuration**:
 323 ```bash
 324 # coturn.conf
 325 listening-port=3478
 326 listening-ip=0.0.0.0
 327 realm=macula.io
 328 ```
 329 
 330 ##### 2. TURN Relay Server (EXTERNAL SERVICE)
 331 **Purpose**: Relay traffic for symmetric NAT (worst case)  
 332 **Technology**: `coturn` (same as STUN, adds TURN protocol)  
 333 **Deployment**:
 334 - Hosted service: `turn.macula.io:3478`
 335 - Protocol: TURN over UDP/TCP
 336 - Cost: Medium-High (relays all traffic for symmetric NAT)
 337 
 338 **Configuration**:
 339 ```bash
 340 # coturn.conf (additional TURN settings)
 341 relay-ip=<public-ip>
 342 min-port=49152
 343 max-port=65535
 344 user=macula:password
 345 ```
 346 
 347 #### New Modules to Create
 348 
 349 ##### 1. `macula_stun_client.erl` (NEW)
 350 **Purpose**: STUN client for address discovery  
 351 **Dependencies**: `stun` library from Hex.pm  
 352 **Responsibilities**:
 353 - Query STUN server for public address
 354 - Handle STUN binding requests/responses
 355 - Cache STUN results
 356 
 357 **API**:
 358 ```erlang
 359 -module(macula_stun_client).
 360 
 361 %% Discover public address via STUN
 362 -spec discover_address(StunServer :: string()) -> 
 363     {ok, {inet:ip_address(), inet:port_number()}} | {error, term()}.
 364 
 365 %% Test connectivity via STUN
 366 -spec test_connectivity(StunServer :: string()) -> 
 367     ok | {error, term()}.
 368 ```
 369 
 370 ##### 2. `macula_turn_client.erl` (NEW)
 371 **Purpose**: TURN client for relay allocation  
 372 **Dependencies**: `turn` library or custom implementation  
 373 **Responsibilities**:
 374 - Allocate relay addresses from TURN server
 375 - Maintain TURN allocations (refresh)
 376 - Send/receive data via TURN relay
 377 
 378 **API**:
 379 ```erlang
 380 -module(macula_turn_client).
 381 
 382 %% Allocate relay address
 383 -spec allocate_relay(TurnServer :: string(), Credentials :: map()) -> 
 384     {ok, RelayAddr :: {inet:ip_address(), inet:port_number()}} | {error, term()}.
 385 
 386 %% Send data via TURN relay
 387 -spec send_via_relay(RelayAddr :: term(), Data :: binary()) -> 
 388     ok | {error, term()}.
 389 ```
 390 
 391 ##### 3. `macula_ice_agent.erl` (NEW)
 392 **Purpose**: ICE-like candidate gathering and connectivity checks  
 393 **Responsibilities**:
 394 - Gather candidates (local, STUN, TURN)
 395 - Prioritize candidates
 396 - Perform connectivity checks
 397 - Select best candidate pair
 398 
 399 **API**:
 400 ```erlang
 401 -module(macula_ice_agent).
 402 
 403 %% Gather all connection candidates
 404 -spec gather_candidates(Opts :: map()) -> 
 405     {ok, [candidate()]} | {error, term()}.
 406 
 407 %% Perform connectivity checks
 408 -spec check_connectivity(
 409     LocalCandidates :: [candidate()],
 410     RemoteCandidates :: [candidate()]
 411 ) -> {ok, BestPair :: {candidate(), candidate()}} | {error, term()}.
 412 
 413 %% Types
 414 -type candidate() :: #{
 415     type => local | stun | turn | relay,
 416     address => {inet:ip_address(), inet:port_number()},
 417     priority => non_neg_integer()
 418 }.
 419 ```
 420 
 421 ##### 4. `macula_candidate_exchange.erl` (NEW)
 422 **Purpose**: Exchange ICE candidates via gateway signaling  
 423 **Responsibilities**:
 424 - Serialize/deserialize candidates
 425 - Exchange via gateway relay
 426 - Handle candidate trickling (send as discovered)
 427 
 428 **API**:
 429 ```erlang
 430 -module(macula_candidate_exchange).
 431 
 432 %% Send candidates to remote peer
 433 -spec send_candidates(
 434     PeerId :: binary(),
 435     Candidates :: [macula_ice_agent:candidate()],
 436     GatewayPid :: pid()
 437 ) -> ok | {error, term()}.
 438 
 439 %% Receive candidates from remote peer
 440 -spec receive_candidates(Timeout :: timeout()) -> 
 441     {ok, [macula_ice_agent:candidate()]} | {error, term()}.
 442 ```
 443 
 444 ##### 5. `macula_connection_strategy.erl` (NEW)
 445 **Purpose**: Decide connection strategy based on NAT type and candidates  
 446 **Responsibilities**:
 447 - Select best connection method
 448 - Coordinate ICE-like checks
 449 - Monitor connection quality
 450 - Handle re-negotiation
 451 
 452 **API**:
 453 ```erlang
 454 -module(macula_connection_strategy).
 455 
 456 %% Establish optimal connection
 457 -spec establish_connection(
 458     PeerId :: binary(),
 459     LocalCandidates :: [macula_ice_agent:candidate()],
 460     RemoteCandidates :: [macula_ice_agent:candidate()],
 461     Opts :: map()
 462 ) -> {ok, Connection :: pid()} | {error, term()}.
 463 
 464 %% Re-negotiate if connection degrades
 465 -spec renegotiate_if_needed(Connection :: pid()) -> 
 466     ok | {upgraded, NewConnection :: pid()}.
 467 ```
 468 
 469 #### Configuration Changes
 470 
 471 **New sys.config Options**:
 472 ```erlang
 473 {macula, [
 474     %% STUN/TURN servers
 475     {stun_servers, [
 476         "stun.macula.io:3478",
 477         "stun.l.google.com:19302"  % Fallback
 478     ]},
 479     
 480     {turn_servers, [
 481         #{
 482             server => "turn.macula.io:3478",
 483             username => "macula",
 484             credential => "<password>",
 485             transport => udp
 486         }
 487     ]},
 488     
 489     %% ICE settings
 490     {ice, #{
 491         gather_timeout => 5000,      % ms to gather candidates
 492         check_timeout => 10000,      % ms for connectivity checks
 493         candidate_types => [local, stun, turn, relay],  % preference order
 494         enable_trickling => true     % send candidates as discovered
 495     }}
 496 ]}
 497 ```
 498 
 499 #### Deployment Requirements
 500 
 501 **New Services**:
 502 1. **STUN Server**: 
 503    - Deploy `coturn` on public IP
 504    - DNS: `stun.macula.io`
 505    - Port: 3478 UDP
 506    - Cost: $5-10/month (minimal traffic)
 507 
 508 2. **TURN Server**:
 509    - Deploy `coturn` with TURN enabled
 510    - DNS: `turn.macula.io`
 511    - Ports: 3478 UDP/TCP + 49152-65535 UDP (relay range)
 512    - Cost: $20-50/month (depends on relay traffic)
 513 
 514 **Monitoring**:
 515 - Track STUN query success rate
 516 - Track TURN allocation usage
 517 - Monitor relay bandwidth costs
 518 - Alert if STUN/TURN unavailable
 519 
 520 #### Testing Strategy
 521 
 522 **New Test Modules**:
 523 1. `test/macula_stun_client_tests.erl`
 524 2. `test/macula_turn_client_tests.erl`
 525 3. `test/macula_ice_agent_tests.erl`
 526 4. `test/macula_candidate_exchange_tests.erl`
 527 
 528 **Integration Tests**:
 529 - Test all NAT types (full cone, symmetric, etc.)
 530 - Test STUN discovery
 531 - Test TURN relay fallback
 532 - Test candidate prioritization
 533 - Test ICE re-negotiation
 534 
 535 **Docker Test Setup**:
 536 ```yaml
 537 # docker-compose.ice-test.yml
 538 services:
 539   stun:
 540     image: coturn/coturn
 541     command: -n --log-file=stdout
 542     
 543   turn:
 544     image: coturn/coturn
 545     command: -n --log-file=stdout
 546     
 547   gateway:
 548     image: macula:0.9.0
 549   
 550   peer_symmetric_nat:
 551     image: macula:0.9.0
 552     # Simulated symmetric NAT
 553 ```
 554 
 555 #### Success Metrics (v0.9.0)
 556 
 557 - ✅ 95%+ direct P2P connections (including symmetric NAT via TURN)
 558 - ✅ 100% connectivity (with multi-tier fallback)
 559 - ✅ <5% traffic via TURN relay (cost optimization)
 560 - ✅ <200ms ICE negotiation time
 561 - ✅ Automatic re-negotiation on network change
 562 
 563 ---
 564 
 565 ## Comparison: v0.7.x vs v0.8.0 vs v0.9.0
 566 
 567 | Feature | v0.7.x (Current) | v0.8.0 (Hole Punch) | v0.9.0 (STUN/TURN) |
 568 |---------|------------------|---------------------|---------------------|
 569 | **Direct P2P** | 0% | 80% | 95% |
 570 | **NAT Support** | All (via relay) | Cone NAT only | All NAT types |
 571 | **Gateway Load** | 100% | 20% | 5% |
 572 | **Latency** | 2 hops | 1 hop (direct) | 1 hop (direct) |
 573 | **Infrastructure** | Gateway only | Gateway only | Gateway + STUN + TURN |
 574 | **Complexity** | Low | Medium | High |
 575 | **Cost** | Low | Low | Medium |
 576 | **Reliability** | 100% | 100% | 100% |
 577 
 578 ---
 579 
 580 ## Migration Strategy
 581 
 582 ### v0.7.x → v0.8.0
 583 
 584 **Backward Compatibility**: Full  
 585 **Migration**: Automatic (no config changes required)  
 586 **Behavior**: 
 587 - Old clients (v0.7.x) continue using relay
 588 - New clients (v0.8.0) attempt hole punching
 589 - Mixed deployments work seamlessly
 590 
 591 **Feature Flags**:
 592 ```erlang
 593 %% Disable hole punching if needed
 594 {macula, [{nat_traversal, #{enabled => false}}]}
 595 ```
 596 
 597 ### v0.8.0 → v0.9.0
 598 
 599 **Backward Compatibility**: Full  
 600 **Migration**: Add STUN/TURN server configuration  
 601 **Behavior**:
 602 - v0.8.0 clients: Hole punching only
 603 - v0.9.0 clients: Full ICE with STUN/TURN
 604 - Graceful fallback chain
 605 
 606 **Configuration Migration**:
 607 ```erlang
 608 %% Add STUN/TURN servers
 609 {macula, [
 610     {stun_servers, ["stun.macula.io:3478"]},
 611     {turn_servers, [#{server => "turn.macula.io:3478", ...}]}
 612 ]}
 613 ```
 614 
 615 ---
 616 
 617 ## Dependencies
 618 
 619 ### Erlang Libraries Needed
 620 
 621 **v0.8.0**:
 622 - None (all custom implementation)
 623 
 624 **v0.9.0**:
 625 - `stun` (Hex.pm) - STUN client library
 626 - `turn` (may need custom implementation)
 627 - OR use `ejabberd`'s STUN/TURN code as reference
 628 
 629 ### External Services
 630 
 631 **v0.8.0**:
 632 - None (uses existing gateway)
 633 
 634 **v0.9.0**:
 635 - STUN server: `coturn` or `ejabberd`
 636 - TURN server: `coturn` with authentication
 637 - Monitoring: Prometheus metrics
 638 
 639 ---
 640 
 641 ## Risk Mitigation
 642 
 643 ### v0.8.0 Risks
 644 
 645 | Risk | Mitigation |
 646 |------|------------|
 647 | Hole punching fails | Always fallback to relay (100% reliability) |
 648 | Connection upgrade fails | Detect and retry, keep relay active |
 649 | NAT type detection wrong | Conservative: assume worst-case, fallback |
 650 | Gateway coordination overhead | Async operations, non-blocking |
 651 
 652 ### v0.9.0 Risks
 653 
 654 | Risk | Mitigation |
 655 |------|------------|
 656 | STUN/TURN unavailable | Multiple STUN servers, fallback to relay |
 657 | TURN bandwidth costs | Monitor usage, limit per-peer allocation time |
 658 | Complex state machine | Comprehensive testing, formal verification |
 659 | ICE negotiation timeout | Short timeouts, fast fallback |
 660 
 661 ---
 662 
 663 ## Success Criteria
 664 
 665 ### v0.8.0 Launch Criteria
 666 
 667 - ✅ All tests passing (unit + integration)
 668 - ✅ 80%+ hole punch success in simulated NAT environments
 669 - ✅ 100% connectivity with relay fallback
 670 - ✅ Zero message loss during connection upgrade
 671 - ✅ Gateway bandwidth reduced by 50%+
 672 - ✅ Performance regression tests pass
 673 - ✅ Documentation complete
 674 
 675 ### v0.9.0 Launch Criteria
 676 
 677 - ✅ All v0.8.0 criteria met
 678 - ✅ 95%+ direct P2P success with STUN/TURN
 679 - ✅ STUN/TURN infrastructure deployed and monitored
 680 - ✅ <5% traffic via TURN (cost control)
 681 - ✅ Automatic re-negotiation working
 682 - ✅ Production validation with real NAT environments
 683 - ✅ Cost analysis complete (TURN relay costs acceptable)
 684 
 685 ---
 686 
 687 ## Timeline Summary
 688 
 689 | Version | Features | Duration | Target Date |
 690 |---------|----------|----------|-------------|
 691 | v0.7.2 | Nested case refactoring, DNS fix | 2 weeks | Jan 2025 ✅ |
 692 | v0.8.0 | Opportunistic hole punching | 6-8 weeks | Q2 2025 |
 693 | v0.9.0 | Full STUN/TURN/ICE | 8-10 weeks | Q4 2025 |
 694 
 695 **Total**: ~6 months to full P2P NAT traversal
 696 
 697 ---
 698 
 699 ## References
 700 
 701 - **WebRTC Spec**: https://www.w3.org/TR/webrtc/
 702 - **ICE RFC 8445**: https://datatracker.ietf.org/doc/html/rfc8445
 703 - **STUN RFC 8489**: https://datatracker.ietf.org/doc/html/rfc8489
 704 - **TURN RFC 8656**: https://datatracker.ietf.org/doc/html/rfc8656
 705 - **libp2p Hole Punching**: https://blog.ipfs.tech/2022-01-20-libp2p-hole-punching/
 706 - **coturn**: https://github.com/coturn/coturn
 707 - **Erlang STUN**: https://hex.pm/packages/stun
 708 
 709 ---
 710 
 711 **Document Status**: Planning (v0.7.2)  
 712 **Next Review**: After v0.7.2 release  
 713 **Owner**: Architecture Team
