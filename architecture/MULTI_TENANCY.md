# Macula HTTP/3 Mesh: Isolation Mechanisms

## Multi-Tenancy, Realms, and Cross-Realm Communication

**Version**: 1.0
**Date**: 2025-01-08
**Status**: Design Document

---

## Table of Contents

1. [Overview](#overview)
2. [Core Concepts](#core-concepts)
3. [Architecture Layers](#architecture-layers)
4. [Identity and Namespacing](#identity-and-namespacing)
5. [Cross-Realm Communication Models](#cross-realm-communication-models)
6. [Protocol-Level Support](#protocol-level-support)
7. [Discovery and Membership](#discovery-and-membership)
8. [Pub/Sub with Realm Scoping](#pubsub-with-realm-scoping)
9. [Security and Authorization](#security-and-authorization)
10. [Use Cases and Patterns](#use-cases-and-patterns)
11. [Implementation Roadmap](#implementation-roadmap)
12. [Performance Considerations](#performance-considerations)
13. [Operational Concerns](#operational-concerns)

---

## Overview

The Macula HTTP/3 Mesh architecture supports multi-tenancy and realm isolation as a first-class concept. This document describes how isolation mechanisms work, how cross-realm communication is achieved, and the security/operational considerations.

### Key Design Principles

1. **Isolation by Default**: Traffic stays within realm boundaries unless explicitly bridged
2. **Explicit Federation**: Cross-realm communication is visible, controlled, and auditable
3. **Scalable Partitioning**: Each realm has independent routing/membership (O(log N) per realm)
4. **Flexible Policies**: Support strict isolation, controlled gateways, or open federation
5. **Certificate-Based Trust**: Realm membership verified via TLS certificates

### Why Realm Isolation?

**Multi-Tenancy Requirements**:
- **SaaS Deployments**: Multiple customers sharing infrastructure
- **Data Sovereignty**: Legal requirements to keep data within boundaries
- **Performance**: Limit routing table growth to relevant nodes
- **Security**: Prevent unauthorized cross-tenant access
- **Billing**: Track resource usage per tenant/organization

**Federation Use Cases**:
- **Partner Integration**: Controlled data sharing between organizations
- **Hierarchical Systems**: Edge → Regional → Cloud data flows
- **Market Systems**: Public marketplaces with private participants
- **IoT Ecosystems**: Device networks connecting to cloud analytics

---

## Core Concepts

### Realm

A **realm** is a logical isolation boundary within the mesh network. Nodes in the same realm can communicate freely; cross-realm communication requires explicit configuration.

**Realm Naming Convention**:
```
{reverse-dns}.{domain}.{subdomain}

Examples:
com.example.energy         # Example Platform energy trading platform
com.example.analytics      # Example Platform analytics realm
com.acme.iot.factory-42    # Acme Corp factory 42
com.acme.iot.cloud         # Acme Corp cloud analytics
org.example.public         # Public/shared realm
```

**Realm Properties**:
- Globally unique identifier (reverse DNS recommended)
- Independent routing table (Kademlia DHT per realm)
- Separate SWIM membership gossip
- Isolated topic namespace
- Dedicated certificate authority (optional)

### Tenant

A **tenant** represents an organization or customer within a realm. Multiple tenants can coexist in a shared realm with additional topic prefix isolation.

**Tenant Structure**:
```erlang
-record(tenant, {
    id          :: binary(),      % UUID or customer ID
    realm       :: binary(),      % Which realm this tenant belongs to
    name        :: binary(),      % Human-readable name
    topic_prefix :: binary(),     % Enforced topic prefix
    quotas = #{} :: #{            % Resource limits
        max_nodes => 100,
        max_topics => 1000,
        max_messages_per_sec => 10000
    },
    created_at  :: integer()
}).
```

### Gateway Node

A **gateway node** is a special node that participates in multiple realms and routes messages between them according to configurable policies.

**Gateway Capabilities**:
- Member of multiple realms simultaneously
- Policy-based message filtering
- Topic translation and mapping
- Rate limiting per realm/tenant
- Audit logging of cross-realm traffic
- Protocol translation (future: WAMP ↔ Macula)

---

## Architecture Layers

### Layer 1: Identity Layer (Node-Level)

Each node has an identity that includes realm membership:

```erlang
-record(node_identity, {
    node_id      :: binary(),        % Globally unique (SHA-256 hash)
    node_name    :: atom(),          % Erlang node name
    realm        :: binary(),        % Primary realm
    tenant_id    :: binary() | undefined, % Optional tenant
    roles = []   :: [atom()],        % [realm_admin, edge_node, gateway]
    capabilities = #{} :: #{         % What this node can do
        pubsub => true,
        rpc => true,
        streaming => false,
        gateway => false              % Can bridge realms
    },
    supported_realms = [] :: [binary()], % For gateway nodes
    version      :: binary(),        % Protocol version
    created_at   :: integer()
}).
```

**Example Node Identities**:

```erlang
%% Regular edge node
#{
    node_id => <<"a3f5b2e1...">>,
    node_name => 'edge1@192.168.1.100',
    realm => <<"com.example.energy">>,
    tenant_id => <<"customer-456">>,
    roles => [edge_node],
    capabilities => #{pubsub => true, rpc => true}
}

%% Gateway node
#{
    node_id => <<"d8c9a4f2...">>,
    node_name => 'gateway1@10.20.30.40',
    realm => <<"com.example.energy">>,      % Primary realm
    roles => [gateway, realm_admin],
    capabilities => #{pubsub => true, rpc => true, gateway => true},
    supported_realms => [
        <<"com.example.energy">>,
        <<"com.example.analytics">>,
        <<"com.example.market">>
    ]
}
```

### Layer 2: Topic Namespacing (Application-Level)

Topics are hierarchical with realm prefix enforced by the protocol:

```
{realm}.{domain}.{subdomain}.{event_type}

Examples:
com.example.energy.home.measured
com.example.energy.provider.contract_offered
com.example.analytics.aggregated.daily_totals
com.acme.iot.sensor.temperature
com.acme.iot.actuator.valve_state
```

**Topic Validation**:
```erlang
validate_topic(Topic, NodeIdentity) ->
    Realm = NodeIdentity#node_identity.realm,
    case binary:split(Topic, <<".">>) of
        [Realm | _Rest] ->
            %% Topic matches node's realm
            ok;
        [ForeignRealm | _Rest] ->
            %% Check if node is gateway and supports foreign realm
            case is_gateway_node(NodeIdentity) andalso
                 lists:member(ForeignRealm, NodeIdentity#node_identity.supported_realms) of
                true -> {ok, foreign_realm, ForeignRealm};
                false -> {error, realm_mismatch}
            end;
        _ ->
            {error, invalid_topic}
    end.
```

**Tenant-Level Isolation** (within realm):
```
{realm}.{tenant_prefix}.{domain}.{event}

Example:
com.example.energy.customer-456.home.measured
com.example.energy.customer-789.home.measured
```

Tenant prefix enforced by gateway or broker:
```erlang
validate_tenant_topic(Topic, TenantId) ->
    ExpectedPrefix = <<"com.example.energy.", TenantId/binary>>,
    case binary:match(Topic, ExpectedPrefix) of
        {0, _} -> ok;  % Topic starts with expected prefix
        _ -> {error, tenant_violation}
    end.
```

### Layer 3: Routing Table Partitioning

The Kademlia DHT maintains separate routing spaces per realm:

```erlang
-record(routing_state, {
    local_realm       :: binary(),
    routing_tables = #{} :: #{realm() => kademlia_table()},

    %% Local realm routing (primary)
    local_buckets = [] :: [k_bucket()],

    %% Gateway routing to other realms
    gateway_routes = #{} :: #{
        realm() => [gateway_node_id()]
    },

    %% Cache of cross-realm routes
    foreign_routes = #{} :: #{
        {realm(), dest_node_id()} => via_gateway_id()
    }
}).

-record(kademlia_table, {
    realm     :: binary(),
    self_id   :: binary(),
    buckets   :: [k_bucket()],  % 256 buckets (for 256-bit IDs)
    cache = #{} :: #{binary() => node_info()}
}).
```

**Routing Algorithm with Realms**:

```erlang
route_message(DestNodeId, Message, State) ->
    DestRealm = lookup_node_realm(DestNodeId),
    LocalRealm = State#routing_state.local_realm,

    case DestRealm of
        LocalRealm ->
            %% Same realm - use local routing table
            route_local(DestNodeId, Message, State);

        ForeignRealm ->
            %% Different realm - route via gateway
            route_via_gateway(ForeignRealm, DestNodeId, Message, State)
    end.

route_local(DestNodeId, Message, State) ->
    case is_directly_connected(DestNodeId, State) of
        true ->
            macula_connection:send(DestNodeId, Message);
        false ->
            %% Find next hop via Kademlia
            NextHop = kademlia_next_hop(DestNodeId, State),
            forward(NextHop, DestNodeId, Message)
    end.

route_via_gateway(ForeignRealm, DestNodeId, Message, State) ->
    %% Look up gateway for target realm
    GatewayRoutes = State#routing_state.gateway_routes,
    case maps:get(ForeignRealm, GatewayRoutes, undefined) of
        undefined ->
            {error, no_route_to_realm};
        [GatewayId | _] ->
            %% Send to gateway with foreign destination
            GatewayMsg = wrap_foreign_message(ForeignRealm, DestNodeId, Message),
            macula_connection:send(GatewayId, GatewayMsg)
    end.
```

---

## Cross-Realm Communication Models

### Model A: Gateway Nodes (Recommended)

Gateway nodes act as controlled bridges between realms, enforcing policies and providing audit trails.

**Architecture**:
```
Realm A Nodes              Gateway Node             Realm B Nodes
┌──────────┐              ┌────────────┐            ┌──────────┐
│ Node A1  │              │  Gateway   │            │ Node B1  │
│ (tenant1)│──QUIC/TLS──→ │  (multi-   │ ←──QUIC──│ (tenant2)│
│          │              │   realm)   │            │          │
└──────────┘              └────────────┘            └──────────┘
      │                         │                        │
      │                    ┌────▼────┐                   │
      │                    │ Policy  │                   │
      │                    │ Engine  │                   │
      │                    └────┬────┘                   │
      │                         │                        │
      └────────────────────┬────┴────┬───────────────────┘
                           │         │
                      ┌────▼────┐ ┌──▼─────┐
                      │ Audit   │ │ Rate   │
                      │ Log     │ │ Limiter│
                      └─────────┘ └────────┘
```

**Gateway Node Configuration**:

```erlang
-record(gateway_config, {
    node_id       :: binary(),
    realms = []   :: [binary()],  % Realms this gateway bridges

    policies = #{} :: #{
        {from_realm(), to_realm()} => policy()
    },

    translation_rules = #{} :: #{
        {from_realm(), topic_pattern()} => {to_realm(), topic_pattern()}
    },

    rate_limits = #{} :: #{
        {from_realm(), to_realm()} => #{
            max_msg_per_sec => integer(),
            max_bytes_per_sec => integer(),
            burst_size => integer()
        }
    },

    audit_log = #{
        enabled => true,
        destination => audit_logger,
        log_payloads => false  % For privacy/performance
    }
}).
```

**Example Gateway Policy**:

```erlang
%% Allow energy realm to publish aggregated data to analytics realm
#{
    {<<"com.example.energy">>, <<"com.example.analytics">>} => #{
        %% What topics are allowed
        allowed_topics => [
            <<"com.example.energy.aggregated.*">>,   % Aggregated data only
            <<"com.example.energy.public.*">>,       % Public APIs
            <<"com.example.energy.market.prices">>   % Market data
        ],

        %% Forbidden topics (sensitive data)
        denied_topics => [
            <<"com.example.energy.home.*.measured">>, % Individual homes
            <<"com.example.energy.internal.*">>       % Internal events
        ],

        %% Message types
        allowed_operations => [publish, call],  % Not subscribe

        %% Rate limiting
        rate_limit => #{
            max_msg_per_sec => 1000,
            max_bytes_per_sec => 1_000_000,  % 1 MB/sec
            burst_size => 5000
        },

        %% Security
        require_encryption => true,
        require_signature => false,

        %% Transformations
        anonymize_fields => [<<"home_id">>, <<"customer_id">>],

        %% Audit
        log_all_messages => true
    }
}
```

**Gateway Message Processing**:

```erlang
%% Gateway receives message from Realm A destined for Realm B
handle_gateway_message(FromNodeId, Message, State) ->
    #{from_realm := FromRealm, to_realm := ToRealm,
      topic := Topic, payload := Payload} = Message,

    %% 1. Policy check
    case check_policy(FromRealm, ToRealm, Topic, Message, State) of
        {ok, Policy} ->
            %% 2. Rate limiting
            case check_rate_limit(FromRealm, ToRealm, State) of
                ok ->
                    %% 3. Transform message (anonymize, translate)
                    TransformedMsg = apply_transformations(Message, Policy),

                    %% 4. Audit log
                    log_gateway_message(FromRealm, ToRealm, Topic, State),

                    %% 5. Route to destination realm
                    route_to_realm(ToRealm, TransformedMsg, State);

                {error, rate_limit_exceeded} ->
                    log_rate_limit_violation(FromRealm, ToRealm, State),
                    {error, rate_limited}
            end;

        {error, policy_denied} ->
            log_policy_violation(FromNodeId, FromRealm, ToRealm, Topic, State),
            {error, forbidden}
    end.

apply_transformations(Message, Policy) ->
    %% Anonymize sensitive fields
    AnonymizeFields = maps:get(anonymize_fields, Policy, []),
    Payload1 = anonymize_payload(Message.payload, AnonymizeFields),

    %% Translate topic if configured
    Topic1 = translate_topic(Message.topic, Policy),

    Message#{payload => Payload1, topic => Topic1}.

anonymize_payload(Payload, Fields) ->
    lists:foldl(
        fun(Field, Acc) ->
            case maps:get(Field, Acc, undefined) of
                undefined -> Acc;
                Value ->
                    %% Replace with hash
                    Hash = crypto:hash(sha256, term_to_binary(Value)),
                    Acc#{Field => <<"anon_", Hash/binary>>}
            end
        end,
        Payload,
        Fields
    ).
```

**Topic Translation**:

```erlang
%% Example: Translate energy realm topics to analytics realm topics
-define(TRANSLATION_RULES, #{
    {<<"com.example.energy">>, <<"com.example.analytics">>} => [
        %% Source pattern → Destination pattern
        {<<"com.example.energy.aggregated.{metric}">>,
         <<"com.example.analytics.energy.{metric}">>},

        {<<"com.example.energy.market.prices">>,
         <<"com.example.analytics.market.energy_prices">>}
    ]
}).

translate_topic(Topic, Policy) ->
    case maps:get(topic_translation, Policy, undefined) of
        undefined -> Topic;
        Rules -> apply_translation_rules(Topic, Rules)
    end.

apply_translation_rules(Topic, Rules) ->
    %% Try each rule until one matches
    lists:foldl(
        fun({Pattern, TargetPattern}, Acc) ->
            case match_pattern(Topic, Pattern) of
                {ok, Captures} ->
                    substitute_pattern(TargetPattern, Captures);
                no_match ->
                    Acc
            end
        end,
        Topic,
        Rules
    ).
```

### Model B: Federation Protocol

Realms discover each other via DNS SRV records and establish peer-to-peer federation.

**DNS-Based Discovery**:

```
; DNS SRV records for realm gateways
_macula-federation._udp.example.be.  IN SRV 10 5 4433 gateway1.example.be.
_macula-federation._udp.example.be.  IN SRV 10 5 4433 gateway2.example.be.
_macula-federation._udp.acme.com.     IN SRV 10 5 4433 gateway1.acme.com.

; TXT records for realm metadata
example.be. IN TXT "macula-realm=com.example.energy"
example.be. IN TXT "macula-public-topics=com.example.energy.market.*"
example.be. IN TXT "macula-version=1.0"
```

**Federation Handshake**:

```erlang
-define(MSG_FEDERATION_ANNOUNCE, 16#20).
-define(MSG_FEDERATION_ACK, 16#21).
-define(MSG_FEDERATION_SUBSCRIBE, 16#22).

%% Gateway announces its realm to discovered peer
announce_federation(PeerGatewayId, State) ->
    Announcement = #{
        realm => State#state.realm,
        gateway_nodes => get_gateway_nodes(State),
        public_topics => get_public_topics(State),
        capabilities => #{
            pubsub => true,
            rpc => true,
            streaming => false
        },
        policies => get_federation_policies(State),
        version => <<"1.0">>
    },

    Message = encode(?MSG_FEDERATION_ANNOUNCE, Announcement),
    macula_connection:send(PeerGatewayId, Message).

%% Peer gateway acknowledges and sends its own announcement
handle_federation_announce(Announcement, FromNodeId, State) ->
    #{realm := PeerRealm, public_topics := PeerTopics} = Announcement,

    %% Validate federation is allowed
    case is_federation_allowed(PeerRealm, State) of
        true ->
            %% Store peer realm info
            State1 = add_federated_realm(PeerRealm, FromNodeId, Announcement, State),

            %% Send acknowledgment with our info
            Ack = #{
                realm => State#state.realm,
                public_topics => get_public_topics(State),
                status => accepted
            },
            macula_connection:send(FromNodeId, encode(?MSG_FEDERATION_ACK, Ack)),

            {ok, State1};

        false ->
            %% Reject federation
            Reject = #{status => rejected, reason => <<"policy_denied">>},
            macula_connection:send(FromNodeId, encode(?MSG_FEDERATION_ACK, Reject)),
            {ok, State}
    end.
```

**Federation Subscription**:

```erlang
%% Node in Realm A subscribes to public topic in Realm B
subscribe_federated_topic(Topic, ForeignRealm, State) ->
    %% Find gateway to foreign realm
    case find_federation_gateway(ForeignRealm, State) of
        {ok, GatewayId} ->
            %% Send federation subscribe message to gateway
            SubMsg = #{
                type => subscribe,
                realm => ForeignRealm,
                topic => Topic,
                subscriber => self_node_id(State)
            },
            macula_connection:send(GatewayId, encode(?MSG_FEDERATION_SUBSCRIBE, SubMsg));

        error ->
            {error, realm_not_federated}
    end.

%% Gateway forwards subscription to foreign realm gateway
handle_federation_subscribe(SubMsg, FromNodeId, State) ->
    #{realm := TargetRealm, topic := Topic, subscriber := SubscriberId} = SubMsg,

    %% Check if topic is public in target realm
    case is_public_topic(TargetRealm, Topic, State) of
        true ->
            %% Forward to target realm gateway
            TargetGateway = get_realm_gateway(TargetRealm, State),
            forward_subscription(TargetGateway, Topic, FromNodeId, SubscriberId, State);

        false ->
            send_error(FromNodeId, <<"topic_not_public">>)
    end.
```

### Model C: Strict Isolation (Default)

Most deployments use strict isolation with no cross-realm communication.

**Configuration**:
```erlang
%% Node config
#{
    realm => <<"com.example.energy">>,
    isolation_mode => strict,  % No cross-realm communication
    allow_gateways => false    % Don't connect to gateway nodes
}
```

**Enforcement**:
```erlang
validate_message_destination(DestNodeId, Message, State) ->
    DestRealm = lookup_node_realm(DestNodeId),
    LocalRealm = State#state.realm,
    IsolationMode = State#state.isolation_mode,

    case {IsolationMode, DestRealm} of
        {strict, LocalRealm} ->
            ok;  % Same realm, allowed
        {strict, _ForeignRealm} ->
            {error, cross_realm_forbidden};
        {gateway, _} ->
            ok;  % Gateway mode allows cross-realm
        {federated, _} ->
            ok   % Federation mode allows cross-realm
    end.
```

**Cross-Realm via External APIs**:

Even with strict isolation, realms can communicate via traditional mechanisms:
- **REST/GraphQL APIs**: HTTP endpoints exposed by each realm
- **Message Queues**: RabbitMQ/Kafka bridges between realms
- **Database Replication**: PostgreSQL logical replication
- **File Transfer**: S3/SFTP for batch data exchange

```
Realm A                    External Bridge              Realm B
┌─────────────┐           ┌──────────────┐           ┌─────────────┐
│ Macula Mesh │──HTTP───→ │ REST API     │ ←──HTTP───│ Macula Mesh │
│             │           │              │           │             │
│             │──AMQP───→ │ RabbitMQ     │ ←──AMQP───│             │
│             │           │              │           │             │
│ PostgreSQL  │──SQL────→ │ Replication  │ ←──SQL────│ PostgreSQL  │
└─────────────┘           └──────────────┘           └─────────────┘
```

---

## Protocol-Level Support

### Enhanced Handshake with Realm

Extend the wire protocol to include realm in handshake:

```erlang
-define(MSG_HANDSHAKE, 16#01).
-define(VERSION, 1).

%% Handshake message structure
-record(handshake, {
    version :: integer(),
    node_id :: binary(),
    realm :: binary(),
    tenant_id :: binary() | undefined,
    capabilities = #{} :: map(),
    supported_realms = [] :: [binary()],  % For gateway nodes
    certificate_hash :: binary() | undefined,
    timestamp :: integer()
}).

encode_handshake(Handshake) ->
    Payload = #{
        version => Handshake#handshake.version,
        node_id => Handshake#handshake.node_id,
        realm => Handshake#handshake.realm,
        tenant_id => Handshake#handshake.tenant_id,
        capabilities => Handshake#handshake.capabilities,
        supported_realms => Handshake#handshake.supported_realms,
        cert_hash => Handshake#handshake.certificate_hash,
        timestamp => Handshake#handshake.timestamp
    },
    encode(?MSG_HANDSHAKE, Payload).

decode_handshake(Payload) ->
    #handshake{
        version = maps:get(version, Payload),
        node_id = maps:get(node_id, Payload),
        realm = maps:get(realm, Payload),
        tenant_id = maps:get(tenant_id, Payload, undefined),
        capabilities = maps:get(capabilities, Payload, #{}),
        supported_realms = maps:get(supported_realms, Payload, []),
        certificate_hash = maps:get(cert_hash, Payload, undefined),
        timestamp = maps:get(timestamp, Payload)
    }.
```

### Connection Establishment with Realm Validation

```erlang
%% Client initiates connection
connect_to_peer(Address, Port, State) ->
    {ok, QuicConn} = quicer:connect(Address, Port, connection_opts(State)),
    {ok, QuicStream} = quicer:start_stream(QuicConn, stream_opts(State)),

    %% Send handshake
    Handshake = #handshake{
        version = ?VERSION,
        node_id = State#state.node_id,
        realm = State#state.realm,
        tenant_id = State#state.tenant_id,
        capabilities = State#state.capabilities,
        supported_realms = State#state.supported_realms,
        timestamp = erlang:system_time(millisecond)
    },

    HandshakeMsg = encode_handshake(Handshake),
    ok = quicer:send(QuicStream, HandshakeMsg),

    %% Wait for handshake response
    receive
        {quic, HandshakeReply, QuicStream, _} ->
            handle_handshake_reply(HandshakeReply, QuicConn, QuicStream, State)
    after 5000 ->
        {error, handshake_timeout}
    end.

%% Server receives connection
handle_new_connection(QuicConn, State) ->
    {ok, QuicStream} = quicer:accept_stream(QuicConn, []),

    %% Wait for handshake
    receive
        {quic, HandshakeMsg, QuicStream, _} ->
            case decode_handshake(HandshakeMsg) of
                {ok, Handshake} ->
                    validate_and_complete_handshake(Handshake, QuicConn, QuicStream, State);
                {error, Reason} ->
                    {error, {invalid_handshake, Reason}}
            end
    after 5000 ->
        {error, handshake_timeout}
    end.

validate_and_complete_handshake(Handshake, QuicConn, QuicStream, State) ->
    MyRealm = State#state.realm,
    TheirRealm = Handshake#handshake.realm,

    %% Determine realm compatibility
    case realm_compatibility(MyRealm, TheirRealm, State) of
        {ok, same_realm} ->
            %% Same realm - full trust
            complete_handshake(Handshake, QuicConn, QuicStream, same_realm, State);

        {ok, {gateway, Policy}} ->
            %% Gateway connection - apply policy
            complete_handshake(Handshake, QuicConn, QuicStream, {gateway, Policy}, State);

        {ok, federated} ->
            %% Federated realm - limited trust
            complete_handshake(Handshake, QuicConn, QuicStream, federated, State);

        {error, realm_mismatch} ->
            %% Reject connection
            reject_handshake(QuicConn, QuicStream, realm_mismatch),
            {error, realm_mismatch}
    end.

realm_compatibility(MyRealm, TheirRealm, State) ->
    case {MyRealm, TheirRealm, is_gateway_node(State)} of
        {Same, Same, _} ->
            {ok, same_realm};

        {_, TheirRealm, true} ->
            %% I'm a gateway - check if I support their realm
            case lists:member(TheirRealm, State#state.supported_realms) of
                true ->
                    Policy = get_gateway_policy(MyRealm, TheirRealm, State),
                    {ok, {gateway, Policy}};
                false ->
                    {error, realm_not_supported}
            end;

        {_, TheirRealm, false} ->
            %% I'm not a gateway - check if federation is enabled
            case is_federation_enabled(State) of
                true ->
                    case is_realm_federated(TheirRealm, State) of
                        true -> {ok, federated};
                        false -> {error, realm_not_federated}
                    end;
                false ->
                    {error, realm_mismatch}
            end
    end.

complete_handshake(Handshake, QuicConn, QuicStream, ConnectionType, State) ->
    %% Send handshake acknowledgment
    Ack = #{
        status => accepted,
        realm => State#state.realm,
        connection_type => ConnectionType,
        timestamp => erlang:system_time(millisecond)
    },

    AckMsg = encode(?MSG_HANDSHAKE_ACK, Ack),
    ok = quicer:send(QuicStream, AckMsg),

    %% Register connection
    PeerInfo = #{
        node_id => Handshake#handshake.node_id,
        realm => Handshake#handshake.realm,
        connection_type => ConnectionType,
        quic_conn => QuicConn,
        quic_stream => QuicStream
    },

    {ok, add_peer_connection(PeerInfo, State)}.
```

---

## Discovery and Membership

### SWIM Gossip Partitioned by Realm

Each node maintains separate membership per realm:

```erlang
-record(membership_state, {
    local_realm :: binary(),

    %% Nodes in my realm
    local_members = [] :: [member()],

    %% Gateway nodes (multi-realm capable)
    gateway_members = [] :: [gateway_member()],

    %% Known remote realms (via gateways)
    remote_realms = #{} :: #{
        realm() => #{
            gateway_nodes => [node_id()],
            public_topics => [topic_pattern()],
            last_seen => integer()
        }
    },

    %% SWIM protocol state
    swim_state = #{
        protocol_period => 1000,     % 1 second
        infection_factor => 3,       % Gossip to 3 nodes
        suspicion_timeout => 5000,   % 5 seconds
        failure_timeout => 10000     % 10 seconds
    }
}).

-record(member, {
    node_id :: binary(),
    realm :: binary(),
    address :: {inet:ip_address(), inet:port_number()},
    state :: alive | suspect | dead,
    incarnation :: integer(),
    last_seen :: integer(),
    metadata = #{} :: map()
}).

-record(gateway_member, {
    node_id :: binary(),
    primary_realm :: binary(),
    supported_realms = [] :: [binary()],
    address :: {inet:ip_address(), inet:port_number()},
    state :: alive | suspect | dead,
    policies = #{} :: #{
        {realm(), realm()} => policy()
    },
    last_seen :: integer()
}).
```

### SWIM Tick with Realm Scoping

```erlang
swim_tick(State) ->
    LocalRealm = State#membership_state.local_realm,

    %% 1. Select random local member to ping
    case select_random_member(State#membership_state.local_members) of
        {ok, Target} ->
            swim_ping_member(Target, LocalRealm, State);
        error ->
            State
    end,

    %% 2. Select random gateway to ping (if any)
    case select_random_gateway(State#membership_state.gateway_members) of
        {ok, Gateway} ->
            swim_ping_gateway(Gateway, State);
        error ->
            State
    end,

    %% 3. Gossip membership changes to N random members
    gossip_membership(State),

    State.

swim_ping_member(Target, Realm, State) ->
    PingMsg = #{
        type => ping,
        realm => Realm,
        sender => self_node_id(),
        sequence => next_sequence(),
        timestamp => erlang:system_time(millisecond)
    },

    case send_with_timeout(Target#member.node_id, PingMsg, 1000) of
        {ok, pong} ->
            %% Member is alive
            update_member_state(Target#member.node_id, alive, State);

        timeout ->
            %% Try indirect ping
            indirect_ping(Target, Realm, State)
    end.

indirect_ping(Target, Realm, State) ->
    %% Select K random members to probe target
    K = 3,
    Probers = select_random_members(K, State),

    IndirectPingMsg = #{
        type => indirect_ping,
        realm => Realm,
        target => Target#member.node_id,
        sender => self_node_id(),
        sequence => next_sequence()
    },

    %% Send to K probers
    lists:foreach(
        fun(Prober) ->
            send(Prober#member.node_id, IndirectPingMsg)
        end,
        Probers
    ),

    %% Wait for any response
    receive
        {indirect_ack, Target#member.node_id} ->
            %% Target is alive (via indirect route)
            update_member_state(Target#member.node_id, alive, State)
    after 2000 ->
        %% No response - mark as suspect
        mark_suspect(Target#member.node_id, State)
    end.

gossip_membership(State) ->
    %% Build gossip payload
    Changes = get_recent_membership_changes(State),

    GossipMsg = #{
        type => gossip,
        realm => State#membership_state.local_realm,
        changes => Changes,
        timestamp => erlang:system_time(millisecond)
    },

    %% Send to N random members
    N = State#membership_state.swim_state.infection_factor,
    Members = select_random_members(N, State),

    lists:foreach(
        fun(Member) ->
            send(Member#member.node_id, GossipMsg)
        end,
        Members
    ).
```

### Discovery with Realm Information

**mDNS Announcement**:
```erlang
announce_via_mdns(State) ->
    Realm = State#state.realm,
    NodeId = State#state.node_id,
    Port = State#state.listen_port,

    %% Announce on mDNS
    ServiceName = <<"_macula._udp.local">>,
    InstanceName = <<NodeId/binary, "._macula._udp.local">>,

    TxtRecords = [
        <<"realm=", Realm/binary>>,
        <<"tenant=", (get_tenant_id(State))/binary>>,
        <<"caps=pubsub,rpc">>,
        <<"version=1.0">>
    ],

    mdns:announce(ServiceName, InstanceName, Port, TxtRecords).
```

**DNS SRV Discovery**:
```erlang
discover_via_dns(Realm) ->
    %% Query for realm-specific SRV records
    DnsName = iolist_to_binary([
        <<"_macula._udp.">>,
        realm_to_dns(Realm)
    ]),

    %% Example: _macula._udp.example.be
    case inet_res:lookup(DnsName, in, srv) of
        [] ->
            {error, no_nodes_found};
        SrvRecords ->
            Nodes = lists:map(
                fun({Priority, Weight, Port, Host}) ->
                    #{
                        address => Host,
                        port => Port,
                        priority => Priority,
                        weight => Weight
                    }
                end,
                SrvRecords
            ),
            {ok, Nodes}
    end.

realm_to_dns(<<"com.example.energy">>) -> <<"example.be">>;
realm_to_dns(<<"com.acme.iot">>) -> <<"acme.com">>;
realm_to_dns(Realm) ->
    %% Generic: reverse the realm parts
    Parts = binary:split(Realm, <<".">>, [global]),
    ReversedParts = lists:reverse(Parts),
    iolist_to_binary(lists:join(<<".">>, ReversedParts)).
```

---

## Pub/Sub with Realm Scoping

### Subscribe Within Realm

```erlang
subscribe(Topic) ->
    subscribe(Topic, #{}).

subscribe(Topic, Opts) ->
    gen_server:call(?MODULE, {subscribe, Topic, Opts}).

handle_call({subscribe, Topic, Opts}, _From, State) ->
    LocalRealm = State#state.realm,
    TargetRealm = maps:get(realm, Opts, LocalRealm),

    case TargetRealm of
        LocalRealm ->
            %% Local subscription
            State1 = subscribe_local(Topic, State),
            {reply, ok, State1};

        ForeignRealm ->
            %% Cross-realm subscription
            case is_cross_realm_allowed(State) of
                true ->
                    State1 = subscribe_foreign(Topic, ForeignRealm, Opts, State),
                    {reply, ok, State1};
                false ->
                    {reply, {error, cross_realm_forbidden}, State}
            end
    end.

subscribe_local(Topic, State) ->
    Realm = State#state.realm,
    FullTopic = <<Realm/binary, ".", Topic/binary>>,

    %% Add to local subscription table
    Subs = State#state.subscriptions,
    Subs1 = maps:update_with(
        FullTopic,
        fun(Subscribers) -> [self() | Subscribers] end,
        [self()],
        Subs
    ),

    %% Announce subscription to mesh (so publishers can route)
    announce_subscription(FullTopic, State),

    State#state{subscriptions = Subs1}.

subscribe_foreign(Topic, ForeignRealm, Opts, State) ->
    %% Find gateway to foreign realm
    case find_gateway_to_realm(ForeignRealm, State) of
        {ok, GatewayNodeId} ->
            %% Send subscription request to gateway
            SubMsg = #{
                type => subscribe,
                realm => ForeignRealm,
                topic => Topic,
                options => Opts,
                subscriber => self_node_id(State)
            },

            send(GatewayNodeId, SubMsg),

            %% Track foreign subscription
            ForeignSubs = State#state.foreign_subscriptions,
            ForeignSubs1 = maps:update_with(
                {ForeignRealm, Topic},
                fun(Subs) -> [self() | Subs] end,
                [self()],
                ForeignSubs
            ),

            State#state{foreign_subscriptions = ForeignSubs1};

        error ->
            error({no_gateway_to_realm, ForeignRealm})
    end.
```

### Publish with Realm Routing

```erlang
publish(Topic, Payload) ->
    publish(Topic, Payload, #{}).

publish(Topic, Payload, Opts) ->
    gen_server:call(?MODULE, {publish, Topic, Payload, Opts}).

handle_call({publish, Topic, Payload, Opts}, _From, State) ->
    Realm = State#state.realm,
    FullTopic = <<Realm/binary, ".", Topic/binary>>,

    %% Build publication message
    PubMsg = #{
        type => publish,
        topic => FullTopic,
        payload => Payload,
        publisher => self_node_id(State),
        timestamp => erlang:system_time(millisecond),
        options => Opts
    },

    %% Route to subscribers
    route_publication(FullTopic, PubMsg, State),

    {reply, ok, State}.

route_publication(Topic, PubMsg, State) ->
    %% 1. Deliver to local subscribers
    LocalSubs = maps:get(Topic, State#state.subscriptions, []),
    lists:foreach(
        fun(Subscriber) ->
            Subscriber ! {event, Topic, PubMsg}
        end,
        LocalSubs
    ),

    %% 2. Route to remote subscribers in same realm
    route_to_remote_subscribers(Topic, PubMsg, State),

    %% 3. Route to gateway if topic is public
    case is_public_topic(Topic, State) of
        true ->
            route_to_gateways(Topic, PubMsg, State);
        false ->
            ok
    end.

route_to_remote_subscribers(Topic, PubMsg, State) ->
    %% Query DHT for nodes subscribed to this topic
    case macula_routing:lookup_subscribers(Topic) of
        {ok, Subscribers} ->
            lists:foreach(
                fun(NodeId) ->
                    case NodeId =/= self_node_id(State) of
                        true -> send(NodeId, PubMsg);
                        false -> ok
                    end
                end,
                Subscribers
            );
        error ->
            ok
    end.
```

---

## Security and Authorization

### Certificate-Based Realm Verification

Each node has a TLS certificate that encodes its realm membership:

```erlang
%% Generate node certificate with realm in Subject Alternative Name
generate_node_cert(NodeId, Realm, TenantId) ->
    %% Certificate subject
    Subject = [
        {commonName, binary_to_list(NodeId)},
        {organizationName, binary_to_list(Realm)},
        {organizationalUnitName, binary_to_list(TenantId)}
    ],

    %% Subject Alternative Names
    SANs = [
        {dNSName, binary_to_list(<<NodeId/binary, ".macula.local">>)},
        {uniformResourceIdentifier, binary_to_list(
            <<"macula://", Realm/binary, "/", NodeId/binary>>
        )}
    ],

    %% Generate key pair
    {ok, PrivKey} = public_key:generate_key({rsa, 2048, 65537}),

    %% Create certificate
    Cert = public_key:pkix_sign(
        #{
            subject => Subject,
            extensions => [
                #{extnID => ?'id-ce-subjectAltName',
                  extnValue => SANs}
            ],
            validity => {not_before(), not_after()},
            subject_pk_info => PrivKey
        },
        ca_key()  % Signed by CA
    ),

    {ok, Cert, PrivKey}.

%% Gateway certificate includes multiple realms
generate_gateway_cert(NodeId, Realms) ->
    %% SANs for each realm
    SANs = lists:map(
        fun(Realm) ->
            {uniformResourceIdentifier, binary_to_list(
                <<"macula://", Realm/binary, "/", NodeId/binary>>
            )}
        end,
        Realms
    ),

    %% ... rest of certificate generation
    generate_node_cert(NodeId, hd(Realms), <<"gateway">>).
```

### TLS Handshake with Realm Validation

```erlang
%% QUIC connection options with TLS
connection_opts(State) ->
    #{
        cert => State#state.cert,
        key => State#state.key,
        cacerts => State#state.ca_certs,
        alpn => [<<"macula/1.0">>],
        verify => verify_peer,
        verify_fun => {fun verify_peer_cert/3, State},
        fail_if_no_peer_cert => true
    }.

%% Verify peer certificate during TLS handshake
verify_peer_cert(Cert, Event, State) ->
    case Event of
        {bad_cert, Reason} ->
            {fail, Reason};

        {extension, _} ->
            {unknown, State};

        valid ->
            %% Extract realm from certificate
            case extract_realm_from_cert(Cert) of
                {ok, Realms} ->
                    %% Check if any realm is compatible
                    case is_any_realm_compatible(Realms, State) of
                        true ->
                            {valid, State#{peer_realms => Realms}};
                        false ->
                            {fail, {realm_mismatch, Realms}}
                    end;

                error ->
                    {fail, no_realm_in_cert}
            end;

        valid_peer ->
            {valid, State}
    end.

extract_realm_from_cert(Cert) ->
    %% Parse certificate
    OTPCert = public_key:pkix_decode_cert(Cert, otp),

    %% Get Subject Alternative Names extension
    Extensions = OTPCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.extensions,

    case lists:keyfind(?'id-ce-subjectAltName', #'Extension'.extnID, Extensions) of
        #'Extension'{extnValue = SANs} ->
            %% Extract realms from URI SANs
            Realms = lists:filtermap(
                fun({uniformResourceIdentifier, URI}) ->
                    case parse_macula_uri(URI) of
                        {ok, Realm, _NodeId} -> {true, Realm};
                        error -> false
                    end;
                   (_) -> false
                end,
                SANs
            ),
            case Realms of
                [] -> error;
                _ -> {ok, Realms}
            end;

        false ->
            error
    end.

parse_macula_uri(URI) ->
    %% Parse: macula://com.example.energy/node123
    case string:split(URI, "://", leading) of
        ["macula", Rest] ->
            case string:split(Rest, "/", leading) of
                [Realm, NodeId] ->
                    {ok, list_to_binary(Realm), list_to_binary(NodeId)};
                _ ->
                    error
            end;
        _ ->
            error
    end.

is_any_realm_compatible(PeerRealms, State) ->
    MyRealm = State#state.realm,

    %% Check if any peer realm matches my realm
    lists:member(MyRealm, PeerRealms) orelse

    %% Or if I'm a gateway supporting any of their realms
    (is_gateway_node(State) andalso
     lists:any(
         fun(Realm) ->
             lists:member(Realm, State#state.supported_realms)
         end,
         PeerRealms
     )).
```

### Access Control Lists (ACLs)

```erlang
-record(acl_rule, {
    realm :: binary(),
    tenant_id :: binary() | '_',
    resource :: topic | rpc | stream,
    pattern :: binary(),  % Topic/RPC pattern
    action :: publish | subscribe | call | register,
    allow :: boolean()
}).

%% Example ACL rules
-define(ACL_RULES, [
    %% Allow all nodes in energy realm to publish home measurements
    #acl_rule{
        realm = <<"com.example.energy">>,
        tenant_id = '_',
        resource = topic,
        pattern = <<"com.example.energy.home.*.measured">>,
        action = publish,
        allow = true
    },

    %% Only analytics realm can subscribe to individual home data
    #acl_rule{
        realm = <<"com.example.analytics">>,
        tenant_id = '_',
        resource = topic,
        pattern = <<"com.example.energy.home.*.measured">>,
        action = subscribe,
        allow = true
    },

    %% Block cross-tenant access within realm
    #acl_rule{
        realm = '_',
        tenant_id = '_',
        resource = topic,
        pattern = <<"*.{tenant}.*.private.*">>,
        action = subscribe,
        allow = false  % Must match tenant_id
    }
]).

check_acl(Action, Resource, Pattern, NodeIdentity) ->
    Realm = NodeIdentity#node_identity.realm,
    TenantId = NodeIdentity#node_identity.tenant_id,

    %% Find matching rules (most specific first)
    MatchingRules = find_matching_rules(Action, Resource, Pattern, Realm, TenantId),

    case MatchingRules of
        [] ->
            %% Default deny
            {deny, no_matching_rule};

        [Rule | _] ->
            %% Apply first matching rule
            case Rule#acl_rule.allow of
                true -> allow;
                false -> {deny, Rule}
            end
    end.

find_matching_rules(Action, Resource, Pattern, Realm, TenantId) ->
    lists:filter(
        fun(Rule) ->
            rule_matches(Rule, Action, Resource, Pattern, Realm, TenantId)
        end,
        ?ACL_RULES
    ).

rule_matches(Rule, Action, Resource, Pattern, Realm, TenantId) ->
    %% Check action
    (Rule#acl_rule.action =:= Action) andalso

    %% Check resource type
    (Rule#acl_rule.resource =:= Resource) andalso

    %% Check realm (wildcard or exact match)
    (Rule#acl_rule.realm =:= '_' orelse Rule#acl_rule.realm =:= Realm) andalso

    %% Check tenant (wildcard or exact match)
    (Rule#acl_rule.tenant_id =:= '_' orelse
     Rule#acl_rule.tenant_id =:= TenantId) andalso

    %% Check pattern (wildcard matching)
    match_pattern(Pattern, Rule#acl_rule.pattern).

match_pattern(Topic, Pattern) ->
    %% Simple wildcard matching (* = any segment, ** = any remaining)
    TopicParts = binary:split(Topic, <<".">>, [global]),
    PatternParts = binary:split(Pattern, <<".">>, [global]),

    match_parts(TopicParts, PatternParts).

match_parts([], []) -> true;
match_parts(_, [<<"**">>]) -> true;
match_parts([_|TRest], [<<"*">>|PRest]) -> match_parts(TRest, PRest);
match_parts([T|TRest], [T|PRest]) -> match_parts(TRest, PRest);
match_parts(_, _) -> false.
```

---

## Use Cases and Patterns

### Use Case 1: Multi-Customer SaaS Platform

**Scenario**: Energy monitoring SaaS with multiple customers, each with isolated data.

**Architecture**:
```
Customer A Realm: com.energysaas.customer-a
Customer B Realm: com.energysaas.customer-b
Customer C Realm: com.energysaas.customer-c
Analytics Realm:  com.energysaas.analytics (shared)
```

**Node Deployment**:
```
Customer A Edge Nodes:
  - 50 home devices (realm: com.energysaas.customer-a)
  - Publishing: com.energysaas.customer-a.home.*.measured

Customer B Edge Nodes:
  - 100 home devices (realm: com.energysaas.customer-b)
  - Publishing: com.energysaas.customer-b.home.*.measured

Analytics Realm Nodes:
  - Gateway nodes with policies:
    - Read-only access to all customer realms
    - Can subscribe to *.home.*.measured
    - Cannot publish to customer realms
```

**Gateway Policy**:
```erlang
%% Analytics can read from all customer realms
#{
    {<<"com.energysaas.customer-a">>, <<"com.energysaas.analytics">>} => #{
        allowed_topics => [
            <<"com.energysaas.customer-a.home.*.measured">>,
            <<"com.energysaas.customer-a.aggregated.*">>
        ],
        allowed_operations => [subscribe],  % Read-only
        anonymize_fields => [<<"customer_id">>, <<"home_address">>],
        rate_limit => #{max_msg_per_sec => 5000}
    },

    %% Same for customer B, C, etc.
    {<<"com.energysaas.customer-b">>, <<"com.energysaas.analytics">>} => #{
        allowed_topics => [
            <<"com.energysaas.customer-b.home.*.measured">>,
            <<"com.energysaas.customer-b.aggregated.*">>
        ],
        allowed_operations => [subscribe],
        anonymize_fields => [<<"customer_id">>, <<"home_address">>],
        rate_limit => #{max_msg_per_sec => 10000}  % Customer B has more devices
    }
}
```

**Benefits**:
- Complete customer isolation (no cross-customer data leakage)
- Shared analytics infrastructure (cost savings)
- Centralized monitoring and alerting
- Per-customer rate limiting and quotas

### Use Case 2: Energy Market with Private Participants

**Scenario**: Energy trading market where homes/providers keep data private but publish aggregated market data.

**Architecture**:
```
Home Realm:     com.example.energy.homes
Provider Realm: com.example.energy.providers
Market Realm:   com.example.energy.market (public)
```

**Data Flow**:
```
Homes (private realm):
  - Publish internally: com.example.energy.homes.home.{id}.measured
  - Aggregate locally
  - Publish to market: com.example.energy.market.demand.total

Providers (private realm):
  - Publish internally: com.example.energy.providers.provider.{id}.price
  - Publish to market: com.example.energy.market.supply.offers

Market (public realm):
  - Subscribe to: com.example.energy.market.demand.*
  - Subscribe to: com.example.energy.market.supply.*
  - Publish: com.example.energy.market.clearing_price
```

**Gateway Configuration**:
```erlang
%% Gateway between homes realm and market realm
#{
    {<<"com.example.energy.homes">>, <<"com.example.energy.market">>} => #{
        %% Only allow aggregated data
        allowed_topics => [
            <<"com.example.energy.homes.aggregated.*">>,
            <<"com.example.energy.homes.demand.total">>
        ],
        denied_topics => [
            <<"com.example.energy.homes.home.*">>  % No individual homes
        ],
        allowed_operations => [publish],
        require_aggregation => true  % Must be aggregate, not raw
    },

    %% Market can publish clearing prices back to homes
    {<<"com.example.energy.market">>, <<"com.example.energy.homes">>} => #{
        allowed_topics => [
            <<"com.example.energy.market.clearing_price">>,
            <<"com.example.energy.market.announcements">>
        ],
        allowed_operations => [publish],
        rate_limit => #{max_msg_per_sec => 100}
    }
}
```

**Benefits**:
- Privacy: Individual home data never leaves homes realm
- Market efficiency: Aggregated demand/supply visible to all
- Fair pricing: All participants see same clearing price
- Auditability: Gateway logs all cross-realm publications

### Use Case 3: IoT Edge-to-Cloud with Local Control

**Scenario**: Factory IoT devices with local control loop and cloud analytics.

**Architecture**:
```
Edge Realm:  com.iot.edge.factory-42
Cloud Realm: com.iot.cloud.analytics
```

**Deployment**:
```
Edge Realm (on-premise):
  - 1000 sensors publishing: com.iot.edge.factory-42.sensor.{id}.reading
  - Local control system subscribing and publishing actuator commands
  - Gateway buffering data for cloud upload

Cloud Realm (AWS):
  - Analytics nodes subscribing to buffered sensor data
  - ML models publishing predictions back to edge
```

**Gateway Buffering**:
```erlang
%% Gateway buffers high-frequency edge data
-record(gateway_buffer, {
    realm_pair :: {realm(), realm()},
    buffer = [] :: [message()],
    max_size = 10000 :: integer(),
    flush_interval = 60000 :: integer(),  % 1 minute
    last_flush :: integer()
}).

handle_edge_message(Msg, Buffer) ->
    #{topic := Topic, payload := Payload} = Msg,

    %% Buffer message
    Buffer1 = Buffer#gateway_buffer{
        buffer = [Msg | Buffer#gateway_buffer.buffer]
    },

    %% Check if should flush
    Now = erlang:system_time(millisecond),
    ShouldFlush =
        (length(Buffer1#gateway_buffer.buffer) >= Buffer1#gateway_buffer.max_size) orelse
        ((Now - Buffer1#gateway_buffer.last_flush) >= Buffer1#gateway_buffer.flush_interval),

    case ShouldFlush of
        true ->
            %% Batch upload to cloud
            flush_to_cloud(Buffer1),
            Buffer1#gateway_buffer{buffer = [], last_flush = Now};
        false ->
            Buffer1
    end.

flush_to_cloud(Buffer) ->
    %% Aggregate sensor readings
    Aggregated = aggregate_sensor_data(Buffer#gateway_buffer.buffer),

    %% Publish to cloud realm
    CloudTopic = <<"com.iot.cloud.analytics.factory-42.aggregated">>,
    publish_to_realm(
        <<"com.iot.cloud.analytics">>,
        CloudTopic,
        Aggregated
    ).
```

**Benefits**:
- Local control continues even if cloud connection lost
- Reduced cloud bandwidth (buffering + aggregation)
- Real-time edge responsiveness
- Cloud analytics on aggregated data

### Use Case 4: Partner Integration and Data Exchange

**Scenario**: Energy company (Company A) shares market data with partner (Company B).

**Architecture**:
```
Company A Realm: com.companya.energy
Company B Realm: com.companyb.energy
Shared Market Realm: org.energymarket.public
```

**Federation Setup**:
```erlang
%% Company A publishes market data to shared realm
#{
    {<<"com.companya.energy">>, <<"org.energymarket.public">>} => #{
        allowed_topics => [
            <<"com.companya.energy.market.prices">>,
            <<"com.companya.energy.market.capacity">>
        ],
        topic_translation => [
            {<<"com.companya.energy.market.{metric}">>,
             <<"org.energymarket.public.companya.{metric}">>}
        ],
        allowed_operations => [publish],
        require_signature => true,  % Cryptographically sign publications
        rate_limit => #{max_msg_per_sec => 1000}
    },

    %% Company B can read from shared market
    {<<"org.energymarket.public">>, <<"com.companyb.energy">>} => #{
        allowed_topics => [
            <<"org.energymarket.public.*.prices">>,
            <<"org.energymarket.public.*.capacity">>
        ],
        allowed_operations => [subscribe],
        verify_signature => true  % Verify publisher signatures
    }
}
```

**Benefits**:
- Controlled data sharing (only specific topics)
- Topic translation (namespace isolation)
- Cryptographic authentication (signatures)
- Audit trail of all exchanges

---

## Implementation Roadmap

This feature set would be implemented as **Phase 6** of the main Macula HTTP/3 Mesh roadmap (Weeks 21-26).

### Week 21: Realm Identity and Protocol

**Deliverables**:
- [ ] Extend `node_identity` record with `realm`, `tenant_id`, `supported_realms`
- [ ] Update `MSG_HANDSHAKE` to include realm fields
- [ ] Implement `realm_compatibility()` validation logic
- [ ] Add realm extraction from TLS certificates
- [ ] Write unit tests for realm validation

**Code Modules**:
- `macula_identity.erl` - Node identity with realm
- `macula_handshake.erl` - Enhanced handshake with realm
- `macula_cert.erl` - Certificate generation/validation

### Week 22: Routing Table Partitioning

**Deliverables**:
- [ ] Partition Kademlia DHT by realm
- [ ] Implement `routing_state` with per-realm tables
- [ ] Update `route_message()` to handle cross-realm routing
- [ ] Add gateway route discovery
- [ ] Integration tests for multi-realm routing

**Code Modules**:
- `macula_routing.erl` - Realm-aware routing
- `macula_kademlia.erl` - Per-realm DHT

### Week 23: SWIM Membership with Realms

**Deliverables**:
- [ ] Update SWIM gossip to scope by realm
- [ ] Separate `local_members` and `gateway_members`
- [ ] Implement gateway node discovery
- [ ] Realm-specific failure detection
- [ ] Multi-realm membership tests

**Code Modules**:
- `macula_membership.erl` - Realm-scoped SWIM

### Week 24: Gateway Node Implementation

**Deliverables**:
- [ ] Implement `gateway_config` with policies
- [ ] Policy engine for message filtering
- [ ] Topic translation logic
- [ ] Rate limiting per realm pair
- [ ] Gateway buffering and batching

**Code Modules**:
- `macula_gateway.erl` - Gateway node GenServer
- `macula_policy.erl` - Policy evaluation engine
- `macula_translation.erl` - Topic translation

### Week 25: Pub/Sub with Cross-Realm

**Deliverables**:
- [ ] Realm-scoped subscribe/publish APIs
- [ ] Cross-realm subscription via gateways
- [ ] Foreign subscription tracking
- [ ] ACL enforcement for topics
- [ ] End-to-end pub/sub tests across realms

**Code Modules**:
- `macula_pubsub.erl` - Update with realm support
- `macula_acl.erl` - Access control lists

### Week 26: Security and Audit

**Deliverables**:
- [ ] Certificate-based realm validation
- [ ] ACL rule engine
- [ ] Audit logging for gateway traffic
- [ ] Message signing/verification (optional)
- [ ] Security audit and penetration testing

**Code Modules**:
- `macula_acl.erl` - Complete ACL implementation
- `macula_audit.erl` - Audit logging
- `macula_crypto.erl` - Message signing

### Week 27: Federation Protocol (Optional)

**Deliverables**:
- [ ] DNS-based realm discovery
- [ ] Federation announcement/acknowledgment
- [ ] Federated subscription forwarding
- [ ] Public topic registry
- [ ] Federation tests

**Code Modules**:
- `macula_federation.erl` - Federation protocol

---

## Performance Considerations

### Routing Table Size

**Problem**: Multiple routing tables (one per realm) could increase memory usage.

**Solutions**:
1. **Lazy Loading**: Only load routing tables for realms we're actively communicating with
2. **Shared Structure**: Use persistent data structures (e.g., `gb_trees`) to share common nodes
3. **TTL and Eviction**: Expire unused realm routing tables after inactivity

**Estimates**:
- Single realm routing table: ~256 buckets × 20 nodes/bucket × 200 bytes/node = ~1 MB
- 10 active realms: ~10 MB (acceptable)
- 100 active realms: ~100 MB (may need eviction)

### Gateway Throughput

**Problem**: Gateway nodes could become bottlenecks.

**Solutions**:
1. **Multiple Gateways**: Deploy multiple gateway nodes per realm pair
2. **Load Balancing**: Distribute cross-realm traffic across gateways
3. **Direct Peering**: Allow nodes to establish direct cross-realm connections after gateway introduction
4. **Batching**: Buffer and batch messages at gateway (reduces per-message overhead)

**Benchmarks** (target):
- Gateway throughput: 100,000 msg/sec
- Gateway latency: < 5ms added latency
- Policy evaluation: < 100 µs per message

### SWIM Gossip Overhead

**Problem**: Running SWIM for multiple realms increases gossip traffic.

**Solutions**:
1. **Independent Protocols**: Each realm runs independent SWIM (no cross-realm gossip)
2. **Longer Periods**: Increase gossip period for gateway/foreign realms (less critical)
3. **Piggybacking**: Include gateway announcements in local realm gossip

**Estimates**:
- Local realm SWIM: 1000 nodes × 3 gossips/sec × 1 KB = 3 MB/sec
- Gateway realm SWIM: 10 nodes × 1 gossip/sec × 1 KB = 10 KB/sec (negligible)

### Certificate Validation

**Problem**: TLS handshake with certificate validation adds latency.

**Solutions**:
1. **Certificate Caching**: Cache validated certificates by node ID
2. **Session Resumption**: Use TLS session tickets (0-RTT on reconnect)
3. **OCSP Stapling**: Include OCSP response in handshake (avoid extra roundtrip)

**Benchmarks**:
- First connection (full handshake): ~50ms
- Reconnection (0-RTT): ~1ms

---

## Operational Concerns

### Certificate Management

**Challenges**:
- Certificate generation for each node
- Certificate rotation and renewal
- Certificate revocation (CRL/OCSP)
- CA key management

**Solutions**:
1. **Automated CA**: Use `cfssl` or `step-ca` for automated cert issuance
2. **Short-lived Certs**: Issue 30-day certificates (reduces revocation complexity)
3. **Auto-renewal**: Nodes automatically renew certificates before expiry
4. **Cert Pinning**: Pin gateway certificates in configuration (prevent MITM)

**Example Cert Rotation**:
```erlang
%% Node monitors cert expiry
check_cert_expiry(State) ->
    ExpiryTime = cert_expiry_time(State#state.cert),
    Now = erlang:system_time(second),
    TimeToExpiry = ExpiryTime - Now,

    %% Renew if < 7 days remaining
    if
        TimeToExpiry < (7 * 24 * 3600) ->
            renew_certificate(State);
        true ->
            ok
    end.

renew_certificate(State) ->
    %% Request new cert from CA
    {ok, NewCert, NewKey} = request_cert_from_ca(
        State#state.node_id,
        State#state.realm
    ),

    %% Update TLS configuration
    update_tls_config(NewCert, NewKey, State).
```

### Gateway High Availability

**Challenges**:
- Gateway nodes are single points of failure for cross-realm communication
- Need redundancy without creating complexity

**Solutions**:
1. **Active-Active**: Deploy multiple gateways per realm pair (load balance)
2. **Health Checks**: Nodes monitor gateway health via SWIM
3. **Automatic Failover**: Switch to backup gateway if primary fails
4. **Stateless Gateways**: Gateways don't hold state (easy to scale horizontally)

**Example Gateway Selection**:
```erlang
select_gateway(FromRealm, ToRealm, State) ->
    %% Get all gateways for realm pair
    Gateways = maps:get({FromRealm, ToRealm}, State#state.gateway_routes, []),

    case Gateways of
        [] ->
            {error, no_gateway};

        [Gateway] ->
            %% Single gateway
            {ok, Gateway};

        Multiple ->
            %% Load balance across healthy gateways
            Healthy = lists:filter(
                fun(GW) -> is_gateway_healthy(GW, State) end,
                Multiple
            ),

            case Healthy of
                [] -> {error, no_healthy_gateway};
                _ -> {ok, random_select(Healthy)}
            end
    end.
```

### Monitoring and Observability

**Key Metrics**:
1. **Per-realm metrics**:
   - Message rate (pub/sub/rpc)
   - Node count (alive/suspect/dead)
   - Routing table size
   - Latency (p50, p95, p99)

2. **Gateway metrics**:
   - Cross-realm message rate
   - Policy denials
   - Rate limit violations
   - Buffered message count
   - Latency added by gateway

3. **Security metrics**:
   - Failed certificate validations
   - ACL denials
   - Anomalous traffic patterns

**Prometheus Metrics**:
```erlang
%% Expose metrics
prometheus_counter:inc(macula_messages_total, [Realm, Type]),
prometheus_histogram:observe(macula_gateway_latency_ms, [FromRealm, ToRealm], Latency),
prometheus_gauge:set(macula_nodes_alive, [Realm], AliveCount).
```

### Configuration Management

**Challenges**:
- Complex gateway policies
- ACL rules across multiple realms
- Certificate authorities per realm
- DNS/discovery configuration

**Solutions**:
1. **Configuration as Code**: Store policies in Git, version controlled
2. **Dynamic Reload**: Support hot-reload of policies without restart
3. **Validation**: Validate policies before deployment (CI/CD)
4. **Centralized Config**: Use etcd/Consul for policy distribution

**Example Policy Config** (YAML):
```yaml
# gateway-policy.yaml
gateway:
  node_id: gateway1
  realms:
    - com.example.energy
    - com.example.analytics

  policies:
    - from_realm: com.example.energy
      to_realm: com.example.analytics
      allowed_topics:
        - com.example.energy.aggregated.*
        - com.example.energy.public.*
      denied_topics:
        - com.example.energy.home.*.measured
      allowed_operations:
        - publish
        - call
      rate_limit:
        max_msg_per_sec: 1000
        max_bytes_per_sec: 1000000
        burst_size: 5000
      transformations:
        anonymize_fields:
          - home_id
          - customer_id
      audit:
        log_all: true
        log_payloads: false
```

Load and apply:
```erlang
load_gateway_policy(ConfigFile) ->
    {ok, PolicyYaml} = file:read_file(ConfigFile),
    {ok, PolicyMap} = yaml:load(PolicyYaml),

    %% Validate policy
    case validate_policy(PolicyMap) of
        ok ->
            %% Convert to internal format
            Policy = parse_policy(PolicyMap),

            %% Apply to gateway
            macula_gateway:update_policy(Policy);

        {error, Errors} ->
            {error, {invalid_policy, Errors}}
    end.
```

---

## Conclusion

The Macula HTTP/3 Mesh architecture naturally supports multi-tenancy and realm isolation through:

1. **Protocol-level realm support**: Handshake includes realm, validated via TLS certificates
2. **Partitioned routing**: Separate Kademlia DHT per realm (scalable)
3. **Controlled federation**: Gateway nodes with policy-based routing
4. **Security by default**: Certificate validation, ACLs, audit logging
5. **Flexible deployment**: Strict isolation, gateways, or full federation

This design enables:
- **SaaS multi-tenancy**: Complete customer isolation with shared infrastructure
- **Partner integration**: Controlled data exchange between organizations
- **Hierarchical systems**: Edge → Regional → Cloud data flows
- **Privacy**: Sensitive data stays within realm boundaries

The implementation roadmap (Weeks 21-27) adds these capabilities without breaking existing functionality, demonstrating the extensibility of the Macula platform.

---

## References

- Main Roadmap: `macula_http3_mesh_roadmap.md`
- C4 Diagrams: `macula_http3_mesh_c4_diagrams.md`
- QUIC RFC 9000: https://www.rfc-editor.org/rfc/rfc9000.html
- HTTP/3 RFC 9114: https://www.rfc-editor.org/rfc/rfc9114.html
- SWIM Protocol: https://www.cs.cornell.edu/projects/Quicksilver/public_pdfs/SWIM.pdf
- Kademlia DHT: https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf
