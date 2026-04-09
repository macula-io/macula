#!/bin/bash
# Strip macula to SDK-only by removing relay/server modules.
# Phase 4 of the v1.0 SDK/Relay separation.
set -eu

SRC="/home/rl/work/github.com/macula-io/macula/src"

# Remove entire system directories (relay-side)
RELAY_SYSTEMS=(
    macula_bootstrap_system
    macula_bridge_system
    macula_content_system
    macula_gateway_system
    macula_membership_system
    macula_peer_system
    macula_platform_system
    macula_pubsub_system
    macula_registry_system
    macula_routing_system
    macula_rpc_system
)

for sys in "${RELAY_SYSTEMS[@]}"; do
    if [ -d "${SRC}/${sys}" ]; then
        rm -rf "${SRC}/${sys}"
        echo "Removed ${sys}/"
    fi
done

# Remove standalone relay modules
RELAY_MODULES=(
    macula_advertisement_manager.erl
    macula_authorization.erl
    macula_authorization_audit.erl
    macula_connection.erl
    macula_connection_dispatch.erl
    macula_connection_pool.erl
    macula_did_cache.erl
    macula_discovery.erl
    macula_gatekeeper.erl
    macula_gateway.erl
    macula_peer.erl
    macula_peers_sup.erl
    macula_protocol.erl
    macula_quic_cert.erl
    macula_quic_stream_acceptor.erl
    macula_realm_trust.erl
    macula_relay.erl
    macula_relay_discovery.erl
    macula_relay_handler.erl
    macula_relay_node.erl
    macula_relay_registry.erl
    macula_stream_acceptor.erl
    macula_tls.erl
    macula_ucan_revocation.erl
)

for mod in "${RELAY_MODULES[@]}"; do
    if [ -f "${SRC}/${mod}" ]; then
        rm "${SRC}/${mod}"
        echo "Removed ${mod}"
    fi
done

echo ""
echo "SDK modules remaining:"
find "${SRC}" -name "*.erl" | wc -l
