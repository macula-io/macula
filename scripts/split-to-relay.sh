#!/bin/bash
# Copy relay modules from macula/src/ to macula-relay/src/
# Phase 2 of the v1.0 SDK/Relay separation.
set -eu

MACULA_SRC="/home/rl/work/github.com/macula-io/macula/src"
RELAY_DST="/home/rl/work/github.com/macula-io/macula-relay/src"

# Create system subdirectories in relay
SYSTEMS=(
    macula_bootstrap_system
    macula_bridge_system
    macula_gateway_system
    macula_membership_system
    macula_peer_system
    macula_platform_system
    macula_pubsub_system
    macula_registry_system
    macula_routing_system
    macula_rpc_system
    macula_content_system
)

for sys in "${SYSTEMS[@]}"; do
    mkdir -p "${RELAY_DST}/${sys}"
    echo "Created ${RELAY_DST}/${sys}/"
done

# Copy system directories (all modules inside)
for sys in "${SYSTEMS[@]}"; do
    cp "${MACULA_SRC}/${sys}/"*.erl "${RELAY_DST}/${sys}/"
    echo "Copied ${sys}/ modules"
done

# Copy standalone relay modules (not in system dirs)
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
    cp "${MACULA_SRC}/${mod}" "${RELAY_DST}/${mod}"
    echo "Copied ${mod}"
done

echo ""
echo "Done. Copied $(find "${RELAY_DST}" -name '*.erl' -newer "${RELAY_DST}/macula_relay_app.erl" | wc -l) relay modules."
