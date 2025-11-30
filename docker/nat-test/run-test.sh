#!/bin/bash
# NAT Traversal Test Runner

NODE_TYPE=${NODE_TYPE:-peer}
NODE_ID=${NODE_ID:-test-node}
QUIC_PORT=${QUIC_PORT:-4433}
HTTP_PORT=${HTTP_PORT:-8080}
BOOTSTRAP_HOST=${BOOTSTRAP_HOST:-}
BOOTSTRAP_PORT=${BOOTSTRAP_PORT:-4433}
RELAY_ENABLED=${RELAY_ENABLED:-false}

# Demo mode: "chatter" (broadcast) or "ping_pong" (bidirectional)
DEMO_MODE=${DEMO_MODE:-chatter}
PING_INTERVAL=${PING_INTERVAL:-5000}
PING_TIMEOUT=${PING_TIMEOUT:-3000}

# Set Macula environment variables
export MACULA_QUIC_PORT=$QUIC_PORT
export GATEWAY_PORT=$QUIC_PORT
export HEALTH_PORT=$HTTP_PORT
export MACULA_REALM="com.macula.nat-test"
export MACULA_TLS_MODE=development
export TLS_CERT_FILE=/opt/macula/certs/cert.pem
export TLS_KEY_FILE=/opt/macula/certs/key.pem
export MACULA_TLS_CERTFILE=/opt/macula/certs/cert.pem
export MACULA_TLS_KEYFILE=/opt/macula/certs/key.pem
export NODE_NAME=$NODE_ID
export HOSTNAME=$NODE_ID
export MACULA_HOSTNAME=$(hostname -i)
export RELAY_ENABLED=$RELAY_ENABLED

# Set bootstrap peers if we're a peer node
if [ "$NODE_TYPE" = "peer" ] && [ -n "$BOOTSTRAP_HOST" ]; then
    export MACULA_BOOTSTRAP_PEERS="https://${BOOTSTRAP_HOST}:${BOOTSTRAP_PORT}"
fi

echo "Starting NAT test node..."
echo "  Node Type: $NODE_TYPE"
echo "  Node ID: $NODE_ID"
echo "  QUIC Port: $QUIC_PORT"
echo "  HTTP Port: $HTTP_PORT"
echo "  Bootstrap: $BOOTSTRAP_HOST:$BOOTSTRAP_PORT"
echo "  Relay Enabled: $RELAY_ENABLED"
echo "  TLS Cert: $TLS_CERT_FILE"
echo "  Macula Realm: $MACULA_REALM"
echo "  Demo Mode: $DEMO_MODE"
if [ "$DEMO_MODE" = "ping_pong" ]; then
    echo "  Ping Interval: ${PING_INTERVAL}ms"
    echo "  Ping Timeout: ${PING_TIMEOUT}ms"
fi

# Wait for network to be ready
sleep 2

# If we're a peer behind NAT and have a bootstrap host, set up routing
if [ "$NODE_TYPE" = "peer" ] && [ -n "$BOOTSTRAP_HOST" ]; then
    echo "Setting up NAT routing..."
    # Get the current network prefix (e.g., 172.31.1 from 172.31.1.10)
    MY_IP=$(hostname -i)
    NETWORK_PREFIX=$(echo $MY_IP | cut -d. -f1-3)
    ROUTER_IP="${NETWORK_PREFIX}.2"

    # Add route to public network via the NAT router
    # Derive the public network subnet from BOOTSTRAP_HOST (e.g., 10.100.0.10 -> 10.100.0.0/24)
    BOOTSTRAP_NETWORK=$(echo $BOOTSTRAP_HOST | cut -d. -f1-3).0/24
    echo "  Router IP: $ROUTER_IP"
    echo "  Adding route to $BOOTSTRAP_NETWORK via $ROUTER_IP"
    ip route add $BOOTSTRAP_NETWORK via $ROUTER_IP 2>/dev/null || true

    # Verify connectivity
    echo "  Testing connectivity to $BOOTSTRAP_HOST..."
    ping -c 1 -W 2 $BOOTSTRAP_HOST && echo "  SUCCESS: Can reach bootstrap" || echo "  WARNING: Cannot reach bootstrap yet"
fi

# Start the Erlang application with clean logging
exec erl -pa _build/default/lib/*/ebin \
    -config config/sys.config \
    -noshell \
    -name "${NODE_ID}@$(hostname -i)" \
    -setcookie nat_test_cookie \
    -eval "
        case application:ensure_all_started(macula) of
            {ok, _Started} ->
                timer:sleep(500),
                case whereis(macula_root) of
                    undefined ->
                        macula_console:error(list_to_binary(\"$NODE_ID\"), <<\"macula_root crashed after startup!\">>),
                        init:stop(1);
                    _ ->
                        %% Show beautiful startup banner
                        macula_console:banner(#{
                            node_id => list_to_binary(\"$NODE_ID\"),
                            realm => list_to_binary(\"$MACULA_REALM\"),
                            mode => list_to_binary(\"$NODE_TYPE\")
                        }),
                        %% Only start demo on peer nodes (not on bootstrap)
                        case os:getenv(\"NODE_TYPE\") of
                            \"peer\" ->
                                %% Wait for connection to be established before starting demo
                                macula_console:info(list_to_binary(\"$NODE_ID\"), <<\"Waiting for bootstrap connection...\">>),
                                timer:sleep(5000),
                                %% Choose demo mode based on environment variable
                                case os:getenv(\"DEMO_MODE\") of
                                    \"ping_pong\" ->
                                        PingInterval = list_to_integer(os:getenv(\"PING_INTERVAL\", \"5000\")),
                                        PingTimeout = list_to_integer(os:getenv(\"PING_TIMEOUT\", \"3000\")),
                                        case macula_ping_pong:start_link(#{interval => PingInterval, timeout => PingTimeout}) of
                                            {ok, _Pid} ->
                                                macula_console:success(list_to_binary(\"$NODE_ID\"), <<\"Ping/Pong demo started!\">>);
                                            {error, PingErr} ->
                                                macula_console:error(list_to_binary(\"$NODE_ID\"), iolist_to_binary([<<\"Ping/Pong failed: \">>, io_lib:format(\"~p\", [PingErr])]))
                                        end;
                                    _ ->
                                        case macula_chatter:start_link(#{interval => 10000}) of
                                            {ok, _Pid} ->
                                                macula_console:success(list_to_binary(\"$NODE_ID\"), <<\"Chatter demo started!\">>);
                                            {error, ChatterErr} ->
                                                macula_console:error(list_to_binary(\"$NODE_ID\"), iolist_to_binary([<<\"Chatter failed: \">>, io_lib:format(\"~p\", [ChatterErr])]))
                                        end
                                end;
                            _ ->
                                macula_console:info(list_to_binary(\"$NODE_ID\"), <<\"Bootstrap/relay node - awaiting peer connections\">>)
                        end,
                        receive after infinity -> ok end
                end;
            {error, Reason} ->
                io:format(\"Failed to start macula: ~p~n\", [Reason]),
                init:stop(1)
        end
    "
