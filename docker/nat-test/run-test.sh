#!/bin/sh
# NAT Traversal Test Runner

NODE_TYPE=${NODE_TYPE:-peer}
NODE_ID=${NODE_ID:-test-node}
QUIC_PORT=${QUIC_PORT:-4433}
HTTP_PORT=${HTTP_PORT:-8080}
BOOTSTRAP_HOST=${BOOTSTRAP_HOST:-}
BOOTSTRAP_PORT=${BOOTSTRAP_PORT:-4433}
RELAY_ENABLED=${RELAY_ENABLED:-false}

echo "Starting NAT test node..."
echo "  Node Type: $NODE_TYPE"
echo "  Node ID: $NODE_ID"
echo "  QUIC Port: $QUIC_PORT"
echo "  HTTP Port: $HTTP_PORT"
echo "  Bootstrap: $BOOTSTRAP_HOST:$BOOTSTRAP_PORT"
echo "  Relay Enabled: $RELAY_ENABLED"

# Wait for network to be ready
sleep 2

# Start the Erlang shell with NAT test module
exec erl -pa _build/default/lib/*/ebin \
    -noshell \
    -name "${NODE_ID}@$(hostname -i)" \
    -setcookie nat_test_cookie \
    -eval "
        application:ensure_all_started(macula),

        %% Start NAT system
        macula_nat_system:start_link(#{}),

        %% Detect local NAT
        case macula_nat_detector:detect() of
            {ok, Profile} ->
                io:format(\"NAT Profile: ~p~n\", [Profile]);
            {error, Reason} ->
                io:format(\"NAT Detection failed: ~p~n\", [Reason])
        end,

        %% Enable relay if configured
        case os:getenv(\"RELAY_ENABLED\") of
            \"true\" ->
                macula_relay_node:enable(#{
                    node_id => <<\"$NODE_ID\">>,
                    endpoint => {<<\"0.0.0.0\">>, $QUIC_PORT}
                }),
                io:format(\"Relay enabled~n\", []);
            _ ->
                ok
        end,

        %% Connect to bootstrap if specified
        case os:getenv(\"BOOTSTRAP_HOST\") of
            false -> ok;
            \"\" -> ok;
            Host ->
                Port = list_to_integer(os:getenv(\"BOOTSTRAP_PORT\", \"4433\")),
                io:format(\"Connecting to bootstrap ~s:~p~n\", [Host, Port])
        end,

        io:format(\"Node ready: $NODE_ID~n\", []),

        %% Keep running
        receive stop -> ok end
    "
