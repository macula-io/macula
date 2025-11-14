#!/bin/bash
# Multi-node DHT service discovery test
# Starts 3 Erlang nodes with Macula and tests service advertisement

set -e

echo "=== Multi-Node DHT Service Discovery Test ==="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Cleanup function
cleanup() {
    echo ""
    echo "${YELLOW}Cleaning up...${NC}"
    pkill -f "macula_node" || true
    rm -f /tmp/macula_node*.log
    echo "${GREEN}Cleanup complete${NC}"
}

trap cleanup EXIT

# Ensure code is compiled
echo "${YELLOW}Compiling Macula...${NC}"
rebar3 compile
echo ""

# Start node 1 (provider)
echo "${GREEN}Starting Node 1 (Service Provider)...${NC}"
erl -pa _build/default/lib/*/ebin \
    -name macula_node1@127.0.0.1 \
    -setcookie macula_test \
    -noshell \
    -eval "
        application:ensure_all_started(macula),
        io:format(\"Node 1 started: ~p~n\", [node()]),

        %% Connect to mesh
        {ok, Client1} = macula_client:connect(<<\"https://localhost:9443\">>, #{
            realm => <<\"com.test.multinode\">>,
            node_id => <<\"node-1-provider\">>
        }),
        io:format(\"Node 1 connected~n\"),

        %% Advertise a service
        Handler = fun(Args) ->
            io:format(\"[Node 1 Handler] Received call with args: ~p~n\", [Args]),
            case Args of
                #{operation := <<\"add\">>, a := A, b := B} ->
                    Result = A + B,
                    {ok, #{result => Result, node => <<\"node-1\">>}};
                _ ->
                    {error, invalid_args}
            end
        end,

        {ok, _Ref} = macula_client:advertise(
            Client1,
            <<\"test.calculator\">>,
            Handler,
            #{metadata => #{version => <<\"1.0\">>, node => <<\"node-1\">>}, ttl => 60}
        ),
        io:format(\"Node 1 advertised service 'test.calculator'~n\"),

        %% Keep node alive
        receive stop -> ok end
    " \
    > /tmp/macula_node1.log 2>&1 &
NODE1_PID=$!

sleep 3

# Start node 2 (consumer)
echo "${GREEN}Starting Node 2 (Service Consumer)...${NC}"
erl -pa _build/default/lib/*/ebin \
    -name macula_node2@127.0.0.1 \
    -setcookie macula_test \
    -noshell \
    -eval "
        application:ensure_all_started(macula),
        io:format(\"Node 2 started: ~p~n\", [node()]),

        %% Connect to mesh
        {ok, Client2} = macula_client:connect(<<\"https://localhost:9443\">>, #{
            realm => <<\"com.test.multinode\">>,
            node_id => <<\"node-2-consumer\">>
        }),
        io:format(\"Node 2 connected~n\"),

        %% Wait a bit for DHT to propagate
        timer:sleep(2000),

        %% Try to call the service (should discover via DHT)
        io:format(\"Node 2 calling service 'test.calculator' (discovering via DHT)...~n\"),
        case macula_client:call(Client2, <<\"test.calculator\">>, #{operation => <<\"add\">>, a => 10, b => 5}) of
            {ok, Result} ->
                io:format(\"Node 2 SUCCESS! Received: ~p~n\", [Result]);
            {error, Reason} ->
                io:format(\"Node 2 ERROR: ~p~n\", [Reason])
        end,

        %% Keep node alive
        receive stop -> ok end
    " \
    > /tmp/macula_node2.log 2>&1 &
NODE2_PID=$!

sleep 3

# Start node 3 (second provider)
echo "${GREEN}Starting Node 3 (Second Service Provider)...${NC}"
erl -pa _build/default/lib/*/ebin \
    -name macula_node3@127.0.0.1 \
    -setcookie macula_test \
    -noshell \
    -eval "
        application:ensure_all_started(macula),
        io:format(\"Node 3 started: ~p~n\", [node()]),

        %% Connect to mesh
        {ok, Client3} = macula_client:connect(<<\"https://localhost:9443\">>, #{
            realm => <<\"com.test.multinode\">>,
            node_id => <<\"node-3-provider\">>
        }),
        io:format(\"Node 3 connected~n\"),

        %% Advertise the SAME service (multi-provider scenario)
        Handler = fun(Args) ->
            io:format(\"[Node 3 Handler] Received call with args: ~p~n\", [Args]),
            case Args of
                #{operation := <<\"add\">>, a := A, b := B} ->
                    Result = A + B,
                    {ok, #{result => Result, node => <<\"node-3\">>}};
                _ ->
                    {error, invalid_args}
            end
        end,

        {ok, _Ref} = macula_client:advertise(
            Client3,
            <<\"test.calculator\">>,
            Handler,
            #{metadata => #{version => <<\"1.0\">>, node => <<\"node-3\">>}, ttl => 60}
        ),
        io:format(\"Node 3 advertised service 'test.calculator'~n\"),

        %% Keep node alive
        receive stop -> ok end
    " \
    > /tmp/macula_node3.log 2>&1 &
NODE3_PID=$!

echo ""
echo "${GREEN}All nodes started!${NC}"
echo ""
echo "Node 1 PID: $NODE1_PID (Provider)"
echo "Node 2 PID: $NODE2_PID (Consumer)"
echo "Node 3 PID: $NODE3_PID (Second Provider)"
echo ""
echo "${YELLOW}Logs:${NC}"
echo "  Node 1: /tmp/macula_node1.log"
echo "  Node 2: /tmp/macula_node2.log"
echo "  Node 3: /tmp/macula_node3.log"
echo ""
echo "${YELLOW}Waiting 5 seconds for nodes to interact...${NC}"
sleep 5

echo ""
echo "${GREEN}=== Node 1 Log (Provider) ===${NC}"
cat /tmp/macula_node1.log
echo ""

echo "${GREEN}=== Node 2 Log (Consumer) ===${NC}"
cat /tmp/macula_node2.log
echo ""

echo "${GREEN}=== Node 3 Log (Second Provider) ===${NC}"
cat /tmp/macula_node3.log
echo ""

echo "${YELLOW}Test complete. Nodes will be cleaned up on exit.${NC}"
echo "Press Ctrl+C to stop all nodes and exit."

# Wait for user interrupt
wait
