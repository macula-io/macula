#!/bin/bash
# Multi-node DHT test using Docker Compose
# Creates 3 containerized Macula nodes with HTTP/3 networking

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=== Multi-Node DHT Test (Docker) ==="
echo ""

# Create Dockerfile for Macula node
cat > "$PROJECT_ROOT/Dockerfile.test" <<'EOF'
FROM erlang:26-alpine

# Install build dependencies
RUN apk add --no-cache git rebar3 make gcc musl-dev openssl-dev

# Set working directory
WORKDIR /app

# Copy project files
COPY . .

# Compile
RUN rebar3 compile

# Expose QUIC port (HTTP/3)
EXPOSE 9443/udp

CMD ["rebar3", "shell"]
EOF

# Create docker-compose.yml
cat > "$PROJECT_ROOT/docker-compose.test.yml" <<'EOF'
version: '3.8'

services:
  macula-node1:
    build:
      context: .
      dockerfile: Dockerfile.test
    container_name: macula_node1
    hostname: macula-node1
    networks:
      macula_mesh:
        ipv4_address: 172.20.0.11
    ports:
      - "9443:9443/udp"
    environment:
      - ERLANG_COOKIE=macula_test_cookie
      - NODE_NAME=macula_node1@macula-node1
      - MACULA_REALM=com.test.docker
    command: >
      sh -c "
      erl -pa _build/default/lib/*/ebin \
          -name macula_node1@macula-node1 \
          -setcookie macula_test_cookie \
          -noshell \
          -eval '
            application:ensure_all_started(macula),
            io:format(\"Node 1 (Provider) started: ~p~n\", [node()]),

            {ok, Client} = macula_client:connect(<<\"https://0.0.0.0:9443\">>, #{
              realm => <<\"com.test.docker\">>,
              node_id => <<\"docker-node-1\">>
            }),

            Handler = fun(Args) ->
              io:format(\"[Node 1] Received call: ~p~n\", [Args]),
              #{a := A, b := B} = Args,
              {ok, #{result => A + B, node => <<\"node-1\">>}}
            end,

            {ok, _} = macula_client:advertise(
              Client,
              <<\"math.add\">>,
              Handler,
              #{metadata => #{node => <<\"docker-node-1\">>}, ttl => 60}
            ),

            io:format(\"Service math.add advertised on node 1~n\"),
            receive stop -> ok end
          '
      "

  macula-node2:
    build:
      context: .
      dockerfile: Dockerfile.test
    container_name: macula_node2
    hostname: macula-node2
    networks:
      macula_mesh:
        ipv4_address: 172.20.0.12
    environment:
      - ERLANG_COOKIE=macula_test_cookie
      - NODE_NAME=macula_node2@macula-node2
      - MACULA_REALM=com.test.docker
    depends_on:
      - macula-node1
    command: >
      sh -c "
      sleep 5 &&
      erl -pa _build/default/lib/*/ebin \
          -name macula_node2@macula-node2 \
          -setcookie macula_test_cookie \
          -noshell \
          -eval '
            application:ensure_all_started(macula),
            io:format(\"Node 2 (Consumer) started: ~p~n\", [node()]),

            {ok, Client} = macula_client:connect(<<\"https://0.0.0.0:9443\">>, #{
              realm => <<\"com.test.docker\">>,
              node_id => <<\"docker-node-2\">>
            }),

            timer:sleep(3000),

            io:format(\"Calling service math.add via DHT discovery...~n\"),
            case macula_client:call(Client, <<\"math.add\">>, #{a => 42, b => 8}) of
              {ok, Result} ->
                io:format(\"SUCCESS! Result: ~p~n\", [Result]);
              {error, Reason} ->
                io:format(\"ERROR: ~p~n\", [Reason])
            end,

            receive stop -> ok end
          '
      "

  macula-node3:
    build:
      context: .
      dockerfile: Dockerfile.test
    container_name: macula_node3
    hostname: macula-node3
    networks:
      macula_mesh:
        ipv4_address: 172.20.0.13
    environment:
      - ERLANG_COOKIE=macula_test_cookie
      - NODE_NAME=macula_node3@macula-node3
      - MACULA_REALM=com.test.docker
    depends_on:
      - macula-node1
    command: >
      sh -c "
      sleep 5 &&
      erl -pa _build/default/lib/*/ebin \
          -name macula_node3@macula-node3 \
          -setcookie macula_test_cookie \
          -noshell \
          -eval '
            application:ensure_all_started(macula),
            io:format(\"Node 3 (Second Provider) started: ~p~n\", [node()]),

            {ok, Client} = macula_client:connect(<<\"https://0.0.0.0:9443\">>, #{
              realm => <<\"com.test.docker\">>,
              node_id => <<\"docker-node-3\">>
            }),

            Handler = fun(Args) ->
              io:format(\"[Node 3] Received call: ~p~n\", [Args]),
              #{a := A, b := B} = Args,
              {ok, #{result => A + B, node => <<\"node-3\">>}}
            end,

            {ok, _} = macula_client:advertise(
              Client,
              <<\"math.add\">>,
              Handler,
              #{metadata => #{node => <<\"docker-node-3\">>}, ttl => 60}
            ),

            io:format(\"Service math.add advertised on node 3~n\"),
            receive stop -> ok end
          '
      "

networks:
  macula_mesh:
    driver: bridge
    ipam:
      config:
        - subnet: 172.20.0.0/16
EOF

echo "Docker Compose configuration created"
echo ""
echo "Starting containers..."
docker-compose -f "$PROJECT_ROOT/docker-compose.test.yml" up --build

# Cleanup on exit
echo ""
echo "Cleaning up..."
docker-compose -f "$PROJECT_ROOT/docker-compose.test.yml" down
rm -f "$PROJECT_ROOT/Dockerfile.test" "$PROJECT_ROOT/docker-compose.test.yml"
