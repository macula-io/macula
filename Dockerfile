# Multi-stage Dockerfile for Macula HTTP/3 mesh network
# Stage 1: Builder - compile everything from source in Alpine
FROM erlang:27-alpine AS builder

# Install build dependencies for quicer and Macula
RUN apk add --no-cache \
    git \
    make \
    gcc \
    g++ \
    cmake \
    musl-dev \
    linux-headers \
    openssl-dev \
    perl \
    bash \
    rebar3

# Create build directory
WORKDIR /build

# Copy source code
COPY src /build/src
COPY include /build/include
COPY _checkouts /build/_checkouts
COPY rebar.config /build/rebar.config
COPY rebar.lock /build/rebar.lock

# Compile Macula and all dependencies (including quicer)
# This compiles quicer against musl libc in Alpine
# Clean first to ensure fresh compilation
RUN rebar3 clean && rebar3 compile

# Stage 2: Runtime - minimal Alpine image with compiled artifacts
FROM erlang:27-alpine

# Install runtime dependencies
RUN apk add --no-cache \
    libstdc++ \
    libgcc \
    openssl

# Create application directory
WORKDIR /macula

# Copy compiled artifacts from builder stage
COPY --from=builder /build/_build /macula/_build
COPY --from=builder /build/src /macula/src
COPY --from=builder /build/include /macula/include
COPY --from=builder /build/_checkouts /macula/_checkouts
COPY --from=builder /build/rebar.config /macula/rebar.config
COPY --from=builder /build/rebar.lock /macula/rebar.lock

# Copy and run certificate generation script
COPY docker/generate-test-certs.sh /tmp/generate-test-certs.sh
RUN chmod +x /tmp/generate-test-certs.sh && /tmp/generate-test-certs.sh

# Copy entrypoint script
COPY docker/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

# Copy test scripts (pub/sub and RPC)
COPY docker/test_publisher.erl /macula/docker/test_publisher.erl
COPY docker/test_subscriber.erl /macula/docker/test_subscriber.erl
COPY docker/test_pubsub_client.erl /macula/docker/test_pubsub_client.erl
COPY docker/test_publisher_wildcard.erl /macula/docker/test_publisher_wildcard.erl
COPY docker/test_subscriber_wildcard.erl /macula/docker/test_subscriber_wildcard.erl
COPY docker/test_rpc_client.erl /macula/docker/test_rpc_client.erl
COPY docker/test_rpc_multi_hop_client.erl /macula/docker/test_rpc_multi_hop_client.erl
RUN chmod +x /macula/docker/test_publisher.erl && \
    chmod +x /macula/docker/test_subscriber.erl && \
    chmod +x /macula/docker/test_pubsub_client.erl && \
    chmod +x /macula/docker/test_publisher_wildcard.erl && \
    chmod +x /macula/docker/test_subscriber_wildcard.erl && \
    chmod +x /macula/docker/test_rpc_client.erl && \
    chmod +x /macula/docker/test_rpc_multi_hop_client.erl

# Expose common ports
EXPOSE 9000-9010

# Set entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
