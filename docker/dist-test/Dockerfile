# Macula QUIC Distribution Test Node
#
# This Dockerfile builds a test application that uses macula_dist
# for QUIC-based Erlang distribution instead of EPMD + TCP.
#
# The test application:
#   - Starts with QUIC distribution enabled
#   - Automatically connects to other cluster nodes
#   - Provides health endpoints for Docker healthchecks
#   - Reports cluster status
#
# Usage:
#   docker build -t macula-dist-test .
#   docker compose up

# Fedora 40 has GLIBC 2.39 which is required by quicer NIF
FROM fedora:40 AS builder

# Install Erlang and build dependencies
RUN dnf install -y \
    erlang \
    git \
    make \
    gcc \
    gcc-c++ \
    glibc-devel \
    openssl-devel \
    openssl \
    cmake \
    kernel-devel \
    perl \
    bash \
    ninja-build \
    wget \
    ca-certificates \
    && dnf clean all

# Install rebar3
RUN wget https://s3.amazonaws.com/rebar3/rebar3 -O /usr/local/bin/rebar3 && \
    chmod +x /usr/local/bin/rebar3

WORKDIR /build

# Copy test application (simplified - no macula dependency for now)
COPY docker/dist-test/app/ ./app/

WORKDIR /build/app

# Clean any cached build artifacts from host
RUN rm -rf _build

# Compile and build release
RUN rebar3 compile
RUN rebar3 release

# --- Runtime Stage ---
# Use Fedora 40 for GLIBC 2.39 compatibility with quicer NIF
FROM fedora:40

# Install Erlang runtime and dependencies
RUN dnf install -y \
    erlang \
    openssl \
    ncurses \
    libstdc++ \
    libatomic \
    numactl-libs \
    curl \
    bash \
    nmap-ncat \
    && dnf clean all

WORKDIR /opt/app

# Copy release from builder
COPY --from=builder /build/app/_build/default/rel/macula_dist_test ./

# Copy entrypoint
COPY docker/dist-test/entrypoint.sh /opt/app/
RUN chmod +x /opt/app/entrypoint.sh

# Create directories
RUN mkdir -p /opt/app/log /opt/app/data /opt/app/certs

# Generate self-signed certificates for QUIC TLS
RUN openssl req -x509 -newkey rsa:2048 \
    -keyout /opt/app/certs/key.pem \
    -out /opt/app/certs/cert.pem \
    -days 365 -nodes \
    -subj '/CN=macula-dist-test'

# Environment variables
ENV MACULA_DIST_PORT=4433
ENV HEALTH_PORT=9100
ENV HOME=/opt/app

ENTRYPOINT ["/opt/app/entrypoint.sh"]
