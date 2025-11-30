# NAT Traversal Test Node
# Using Erlang 27 for built-in json module support
FROM erlang:27-alpine

WORKDIR /app

# Install build dependencies (including tools for quicer native compilation)
RUN apk add --no-cache \
    git \
    make \
    gcc \
    g++ \
    musl-dev \
    openssl \
    openssl-dev \
    cmake \
    curl \
    bash \
    linux-headers \
    perl

# Copy rebar files first (for caching)
COPY rebar.config rebar.lock ./

# Fetch dependencies (separate layer for better caching)
RUN rebar3 get-deps

# Copy source code
COPY src ./src
COPY include ./include
COPY priv ./priv
COPY config ./config
COPY _checkouts ./_checkouts

# Clean and compile application (ensure fresh build)
RUN rebar3 clean && rebar3 compile

# Generate self-signed TLS certificates for testing
RUN mkdir -p /opt/macula/certs && \
    openssl req -x509 -newkey rsa:2048 -nodes \
        -keyout /opt/macula/certs/key.pem \
        -out /opt/macula/certs/cert.pem \
        -days 365 \
        -subj "/CN=macula-test/O=Macula/C=US"

# Copy test runner script
COPY docker/nat-test/run-test.sh /app/run-test.sh
RUN chmod +x /app/run-test.sh

EXPOSE 4433/udp
EXPOSE 8080

CMD ["/app/run-test.sh"]
