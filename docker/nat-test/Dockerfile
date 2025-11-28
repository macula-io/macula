# NAT Traversal Test Node
FROM erlang:26-alpine

WORKDIR /app

# Install build dependencies
RUN apk add --no-cache \
    git \
    make \
    gcc \
    g++ \
    libc-dev \
    openssl-dev \
    cmake \
    linux-headers \
    curl

# Copy source code
COPY . /app/

# Compile
RUN rebar3 compile

# Copy test runner script
COPY docker/nat-test/run-test.sh /app/run-test.sh
RUN chmod +x /app/run-test.sh

EXPOSE 4433/udp
EXPOSE 8080

CMD ["/app/run-test.sh"]
