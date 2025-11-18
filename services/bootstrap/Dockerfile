# Multi-stage build for Macula Bootstrap Service
# Stage 1: Build
FROM erlang:27-alpine AS builder

# Install build dependencies
RUN apk add --no-cache \
    git \
    bash \
    make \
    cmake \
    g++ \
    perl \
    openssl-dev \
    linux-headers

WORKDIR /build

# Copy entire macula project
COPY . /build/

# Compile macula and all dependencies
RUN rebar3 compile

# Build bootstrap release by creating it at the root level
# Copy bootstrap config files to a temp location
RUN mkdir -p /tmp/bootstrap && \
    cp -r services/bootstrap/config /tmp/bootstrap/

# Create a standalone rebar.config for the release
RUN cd /tmp/bootstrap && \
    cat > rebar.config <<'EOF'
{relx, [
    {release, {macula_bootstrap, "0.1.0"}, [
        macula,
        sasl,
        runtime_tools
    ]},
    {mode, prod},
    {include_erts, true},
    {extended_start_script, true},
    {sys_config, "./config/sys.config"},
    {vm_args, "./config/vm.args"},
    {overlay, [
        {mkdir, "log"},
        {copy, "/build/README.md", "README.md"}
    ]}
]}.

{profiles, [
    {prod, [
        {erl_opts, [no_debug_info, deterministic]},
        {relx, [
            {mode, prod},
            {dev_mode, false},
            {include_src, false}
        ]}
    ]}
]}.
EOF

# Set ERL_LIBS to find the compiled macula
ENV ERL_LIBS=/build/_build/default/lib

# Build the release
RUN cd /tmp/bootstrap && rebar3 as prod release

# Stage 2: Runtime
FROM erlang:27-alpine

# Install runtime dependencies (use same base as builder for OpenSSL compatibility)
RUN apk add --no-cache \
    libstdc++ \
    ncurses-libs \
    openssl

WORKDIR /opt/macula

# Copy release from builder
COPY --from=builder /tmp/bootstrap/_build/prod/rel/macula_bootstrap ./

# Create directories
RUN mkdir -p log certs

# Run as non-root user
RUN addgroup -g 1000 macula && \
    adduser -D -u 1000 -G macula macula && \
    chown -R macula:macula /opt/macula

USER macula

# Expose QUIC/HTTP3 port
EXPOSE 4433/udp

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
    CMD /opt/macula/bin/macula_bootstrap ping || exit 1

# Start
CMD ["/opt/macula/bin/macula_bootstrap", "foreground"]
