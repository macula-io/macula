# Gateway TLS Certificate Implementation

**Date:** 2025-11-10
**Context:** After fixing empty supervision tree, gateway crashes with TLS error

## Problem Statement

After successfully implementing conditional supervisor startup, the gateway now starts but immediately crashes with:

```
Starting Macula Gateway on port 9443 (realm: be.cortexiq.energy)
=CRASH REPORT==== 10-Nov-2025::09:30:51.573905 ===
  crasher:
    initial call: macula_gateway:init/1
    pid: <0.520.0>
    exception exit: {listen_failed,{config_error,tls_error}}
```

**Progress:** This is actually good progress - we moved from "exits immediately with code 0" to "starts and attempts to listen on port 9443".

## Investigation Findings

### 1. OpenSSL Availability ✅

OpenSSL is installed and working in the Docker image:

```bash
$ docker run --rm --entrypoint /bin/sh macula/macula-gateway:latest -c "which openssl"
/usr/bin/openssl

$ docker run --rm --entrypoint /bin/sh macula/macula-gateway:latest -c "openssl version"
OpenSSL 3.0.17 1 Jul 2025 (Library: OpenSSL 3.0.17 1 Jul 2025)
```

**Test:** Certificate generation works manually:
```bash
$ docker run --rm --entrypoint /bin/sh macula/macula-gateway:latest -c \
  "openssl req -x509 -newkey rsa:2048 -nodes -keyout /tmp/test_key.pem \
   -out /tmp/test_cert.pem -days 365 -subj '/CN=macula.local' 2>&1 && \
   ls -la /tmp/test*.pem"

-rw-r--r-- 1 macula macula 1123 Nov 10 09:35 /tmp/test_cert.pem
-rw------- 1 macula macula 1704 Nov 10 09:35 /tmp/test_key.pem
```

✅ **Conclusion:** OpenSSL installation is not the problem.

### 2. Certificate Generation Code

**File:** `apps/macula_quic/src/macula_quic_cert.erl`

The module provides `generate_self_signed/0` which:
1. Creates a unique temp directory: `/tmp/macula_certs_PID`
2. Uses `filelib:ensure_dir/1` to create directory
3. Generates cert.pem and key.pem using openssl command
4. Returns `{ok, CertFile, KeyFile}` on success

**Code Flow:**
```erlang
%% macula_gateway.erl:103
{ok, CertFile, KeyFile} = macula_quic_cert:generate_self_signed(),

%% Passes to QUIC listener
ListenOpts = [
    {cert, CertFile},
    {key, KeyFile},
    {alpn, ["macula"]},
    {peer_unidi_stream_count, 3}
],

case macula_quic:listen(Port, ListenOpts) of
    {ok, Listener} -> ...
    {error, Reason} -> {stop, {listen_failed, Reason}};
    {error, Type, Details} -> {stop, {listen_failed, {Type, Details}}};
    Other -> {stop, {listen_failed, Other}}
end.
```

**QUIC Listen Chain:**
```
macula_gateway:init/1
  ↓
macula_quic_cert:generate_self_signed()  [generates /tmp/macula_certs_<PID>/{cert,key}.pem]
  ↓
macula_quic:listen(Port, ListenOpts)     [wraps quicer API]
  ↓
quicer:listen(Port, QuicerOpts)          [underlying QUIC library]
```

### 3. Error Pattern Analysis

The crash shows:
```erlang
exception exit: {listen_failed,{config_error,tls_error}}
```

This matches the pattern in `macula_gateway.erl:134`:
```erlang
{error, Type, Details} ->
    {stop, {listen_failed, {Type, Details}}};
```

So `quicer:listen/2` is returning `{error, config_error, tls_error}`.

### 4. Quicer Library Error

The `quicer` library (Erlang binding for Microsoft MsQuic) is returning a 3-tuple error:
- Type: `config_error`
- Details: `tls_error`

**Potential Root Causes:**
1. Certificate/key files not readable by quicer
2. Certificate format incompatible with MsQuic
3. File permissions issue (user 'macula' in container)
4. Certificate path not accessible at the time quicer reads it
5. Missing intermediate call or configuration for MsQuic TLS

### 5. File Permissions

The Docker image runs as user `macula`:
```dockerfile
RUN groupadd -r macula && useradd --no-log-init -r -g macula macula
USER macula
```

The certificates are generated in `/tmp/macula_certs_<PID>/`:
- Directory created by `filelib:ensure_dir/1` (should inherit user permissions)
- Files created by `os:cmd("openssl ...")` (runs as 'macula' user)
- Should have correct ownership

## Hypothesis

The most likely issue is that `quicer:listen/2` expects certificates in a specific format or has additional configuration requirements that aren't being met. The error `{config_error, tls_error}` is generic and doesn't tell us exactly what's wrong with the TLS configuration.

## Possible Solutions

### Option 1: Add Better Error Handling and Logging (Diagnostic)

Modify `macula_gateway.erl` to capture more detailed error information:

```erlang
init(Opts) ->
    Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),
    Realm = proplists:get_value(realm, Opts, ?DEFAULT_REALM),

    %% Generate self-signed cert for testing
    io:format("Generating TLS certificates...~n"),
    case macula_quic_cert:generate_self_signed() of
        {ok, CertFile, KeyFile} ->
            io:format("Generated certificates:~n"),
            io:format("  Cert: ~s~n", [CertFile]),
            io:format("  Key:  ~s~n", [KeyFile]),

            %% Verify files exist
            case filelib:is_file(CertFile) andalso filelib:is_file(KeyFile) of
                true ->
                    io:format("Certificate files verified~n"),
                    start_quic_listener(Port, Realm, CertFile, KeyFile);
                false ->
                    {stop, {cert_files_missing, #{cert => CertFile, key => KeyFile}}}
            end;
        {error, Reason} ->
            {stop, {cert_generation_failed, Reason}}
    end.
```

### Option 2: Pre-generate Certificates During Build

Generate certificates at Docker build time and include them in the image:

**Dockerfile.gateway:**
```dockerfile
# Generate self-signed certificate at build time
RUN mkdir -p /opt/macula/certs && \
    openssl req -x509 -newkey rsa:2048 -nodes \
    -keyout /opt/macula/certs/key.pem \
    -out /opt/macula/certs/cert.pem \
    -days 365 -subj '/CN=macula.local' && \
    chown -R macula:macula /opt/macula/certs
```

**gateway.erl:**
```erlang
%% Use pre-generated certificates
CertFile = "/opt/macula/certs/cert.pem",
KeyFile = "/opt/macula/certs/key.pem",
```

**Pros:**
- Certificates available before Erlang starts
- No runtime dependency on openssl command
- Simpler initialization
- Works for testing/development

**Cons:**
- Same certificate for all instances (not ideal for production)
- Certificate baked into image (security concern)
- Can't customize subject/validity at runtime

### Option 3: Use Erlang's Built-in Crypto

Generate certificates using Erlang's `public_key` module instead of shelling out to openssl:

```erlang
generate_self_signed_native() ->
    %% Generate RSA key pair
    {ok, PrivateKey} = public_key:generate_key({rsa, 2048, 65537}),

    %% Create certificate
    Subject = {rdnSequence, [
        [{'AttributeTypeAndValue', ?'id-at-commonName', {utf8String, "macula.local"}}]
    ]},

    Cert = public_key:pkix_sign(
        #'OTPTBSCertificate'{
            version = v3,
            serialNumber = crypto:rand_uniform(1, 1000000000),
            signature = #'SignatureAlgorithm'{algorithm = ?'sha256WithRSAEncryption'},
            issuer = Subject,
            validity = #'Validity'{
                notBefore = {utcTime, "250110000000Z"},
                notAfter = {utcTime, "260110000000Z"}
            },
            subject = Subject,
            subjectPublicKeyInfo = public_key:pem_entry_encode(...),
            extensions = []
        },
        PrivateKey
    ),

    %% Write to files
    ...
```

**Pros:**
- Pure Erlang, no external dependencies
- More control over certificate generation
- Programmatic access to all certificate fields

**Cons:**
- More complex code
- Still need to write PEM files for quicer
- Certificate API is verbose

### Option 4: Mount Certificates via Kubernetes Secrets

For production, mount real certificates from Kubernetes secrets:

**Kubernetes manifest:**
```yaml
volumes:
  - name: tls-certs
    secret:
      secretName: macula-gateway-tls
volumeMounts:
  - name: tls-certs
    mountPath: /etc/macula/certs
    readOnly: true
env:
  - name: TLS_CERT_FILE
    value: /etc/macula/certs/tls.crt
  - name: TLS_KEY_FILE
    value: /etc/macula/certs/tls.key
```

**gateway.erl:**
```erlang
CertFile = os:getenv("TLS_CERT_FILE", "/opt/macula/certs/cert.pem"),
KeyFile = os:getenv("TLS_KEY_FILE", "/opt/macula/certs/key.pem"),
```

**Pros:**
- Production-ready approach
- Real certificates from trusted CA
- Secrets managed by Kubernetes
- Can use cert-manager for automation

**Cons:**
- Requires Kubernetes infrastructure
- More setup complexity
- Still need fallback for local testing

## Recommended Approach

**For immediate diagnosis:** Option 1 (Better logging)
- Add detailed logging to see exactly what's happening
- Verify certificate files are created and readable
- Capture actual error from quicer library

**For short-term fix:** Option 2 (Pre-generate at build)
- Quick fix to unblock development
- Good enough for PoC/testing
- Can iterate to better solution later

**For production:** Option 4 (Kubernetes secrets)
- Use real certificates in production
- Keep development simple with pre-generated certs
- Implement proper secret management

**Hybrid Approach (Recommended):**
```erlang
init(Opts) ->
    %% Check for mounted certificates first (production)
    CertFile = os:getenv("TLS_CERT_FILE"),
    KeyFile = os:getenv("TLS_KEY_FILE"),

    {Cert, Key} = case {CertFile, KeyFile} of
        {false, false} ->
            %% No env vars, use pre-generated certs
            io:format("Using pre-generated certificates~n"),
            {"/opt/macula/certs/cert.pem", "/opt/macula/certs/key.pem"};
        {C, K} when C =/= false andalso K =/= false ->
            %% Use mounted certificates
            io:format("Using mounted certificates~n"),
            {C, K}
    end,

    %% Verify files exist
    case macula_quic_cert:validate_files(Cert, Key) of
        ok -> start_listener(Port, Realm, Cert, Key);
        {error, Reason} -> {stop, {cert_validation_failed, Reason}}
    end.
```

## Next Steps

1. Implement Option 1 (better logging) to diagnose exact error
2. Implement Option 2 (pre-generate certs) as immediate fix
3. Test gateway startup with pre-generated certificates
4. Plan Option 4 (Kubernetes secrets) for production deployment

## Related Files

- `/home/rl/work/github.com/macula-io/macula/apps/macula_gateway/src/macula_gateway.erl:98-139` - Gateway init with TLS
- `/home/rl/work/github.com/macula-io/macula/apps/macula_quic/src/macula_quic.erl:31-48` - QUIC listen wrapper
- `/home/rl/work/github.com/macula-io/macula/apps/macula_quic/src/macula_quic_cert.erl` - Certificate generation utilities
- `/home/rl/work/github.com/macula-io/macula/Dockerfile.gateway` - Docker build configuration

## Security Considerations

### Development/Testing
- Self-signed certificates are acceptable
- Can use same cert for all instances
- Focus on functionality over security

### Production
- MUST use certificates from trusted CA
- Each instance should have unique certificates
- Consider using cert-manager in Kubernetes
- Implement certificate rotation
- Store private keys in Kubernetes secrets (not ConfigMaps)
- Set appropriate RBAC for secret access

### Certificate Validity
- Development: 365 days is fine
- Production: Follow organizational PKI policy
- Implement monitoring for expiring certificates
- Automate renewal (cert-manager)

## References

- quicer library: https://github.com/qzhuyan/quicer
- Microsoft MsQuic: https://github.com/microsoft/msquic
- Erlang public_key: https://www.erlang.org/doc/man/public_key.html
- OpenSSL req command: https://www.openssl.org/docs/man3.0/man1/openssl-req.html
- Kubernetes TLS secrets: https://kubernetes.io/docs/concepts/configuration/secret/#tls-secrets
