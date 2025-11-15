# Gateway Upgrade to OTP 27 - Change Summary

**Date:** 2025-11-10
**Scope:** Macula Gateway Docker image improvements

## Overview

Upgraded the Macula gateway from OTP 26 to OTP 27, removed external JSON dependency, added better startup diagnostics, and optimized for headless deployment.

## Changes Made

### 1. OTP Version Upgrade (26 → 27)

**File:** `Dockerfile.gateway`

```diff
- FROM erlang:26 AS builder
+ FROM erlang:27 AS builder

- FROM erlang:26-slim
+ FROM erlang:27-slim
```

**Benefits:**
- Access to built-in `json` module (introduced in OTP 27)
- Performance improvements and bug fixes from OTP 27
- Better future compatibility

**Why OTP 27 instead of OTP 28?**
- OTP 27 is more mature (released ~May 2024)
- Better ecosystem support and stability
- OTP 28 (released May 2025) is newer and less battle-tested
- Can upgrade to OTP 28 in Q1-Q2 2026 once it's more mature

### 2. Removed jiffy Dependency

**Files Modified:**
- `rebar.config` - Removed `{jiffy, "1.1.2"}` from deps
- `apps/macula/src/macula.app.src` - Removed `jiffy` from applications
- `apps/macula_client/src/macula_client.app.src` - Removed `jiffy` from applications
- `apps/macula_client/src/macula_client_client.erl` - Updated JSON functions
- `apps/macula_client/test/macula_client_test_server.erl` - Updated JSON functions

**Changes:**
```erlang
%% Before (using jiffy)
encode_json(Data) ->
    jiffy:encode(Data).

decode_json(Binary) ->
    jiffy:decode(Binary, [return_maps]).

%% After (using built-in json module from OTP 27)
encode_json(Data) ->
    json:encode(Data).

decode_json(Binary) ->
    json:decode(Binary).
```

**Benefits:**
- One less external dependency
- Native OTP performance
- Better long-term maintainability
- No NIF compilation issues

### 3. Created Entrypoint Script

**File Created:** `entrypoint.sh`

**Features:**
- Environment variable validation and logging
- Startup diagnostics (OTP version, configuration)
- Clear error messages
- Proper signal handling
- Shows:
  - Node name
  - Gateway port
  - Realm
  - Release path
  - OTP version

**File:** `Dockerfile.gateway` (updated to use entrypoint)

```dockerfile
# Copy entrypoint script
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh && \
    chown -R macula:macula /opt/macula

# Use entrypoint script for better startup control
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
```

**Benefits:**
- Better debugging capabilities
- Clear visibility into startup configuration
- Easier troubleshooting
- Professional production-ready startup

### 4. Removed Observer Application

**File:** `rebar.config`

```diff
  {release, {macula, "0.1.0"}, [
      macula,
      ...
      sasl,
-     runtime_tools,
-     observer
+     runtime_tools
+     %% observer - removed for headless deployments (requires wx/OpenGL)
  ]},
```

**Reason:**
- Observer requires wx (wxWidgets GUI library)
- wx requires OpenGL libraries (libGLU.so)
- These are not needed for headless gateway deployment
- Reduces image size and eliminates unnecessary runtime dependencies

**Benefits:**
- No GUI library crashes in headless environments
- Smaller Docker image
- Fewer security surface areas
- Cleaner dependency tree

## Image Details

**Image Tags:**
- `macula/macula-gateway:latest`
- `macula/macula-gateway:otp27`

**Base Images:**
- Builder: `erlang:27`
- Runtime: `erlang:27-slim`

**ERTS Version:** 15.2.7.2 (OTP 27)

**Image Size:** ~431MB (was ~387MB with OTP 26, slight increase due to newer ERTS)

## Testing

### Local Testing
```bash
# Test the container starts correctly
docker run --rm macula/macula-gateway:latest

# Expected output:
# ==================================================
# Starting Macula Gateway
# ==================================================
# Node Name:    macula@127.0.0.1
# Gateway Port: 9443
# Realm:        be.cortexiq.energy
# Release Path: /opt/macula
# ==================================================
# Erlang/OTP version:
# "27"
# ==================================================
```

### Verify Image
```bash
docker images | grep macula-gateway
docker inspect macula/macula-gateway:latest | grep OTP
```

## Deployment

### Push to Registry
```bash
# Tag for local registry
docker tag macula/macula-gateway:latest registry.macula.local:5000/macula/macula-gateway:latest

# Push to registry
docker push registry.macula.local:5000/macula/macula-gateway:latest
```

### Deploy via GitOps
1. Ensure image is in registry
2. Update manifests in `cortex-iq-deploy` if needed
3. Commit and push to trigger Flux reconciliation
4. Monitor pods: `kubectl --context kind-macula-hub get pods -n cortex-iq -w`
5. Check logs: `kubectl --context kind-macula-hub logs -n cortex-iq -l app=macula-gateway`

## Package Publishing

**Question:** Do we need to publish new Erlang packages for the OTP version bump?

**Answer:** **No**, not immediately.

**Reasoning:**
- Macula is currently a monorepo/umbrella application (not published to Hex.pm)
- All apps are at version 0.3.4 and are internal
- External dependencies (`quicer`, `msgpack`, `gproc`, `envy`) work with OTP 27
- If you publish `macula_client` as a standalone library later:
  - Bump version to 0.3.5 or 0.4.0
  - Add `{minimum_otp_vsn, "27.0"}` to rebar.config
  - Document OTP 27 requirement
  - Publish to Hex.pm

## Rollback Plan

If issues arise with OTP 27:

```bash
# Revert Dockerfile.gateway to OTP 26
sed -i 's/erlang:27/erlang:26/g' Dockerfile.gateway

# Restore jiffy dependency
# Revert changes to rebar.config and source files
git checkout HEAD~1 -- rebar.config apps/macula*/src/*.erl

# Rebuild with OTP 26
docker build -f Dockerfile.gateway -t macula/macula-gateway:otp26 .
```

## References

- Erlang OTP 27 Release: https://www.erlang.org/news/171
- Built-in JSON module docs: https://www.erlang.org/doc/man/json.html
- OTP 28 status: Released May 2025, currently at 28.1.1

## Notes

- The `entrypoint.sh` script provides much better diagnostics for production
- Removing `observer` is standard practice for headless deployments
- The built-in `json` module has the same API surface as jiffy for our use cases
- Image size increased slightly (~44MB) due to newer ERTS, but worth it for improvements

## Deployment Status

### Completed ✅

1. ✅ **OTP 27 Upgrade** - Successfully upgraded from OTP 26 to OTP 27
2. ✅ **Dependency Modernization** - Removed jiffy, using built-in json module
3. ✅ **Supervisor Fix** - Fixed empty supervision tree with conditional gateway startup
4. ✅ **TLS Certificate Implementation** - Pre-generated certificates at build time
5. ✅ **Local Testing** - Gateway starts, listens on port 9443, stays running
6. ✅ **Image Build** - Docker image built with all fixes (sha256:02b768...)
7. ✅ **Registry Push** - Image pushed to registry.macula.local:5000
8. ✅ **KinD Deployment** - Image loaded to KinD cluster
9. ✅ **Gateway Running** - Pod logs show: "Macula Gateway listening on port 9443"

### Current Issue ⚠️

**Health Probes Failing** - Kubernetes TCP health probes cannot connect to port 9443.

**Root Cause:** QUIC protocol uses UDP, not TCP. The gateway is listening on UDP port 9443 for QUIC connections, but Kubernetes `tcpSocket` probes try to connect via TCP.

**Logs confirm gateway is healthy:**
```
Starting Macula Gateway on port 9443 (realm: be.cortexiq.energy)
Using pre-generated TLS certificates
Using TLS certificates:
  Cert: /opt/macula/certs/cert.pem
  Key:  /opt/macula/certs/key.pem
Macula Gateway listening on port 9443 (realm: be.cortexiq.energy)
```

**Solution Options:**

1. **Remove health probes temporarily** (for PoC/development):
   ```yaml
   # Comment out or remove livenessProbe and readinessProbe
   ```

2. **Implement HTTP health endpoint** (recommended for production):
   - Add HTTP server on port 8080 for health checks
   - Keep QUIC on 9443 for actual traffic
   - Change probes to `httpGet: {path: /health, port: 8080}`

3. **Use exec probe** (workaround):
   ```yaml
   livenessProbe:
     exec:
       command: ["/bin/sh", "-c", "test -f /tmp/gateway-ready"]
     initialDelaySeconds: 30
   ```

**Recommendation:** Implement option #2 (HTTP health endpoint) for production readiness.

## Next Steps

1. ⏳ Implement HTTP health endpoint on separate port
2. ⏳ Update Kubernetes deployment with HTTP health probes
3. ⏳ Verify gateway accepts QUIC connections
4. ⏳ Update documentation with final deployment

## Supervisor Fix (2025-11-10)

### Issue Discovered

After deploying OTP 27 image, gateway pods were crashing with exit code 0:
- Application started successfully
- But immediately exited
- Port 9443 never opened
- Kubernetes probes failed

**Root Cause:** `macula_sup.erl` had an empty child list (`ChildSpecs = []`), so no processes were started.

### Solution Implemented

Modified `apps/macula/src/macula_sup.erl` to conditionally start the gateway:

```erlang
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    },
    %% Conditionally start gateway based on configuration
    ChildSpecs = maybe_start_gateway(),
    {ok, {SupFlags, ChildSpecs}}.

maybe_start_gateway() ->
    case application:get_env(macula, start_gateway, true) of
        true ->
            Port = get_gateway_port(),  %% Reads GATEWAY_PORT env var
            Realm = get_gateway_realm(), %% Reads GATEWAY_REALM env var
            [#{
                id => macula_gateway,
                start => {macula_gateway, start_link, [[{port, Port}, {realm, Realm}]]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [macula_gateway]
            }];
        false ->
            []  %% For embedded use
    end.
```

### Configuration Added

Updated `config/sys.config`:

```erlang
[
  {macula, [
    {start_gateway, true},
    {gateway_port, 9443},
    {gateway_realm, <<"be.cortexiq.energy">>}
  ]}
].
```

### Test Results

```bash
$ timeout 15 docker run --rm macula/macula-gateway:latest

==================================================
Starting Macula Gateway
==================================================
Node Name:    macula@127.0.0.1
Gateway Port: 9443
Realm:        be.cortexiq.energy
Release Path: /opt/macula
==================================================
Erlang/OTP version:
"27"
==================================================
Starting Macula Gateway on port 9443 (realm: be.cortexiq.energy)
=CRASH REPORT==== 10-Nov-2025::09:30:51.573905 ===
  crasher:
    initial call: macula_gateway:init/1
    pid: <0.520.0>
    exception exit: {listen_failed,{config_error,tls_error}}
```

**Analysis:**
- ✅ Supervisor starts the gateway process
- ✅ Gateway attempts to listen on port 9443
- ❌ TLS certificate generation fails
- **This is progress!** We moved from "exits immediately" to "starts but needs TLS"

### Backward Compatibility

The fix maintains full backward compatibility:
- **Standalone mode** (default): Gateway starts automatically
- **Embedded mode**: Set `{start_gateway, false}` to prevent auto-start
- **Environment variables**: Supervisor reads `GATEWAY_PORT` and `GATEWAY_REALM` from environment
- **Config fallback**: Uses `sys.config` values if env vars not set

### TLS Certificate Fix (2025-11-10)

**Issue:** Gateway crashed with `{config_error,tls_error}` when trying to generate TLS certificates at runtime.

**Root Cause:** Runtime certificate generation with `os:cmd("openssl...")` was failing during `quicer:listen/2`.

**Solution:** Pre-generate TLS certificates during Docker build.

**Changes Made:**

1. **Dockerfile.gateway**: Added certificate generation during build
   ```dockerfile
   RUN mkdir -p /opt/macula/certs && \
       openssl req -x509 -newkey rsa:2048 -nodes \
       -keyout /opt/macula/certs/key.pem \
       -out /opt/macula/certs/cert.pem \
       -days 365 -subj '/CN=macula.local'
   ```

2. **macula_gateway.erl**: Updated to use pre-generated certs with fallback to environment variables
   ```erlang
   get_tls_certificates() ->
       case {os:getenv("TLS_CERT_FILE"), os:getenv("TLS_KEY_FILE")} of
           {false, false} ->
               %% Use pre-generated certs (development)
               {"/opt/macula/certs/cert.pem", "/opt/macula/certs/key.pem"};
           {CertEnv, KeyEnv} when CertEnv =/= false andalso KeyEnv =/= false ->
               %% Use mounted certificates (production)
               {CertEnv, KeyEnv}
       end.
   ```

**Test Results:**
```bash
$ timeout 15 docker run --rm macula/macula-gateway:latest

==================================================
Starting Macula Gateway
==================================================
Node Name:    macula@127.0.0.1
Gateway Port: 9443
Realm:        be.cortexiq.energy
Release Path: /opt/macula
==================================================
Erlang/OTP version:
"27"
==================================================
Starting Macula Gateway on port 9443 (realm: be.cortexiq.energy)
Using pre-generated TLS certificates
Using TLS certificates:
  Cert: /opt/macula/certs/cert.pem
  Key:  /opt/macula/certs/key.pem
Macula Gateway listening on port 9443 (realm: be.cortexiq.energy)
```

✅ **SUCCESS**: Gateway starts, listens on port 9443, and stays running!

**Production Deployment:**

For production, mount real TLS certificates using Kubernetes secrets:
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

See `GATEWAY_TLS_IMPLEMENTATION.md` for complete TLS certificate handling documentation.
