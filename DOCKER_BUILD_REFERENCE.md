# Docker Build Reference for Macula

**CRITICAL INFORMATION - DO NOT FORGET THIS!**

## Key Facts

### Image Tag
- **Docker Compose uses**: `macula:test` (NOT `macula:latest`!)
- **Location**: `docker/docker-compose.multi-node-test.yml` line 6+

### Build Directory
- **MUST build from**: `/home/rl/work/github.com/macula-io/macula` (project root)
- **Dockerfile location**: `/home/rl/work/github.com/macula-io/macula/Dockerfile`
- **Source code location**: `/home/rl/work/github.com/macula-io/macula/src/`

### Correct Build Command
```bash
cd /home/rl/work/github.com/macula-io/macula
docker rmi -f macula:test
docker build --no-cache --pull -t macula:test .
```

### Correct Test Command
```bash
cd /home/rl/work/github.com/macula-io/macula/docker
docker compose -f docker-compose.multi-node-test.yml down
docker compose -f docker-compose.multi-node-test.yml up
```

## Common Mistakes

1. ❌ Building from `/docker` subdirectory → source files not found
2. ❌ Building as `macula:latest` → docker-compose won't use it
3. ❌ Not removing old image before rebuild → old code persists
4. ❌ Running docker compose from wrong directory → can't find compose file

## Verification

Check image has new code:
```bash
docker run --rm macula:test grep "active mode enabled" /macula/src/macula_gateway.erl
```

Should return the line with "active mode enabled" text.

## Build Time
Full build with `--no-cache`: ~3-5 minutes (compiles MsQuic from source)
