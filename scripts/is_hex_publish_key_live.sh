#!/usr/bin/env bash
# Is HEX_API_KEY set, and does hex.pm accept it? Run FIRST in the publish job,
# before anything is published.
#
# The key lives only in the `hex-publish' environment, which admits v* tags
# alone, so nothing can inspect it before a tag. A key that is blank (mcl-embed,
# 2026-09-23: the publish failed "No write key found") or revoked used to fail
# only inside `rebar3 hex publish'. This names the cause in one line instead.
# The secret is read at RUN time, so after fixing the key, re-running the
# failed job is enough: nothing was published and the version is not burned.
#
# The value is never printed. hex.pm answers /api/users/me with:
#   401  no key, or a key it does not accept   -> refuse
#   200  a key with read scope                 -> accept
#   403  a valid key without read scope (a publish-only key may be one) -> accept
# Anything else (hex.pm down, network) also refuses: publishing would fail too.
#
# Usage (publish job, env HEX_API_KEY: secrets.HEX_API_KEY):
#   bash scripts/is_hex_publish_key_live.sh
set -u

HEX_API="${HEX_API_URL:-https://hex.pm/api}"

[ -n "${HEX_API_KEY:-}" ] || {
    echo "::error::HEX_API_KEY is EMPTY in this job. Set the secret in the hex-publish environment, then re-run this job."
    exit 1
}

code="$(curl -s -o /dev/null -w '%{http_code}' --max-time 30 \
    -H "authorization: $HEX_API_KEY" "$HEX_API/users/me")"

case "$code" in
    200|403)
        echo "OK: hex.pm accepts HEX_API_KEY (HTTP $code from $HEX_API/users/me)." ;;
    401)
        echo "::error::hex.pm REJECTS HEX_API_KEY (HTTP 401): revoked, mistyped or not a hex key. Replace it in the hex-publish environment, then re-run this job."
        exit 1 ;;
    *)
        echo "::error::could not check HEX_API_KEY: HTTP ${code:-none} from $HEX_API/users/me. Publishing would not reach hex.pm either."
        exit 1 ;;
esac
