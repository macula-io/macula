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
# The question publishing needs answered is whether the key may WRITE, and
# hex.pm answers it at /api/auth?domain=api&resource=write:
#   204  the key may write (200 read the same)  -> accept
#   401  no key, or a key it does not accept   -> refuse
#   403  a valid key without write permission  -> refuse: it cannot publish
# Anything else (hex.pm down, network) also refuses: publishing would fail too.
# The value is never printed.
#
# This asked /api/users/me until 2026-09-23, which answers 404 for a key with
# no user behind it, an organisation key. mcl_om publishes with one, so the
# check refused a key that had published three versions that same day, and
# stopped mcl_om 0.26.4 before it was sent.
#
# Usage (publish job, env HEX_API_KEY: secrets.HEX_API_KEY):
#   bash scripts/is_hex_publish_key_live.sh
set -u

HEX_API="${HEX_API_URL:-https://hex.pm/api}"

[ -n "${HEX_API_KEY:-}" ] || {
    echo "::error::HEX_API_KEY is EMPTY in this job. Set the secret in the hex-publish environment, then re-run this job."
    exit 1
}

AUTH="$HEX_API/auth?domain=api&resource=write"
code="$(curl -s -o /dev/null -w '%{http_code}' --max-time 30 \
    -H "authorization: $HEX_API_KEY" "$AUTH")"

case "$code" in
    200|204)
        echo "OK: hex.pm lets HEX_API_KEY write (HTTP $code from $AUTH)." ;;
    401)
        echo "::error::hex.pm REJECTS HEX_API_KEY (HTTP 401): revoked, mistyped or not a hex key. Replace it in the hex-publish environment, then re-run this job."
        exit 1 ;;
    403)
        echo "::error::HEX_API_KEY is valid but may not write (HTTP 403 from $AUTH): it cannot publish. Replace it with a key that has api:write, then re-run this job."
        exit 1 ;;
    *)
        echo "::error::could not check HEX_API_KEY: HTTP ${code:-none} from $AUTH. Publishing would not reach hex.pm either."
        exit 1 ;;
esac
