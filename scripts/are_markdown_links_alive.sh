#!/usr/bin/env bash
# Do the external links in a markdown file still answer?
#
# CLAUDE.md asks for every external link to be checked before a documentation
# commit. This requests each distinct http(s) URL in the file once, follows
# redirects, and prints one line per URL:
#
#   ALIVE        200-299 on the URL itself
#   MOVED        200-299, but only after a redirect; check it did not land on a home page
#   BLOCKED      202, 401, 403 or 429: the site refuses scripted clients, so unverified, not dead
#   DEAD         any other status, for example 404 or 410
#   UNREACHABLE  no HTTP answer at all (DNS, TLS, timeout)
#
# BLOCKED, DEAD and UNREACHABLE lines also name the Internet Archive's newest
# capture of the URL that was a page (2xx) or a redirect (3xx). A 200 capture
# shows the page existed then; it does not show the page is still there. The
# archive rate-limits, and a refused lookup prints "archive lookup failed",
# never "no copy".
#
# Only the status is checked. A site that answers 200 for a missing page passes.
#
# Exits 1 when any URL is DEAD or UNREACHABLE. BLOCKED alone does not fail,
# because it says nothing about the page; open those in a browser.
#
# Usage:
#   scripts/are_markdown_links_alive.sh <markdown-file>
#   TIMEOUT_SECONDS=40 scripts/are_markdown_links_alive.sh <markdown-file>
set -uo pipefail

MARKDOWN_FILE="${1:-}"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-20}"
USER_AGENT="${USER_AGENT:-Mozilla/5.0 (X11; Linux x86_64; rv:140.0) Gecko/20100101 Firefox/140.0}"
WAYBACK_CDX_API="${WAYBACK_CDX_API:-https://web.archive.org/cdx/search/cdx}"
# Stops at whitespace and at the characters markdown puts around a URL.
URL_PATTERN='https?://[^][<>()"`'"'"'[:space:]|]+'
TRAILING_PUNCTUATION='[.,;:]+$'

[ -r "$MARKDOWN_FILE" ] || { echo "REFUSED: usage: $0 <markdown-file>"; exit 1; }
command -v curl >/dev/null || { echo "REFUSED: curl is not on PATH"; exit 1; }

mapfile -t URLS < <(grep -oE "$URL_PATTERN" "$MARKDOWN_FILE" | sed -E "s/$TRAILING_PUNCTUATION//" | awk '!seen[$0]++')

[ "${#URLS[@]}" -gt 0 ] || { echo "OK: no external links in $MARKDOWN_FILE"; exit 0; }

# Prints "<status> <final url>"; the status is 000 when there was no HTTP answer.
request() {
    curl --silent --location --output /dev/null \
        --max-time "$TIMEOUT_SECONDS" --user-agent "$USER_AGENT" \
        --write-out '%{http_code} %{url_effective}' "$1"
}

# Prints "(archived YYYY-MM-DD with <status>)" for the newest capture that was a
# page or a redirect, "(no page or redirect archived)" when the archive has none,
# and "(archive lookup failed)" when the archive did not answer.
archived() {
    local answer timestamp status
    answer="$(curl --silent --fail --get --max-time "$TIMEOUT_SECONDS" \
        --data-urlencode "url=$1" \
        --data-urlencode 'fl=timestamp,statuscode' \
        --data-urlencode 'filter=statuscode:[23]..' \
        --data-urlencode 'limit=-1' \
        "$WAYBACK_CDX_API")" || { echo "(archive lookup failed)"; return; }
    read -r timestamp status <<< "$answer"
    if [ -n "${timestamp:-}" ]; then
        echo "(archived ${timestamp:0:4}-${timestamp:4:2}-${timestamp:6:2} with $status)"
    else
        echo "(no page or redirect archived)"
    fi
}

FAILED=0
for URL in "${URLS[@]}"; do
    read -r CODE FINAL_URL <<< "$(request "$URL")"
    case "$CODE" in
        202|401|403|429) echo "BLOCKED      $CODE $URL $(archived "$URL")" ;;
        2??)
            if [ "$FINAL_URL" = "$URL" ]; then
                echo "ALIVE        $CODE $URL"
            else
                echo "MOVED        $CODE $URL -> $FINAL_URL"
            fi
            ;;
        000) echo "UNREACHABLE  $CODE $URL $(archived "$URL")"; FAILED=1 ;;
        *)   echo "DEAD         $CODE $URL $(archived "$URL")"; FAILED=1 ;;
    esac
done

exit "$FAILED"
