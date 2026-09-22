#!/usr/bin/env bash
# Writes the IETF LAMPS draft's own test vector for id-MLDSA87-RSA4096-PSS-SHA512
# into test/fixtures/lamps_mldsa87_rsa4096_pss_sha512/, from the vector file at a
# pinned commit of the draft's repository. The EU profile's hybrid signature is
# that composite (plan decision D7, amended 2026-09-22), and the fixture is what
# proves Macula builds it as the draft does.
#
#   scripts/fetch-lamps-composite-vector.sh
#
# The vector file's git blob hash is checked before anything is written, so a
# moved or rewritten file stops the run rather than changing the fixture.
set -euo pipefail
cd "$(dirname "$0")/.."

repo="lamps-wg/draft-composite-sigs"
commit="f0627ab34acfe1aee0abce4bee91ed2b577eab76"
blob="eac3df25ca915882d3bcb3062835555787b02f5a"
alg="id-MLDSA87-RSA4096-PSS-SHA512"
out="test/fixtures/lamps_mldsa87_rsa4096_pss_sha512"

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
curl -fsSL -o "$tmp/testvectors.json" "https://raw.githubusercontent.com/$repo/$commit/src/testvectors.json"
got=$(git hash-object "$tmp/testvectors.json")
if [ "$got" != "$blob" ]; then
  echo "testvectors.json at $commit hashes to $got, expected $blob" >&2
  exit 1
fi

mkdir -p "$out"
python3 - "$tmp/testvectors.json" "$alg" "$out" <<'PY'
import base64, json, os, sys
vectors, alg, out = sys.argv[1:4]
doc = json.load(open(vectors))
(entry,) = [t for t in doc["tests"] if t["tcId"] == alg]
files = {"m.bin": doc["m"], "ctx.bin": doc["ctx"], "pk.bin": entry["pk"], "sk.bin": entry["sk"],
         "s.bin": entry["s"], "s_with_context.bin": entry["sWithContext"]}
for name, value in files.items():
    with open(os.path.join(out, name), "wb") as f:
        f.write(base64.b64decode(value))
PY
ls -l "$out"
