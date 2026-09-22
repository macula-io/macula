#!/usr/bin/env bash
# Is this checkout ready to be released? Runs every guard the release runs,
# builds the package and the docs, and dry-runs the publish.
#
# IT DOES NOT PUBLISH, AND THAT IS THE POINT. It used to. It ran the same
# guards it runs now and then called `rebar3 hex publish --yes', which reaches
# hex.pm directly.
#
# The release path is .github/workflows/publish-hex.yml, armed by pushing the
# tag. Its verify job repeats these checks on a clean runner from the pushed
# tag, and its publish job then publishes with no further gate, by design:
# pushing the tag IS the release, and the push is where it is approved. A
# script on a maintainer's machine that also published would be a second path
# to one version, built from whatever that machine held.
#
# So this is now the local half of that workflow: everything up to, and
# including, the dry run. Run it before pushing a tag, to find out on your own
# machine what would otherwise fail in CI after the tag exists. A tag can be
# deleted and recut; a publish cannot be unpublished.
#
# Usage: scripts/publish-to-hex.sh   (from a clean checkout of the release tag)
set -eo pipefail

cd "$(dirname "$0")/.."

# Refuses unless the checkout is the clean release tag v<vsn> and origin has
# that tag at the same commit. `rebar3 hex publish' packages the WORKING TREE,
# not a tag, so this is what keeps the two from disagreeing.
bash scripts/is_checkout_publishable.sh

# Refuses unless the erl on PATH is the Erlang/OTP that .tool-versions pins.
# OTP 29's edoc chunks hold only exported functions, so a build from the wrong
# major publishes different docs from the same source.
bash scripts/is_erlang_the_pinned_version.sh

MACULA_VERSION="$(sed -n 's/.*{vsn, *"\([^"]*\)"}.*/\1/p' src/macula.app.src | head -n 1)"

echo
echo "==> macula ${MACULA_VERSION}: building the package and docs"
rebar3 hex build

# The build compiled the NIFs from this tree's native/ sources. A macula_quic
# that does not load from that build stops a release here rather than after it.
echo
echo "==> checking the QUIC NIF loads from this build"
bash scripts/is_quic_nif_built_from_this_tree.sh _build/default/lib

# The publish command as a dry run, with a placeholder key and a dead API URL
# so the placeholder cannot reach hex.pm. This fails if the command stops
# reading a key from HEX_API_KEY; it cannot tell whether the real key works.
echo
echo "==> dry-running the publish command"
HEX_API_KEY=placeholder-not-a-real-key \
HEX_API_URL=http://127.0.0.1:9 \
    rebar3 hex publish --repo hexpm --yes --dry-run

cat <<EOF

==> macula ${MACULA_VERSION} is ready to release. Nothing was published.

To release it:
  git push origin v${MACULA_VERSION}

That arms .github/workflows/publish-hex.yml, which verifies the tag and then
publishes with no further gate: pushing the tag IS the release.

Afterwards, to check hex serves the tagged code rather than assuming it:
  scripts/is_hex_serving_what_git_says.sh ${MACULA_VERSION}
EOF
