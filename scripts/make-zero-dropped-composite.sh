#!/usr/bin/env bash
# Writes test/fixtures/lamps_composite_zero_dropped/sig.bin: a composite
# id-MLDSA87-RSA4096-PSS-SHA512 signature over the LAMPS draft's message, by the
# draft's key, whose RSA-PSS half began with a zero byte that has been dropped,
# 4,627 + 511 bytes.
#
#   scripts/make-zero-dropped-composite.sh
#
# Each half still verifies on its own, since RSA accepts the value in fewer
# bytes, so only the composite's fixed length refuses it. Every stack must
# refuse it; the fixed bytes are here for the other stacks to test against.
# Run scripts/fetch-lamps-composite-vector.sh first. A PSS salt is random, so
# about one signature in 256 has the leading zero, and the bytes differ on
# every run.
set -euo pipefail
cd "$(dirname "$0")/.."

vector="test/fixtures/lamps_mldsa87_rsa4096_pss_sha512"
out="test/fixtures/lamps_composite_zero_dropped"

rebar3 as test compile
mkdir -p "$out"
erl -noshell -pa _build/test/lib/*/ebin -eval "
    Read = fun(Name) -> {ok, Bin} = file:read_file(filename:join(\"$vector\", Name)), Bin end,
    <<Seed:32/binary, RsaPrivate/binary>> = Read(\"sk.bin\"),
    <<MlDsaPublic:2592/binary, RsaPublic/binary>> = Read(\"pk.bin\"),
    Key = #{purpose => identity, profile => pq_hybrid,
            components => [#{algorithm => mldsa87, public => MlDsaPublic, private => Seed},
                           #{algorithm => rsa_pss, public => RsaPublic, private => RsaPrivate}]},
    Message = Read(\"m.bin\"),
    Dropped = fun Sign() ->
                  case macula_node_keys:sign(Message, Key) of
                      <<MlDsa:4627/binary, 0, Rsa:511/binary>> -> <<MlDsa/binary, Rsa/binary>>;
                      _LeadingByteNotZero -> Sign()
                  end
              end,
    ok = file:write_file(filename:join(\"$out\", \"sig.bin\"), Dropped()),
    halt(0)."
ls -l "$out"
