# id-MLDSA87-RSA4096-PSS-SHA512, the LAMPS draft's own vector

The EU profile signs with this composite (plan decision D7). These are the
draft's bytes, not Macula's: `src/testvectors.json` of
[lamps-wg/draft-composite-sigs](https://github.com/lamps-wg/draft-composite-sigs)
at commit `f0627ab34acfe1aee0abce4bee91ed2b577eab76` (git blob
`eac3df25ca915882d3bcb3062835555787b02f5a`), the vectors of
`draft-ietf-lamps-pq-composite-sigs`. `scripts/fetch-lamps-composite-vector.sh`
writes them and checks the blob hash first.

| File | Bytes | What it is |
|------|------:|------------|
| `m.bin` | 44 | the message |
| `ctx.bin` | 71 | the context string `s_with_context.bin` was made with |
| `pk.bin` | 3,118 | ML-DSA-87 public key, then the DER `RSAPublicKey` |
| `sk.bin` | 2,380 | ML-DSA-87 seed (32 bytes), then the DER `RSAPrivateKey` |
| `s.bin` | 5,139 | signature over `m.bin` with an empty ctx |
| `s_with_context.bin` | 5,139 | signature over `m.bin` with `ctx.bin` |
