# A composite every stack must refuse

`sig.bin` is an `id-MLDSA87-RSA4096-PSS-SHA512` signature over the LAMPS
draft's `m.bin`, by the draft's key (both in
`../lamps_mldsa87_rsa4096_pss_sha512/`), whose RSA-PSS half began with a zero
byte that was dropped: 4,627 + 511 bytes. Each half verifies on its own, since
RSA accepts the value in fewer bytes, so only the composite's fixed length of
5,139 bytes refuses it. Written by `scripts/make-zero-dropped-composite.sh`.
