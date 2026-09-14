# Sample frames from the other Macula SDKs

One frame per file, named after its frame type: the 4-byte length prefix and
the CBOR body exactly as that SDK's encoder writes them, signed where the SDK
signs, with throwaway identities. `test/macula_frame_received_tests.erl`
checks that every one decodes with no bytes left over and passes
`macula_frame:validate_received/1`, so a change to macula's frame rules that
would refuse another stack's frames fails the tests.

| Directory | SDK | Source |
|---|---|---|
| `go/` | macula-go | branch verified-caller at 8a8f718; frame encoding unchanged since v0.9.0 (251177d). Written by the SDK's own send paths, except `connect` and `stream_open`, built the way `connectOne` and `stream.Open` build them. 2026-09-12. |
| `rust/` | macula-rust | db2ffe3, built with the SDK's own frame builders and signed as it signs. 2026-09-12. |
| `dotnet/` | macula-dotnet | 61e1fbb, built with the SDK's own frame builders and signed as it signs. 2026-09-12. |
| `go-v0.9.0/` | macula-go | tag v0.9.0 (251177d), from a git archive of the tag with the release's own builders, signed as its send paths sign, decoded back and verified. Adds `publish.ttl`. 2026-09-14. |
| `go-v0.8.2/` | macula-go | tag v0.8.2 (5f922f4), built the same way. The Go that macula-ts v0.16.0, and so macula-mcp v0.31.0, and macula-php v0.4.0 embed. 2026-09-14. |
| `go-v0.7.1/` | macula-go | tag v0.7.1 (dcaddff), built the same way. The Go that macula-cli v0.7.1 embeds. Frame builders, CBOR encoding and send-path signing are the same in every earlier tag, v0.1.0 to v0.7.0, including the macula-go-sdk module path, so this set stands for those too. 2026-09-14. |
| `rust-v0.3.0/` | macula-rust | tag v0.3.0 (392ee46), built with the SDK's own builders and signed as its send sites sign at that tag. Adds `call.ucan` and `error.unauthorized`. The frame, stream, direct-dial, content, UCAN and identity code of 0.2.3 and 0.2.4 on crates.io is byte-identical, so this set stands for those too. 2026-09-14. |
| `dotnet-v0.4.1/` | macula-dotnet | tag v0.4.1 (16c8763), built with the SDK's own builders and signed with `Envelope.Sign`, PUBLISH also with `Envelope.SignPublisher`. 2026-09-14. |
| `dotnet-v0.4.0/` | macula-dotnet | tag v0.4.0 (4328523), built the same way. Frame, CBOR and identity code are unchanged from v0.4.0 to v0.4.1. 2026-09-14. |
| `python-v0.1.0/` | macula-py | tag v0.1.0 (ace63ff), from a git archive of the tag with its own builders, signed as its send paths sign. Its PUBLISH carries no `publisher_sig`, it sends no GOODBYE, and its `call.ucan` holds a token minted by Go as opaque bytes. Adds `stream_end.both`. 2026-09-14. |

The versioned directories hold what each release installable from a package
registry sends, so the validator is checked against clients already in use,
not only against current heads.

macula-ts, macula-php, macula-mcp and macula-cli send through the Go encoder.
All three SDKs put a `signer` field on STREAM_DATA, STREAM_END and
STREAM_ERROR that macula's frame spec does not have; the validator ignores
fields it does not check. DHT and content operations go out as CALL frames,
so no SDK sends their frame types.
