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

macula-ts, macula-php, macula-mcp and macula-cli send through the Go encoder.
All three SDKs put a `signer` field on STREAM_DATA, STREAM_END and
STREAM_ERROR that macula's frame spec does not have; the validator ignores
fields it does not check. DHT and content operations go out as CALL frames,
so no SDK sends their frame types.
