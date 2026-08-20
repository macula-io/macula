%%% @doc Client-side hash verification for single-block `get_content/2'.
%%%
%%% Regression cover for a real gap: chunked content already gets a
%%% client-side integrity check via `macula_manifest:verify/2' over the
%%% reassembled whole, but single-block content had NONE — the client
%%% simply trusted whatever bytes came back with the requested MCID,
%%% relying entirely on the SERVING station's own verification at PUT
%%% time. That's fine when the client picked its own connected station,
%%% but `get_content_station/5' lets a caller deliberately dial a
%%% resolved, third-party provider — at that point "the station that
%%% answered verified this once, a while ago, maybe a different station
%%% than the one that stored it" is not good enough. MCID's own BLAKE3
%%% hash makes client-side re-verification free to add.
-module(macula_content_block_hash_tests).

-include_lib("eunit/include/eunit.hrl").

matching_bytes_are_accepted_test() ->
    Bytes = <<"hello direct-dial world">>,
    Hash  = macula_blake3_nif:hash(Bytes),
    MCID  = <<1, 16#55, Hash/binary>>,
    ?assertEqual({ok, Bytes}, macula:verify_block_hash(MCID, Bytes)).

tampered_bytes_are_rejected_test() ->
    Bytes    = <<"hello direct-dial world">>,
    Hash     = macula_blake3_nif:hash(Bytes),
    MCID     = <<1, 16#55, Hash/binary>>,
    Tampered = <<"HELLO direct-dial world">>,
    ?assertEqual({error, hash_mismatch},
                 macula:verify_block_hash(MCID, Tampered)).

wrong_length_hash_is_not_a_single_block_mcid_test() ->
    %% A 34-byte MCID with the right codec bytes and a full 32-byte
    %% hash always matches the pattern — this just locks in that a
    %% malformed (too-short) MCID is refused outright rather than
    %% silently matching a truncated hash.
    ?assertEqual({error, invalid_mcid},
                 macula:verify_block_hash(<<1, 16#55, "short">>, <<"x">>)).

manifest_codec_is_not_single_block_test() ->
    %% Chunked content's own codec byte (16#56) must never be routed
    %% through the single-block hash check — it has a different shape
    %% entirely (verified via macula_manifest:verify/2 instead).
    Hash = crypto:strong_rand_bytes(32),
    ?assertEqual({error, invalid_mcid},
                 macula:verify_block_hash(<<1, 16#56, Hash/binary>>, <<"x">>)).
