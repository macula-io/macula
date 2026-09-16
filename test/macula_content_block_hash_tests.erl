%%% @doc Client-side hash verification for single-block `get_content/2'.
%%%
%%% A station that answers a fetch is not necessarily the one that stored the block, so the client re-checks the
%%% block against the hash its content id names. In the post-quantum format a content id has only tag 2, SHA-384
%%% (D24): <<2, Codec, Hash:48/binary>>. A single block has codec 16#55.
-module(macula_content_block_hash_tests).

-include_lib("eunit/include/eunit.hrl").

matching_bytes_are_accepted_test() ->
    Bytes = <<"hello direct-dial world">>,
    MCID = <<2, 16#55, (crypto:hash(sha384, Bytes))/binary>>,
    ?assertEqual({ok, Bytes}, macula_content_transfer:verify_block_hash(MCID, Bytes)).

tampered_bytes_are_rejected_test() ->
    Bytes = <<"hello direct-dial world">>,
    MCID = <<2, 16#55, (crypto:hash(sha384, Bytes))/binary>>,
    ?assertEqual({error, hash_mismatch},
                 macula_content_transfer:verify_block_hash(MCID, <<"HELLO direct-dial world">>)).

%% Tag 1 names BLAKE3, which the post-quantum format does not have: refused even when its hash matches.
a_blake3_content_id_is_refused_test() ->
    Bytes = <<"hello direct-dial world">>,
    MCID = <<1, 16#55, (macula_blake3_nif:hash(Bytes))/binary>>,
    ?assertEqual({error, invalid_mcid}, macula_content_transfer:verify_block_hash(MCID, Bytes)).

a_short_hash_under_tag_2_is_refused_test() ->
    Bytes = <<"x">>,
    MCID = <<2, 16#55, (crypto:hash(sha256, Bytes))/binary>>,
    ?assertEqual({error, invalid_mcid}, macula_content_transfer:verify_block_hash(MCID, Bytes)).

manifest_codec_is_not_single_block_test() ->
    MCID = <<2, 16#56, (crypto:strong_rand_bytes(48))/binary>>,
    ?assertEqual({error, invalid_mcid}, macula_content_transfer:verify_block_hash(MCID, <<"x">>)).
