%% @doc ML-DSA in the crypto NIF, on `macula-mldsa'.
%%
%% D7, as amended on 2026-09-22: every ML-DSA signature in the stack is
%% made and checked by `macula-mldsa', so macula signs through this NIF
%% rather than OTP `crypto'. The fleet's existing node keys were made by
%% OTP, so the NIF must agree with OTP on them: a public key derived from
%% the same private key, and signatures that verify on the other side, in
%% both directions, with the key expanded and as its seed (D6).
%%
%% There is no fallback to OTP here. A NIF that did not load is an error.
-module(macula_crypto_nif_mldsa_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SETS, [mldsa44, mldsa65, mldsa87]).

mldsa_test_() ->
    [{atom_to_list(Set) ++ ": " ++ Name, fun() -> Test(Set) end}
     || Set <- ?SETS,
        {Name, Test} <- [{"sign and verify with a seed key", fun seed_round_trip/1},
                         {"refused on another message or context", fun refusals/1},
                         {"an OTP key: same public key, signatures cross both ways", fun otp_expanded_key/1},
                         {"our seed key signed by OTP verifies here", fun otp_signs_our_seed/1},
                         {"an inconsistent or wrong-length key is refused", fun bad_keys/1}]].

seed_round_trip(Set) ->
    {ok, {Pub, Seed}} = macula_crypto_nif:mldsa_generate(Set),
    ?assertEqual(32, byte_size(Seed)),
    ?assertEqual({ok, Pub}, macula_crypto_nif:mldsa_public_key(Set, {seed, Seed})),
    {ok, Sig} = macula_crypto_nif:mldsa_sign(Set, {seed, Seed}, <<"message">>, <<>>),
    ?assert(macula_crypto_nif:mldsa_verify(Set, Pub, <<"message">>, Sig, <<>>)),
    {ok, {Pub2, Seed2}} = macula_crypto_nif:mldsa_generate(Set),
    ?assertNotEqual(Seed, Seed2),
    ?assertNotEqual(Pub, Pub2).

refusals(Set) ->
    {ok, {Pub, Seed}} = macula_crypto_nif:mldsa_generate(Set),
    {ok, Sig} = macula_crypto_nif:mldsa_sign(Set, {seed, Seed}, <<"message">>, <<"ctx">>),
    ?assert(macula_crypto_nif:mldsa_verify(Set, Pub, <<"message">>, Sig, <<"ctx">>)),
    ?assertNot(macula_crypto_nif:mldsa_verify(Set, Pub, <<"messagE">>, Sig, <<"ctx">>)),
    ?assertNot(macula_crypto_nif:mldsa_verify(Set, Pub, <<"message">>, Sig, <<>>)),
    ?assertNot(macula_crypto_nif:mldsa_verify(Set, Pub, <<"message">>, <<Sig/binary, 0>>, <<"ctx">>)).

%% A key OTP made, as the fleet's node keys were: OTP generates it in its
%% expanded form.
otp_expanded_key(Set) ->
    {Pub, Expanded} = crypto:generate_key(Set, []),
    ?assertEqual({ok, Pub}, macula_crypto_nif:mldsa_public_key(Set, {expanded, Expanded})),
    {ok, Ours} = macula_crypto_nif:mldsa_sign(Set, {expanded, Expanded}, <<"m">>, <<>>),
    ?assert(crypto:verify(Set, none, <<"m">>, Ours, Pub)),
    Theirs = crypto:sign(Set, none, <<"m">>, {expandedkey, Expanded}),
    ?assert(macula_crypto_nif:mldsa_verify(Set, Pub, <<"m">>, Theirs, <<>>)).

%% OTP cannot derive a public key from a seed, so its signature with our
%% seed is checked under the key the NIF derived: it verifies only if both
%% expand the seed alike.
otp_signs_our_seed(Set) ->
    {ok, {Pub, Seed}} = macula_crypto_nif:mldsa_generate(Set),
    Theirs = crypto:sign(Set, none, <<"m">>, {seed, Seed}),
    ?assert(macula_crypto_nif:mldsa_verify(Set, Pub, <<"m">>, Theirs, <<>>)).

bad_keys(Set) ->
    {_Pub, Expanded} = crypto:generate_key(Set, []),
    Short = binary:part(Expanded, 0, byte_size(Expanded) - 1),
    ?assertEqual({error, wrong_length}, macula_crypto_nif:mldsa_public_key(Set, {expanded, Short})),
    ?assertEqual({error, wrong_length}, macula_crypto_nif:mldsa_sign(Set, {expanded, Short}, <<"m">>, <<>>)),
    <<Rho:32/binary, K:32/binary, Tr:64/binary, First, Rest/binary>> = Expanded,
    Flipped = <<Rho/binary, K/binary, Tr/binary, (First bxor 1), Rest/binary>>,
    ?assertEqual({error, inconsistent_private_key},
                 macula_crypto_nif:mldsa_public_key(Set, {expanded, Flipped})),
    ?assertEqual({error, wrong_length}, macula_crypto_nif:mldsa_public_key(Set, {seed, <<1, 2, 3>>})).

an_unknown_set_is_refused_test() ->
    ?assertError(badarg, macula_crypto_nif:mldsa_generate(ed25519)).

a_context_over_255_bytes_is_refused_test() ->
    {ok, {_Pub, Seed}} = macula_crypto_nif:mldsa_generate(mldsa87),
    ?assertEqual({error, context_too_long},
                 macula_crypto_nif:mldsa_sign(mldsa87, {seed, Seed}, <<"m">>, binary:copy(<<0>>, 256))).
