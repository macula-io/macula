%% A recipient's KEM keypair (E2E seal scheme 1): generated in a profile,
%% carried as bytes whose size names the profile, and read back from those
%% bytes as a public key a sender seals to.
-module(macula_seal_keys_tests).

-include_lib("eunit/include/eunit.hrl").

%% A generated key is of its profile's size and seals to itself.
a_generated_key_seals_and_opens_test_() ->
    [{atom_to_list(Profile), fun() ->
          {Public, Private} = macula_seal:generate_key(Profile),
          Carried = macula_seal:key_as_carried(Public),
          ?assertEqual(macula_seal:carried_key_size(Profile), byte_size(Carried)),
          {Secret, KemCt} = macula_seal:sender_secret(Profile, Public),
          ?assertEqual({ok, Secret}, macula_seal:recipient_secret(Profile, Private, Carried, KemCt))
      end} || Profile <- [pq_pure, pq_hybrid]].

%% A pq_hybrid private scalar is always 48 bytes, the size scheme 1 types it.
a_hybrid_private_scalar_is_48_bytes_test() ->
    [?assertMatch({_, #{p384_priv := <<_:384>>}}, macula_seal:generate_key(pq_hybrid)) || _ <- lists:seq(1, 32)].

%% Carried bytes read back as the public key they carry, with its profile.
carried_bytes_read_back_as_the_public_key_test_() ->
    [{atom_to_list(Profile), fun() ->
          {Public, _} = macula_seal:generate_key(Profile),
          ?assertEqual({ok, Profile, Public}, macula_seal:public_key(macula_seal:key_as_carried(Public)))
      end} || Profile <- [pq_pure, pq_hybrid]].

%% Bytes of another size, or a pq_hybrid tail that is not an uncompressed
%% point, carry no key.
bytes_of_another_shape_carry_no_key_test_() ->
    [?_assertEqual(error, macula_seal:public_key(Bytes))
     || Bytes <- [<<>>, binary:copy(<<1>>, 1567), binary:copy(<<1>>, 1569), binary:copy(<<1>>, 1665),
                  not_a_binary]].
