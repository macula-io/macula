%% EUnit tests for the realm trust a pool pins when it starts: a map from each realm id to that realm's public key as
%% carried, configured per deployment. A pool keeps each key for its own realm id alone. A realm trust that is not a
%% map of 32-byte realm ids to keys well formed for the pool's crypto profile does not start the pool, and no
%% statement issuer starts; a key well formed under another profile is refused by its own name. Links dial
%% unreachable seeds (127.0.0.1, low ports), so a pool that starts stays disconnected.
-module(macula_client_realm_trust_tests).

-include_lib("eunit/include/eunit.hrl").

-define(R1, <<16#11:256>>).
-define(R2, <<16#22:256>>).

realm_trust_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
     fun(ok) -> ok end,
     [{spawn, Test}
      || Test <- [fun a_pool_keeps_each_realm_key_for_its_realm_id_alone/0,
                  fun a_pool_without_realm_trust_holds_no_realm_key/0,
                  fun a_realm_trust_that_is_not_a_map_of_realm_ids_to_keys_does_not_start_the_pool/0,
                  fun a_realm_key_of_another_crypto_profile_does_not_start_the_pool/0]]}.

a_pool_keeps_each_realm_key_for_its_realm_id_alone() ->
    Key = realm_key(pq_pure),
    {ok, Pool} = macula_client:connect([seed(1)], #{realm_trust => #{?R1 => Key}}),
    try
        ?assertEqual({ok, Key}, macula_client:realm_key(Pool, ?R1)),
        ?assertEqual(none, macula_client:realm_key(Pool, ?R2))
    after
        ok = macula_client:close(Pool)
    end.

a_pool_without_realm_trust_holds_no_realm_key() ->
    {ok, Pool} = macula_client:connect([seed(1)], #{}),
    try
        ?assertEqual(none, macula_client:realm_key(Pool, ?R1))
    after
        ok = macula_client:close(Pool)
    end.

%% Not a map, a realm id that is not 32 bytes, a key that is not a binary, a key one byte short or long, and a key
%% nested in a map each stop the start by the same name.
a_realm_trust_that_is_not_a_map_of_realm_ids_to_keys_does_not_start_the_pool() ->
    Key = realm_key(pq_pure),
    Issuers = fun() -> proplists:get_value(active, supervisor:count_children(macula_statement_issuer_sup)) end,
    Before = Issuers(),
    Refused = [not_a_map, [{?R1, Key}], #{<<1:248>> => Key}, #{not_a_realm_id => Key}, #{?R1 => not_a_key},
               #{?R1 => binary:part(Key, 0, byte_size(Key) - 1)}, #{?R1 => <<Key/binary, 0>>},
               #{?R1 => #{realm_key => Key}}],
    [?assertEqual({error, {realm_trust, invalid}}, macula_client:connect([seed(1)], #{realm_trust => Trust}))
     || Trust <- Refused],
    ?assert(Issuers() =< Before).

%% The test VM runs pq_pure, so a pq_hybrid realm key is well formed for a profile other than the pool's.
a_realm_key_of_another_crypto_profile_does_not_start_the_pool() ->
    ?assertEqual({error, {realm_trust, profile_mismatch}},
                 macula_client:connect([seed(1)], #{realm_trust => #{?R1 => realm_key(pq_hybrid)}})).

realm_key(Profile) ->
    {ok, Key} = macula_node_keys:generate(realm, Profile),
    macula_node_keys:public_key(Key).

seed(Port) ->
    #{host => <<"127.0.0.1">>, port => Port, expected_node_id => <<Port:256>>}.
