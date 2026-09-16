%% EUnit tests for node_ids (plan decision D5): SHA-256 over a label, the profile name and the identity key as carried.
-module(macula_node_keys_node_id_tests).

-include_lib("eunit/include/eunit.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).

%%------------------------------------------------------------------
%% Reference vectors, reproduced in Go, Rust and Python (2026-09-10)
%%------------------------------------------------------------------

pq_pure_vector_test() ->
    ?assertEqual(hex(<<"8c6a28c62bda0112065bccb0d8b02b18f46fef03d16e8b7ae91086025dd209ff">>),
                 macula_node_keys:node_id(test_key(2592), pq_pure)).

pq_hybrid_vector_test() ->
    ?assertEqual(hex(<<"e9df1133a8238239c58fc7f886d9667e961449ee272c30e968a0eecbb6b0131c">>),
                 macula_node_keys:node_id(test_key(3118), pq_hybrid)).

profile_separates_node_ids_over_the_same_key_bytes_test() ->
    ?assertEqual(hex(<<"4e79818f04bffbd7f2df71b82e3e543458d9f74cda64f88a80b352ed8e9af10b">>),
                 macula_node_keys:node_id(test_key(2592), pq_hybrid)).

%%------------------------------------------------------------------
%% node_ids of node keys
%%------------------------------------------------------------------

pq_pure_identity_key_node_id_is_derived_from_its_carried_key_test() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    {ok, NodeId} = macula_node_keys:node_id(Key),
    ?assertEqual(32, byte_size(NodeId)),
    ?assertEqual(macula_node_keys:node_id(macula_node_keys:public_key(Key), pq_pure), NodeId).

pq_hybrid_identity_key_node_id_covers_both_halves_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Key} = macula_node_keys:generate(identity, pq_hybrid),
        {ok, NodeId} = macula_node_keys:node_id(Key),
        Public = macula_node_keys:public_key(Key),
        ?assertEqual(macula_node_keys:node_id(Public, pq_hybrid), NodeId),
        <<MlDsa:2592/binary, Rsa/binary>> = Public,
        ?assertNotEqual(NodeId, macula_node_keys:node_id(<<MlDsa/binary, (flip_last(Rsa))/binary>>, pq_hybrid)),
        ?assertNotEqual(NodeId, macula_node_keys:node_id(MlDsa, pq_hybrid))
    end}.

connect_and_tls_keys_have_no_node_id_test() ->
    {ok, Connect} = macula_node_keys:generate(connect, pq_pure),
    {ok, Tls} = macula_node_keys:generate(tls, pq_pure),
    ?assertEqual({error, not_an_identity_key}, macula_node_keys:node_id(Connect)),
    ?assertEqual({error, not_an_identity_key}, macula_node_keys:node_id(Tls)).

unknown_profile_is_refused_test() ->
    ?assertError(function_clause, macula_node_keys:node_id(test_key(2592), rsa_only)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

test_key(Length) ->
    << <<(I rem 256)>> || I <- lists:seq(0, Length - 1) >>.

hex(Hex) ->
    binary:decode_hex(Hex).

flip_last(Bin) ->
    Size = byte_size(Bin) - 1,
    <<Head:Size/binary, Last>> = Bin,
    <<Head/binary, (Last bxor 1)>>.
