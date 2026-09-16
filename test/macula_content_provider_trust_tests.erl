%%% @doc Signer-vs-claim verification for content_announcement providers.
%%%
%%% A content announcement is signed by the node that shares the content and names that node as announcer_node.
%%% macula:decode_provider/2 trusts an announcement only when it verifies under the verifier's profile, and the
%%% verification covers the signer: a record whose payload names a node other than its signer is refused. No node can
%%% sign an announcement that misattributes who serves the content at its endpoint.
-module(macula_content_provider_trust_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LABEL, <<"MACULA-PQ-RECORD-V1">>).
-define(MCID, <<2, 16#55, (crypto:strong_rand_bytes(48))/binary>>).
-define(ENDPOINT, <<"quic://[::1]:4433">>).

a_consistent_signer_and_claim_is_trusted_test() ->
    Key = identity_key(),
    NodeId = macula_node_keys:key_id(Key),
    Wire = signed(macula_record:content_announcement(NodeId, ?MCID, ?ENDPOINT), Key),
    ?assertMatch({true, #{announcer_node := NodeId, endpoint := ?ENDPOINT}}, macula:decode_provider(Wire, pq_pure)).

an_unsigned_announcement_is_refused_test() ->
    Key = identity_key(),
    Unsigned = macula_record:content_announcement(macula_node_keys:key_id(Key), ?MCID, ?ENDPOINT),
    ?assertEqual(false, macula:decode_provider(Unsigned, pq_pure)).

%% macula_record:sign/2 never produces this shape, so the test signs the fields directly: the signature is valid, and
%% the refusal comes from the signer check.
a_signer_claiming_another_announcer_node_is_refused_test() ->
    Key = identity_key(),
    Claimed = macula_node_keys:key_id(identity_key()),
    Wire = crafted(macula_record:content_announcement(Claimed, ?MCID, <<"quic://evil:4433">>), Key),
    ?assertEqual({error, key_id_mismatch}, macula_record:verify(Wire, pq_pure)),
    ?assertEqual(false, macula:decode_provider(Wire, pq_pure)).

a_tampered_announcement_is_refused_test() ->
    Key = identity_key(),
    Unsigned = macula_record:content_announcement(macula_node_keys:key_id(Key), ?MCID, ?ENDPOINT),
    #{tbs := <<Head:20/binary, Byte, Tail/binary>>} = Signed = macula_record:sign(Unsigned, Key),
    Tampered = macula_record:encode(Signed#{tbs := <<Head/binary, (Byte bxor 1), Tail/binary>>}),
    ?assertEqual(false, macula:decode_provider(Tampered, pq_pure)).

a_record_of_another_type_is_not_a_provider_test() ->
    Key = identity_key(),
    Wire = signed(macula_record:node_record(macula_node_keys:key_id(Key), [], 0), Key),
    ?assertEqual(false, macula:decode_provider(Wire, pq_pure)).

identity_key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.

signed(Record, Key) ->
    macula_record:encode(macula_record:sign(Record, Key)).

crafted(#{type := Type, version := Version, created_at := Created, expires_at := Expires, payload := Payload}, Key) ->
    Fields = #{{text, <<"type">>} => Type, {text, <<"version">>} => Version, {text, <<"created_at">>} => Created,
               {text, <<"expires_at">>} => Expires, {text, <<"payload">>} => Payload},
    macula_signed_object:encode(macula_signed_object:sign(?LABEL, Fields, Key)).
