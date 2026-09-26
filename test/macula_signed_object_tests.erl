%% EUnit tests for macula_signed_object: the signed objects of DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md. A signature
%% covers Label || 0x00 || SHA-384(key as carried) || tbs, and a verifier reads an object in the design's order: its
%% shape, the carried key, the signature over tbs as received, tbs under the decoding rule, then alg.
-module(macula_signed_object_tests).

-include_lib("eunit/include/eunit.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).
-define(LABEL, <<"MACULA-PQ-RECORD-V1">>).
-define(OTHER_LABEL, <<"MACULA-PQ-REPLY-V1">>).
-define(FIELDS, #{{text, <<"type">>} => 1, {text, <<"payload">>} => #{{text, <<"name">>} => {text, <<"n1">>}}}).

pq_pure_test_() ->
    {setup, fun() -> keys(pq_pure) end, fun cases/1}.

%% The setup also finds the pq_hybrid object whose RSA half begins with a zero
%% byte: a search of about 256 signings, random in length, so it runs under this
%% group's timeout and not a single case's 5 s (macula#43: on a loaded host it
%% ran past 5 s and eunit cancelled the case).
pq_hybrid_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun() -> with_zero_dropped(keys(pq_hybrid)) end, fun cases/1}}.

%% Every case signs inside its own test, so each one passes or fails on its own.
cases(Keys) ->
    [{case_name(Case), fun() -> Case(Keys) end}
     || Case <- [fun object_carries_the_signer_key/1,
                 fun object_verifies_to_its_key_tbs_and_fields/1,
                 fun signature_covers_label_zero_key_hash_and_tbs/1,
                 fun another_label_is_refused/1,
                 fun a_flipped_signature_is_refused/1,
                 fun altered_tbs_is_refused/1,
                 fun another_valid_key_is_refused/1,
                 fun a_key_not_in_carried_form_is_malformed/1,
                 fun an_extra_field_is_malformed/1,
                 fun a_missing_signature_is_malformed/1,
                 fun a_tbs_that_is_not_bytes_is_malformed/1,
                 fun a_missing_key_is_malformed/1,
                 fun an_object_under_the_other_profile_is_malformed/1,
                 fun a_held_object_has_only_tbs_and_signature/1,
                 fun a_held_object_verifies_with_the_held_key/1,
                 fun a_held_object_refuses_another_key/1,
                 fun held_verify_refuses_an_object_that_carries_a_key/1,
                 fun verify_refuses_a_held_object/1,
                 fun tbs_is_verified_as_received_in_any_key_order_and_width/1,
                 fun a_tbs_that_is_not_a_map_is_malformed/1,
                 fun a_tbs_with_a_duplicate_key_is_malformed/1,
                 fun a_tbs_with_trailing_bytes_is_malformed/1,
                 fun a_tbs_without_alg_is_malformed/1,
                 fun alg_naming_the_other_profile_is_refused/1,
                 fun a_carried_object_round_trips_through_its_wire_form/1,
                 fun a_held_object_round_trips_through_its_wire_form/1,
                 fun decode_accepts_keys_in_any_order/1,
                 fun decode_refuses_an_extra_key/1,
                 fun decode_refuses_a_value_that_is_not_bytes/1,
                 fun decode_refuses_trailing_bytes/1,
                 fun decode_refuses_malformed_bytes/1,
                 fun a_signature_of_another_length_is_refused/1]].

%%------------------------------------------------------------------
%% Signature length
%%------------------------------------------------------------------

%% A signature is exactly its profile's length. One byte short or long is refused as signature_invalid, and in
%% pq_hybrid so is a valid composite whose RSA half had its leading zero byte dropped: the same RSA value, one byte short.
a_signature_of_another_length_is_refused(#{key := Key, profile := Profile} = Keys) ->
    #{signature := Signature} = Object = object(Key),
    Others = [binary:part(Signature, 0, byte_size(Signature) - 1), <<Signature/binary, 0>>
              | leading_zero_dropped(Profile, Keys)],
    [?assertEqual({error, signature_invalid}, macula_signed_object:verify(?LABEL, Object#{signature := Other}, Profile))
     || Other <- Others].

%% In pq_hybrid, a composite for object(Key) whose RSA half began with a zero byte, with that byte dropped, as the
%% setup found it.
leading_zero_dropped(pq_hybrid, #{zero_dropped := ZeroDropped}) -> [ZeroDropped];
leading_zero_dropped(pq_pure, _Keys) -> [].

%% The object's tbs is the same at every signing, and a PSS salt is random, so signing again finds one about once in
%% 256 signatures.
with_zero_dropped(#{key := Key} = Keys) ->
    Keys#{zero_dropped => zero_dropped(object(Key), Key, 4096)}.

zero_dropped(#{signature := <<MlDsa:4627/binary, 0, Rest/binary>>}, _Key, _Left) -> <<MlDsa/binary, Rest/binary>>;
zero_dropped(_Object, Key, Left) when Left > 0 -> zero_dropped(object(Key), Key, Left - 1).

%%------------------------------------------------------------------
%% Objects that carry their key: {key, tbs, signature}
%%------------------------------------------------------------------

object_carries_the_signer_key(#{key := Key}) ->
    ?assertEqual(macula_node_keys:public_key(Key), maps:get(key, object(Key))).

object_verifies_to_its_key_tbs_and_fields(#{key := Key, profile := Profile}) ->
    #{key := Carried, tbs := Tbs} = Object = object(Key),
    ?assertEqual({ok, #{key => Carried, tbs => Tbs, fields => expected_fields(Profile)}},
                 macula_signed_object:verify(?LABEL, Object, Profile)).

signature_covers_label_zero_key_hash_and_tbs(#{key := Key, profile := Profile}) ->
    #{key := Carried, tbs := Tbs, signature := Signature} = object(Key),
    ?assert(macula_node_keys:verify([?LABEL, 0, crypto:hash(sha384, Carried), Tbs], Signature, Carried, Profile)).

another_label_is_refused(#{key := Key, profile := Profile}) ->
    ?assertEqual({error, signature_invalid}, macula_signed_object:verify(?OTHER_LABEL, object(Key), Profile)).

a_flipped_signature_is_refused(#{key := Key, profile := Profile}) ->
    #{signature := Signature} = Object = object(Key),
    ?assertEqual({error, signature_invalid},
                 macula_signed_object:verify(?LABEL, Object#{signature := flip(Signature)}, Profile)).

altered_tbs_is_refused(#{key := Key, profile := Profile}) ->
    #{tbs := Tbs} = Object = object(Key),
    ?assertEqual({error, signature_invalid}, macula_signed_object:verify(?LABEL, Object#{tbs := flip(Tbs)}, Profile)).

another_valid_key_is_refused(#{key := Key, other := Other, profile := Profile}) ->
    Object = object(Key),
    ?assertEqual({error, signature_invalid},
                 macula_signed_object:verify(?LABEL, Object#{key := macula_node_keys:public_key(Other)}, Profile)).

a_key_not_in_carried_form_is_malformed(#{key := Key, profile := Profile}) ->
    #{key := Carried} = Object = object(Key),
    ?assertEqual({error, malformed},
                 macula_signed_object:verify(?LABEL, Object#{key := binary:part(Carried, 0, 100)}, Profile)).

an_extra_field_is_malformed(#{key := Key, profile := Profile}) ->
    ?assertEqual({error, malformed}, macula_signed_object:verify(?LABEL, (object(Key))#{extra => <<>>}, Profile)).

a_missing_signature_is_malformed(#{key := Key, profile := Profile}) ->
    ?assertEqual({error, malformed},
                 macula_signed_object:verify(?LABEL, maps:remove(signature, object(Key)), Profile)).

a_tbs_that_is_not_bytes_is_malformed(#{key := Key, profile := Profile}) ->
    ?assertEqual({error, malformed},
                 macula_signed_object:verify(?LABEL, (object(Key))#{tbs := {text, <<"x">>}}, Profile)).

a_missing_key_is_malformed(#{key := Key, profile := Profile}) ->
    ?assertEqual({error, malformed}, macula_signed_object:verify(?LABEL, maps:remove(key, object(Key)), Profile)).

an_object_under_the_other_profile_is_malformed(#{key := Key, profile := Profile}) ->
    ?assertEqual({error, malformed}, macula_signed_object:verify(?LABEL, object(Key), other_profile(Profile))).

%%------------------------------------------------------------------
%% Objects whose key the verifier holds: {tbs, signature}
%%------------------------------------------------------------------

a_held_object_has_only_tbs_and_signature(#{key := Key}) ->
    ?assertEqual([signature, tbs], lists:sort(maps:keys(held(Key)))).

a_held_object_verifies_with_the_held_key(#{key := Key, profile := Profile}) ->
    #{tbs := Tbs} = Held = held(Key),
    ?assertEqual({ok, #{tbs => Tbs, fields => expected_fields(Profile)}},
                 macula_signed_object:verify_held(?LABEL, Held, macula_node_keys:public_key(Key), Profile)).

%% The key hash is signed even though the key does not travel, so no other key verifies a held object.
a_held_object_refuses_another_key(#{key := Key, other := Other, profile := Profile}) ->
    ?assertEqual({error, signature_invalid},
                 macula_signed_object:verify_held(?LABEL, held(Key), macula_node_keys:public_key(Other), Profile)).

held_verify_refuses_an_object_that_carries_a_key(#{key := Key, profile := Profile}) ->
    Carried = macula_node_keys:public_key(Key),
    ?assertEqual({error, malformed},
                 macula_signed_object:verify_held(?LABEL, (held(Key))#{key => Carried}, Carried, Profile)).

verify_refuses_a_held_object(#{key := Key, profile := Profile}) ->
    ?assertEqual({error, malformed}, macula_signed_object:verify(?LABEL, held(Key), Profile)).

%%------------------------------------------------------------------
%% tbs: signed as received, then decoded under the decoding rule
%%------------------------------------------------------------------

%% {"type": 1, "alg": ...} with the keys out of order and "type" in a one-byte length width.
tbs_is_verified_as_received_in_any_key_order_and_width(#{key := Key, profile := Profile}) ->
    Tbs = <<16#A2, 16#78, 4, "type", 16#01, 16#63, "alg", (cbor_text(alg(Profile)))/binary>>,
    ?assertMatch({ok, #{tbs := Tbs, fields := #{{text, <<"type">>} := 1}}},
                 macula_signed_object:verify(?LABEL, raw(Key, Tbs), Profile)).

a_tbs_that_is_not_a_map_is_malformed(#{key := Key, profile := Profile}) ->
    ?assertEqual({error, malformed}, macula_signed_object:verify(?LABEL, raw(Key, <<16#82, 16#01, 16#02>>), Profile)).

a_tbs_with_a_duplicate_key_is_malformed(#{key := Key, profile := Profile}) ->
    Alg = <<16#63, "alg", (cbor_text(alg(Profile)))/binary>>,
    ?assertEqual({error, malformed},
                 macula_signed_object:verify(?LABEL, raw(Key, <<16#A2, Alg/binary, Alg/binary>>), Profile)).

a_tbs_with_trailing_bytes_is_malformed(#{key := Key, profile := Profile}) ->
    Tbs = <<16#A1, 16#63, "alg", (cbor_text(alg(Profile)))/binary, 16#00>>,
    ?assertEqual({error, malformed}, macula_signed_object:verify(?LABEL, raw(Key, Tbs), Profile)).

a_tbs_without_alg_is_malformed(#{key := Key, profile := Profile}) ->
    ?assertEqual({error, malformed},
                 macula_signed_object:verify(?LABEL, raw(Key, <<16#A1, 16#64, "type", 16#01>>), Profile)).

alg_naming_the_other_profile_is_refused(#{key := Key, profile := Profile}) ->
    Tbs = <<16#A1, 16#63, "alg", (cbor_text(alg(other_profile(Profile))))/binary>>,
    ?assertEqual({error, alg_mismatch}, macula_signed_object:verify(?LABEL, raw(Key, Tbs), Profile)).

%%------------------------------------------------------------------
%% Wire form
%%------------------------------------------------------------------

a_carried_object_round_trips_through_its_wire_form(#{key := Key}) ->
    Object = object(Key),
    ?assertEqual({ok, Object}, macula_signed_object:decode(macula_signed_object:encode(Object))).

a_held_object_round_trips_through_its_wire_form(#{key := Key}) ->
    Held = held(Key),
    ?assertEqual({ok, Held}, macula_signed_object:decode(macula_signed_object:encode(Held))).

decode_accepts_keys_in_any_order(#{key := Key}) ->
    #{key := K, tbs := T, signature := S} = Object = object(Key),
    Bytes = <<16#A3, (cbor_text(<<"signature">>))/binary, (cbor_bytes(S))/binary, (cbor_text(<<"tbs">>))/binary,
              (cbor_bytes(T))/binary, (cbor_text(<<"key">>))/binary, (cbor_bytes(K))/binary>>,
    ?assertEqual({ok, Object}, macula_signed_object:decode(Bytes)).

decode_refuses_an_extra_key(#{key := Key}) ->
    #{key := K, tbs := T, signature := S} = object(Key),
    Map = #{{text, <<"key">>} => K, {text, <<"tbs">>} => T, {text, <<"signature">>} => S, {text, <<"extra">>} => <<>>},
    ?assertEqual({error, malformed}, macula_signed_object:decode(macula_record_cbor:encode(Map))).

decode_refuses_a_value_that_is_not_bytes(#{key := Key}) ->
    #{signature := S} = held(Key),
    Map = #{{text, <<"tbs">>} => {text, <<"x">>}, {text, <<"signature">>} => S},
    ?assertEqual({error, malformed}, macula_signed_object:decode(macula_record_cbor:encode(Map))).

decode_refuses_trailing_bytes(#{key := Key}) ->
    ?assertEqual({error, malformed},
                 macula_signed_object:decode(<<(macula_signed_object:encode(held(Key)))/binary, 16#00>>)).

decode_refuses_malformed_bytes(_Keys) ->
    ?assertEqual({error, malformed}, macula_signed_object:decode(<<16#FF>>)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

keys(Profile) ->
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Other} = macula_node_keys:generate(identity, Profile),
    #{key => Key, other => Other, profile => Profile}.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

object(Key) ->
    macula_signed_object:sign(?LABEL, ?FIELDS, Key).

held(Key) ->
    macula_signed_object:sign_held(?LABEL, ?FIELDS, Key).

%% An object signed over hand-built tbs bytes.
raw(Key, Tbs) ->
    Carried = macula_node_keys:public_key(Key),
    Signature = macula_node_keys:sign([?LABEL, 0, crypto:hash(sha384, Carried), Tbs], Key),
    #{key => Carried, tbs => Tbs, signature => Signature}.

expected_fields(Profile) ->
    maps:put({text, <<"alg">>}, {text, alg(Profile)}, ?FIELDS).

alg(pq_pure) -> <<"ML-DSA-87">>;
alg(pq_hybrid) -> <<"ML-DSA-87-PS384">>.

other_profile(pq_pure) -> pq_hybrid;
other_profile(pq_hybrid) -> pq_pure.

cbor_text(Bin) ->
    macula_record_cbor:encode({text, Bin}).

cbor_bytes(Bin) ->
    macula_record_cbor:encode(Bin).

flip(<<Head:10/binary, Byte, Tail/binary>>) ->
    <<Head/binary, (Byte bxor 1), Tail/binary>>.
