%% A provider's procedure advertisement may carry its KEM key (E2E design,
%% amendment A1): `kem_key', the key as carried, and `kem_key_id', the first
%% 8 bytes of SHA-384 over it, only as a pair. The rule sits in the record's
%% payload check, so sign/2, verify/2 and every station's STORE and ADVERTISE
%% admission refuse a lone field or a mismatched pair identically.
-module(macula_record_kem_key_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LABEL, <<"MACULA-PQ-RECORD-V1">>).

%% A keyed advertisement signs, verifies and reads back its key and id, with
%% a pq_pure key (1568 bytes) and a pq_hybrid one (1665), in its own namespace
%% and with an authorization.
a_keyed_advertisement_signs_verifies_and_reads_test_() ->
    Id = key(),
    [{Name, fun() ->
          Record = macula_record:sign(advertisement(Id, Procedure, Opts#{kem_key => Kem}), Id),
          {ok, Verified} = macula_record:verify(macula_record:encode(Record), pq_pure),
          ?assertMatch(#{kem_key := Kem, kem_key_id := <<_:64>>}, macula_record:read_procedure_advertisement(Verified)),
          ?assertEqual(macula_seal:key_id(Kem), maps:get(kem_key_id, macula_record:read_procedure_advertisement(Verified)))
      end}
     || {Name, Kem} <- [{"pq_pure key", kem_key(macula_seal:carried_key_size(pq_pure))},
                        {"pq_hybrid key", kem_key(macula_seal:carried_key_size(pq_hybrid))}],
        {Procedure, Opts} <- [{own(Id), #{}}, {<<"acme/count_v1">>, #{authorization => authorization()}}]].

%% An advertisement without the pair reads as naming no key.
an_advertisement_without_a_key_names_none_test() ->
    Id = key(),
    Record = macula_record:sign(advertisement(Id, own(Id), #{}), Id),
    {ok, Verified} = macula_record:verify(macula_record:encode(Record), pq_pure),
    Read = macula_record:read_procedure_advertisement(Verified),
    ?assertNot(is_map_key(kem_key, Read)),
    ?assertNot(is_map_key(kem_key_id, Read)).

%% The builder refuses a key of another length before signing.
the_builder_refuses_a_key_of_another_length_test_() ->
    Id = key(),
    [?_assertError({malformed, 16#06}, macula_record:sign(advertisement(Id, own(Id), #{kem_key => kem_key(Size)}), Id))
     || Size <- [0, 1567, 1569, 1664, 1666]].

%% sign/2 refuses a lone field or a mismatched pair, as verify does: both run the same payload check.
a_lone_or_mismatched_field_is_refused_at_sign_test_() ->
    Id = key(),
    Kem = kem_key(1568),
    Unsigned = advertisement(Id, own(Id), #{}),
    [?_assertError({malformed, 16#06},
                   macula_record:sign(Unsigned#{payload := maps:merge(maps:get(payload, Unsigned), Extra)}, Id))
     || Extra <- [#{{text, <<"kem_key">>} => Kem},
                  #{{text, <<"kem_key_id">>} => macula_seal:key_id(Kem)},
                  #{{text, <<"kem_key">>} => Kem, {text, <<"kem_key_id">>} => <<0:64>>}]].

%% A record signed past the builder, with a lone field, a mismatched pair or a
%% key of another length, is refused at verify as malformed.
a_lone_or_mismatched_field_is_refused_at_verify_test_() ->
    Id = key(),
    Kem = kem_key(1568),
    KemId = macula_seal:key_id(Kem),
    Base = macula_record:sign(advertisement(Id, own(Id), #{}), Id),
    [{Name, ?_assertEqual({error, malformed}, macula_record:verify(signed_with(Base, Extra, Id), pq_pure))}
     || {Name, Extra} <- [{"a lone kem_key", #{<<"kem_key">> => Kem}},
                          {"a lone kem_key_id", #{<<"kem_key_id">> => KemId}},
                          {"a mismatched id", #{<<"kem_key">> => Kem, <<"kem_key_id">> => <<0:64>>}},
                          {"a key of another length", #{<<"kem_key">> => <<Kem/binary, 0>>,
                                                        <<"kem_key_id">> => macula_seal:key_id(<<Kem/binary, 0>>)}},
                          {"an id of another length", #{<<"kem_key">> => Kem,
                                                        <<"kem_key_id">> => <<KemId/binary, 0>>}},
                          {"a key that is text", #{<<"kem_key">> => {text, <<"k">>}, <<"kem_key_id">> => KemId}}]].

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

advertisement(Id, Procedure, Opts) ->
    macula_record:procedure_advertisement(macula_node_keys:key_id(Id), fill(5), Procedure, fill(6), Opts).

%% The record Base, its payload extended with Extra, signed by hand past what sign/2 would build.
signed_with(#{type := Type, version := Version, created_at := Created, expires_at := Expires, payload := Payload},
            Extra, Key) ->
    Fields = #{{text, <<"type">>} => Type, {text, <<"version">>} => Version, {text, <<"created_at">>} => Created,
               {text, <<"expires_at">>} => Expires,
               {text, <<"payload">>} => maps:merge(Payload, maps:from_list([{{text, K}, V} || K := V <- Extra]))},
    macula_signed_object:encode(macula_signed_object:sign(?LABEL, Fields, Key)).

authorization() ->
    #{org_directory => <<"directory bytes">>, procedure_delegation => <<"delegation bytes">>}.

own(Id) ->
    <<"~", (binary:encode_hex(macula_node_keys:key_id(Id), lowercase))/binary, "/count_v1">>.

kem_key(Size) ->
    crypto:strong_rand_bytes(Size).

key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.

fill(Byte) ->
    binary:copy(<<Byte>>, 32).
