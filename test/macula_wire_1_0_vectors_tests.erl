%% The wire 1.0 conformance vectors (test/vectors/wire_1_0/, docs/design/WIRE_1_0.md, macula#97), checked for what
%% holds before any wire 1.0 code exists: every file has the format's shape, every id is unique, every key id derives
%% from its key, every object and frame meets the decoding rule, every record's signature verifies (or not) as the
%% file says with the bare primitive, and every custodian case names a reader case. The conformance runner of each
%% class arrives with the issue its cases name in lands_with.
-module(macula_wire_1_0_vectors_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CLASSES, [<<"verify">>, <<"custodian">>, <<"check_order">>, <<"envelope">>, <<"enums">>,
                  <<"dht_procedures">>]).
-define(OUTCOMES, [<<"accept">>, <<"refuse">>, <<"ignore">>, <<"fallback">>, <<"store">>, <<"abort_stream">>]).
-define(PROFILES, [<<"pq_pure">>, <<"pq_hybrid">>]).
-define(RECORD_LABEL, "MACULA-PQ-RECORD-V1").
-define(SIGNED_FIELDS, [{<<"request">>, <<"MACULA-PQ-REQUEST-V1">>}, {<<"publication">>, <<"MACULA-PQ-PUBLICATION-V1">>},
                        {<<"relay_error">>, <<"MACULA-PQ-RELAY-ERROR-V1">>},
                        {<<"caller_stream">>, <<"MACULA-PQ-CALLER-STREAM-V1">>}]).

headers_test_() ->
    [{binary_to_list(C), fun() -> header_checked(C, vectors(C)) end} || C <- ?CLASSES].

header_checked(Class, File) ->
    ?assertMatch(#{<<"wire">> := <<"1.0">>, <<"class">> := Class, <<"format">> := 1,
                   <<"spec">> := <<"docs/design/WIRE_1_0.md">>, <<"cases">> := [_ | _]}, File).

ids_unique_test() ->
    Ids = [maps:get(<<"id">>, C) || Class <- ?CLASSES, C <- cases(Class)],
    ?assertEqual(length(Ids), length(lists:usort(Ids))).

cases_shaped_test_() ->
    [{binary_to_list(maps:get(<<"id">>, C)), fun() -> case_shaped(C) end} || Class <- ?CLASSES, C <- cases(Class)].

case_shaped(#{<<"id">> := Id, <<"profile">> := Profile, <<"covers">> := [_ | _] = Covers,
              <<"lands_with">> := <<"macula#", _/binary>>, <<"input">> := Input,
              <<"expect">> := #{<<"outcome">> := Outcome} = Expect}) ->
    ?assert(is_binary(Id)),
    ?assert(lists:member(Profile, [<<"both">> | ?PROFILES])),
    ?assert(lists:all(fun covers_known/1, Covers)),
    ?assert(is_map(Input)),
    ?assert(lists:member(Outcome, ?OUTCOMES)),
    ?assert(expect_complete(Outcome, Expect)).

covers_known(<<"A">>) -> true;
covers_known(<<"A", N/binary>>) -> lists:member(N, [<<"1">>, <<"2">>, <<"3">>]);
covers_known(<<"D", N/binary>>) -> lists:member(binary_to_integer(N), lists:seq(1, 10)).

expect_complete(<<"refuse">>, #{<<"reason">> := R}) -> is_binary(R);
expect_complete(<<"ignore">>, #{<<"count">> := C}) -> is_binary(C);
expect_complete(<<"fallback">>, #{<<"count">> := C, <<"value">> := _}) -> is_binary(C);
expect_complete(<<"abort_stream">>, #{<<"quic_code">> := Code}) -> is_integer(Code);
expect_complete(Outcome, _Expect) -> lists:member(Outcome, [<<"accept">>, <<"store">>]).

key_ids_test_() ->
    [{binary_to_list(<<P/binary, " ", R/binary>>), fun() -> key_id_derived(R, binary_to_atom(P), E) end}
     || P := Roles <- maps:get(<<"profiles">>, vectors(<<"keys">>)), R := E <- Roles].

key_id_derived(Role, Profile, #{<<"key">> := Key, <<"key_id">> := KeyId}) ->
    ?assertEqual(binary:decode_hex(KeyId), derived_id(Role, binary:decode_hex(Key), Profile)).

derived_id(<<"station_identity">>, Key, Profile) -> macula_node_keys:node_id(Key, Profile);
derived_id(<<"client_identity">>, Key, Profile) -> macula_node_keys:node_id(Key, Profile);
derived_id(_Role, Key, Profile) -> macula_node_keys:key_id(Key, Profile).

%% Every record object decodes, its tbs meets the decoding rule, its key is its signer's in keys.json, and its
%% signature verifies exactly when the case says so.
records_test_() ->
    [{binary_to_list(maps:get(<<"id">>, C)), fun() -> record_checked(C) end}
     || Class <- [<<"verify">>, <<"check_order">>], C <- cases(Class)].

record_checked(#{<<"profile">> := P,
                 <<"input">> := #{<<"object">> := Hex, <<"signer">> := Signer, <<"signature_valid">> := Valid}}) ->
    {ok, #{key := Key, tbs := Tbs, signature := Sig}} = macula_signed_object:decode(binary:decode_hex(Hex)),
    ?assertMatch({ok, _}, macula_record_cbor:decode_strict(Tbs)),
    ?assertEqual(Key, signer_key(P, Signer)),
    Message = <<?RECORD_LABEL, 0, (crypto:hash(sha384, Key))/binary, Tbs/binary>>,
    ?assertEqual(Valid =:= 1, macula_node_keys:verify(Message, Sig, Key, binary_to_atom(P))).

signer_key(Profile, Role) ->
    #{Profile := #{Role := #{<<"key">> := Key}}} = maps:get(<<"profiles">>, vectors(<<"keys">>)),
    binary:decode_hex(Key).

custodian_refs_test() ->
    ReaderIds = [maps:get(<<"id">>, C) || C <- cases(<<"verify">>)],
    [?assert(lists:member(Ref, ReaderIds)) || #{<<"input">> := #{<<"object_of">> := Ref}} <- cases(<<"custodian">>)].

envelope_frames_test_() ->
    [{binary_to_list(Id), fun() -> ?assertMatch({ok, #{}}, macula_record_cbor:decode_strict(binary:decode_hex(F))) end}
     || #{<<"id">> := Id, <<"input">> := #{<<"frame">> := F}} <- cases(<<"envelope">>)].

%% Every enum frame and its context frames, and every _dht.* argument and reply, meet the decoding rule.
enum_frames_test_() ->
    [{binary_to_list(Id), fun() -> [decodes(F) || F <- [Frame | Ctx]] end}
     || #{<<"id">> := Id, <<"input">> := #{<<"frame">> := Frame, <<"context">> := Ctx}} <- cases(<<"enums">>)].

%% Every signed object inside an enum frame verifies over its tbs as received, under its label: the object's own key,
%% or for a held caller stream frame the caller's key from keys.json.
enum_signatures_test_() ->
    [{binary_to_list(Id), fun() -> [signed_ok(P, F) || F <- [Frame | Ctx]] end}
     || #{<<"id">> := Id, <<"profile">> := P, <<"input">> := #{<<"frame">> := Frame, <<"context">> := Ctx}}
            <- cases(<<"enums">>), P =/= <<"both">>].

signed_ok(P, Hex) ->
    {ok, Frame} = macula_record_cbor:decode_strict(binary:decode_hex(Hex)),
    [?assert(object_verifies(P, Label, Object)) || {Field, Label} <- ?SIGNED_FIELDS,
                                                   {ok, Object} <- [maps:find({text, Field}, Frame)]].

object_verifies(P, Label, #{{text, <<"tbs">>} := Tbs, {text, <<"signature">>} := Sig} = Object) ->
    Key = maps:get({text, <<"key">>}, Object, signer_key(P, <<"client_identity">>)),
    Message = <<Label/binary, 0, (crypto:hash(sha384, Key))/binary, Tbs/binary>>,
    macula_node_keys:verify(Message, Sig, Key, binary_to_atom(P)).

dht_values_test_() ->
    [{binary_to_list(Id), fun() -> decodes(maps:get(<<"args">>, Input, maps:get(<<"reply">>, Input, none))) end}
     || #{<<"id">> := Id, <<"input">> := Input} <- cases(<<"dht_procedures">>)].

decodes(Hex) ->
    ?assertMatch({ok, _}, macula_record_cbor:decode_strict(binary:decode_hex(Hex))).

cases(Class) -> maps:get(<<"cases">>, vectors(Class)).

vectors(Class) ->
    {ok, Bytes} = file:read_file(vector_file(<<Class/binary, ".json">>)),
    json:decode(Bytes).

%% The source tree's vector file, from the project root eunit runs in, or from the build tree's copy of the
%% application.
vector_file(Name) ->
    Rel = filename:join("wire_1_0", Name),
    first_existing([filename:join("test/vectors", Rel), filename:join("../../test/vectors", Rel)]
                   ++ [filename:join([Dir, "..", "..", "..", "..", "test", "vectors", Rel])
                       || Dir <- [code:lib_dir(macula)], is_list(Dir)]).

first_existing([F | Rest]) ->
    first_existing(filelib:is_regular(F), F, Rest).

first_existing(true, F, _Rest) -> F;
first_existing(false, _F, Rest) -> first_existing(Rest).
