%% The wire 1.0 conformance vectors (priv/vectors/wire_1_0/, docs/design/WIRE_1_0.md, macula#97), checked for what
%% holds before any wire 1.0 code exists: every file has the format's shape, every id is unique, every key id derives
%% from its key, every object and frame meets the decoding rule, every record's signature verifies (or not) as the
%% file says with the bare primitive, and every custodian case names a reader case. The conformance runner of each
%% class arrives with the issue its cases name in lands_with.
-module(macula_wire_1_0_vectors_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CLASSES, [<<"verify">>, <<"custodian">>, <<"check_order">>, <<"envelope">>, <<"enums">>,
                  <<"dht_procedures">>, <<"handshake">>, <<"tbs">>, <<"frames">>]).
-define(OUTCOMES, [<<"accept">>, <<"refuse">>, <<"ignore">>, <<"fallback">>, <<"store">>, <<"abort_stream">>]).
-define(PROFILES, [<<"pq_pure">>, <<"pq_hybrid">>]).
-define(RECORD_LABEL, "MACULA-PQ-RECORD-V1").
-define(REQUEST_LABEL, "MACULA-PQ-REQUEST-V1").
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
              <<"lands_with">> := [_ | _] = Lands, <<"input">> := Input,
              <<"expect">> := #{<<"outcome">> := Outcome} = Expect}) ->
    ?assert(is_binary(Id)),
    ?assert(lists:all(fun(L) -> binary:match(L, <<"#">>) =/= nomatch end, Lands)),
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

%% Every verify and check_order object decodes, its tbs meets the decoding rule, its key is its signer's in keys.json
%% (of key_profile where the case names one), and its signature verifies exactly when the case says so; a key in the
%% other profile's form does not verify under the case's profile. A case's `label', where it names one, is the label
%% its signing input is built with; a case's `request' is the object it answers, and verifies under its own signer.
records_test_() ->
    [{binary_to_list(maps:get(<<"id">>, C)), fun() -> record_checked(C) end}
     || Class <- [<<"verify">>, <<"check_order">>], C <- cases(Class)].

record_checked(#{<<"profile">> := P,
                 <<"input">> := #{<<"object">> := Hex, <<"signer">> := Signer, <<"signature_valid">> := Valid} = In}) ->
    {ok, #{key := Key, tbs := Tbs, signature := Sig}} = macula_signed_object:decode(binary:decode_hex(Hex)),
    ?assertMatch({ok, _}, macula_record_cbor:decode_strict(Tbs)),
    ?assertEqual(Key, signer_key(maps:get(<<"key_profile">>, In, P), Signer)),
    Message = <<(label(In))/binary, 0, (crypto:hash(sha384, Key))/binary, Tbs/binary>>,
    ?assertEqual(Valid =:= 1, catch macula_node_keys:verify(Message, Sig, Key, binary_to_atom(P))),
    answered(maps:get(<<"request">>, In, undefined), P).

label(#{<<"label">> := Label}) -> Label;
label(_RecordCase) -> <<?RECORD_LABEL>>.

%% The request a reply case answers signs under the request label and verifies under its own signer's key.
answered(undefined, _P) -> ok;
answered(Hex, P) ->
    {ok, #{key := Key, tbs := Tbs, signature := Sig}} = macula_signed_object:decode(binary:decode_hex(Hex)),
    Message = <<?REQUEST_LABEL, 0, (crypto:hash(sha384, Key))/binary, Tbs/binary>>,
    ?assertEqual(true, catch macula_node_keys:verify(Message, Sig, Key, binary_to_atom(P))).

signer_key(Profile, Role) ->
    #{Profile := #{Role := #{<<"key">> := Key}}} = maps:get(<<"profiles">>, vectors(<<"keys">>)),
    binary:decode_hex(Key).

%% A custodian's slot for a tag it does not know: SHA-256 over MACULA-PQ-STORAGE-KEY-V1, 0x00, the type, the
%% MACULA-KEY-ID-V1 key id of the key as carried (even an identity key) and the length-prefixed subject (5.3).
unknown_tag_slots_test_() ->
    Objects = #{Id => C || #{<<"id">> := Id} = C <- cases(<<"verify">>)},
    [{binary_to_list(Id), fun() -> slot_checked(P, maps:get(Ref, Objects), Slot) end}
     || #{<<"id">> := Id, <<"profile">> := P, <<"input">> := #{<<"object_of">> := Ref},
          <<"expect">> := #{<<"slot">> := Slot}} <- cases(<<"custodian">>)].

slot_checked(P, #{<<"input">> := #{<<"object">> := Hex}}, Slot) ->
    {ok, #{key := Key, tbs := Tbs}} = macula_signed_object:decode(binary:decode_hex(Hex)),
    {ok, Fields} = macula_record_cbor:decode_strict(Tbs),
    {Type, Subject} = slotted(Fields),
    KeyId = macula_node_keys:key_id(Key, binary_to_atom(P)),
    ?assertEqual(binary:decode_hex(Slot),
                 crypto:hash(sha256, <<"MACULA-PQ-STORAGE-KEY-V1", 0, Type, KeyId/binary, (byte_size(Subject)):32,
                                       Subject/binary>>)).

%% The tag and subject a slot is derived from: the record's own, or for a tombstone the tag it withdraws and the
%% subject its payload names (WIRE_1_0.md 5.1).
slotted(#{{text, <<"type">>} := 16#0C, {text, <<"payload">>} := #{{text, <<"withdrawn_type">>} := Type,
                                                                    {text, <<"subject">>} := Subject}}) ->
    {Type, Subject};
slotted(#{{text, <<"type">>} := Type, {text, <<"subject">>} := Subject}) ->
    {Type, Subject}.

%% Every STATUS case's statement verifies against the sender it names, as the handshake verified it.
status_statements_test_() ->
    [{binary_to_list(Id), fun() -> statement_checked(binary_to_atom(P), F, Peer, Now) end}
     || #{<<"id">> := Id, <<"profile">> := P,
          <<"input">> := #{<<"frame">> := F, <<"status_peer">> := Peer, <<"now_ms">> := Now}} <- cases(<<"envelope">>)].

statement_checked(P, Hex, #{<<"identity">> := Role, <<"binding">> := #{<<"tbs">> := Tbs, <<"signature">> := Sig}},
                  Now) ->
    #{{text, <<"statement">>} := #{{text, <<"tbs">>} := STbs, {text, <<"signature">>} := SSig}} = decoded(Hex),
    Binding = #{tbs => binary:decode_hex(Tbs), signature => binary:decode_hex(Sig)},
    ?assertMatch({ok, _}, macula_key_bindings:verify_status(#{tbs => STbs, signature => SSig}, Binding,
                                                            signer_key(atom_to_binary(P), Role), P, Now)).

%% A caller's stream frames held against their STREAM_OPEN start at seq 0 (macula_frame's open_stream).
caller_stream_seq_test_() ->
    [{binary_to_list(Id), fun() -> ?assertEqual(0, caller_seq(Frame)) end}
     || #{<<"id">> := Id, <<"input">> := #{<<"frame">> := Frame}} <- cases(<<"enums">>), caller_seq(Frame) =/= none].

caller_seq(Hex) ->
    {ok, Frame} = macula_record_cbor:decode_strict(binary:decode_hex(Hex)),
    caller_stream_seq(maps:find({text, <<"caller_stream">>}, Frame)).

caller_stream_seq({ok, #{{text, <<"tbs">>} := Tbs}}) ->
    {ok, #{{text, <<"seq">>} := Seq}} = macula_record_cbor:decode_strict(Tbs),
    Seq;
caller_stream_seq(error) ->
    none.

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

%% Every transcript step decodes; the CONNECT proof verifies over its proof_message under the CONNECT key the frame
%% carries, and the session proof over its session_proof_message under the station's identity key from keys.json.
handshake_steps_test_() ->
    [{binary_to_list(Id), fun() -> [step_checked(binary_to_atom(P), S) || S <- Steps] end}
     || #{<<"id">> := Id, <<"profile">> := P, <<"input">> := #{<<"steps">> := Steps}} <- cases(<<"handshake">>)].

step_checked(P, #{<<"frame">> := Hex} = Step) ->
    {ok, Frame} = macula_record_cbor:decode_strict(binary:decode_hex(Hex)),
    proof_checked(P, Frame, Step),
    session_proof_checked(P, Frame, Step).

proof_checked(P, #{{text, <<"proof">>} := Proof, {text, <<"connect_key">>} := Key}, #{<<"proof_message">> := Msg}) ->
    ?assert(macula_node_keys:verify(binary:decode_hex(Msg), Proof, Key, P));
proof_checked(_P, _Frame, _Step) ->
    ok.

session_proof_checked(P, #{{text, <<"session_proof">>} := Proof}, #{<<"session_proof_message">> := Msg}) ->
    Key = signer_key(atom_to_binary(P), <<"station_identity">>),
    ?assert(macula_node_keys:verify(binary:decode_hex(Msg), Proof, Key, P));
session_proof_checked(_P, _Frame, _Step) ->
    ok.

%% A tbs case's typed fields encode to exactly its tbs, its signing input is the label, a zero byte, the key hash
%% where the form is keyed, and the tbs, and its signature verifies over that input under the signer's key.
tbs_test_() ->
    [{binary_to_list(Id), fun() -> tbs_checked(C) end} || #{<<"id">> := Id} = C <- cases(<<"tbs">>)].

tbs_checked(#{<<"profile">> := P,
              <<"input">> := #{<<"label">> := Label, <<"signer">> := Role, <<"fields">> := Fields, <<"form">> := Form},
              <<"expect">> := #{<<"tbs">> := TbsHex, <<"signing_input">> := InputHex, <<"signature">> := SigHex}}) ->
    Tbs = binary:decode_hex(TbsHex),
    Key = signer_key(P, Role),
    ?assertEqual(Tbs, macula_record_cbor:encode(from_tj(Fields))),
    Input = signing_input(Form, Label, Key, Tbs),
    ?assertEqual(binary:decode_hex(InputHex), Input),
    ?assert(macula_node_keys:verify(Input, binary:decode_hex(SigHex), Key, binary_to_atom(P))).

signing_input(<<"keyed">>, Label, Key, Tbs) -> <<Label/binary, 0, (crypto:hash(sha384, Key))/binary, Tbs/binary>>;
signing_input(<<"unkeyed">>, Label, _Key, Tbs) -> <<Label/binary, 0, Tbs/binary>>.

%% Typed JSON back to the CBOR encoder's terms (FORMAT.md, Values).
from_tj(#{<<"$hex">> := Hex} = M) when map_size(M) =:= 1 -> binary:decode_hex(Hex);
from_tj(M) when is_map(M) -> #{{text, K} => from_tj(V) || K := V <- M};
from_tj(L) when is_list(L) -> [from_tj(V) || V <- L];
from_tj(B) when is_binary(B) -> {text, B};
from_tj(V) -> V.

%% Every frame type has one frame: version 5, its own frame_type, no capabilities header.
frames_test_() ->
    [{binary_to_list(Type), fun() -> frame_checked(Type, Hex) end}
     || #{<<"input">> := #{<<"frame_type">> := Type, <<"frame">> := Hex}} <- cases(<<"frames">>)].

frame_checked(Type, Hex) ->
    {ok, Frame} = macula_record_cbor:decode_strict(binary:decode_hex(Hex)),
    ?assertMatch(#{{text, <<"version">>} := 5, {text, <<"frame_type">>} := {text, Type}}, Frame),
    ?assertNot(is_map_key({text, <<"capabilities">>}, Frame)),
    ?assertNot(null_field(Frame)).

%% Absent is omitted (section 3.1): no frame a stack must accept carries null in a protocol field, at any depth.
accepted_frames_carry_no_null_test_() ->
    [{binary_to_list(Id), fun() -> ?assertNot(null_field(decoded(F))) end}
     || Class <- [<<"envelope">>, <<"enums">>, <<"frames">>],
        #{<<"id">> := Id, <<"input">> := #{<<"frame">> := F}, <<"expect">> := #{<<"outcome">> := <<"accept">>}}
            <- cases(Class)].

null_field(M) when is_map(M) ->
    lists:any(fun({{text, K}, _V}) when K =:= <<"payload">>; K =:= <<"value">> -> false;
                 ({_K, V}) -> V =:= null orelse null_field(V)
              end, maps:to_list(M));
null_field(L) when is_list(L) -> lists:any(fun(V) -> V =:= null orelse null_field(V) end, L);
null_field(_V) -> false.

decoded(Hex) ->
    {ok, Frame} = macula_record_cbor:decode_strict(binary:decode_hex(Hex)),
    Frame.

dht_values_test_() ->
    [{binary_to_list(Id), fun() -> decodes(maps:get(<<"args">>, Input, maps:get(<<"reply">>, Input, none))) end}
     || #{<<"id">> := Id, <<"input">> := Input} <- cases(<<"dht_procedures">>)].

decodes(Hex) ->
    ?assertMatch({ok, _}, macula_record_cbor:decode_strict(binary:decode_hex(Hex))).

cases(Class) -> maps:get(<<"cases">>, vectors(Class)).

vectors(Class) ->
    {ok, Bytes} = file:read_file(vector_file(<<Class/binary, ".json">>)),
    json:decode(Bytes).

%% The vectors ship in priv, so a consumer of the hex package reads the same files from code:priv_dir(macula).
vector_file(Name) ->
    filename:join([code:priv_dir(macula), "vectors", "wire_1_0", Name]).

