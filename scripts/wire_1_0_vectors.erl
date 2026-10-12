%% Writes the wire 1.0 conformance vectors in test/vectors/wire_1_0/ (docs/design/WIRE_1_0.md, macula#97). Run through
%% scripts/generate-wire-1-0-vectors.sh with the compiled tree on the code path.
%%
%% This is a reference encoder for wire 1.0, independent of the code that will implement it: it builds every object
%% from the deterministic CBOR encoder and the bare signing primitive, taking only payloads from today's record
%% constructors. ML-DSA-87 signing is hedged and pq_hybrid's RSA-PSS half is randomized, so a second run signs other
%% bytes: the committed files are the vectors, generated once. The keys are fixed (private_test_keys.etf, test only)
%% so a regeneration keeps keys.json.
-module(wire_1_0_vectors).

-export([main/0]).

-define(DIR, "test/vectors/wire_1_0").
-define(KEYS_FILE, ?DIR "/private_test_keys.etf").
-define(GENERATOR, <<"scripts/generate-wire-1-0-vectors.sh">>).
-define(T0, 1791800000000).
-define(MIN, 60000).
-define(HOUR, 3600000).
-define(DAY, 86400000).
-define(RECORD_LABEL, <<"MACULA-PQ-RECORD-V1">>).
-define(REQUEST_LABEL, <<"MACULA-PQ-REQUEST-V1">>).
-define(RELAY_ERROR_LABEL, <<"MACULA-PQ-RELAY-ERROR-V1">>).
-define(CALLER_STREAM_LABEL, <<"MACULA-PQ-CALLER-STREAM-V1">>).
-define(PUBLICATION_LABEL, <<"MACULA-PQ-PUBLICATION-V1">>).
-define(PROFILES, [pq_pure, pq_hybrid]).
-define(ROLES, [station_identity, client_identity, client_connect, org]).

main() ->
    {ok, _} = application:ensure_all_started(crypto),
    Keys = keys(),
    write(<<"keys">>, keys_file(Keys)),
    Records = lists:append([record_cases(P, Keys) || P <- ?PROFILES]),
    write_cases(<<"verify">>, [reader_case(C) || C <- Records]),
    write_cases(<<"custodian">>, [custodian_case(C) || C <- Records]),
    write_cases(<<"check_order">>, lists:append([check_order_cases(P, Keys) || P <- ?PROFILES])),
    write_cases(<<"envelope">>, envelope_cases()),
    write_cases(<<"enums">>, enum_hop_cases() ++ lists:append([enum_signed_cases(P, Keys) || P <- ?PROFILES])),
    write_cases(<<"dht_procedures">>, dht_cases(Keys)),
    halt(0).

%%------------------------------------------------------------------
%% Keys

keys() ->
    keys(file:read_file(?KEYS_FILE)).

keys({ok, Bin}) -> binary_to_term(Bin);
keys({error, enoent}) ->
    Keys = maps:from_list([{{P, R}, generated(P, R)} || P <- ?PROFILES, R <- ?ROLES]),
    ok = file:write_file(?KEYS_FILE, term_to_binary(Keys)),
    Keys.

generated(P, station_identity) -> ok_value(macula_node_keys:generate(identity, P));
generated(P, client_identity) -> ok_value(macula_node_keys:generate(identity, P, #{puzzle_difficulty => 8}));
generated(P, client_connect) -> ok_value(macula_node_keys:generate(connect, P));
generated(P, org) -> ok_value(macula_node_keys:generate(org, P)).

ok_value({ok, V}) -> V.

key(Keys, P, R) -> maps:get({P, R}, Keys).

carried(Key) -> macula_node_keys:public_key(Key).

key_id(Key) -> ok_or_value(macula_node_keys:key_id(Key)).

ok_or_value({ok, V}) -> V;
ok_or_value(V) when is_binary(V) -> V.

keys_file(Keys) ->
    #{<<"wire">> => <<"1.0">>, <<"class">> => <<"keys">>, <<"format">> => 1, <<"design">> => <<"macula#83">>,
      <<"generator">> => ?GENERATOR,
      <<"note">> => <<"public keys in carried form; key_id is the node_id for identity keys, else MACULA-KEY-ID-V1">>,
      <<"profiles">> =>
          maps:from_list([{atom_to_binary(P),
                           maps:from_list([{atom_to_binary(R), role_entry(key(Keys, P, R))} || R <- ?ROLES])}
                          || P <- ?PROFILES])}.

role_entry(Key) ->
    #{<<"key">> => hex(carried(Key)), <<"key_id">> => hex(key_id(Key))}.

%%------------------------------------------------------------------
%% Records (WIRE_1_0.md section 5)

%% A wire 1.0 record: the tbs built field by field, signed with the bare primitive. Opts change the tbs:
%% {set, Key, Value}, {drop, Key}, and the signature: corrupt_signature.
record(P, Signer, Type, Payload, Opts) ->
    Base = #{<<"type">> => Type, <<"alg">> => {text, macula_signed_object:alg(P)},
             <<"version">> => macula_record_uuid:v7(),
             <<"created_at">> => ?T0, <<"expires_at">> => ?T0 + ?HOUR,
             <<"schema">> => 1, <<"lifetime_class">> => class_of(Type), <<"payload">> => Payload},
    Tbs = encode(lists:foldl(fun tbs_opt/2, Base, Opts)),
    Signature = signature(macula_node_keys:sign(signed_bytes(?RECORD_LABEL, carried(Signer), Tbs), Signer), Opts),
    macula_signed_object:encode(#{key => carried(Signer), tbs => Tbs, signature => Signature}).

tbs_opt({set, K, V}, M) -> M#{K => V};
tbs_opt({drop, K}, M) -> maps:remove(K, M);
tbs_opt(_Other, M) -> M.

signature(<<First, Rest/binary>>, Opts) ->
    case lists:member(corrupt_signature, Opts) of
        true -> <<(First bxor 1), Rest/binary>>;
        false -> <<First, Rest/binary>>
    end.

signed_bytes(Label, Carried, Tbs) ->
    <<Label/binary, 0, (crypto:hash(sha384, Carried))/binary, Tbs/binary>>.

class_of(16#01) -> 3;
class_of(16#11) -> 3;
class_of(16#06) -> 0;
class_of(16#12) -> 0;
class_of(T) when T >= 16#20 -> 4;
class_of(_) -> 5.

%% Each record case: {Id, Profile, Covers, LandsWith, ObjectBytes, Signer role, NowMs, CustodianExpect, ReaderExpect}.
record_cases(P, Keys) ->
    Client = key(Keys, P, client_identity),
    Station = key(Keys, P, station_identity),
    Org = key(Keys, P, org),
    Node = payload(macula_record:node_record(key_id(Client), [], 0)),
    Endpoint = payload(macula_record:station_endpoint(4433, #{host_advertised => [<<"2001:db8::7">>]})),
    Domain = #{{text, <<"note">>} => {text, <<"a domain payload, never read by a custodian">>}},
    Subject = {set, <<"subject">>, <<"subject-1">>},
    Now = ?T0 + 1000,
    Store = #{<<"outcome">> => <<"store">>},
    Accept = #{<<"outcome">> => <<"accept">>},
    R = fun(Reason) -> #{<<"outcome">> => <<"refuse">>, <<"reason">> => Reason} end,
    Both = fun(Reason) -> {R(Reason), R(Reason)} end,
    Case = fun(Name, Covers, Signer, Bytes, {Cust, Read}) ->
                   {<<"record.", Name/binary>>, P, Covers, <<"macula#100">>, Bytes, Signer, Now, Cust, Read}
           end,
    [Case(<<"node_record.ok">>, [<<"D4">>], client_identity, record(P, Client, 16#01, Node, []), {Store, Accept}),
     Case(<<"station_endpoint.ok">>, [<<"D4">>], station_identity,
          record(P, Station, 16#12, Endpoint, [{set, <<"expires_at">>, ?T0 + 5 * ?MIN}]), {Store, Accept}),
     Case(<<"domain_type.with_subject.ok">>, [<<"D4">>, <<"A3">>], org,
          record(P, Org, 16#20, Domain, [Subject]), {Store, Accept}),
     Case(<<"unknown_builtin_tag.with_subject.stored">>, [<<"D4">>, <<"A3">>], org,
          record(P, Org, 16#1E, Domain, [Subject]), {Store, R(<<"type_unknown">>)}),
     Case(<<"unknown_builtin_tag.no_subject.refused">>, [<<"D4">>, <<"A3">>], org,
          record(P, Org, 16#1E, Domain, []), Both(<<"subject_missing">>)),
     Case(<<"reserved_successor_tag.stored">>, [<<"D7">>], org,
          record(P, Org, 16#14, Domain, [Subject]), {Store, R(<<"type_unknown">>)}),
     Case(<<"schema_missing.refused">>, [<<"D4">>], client_identity,
          record(P, Client, 16#01, Node, [{drop, <<"schema">>}]), Both(<<"malformed">>)),
     Case(<<"schema_zero.refused">>, [<<"D4">>], client_identity,
          record(P, Client, 16#01, Node, [{set, <<"schema">>, 0}]), Both(<<"malformed">>)),
     Case(<<"schema_unknown.stored">>, [<<"D4">>], client_identity,
          record(P, Client, 16#01, Node, [{set, <<"schema">>, 2}]), {Store, R(<<"schema_unsupported">>)}),
     Case(<<"lifetime_class_missing.refused">>, [<<"D4">>], client_identity,
          record(P, Client, 16#01, Node, [{drop, <<"lifetime_class">>}]), Both(<<"malformed">>)),
     Case(<<"lifetime_class_unknown.refused">>, [<<"D4">>], client_identity,
          record(P, Client, 16#01, Node, [{set, <<"lifetime_class">>, 6}]), Both(<<"lifetime_class_unknown">>)),
     Case(<<"lifetime_class_wrong_row.stored">>, [<<"D4">>], client_identity,
          record(P, Client, 16#01, Node, [{set, <<"lifetime_class">>, 4}]),
          {Store, R(<<"lifetime_class_mismatch">>)}),
     Case(<<"lifetime_too_long.refused">>, [<<"D4">>], client_identity,
          record(P, Client, 16#01, Node, [{set, <<"expires_at">>, ?T0 + 48 * ?HOUR + 1}]),
          Both(<<"lifetime_too_long">>)),
     Case(<<"lifetime_at_class_bound.ok">>, [<<"D4">>], client_identity,
          record(P, Client, 16#01, Node, [{set, <<"expires_at">>, ?T0 + 48 * ?HOUR}]), {Store, Accept}),
     Case(<<"lifetime_reversed.refused">>, [<<"D4">>], client_identity,
          record(P, Client, 16#01, Node, [{set, <<"expires_at">>, ?T0}]), Both(<<"lifetime_reversed">>)),
     Case(<<"not_yet_valid.refused">>, [<<"D4">>], client_identity,
          record(P, Client, 16#01, Node, [{set, <<"created_at">>, Now + 6 * ?MIN},
                                          {set, <<"expires_at">>, Now + 7 * ?MIN}]),
          Both(<<"not_yet_valid">>)),
     Case(<<"expired.refused">>, [<<"D4">>], client_identity,
          record(P, Client, 16#01, Node, [{set, <<"created_at">>, ?T0 - 2 * ?HOUR},
                                          {set, <<"expires_at">>, Now - 6 * ?MIN}]),
          Both(<<"expired">>)),
     Case(<<"extra_tbs_key.refused">>, [<<"D9">>], client_identity,
          record(P, Client, 16#01, Node, [{set, <<"colour">>, {text, <<"blue">>}}]), Both(<<"malformed">>)),
     Case(<<"ext_noncritical_unknown.ignored">>, [<<"D8">>], client_identity,
          record(P, Client, 16#01, Node, [{set, <<"ext">>, ext(<<"example.unknown">>, 0)}]),
          {Store, Accept#{<<"count">> => <<"wire_ext_ignored">>}}),
     Case(<<"ext_critical_unknown.refused_by_reader">>, [<<"D8">>], client_identity,
          record(P, Client, 16#01, Node, [{set, <<"ext">>, ext(<<"example.unknown">>, 1)}]),
          {Store, R(<<"extension_unsupported">>)}),
     Case(<<"signature_invalid.refused">>, [<<"D9">>], client_identity,
          record(P, Client, 16#01, Node, [corrupt_signature]), Both(<<"signature_invalid">>)),
     Case(<<"alg_unregistered.refused">>, [<<"D7">>], client_identity,
          record(P, Client, 16#01, Node, [{set, <<"alg">>, {text, <<"ML-DSA-65">>}}]), Both(<<"alg_unsupported">>)),
     Case(<<"content_id_tag_unknown.stored">>, [<<"D7">>], client_identity,
          record(P, Client, 16#11, content_payload(Client, 3), [{set, <<"expires_at">>, ?T0 + ?HOUR}]),
          {Store, R(<<"mcid_tag_unknown">>)})].

payload(#{payload := Payload}) -> Payload.

ext(Name, Critical) ->
    #{{text, Name} => #{{text, <<"critical">>} => Critical, {text, <<"value">>} => {text, <<"x">>}}}.

%% A content announcement whose content id carries Tag: built with tag 2 by today's constructor, then re-tagged.
content_payload(Signer, Tag) ->
    Mcid = <<2, 16#55, (crypto:hash(sha384, <<"wire 1.0 vector content">>))/binary>>,
    Payload = payload(macula_record:content_announcement(key_id(Signer), Mcid,
                                                         #{realm_id => <<1:256>>, serving_station => <<2:256>>,
                                                           procedure => <<"_content.fetch">>})),
    McidKey = {text, <<"mcid">>},
    <<2, Rest/binary>> = maps:get(McidKey, Payload),
    Payload#{McidKey := <<Tag, Rest/binary>>}.

reader_case({Id, P, Covers, Lands, Bytes, Signer, Now, _Cust, Read}) ->
    record_case(<<Id/binary, ".reader">>, P, Covers, Lands, Bytes, Signer, Now, Read).

%% A custodian case names the object of its reader case in verify.json instead of repeating its bytes.
custodian_case({Id, P, Covers, Lands, _Bytes, Signer, Now, Cust, _Read}) ->
    Profile = atom_to_binary(P),
    #{<<"id">> => <<Id/binary, ".custodian.", Profile/binary>>, <<"profile">> => Profile,
      <<"covers">> => Covers, <<"lands_with">> => Lands,
      <<"input">> => #{<<"object_of">> => <<Id/binary, ".reader.", Profile/binary>>,
                       <<"signer">> => atom_to_binary(Signer), <<"now_ms">> => Now},
      <<"expect">> => Cust}.

record_case(Id, P, Covers, Lands, Bytes, Signer, Now, Expect) ->
    #{<<"id">> => <<Id/binary, ".", (atom_to_binary(P))/binary>>, <<"profile">> => atom_to_binary(P),
      <<"covers">> => Covers, <<"lands_with">> => Lands,
      <<"input">> => #{<<"object">> => hex(Bytes), <<"signer">> => atom_to_binary(Signer), <<"now_ms">> => Now,
                       <<"signature_valid">> => signature_valid(Bytes, P)},
      <<"expect">> => Expect}.

%% 1 when the object's signature verifies over its tbs as received under its own key, with the bare primitive:
%% what a runner can check before it reaches any wire 1.0 code.
signature_valid(Bytes, P) ->
    {ok, #{key := Key, tbs := Tbs, signature := Sig}} = macula_signed_object:decode(Bytes),
    bit(macula_node_keys:verify(signed_bytes(?RECORD_LABEL, Key, Tbs), Sig, Key, P)).

bit(true) -> 1;
bit(false) -> 0.

%% Two faults in one object: the refusal is the first in WIRE_1_0.md section 5.2's order.
check_order_cases(P, Keys) ->
    Client = key(Keys, P, client_identity),
    Org = key(Keys, P, org),
    Node = payload(macula_record:node_record(key_id(Client), [], 0)),
    Domain = #{{text, <<"note">>} => {text, <<"x">>}},
    Expired = [{set, <<"created_at">>, ?T0 - 2 * ?HOUR}, {set, <<"expires_at">>, ?T0 - 10 * ?MIN}],
    Now = ?T0 + 1000,
    C = fun(Name, Role, Signer, Bytes, Reason) ->
                record_case(<<"check_order.", Name/binary, ".", Role/binary>>, P, [<<"D9">>], <<"macula#100">>,
                            Bytes, Signer, Now, #{<<"outcome">> => <<"refuse">>, <<"reason">> => Reason,
                                                  <<"role">> => Role})
        end,
    [C(<<"alg_before_signature">>, <<"custodian">>, client_identity,
       record(P, Client, 16#01, Node, [{set, <<"alg">>, {text, <<"ML-DSA-65">>}}, corrupt_signature]),
       <<"alg_unsupported">>),
     C(<<"signature_before_clock">>, <<"custodian">>, client_identity,
       record(P, Client, 16#01, Node, Expired ++ [corrupt_signature]), <<"signature_invalid">>),
     C(<<"tbs_keys_before_clock">>, <<"custodian">>, client_identity,
       record(P, Client, 16#01, Node, Expired ++ [{set, <<"colour">>, 1}]), <<"malformed">>),
     C(<<"clock_before_lifetime">>, <<"custodian">>, client_identity,
       record(P, Client, 16#01, Node, [{set, <<"created_at">>, Now + 10 * ?MIN},
                                       {set, <<"expires_at">>, Now + 10 * ?MIN + 49 * ?HOUR}]),
       <<"not_yet_valid">>),
     C(<<"clock_before_slot">>, <<"custodian">>, org, record(P, Org, 16#1E, Domain, Expired), <<"expired">>),
     C(<<"lifetime_before_slot">>, <<"custodian">>, org,
       record(P, Org, 16#1E, Domain, [{set, <<"lifetime_class">>, 9}]), <<"lifetime_class_unknown">>),
     C(<<"type_before_schema">>, <<"reader">>, org,
       record(P, Org, 16#1E, Domain, [{set, <<"subject">>, <<"s">>}, {set, <<"schema">>, 7}]), <<"type_unknown">>),
     C(<<"schema_before_class_row">>, <<"reader">>, client_identity,
       record(P, Client, 16#01, Node, [{set, <<"schema">>, 2}, {set, <<"lifetime_class">>, 4}]),
       <<"schema_unsupported">>),
     C(<<"class_row_before_ext">>, <<"reader">>, client_identity,
       record(P, Client, 16#01, Node, [{set, <<"lifetime_class">>, 4}, {set, <<"ext">>, ext(<<"example.unknown">>, 1)}]),
       <<"lifetime_class_mismatch">>),
     C(<<"ext_before_payload">>, <<"reader">>, client_identity,
       record(P, Client, 16#01, #{{text, <<"nonsense">>} => 1}, [{set, <<"ext">>, ext(<<"example.unknown">>, 1)}]),
       <<"extension_unsupported">>)].

%%------------------------------------------------------------------
%% Frame envelope (WIRE_1_0.md section 3), no signatures: profile both

envelope_cases() ->
    Ping = fun(Extra) -> maps:merge(#{<<"version">> => 5, <<"frame_type">> => {text, <<"liveness_ping">>},
                                      <<"nonce">> => <<16#5A:128>>}, Extra) end,
    Unknown = fun(Extra) -> maps:merge(#{<<"version">> => 5, <<"frame_type">> => {text, <<"example_future_frame">>}},
                                       Extra) end,
    Ext = fun(N) -> maps:from_list([{{text, <<"example.e", (integer_to_binary(I))/binary>>},
                                      #{{text, <<"critical">>} => 0, {text, <<"value">>} => I}} || I <- lists:seq(1, N)])
          end,
    Name = fun(Len) -> binary:copy(<<"a">>, Len) end,
    Accept = #{<<"outcome">> => <<"accept">>},
    Malformed = #{<<"outcome">> => <<"refuse">>, <<"reason">> => <<"malformed_frame">>},
    Ignore = #{<<"outcome">> => <<"ignore">>, <<"count">> => <<"wire_unknown_frame_type">>},
    E = fun(Id, Covers, Frame, Expect) -> envelope_case(Id, Covers, <<"control_stream">>, Frame, Expect) end,
    [E(<<"known.accept">>, [<<"D5">>], Ping(#{}), Accept),
     E(<<"known.optional_header.accept">>, [<<"D5">>],
       Ping(#{<<"frame_id">> => <<16#0190:16, 0:112>>, <<"sent_at_ms">> => ?T0}), Accept),
     E(<<"version_4.refused">>, [<<"D2">>, <<"D9">>], (Ping(#{}))#{<<"version">> => 4}, Malformed),
     E(<<"version_2.refused">>, [<<"D3">>], (Ping(#{}))#{<<"version">> => 2}, Malformed),
     E(<<"capabilities_header.refused">>, [<<"D9">>], Ping(#{<<"capabilities">> => 0}), Malformed),
     E(<<"extra_core_field.refused">>, [<<"D9">>], Ping(#{<<"colour">> => {text, <<"blue">>}}), Malformed),
     E(<<"unknown_frame_type.ignored">>, [<<"D5">>, <<"D9">>], Unknown(#{}), Ignore),
     E(<<"unknown_frame_type.body_not_validated">>, [<<"D5">>],
       Unknown(#{<<"anything">> => [1, {text, <<"two">>}, <<3>>], <<"nonce">> => {text, <<"not bytes">>}}), Ignore),
     E(<<"unknown_frame_type.version_4.refused">>, [<<"D5">>], (Unknown(#{}))#{<<"version">> => 4}, Malformed),
     E(<<"unknown_frame_type.frame_type_not_text.refused">>, [<<"D5">>],
       (Unknown(#{}))#{<<"frame_type">> => <<"bytes">>}, Malformed),
     E(<<"unknown_frame_type.frame_type_empty.refused">>, [<<"D5">>], (Unknown(#{}))#{<<"frame_type">> => {text, <<>>}},
       Malformed),
     (E(<<"unknown_frame_type.frame_type_64.ignored">>, [<<"D5">>],
        (Unknown(#{}))#{<<"frame_type">> => {text, Name(64)}}, Ignore))#{<<"bound">> => <<"upper">>},
     E(<<"unknown_frame_type.frame_type_65.refused">>, [<<"D5">>],
       (Unknown(#{}))#{<<"frame_type">> => {text, Name(65)}}, Malformed),
     E(<<"retired_v2_connect.ignored">>, [<<"D9">>],
       #{<<"version">> => 5, <<"frame_type">> => {text, <<"connect">>}, <<"node_id">> => <<0:256>>}, Ignore),
     envelope_case(<<"unknown_frame_type.dedicated_stream.aborts">>, [<<"D5">>, <<"A">>], <<"dedicated_stream">>,
                   Unknown(#{}), #{<<"outcome">> => <<"abort_stream">>, <<"quic_code">> => 5}),
     E(<<"ext.noncritical_unknown.ignored">>, [<<"D8">>], Ping(#{<<"ext">> => ext(<<"example.unknown">>, 0)}),
       Accept#{<<"count">> => <<"wire_ext_ignored">>}),
     E(<<"ext.critical_unknown.refused">>, [<<"D8">>, <<"D9">>], Ping(#{<<"ext">> => ext(<<"example.unknown">>, 1)}),
       #{<<"outcome">> => <<"refuse">>, <<"reason">> => <<"extension_unsupported">>, <<"link">> => <<"stays_up">>}),
     E(<<"ext.empty.refused">>, [<<"D8">>], Ping(#{<<"ext">> => #{}}), Malformed),
     E(<<"ext.critical_2.refused">>, [<<"D8">>], Ping(#{<<"ext">> => ext(<<"example.unknown">>, 2)}), Malformed),
     E(<<"ext.entry_not_a_map.refused">>, [<<"D8">>], Ping(#{<<"ext">> => #{{text, <<"example.e">>} => 1}}),
       Malformed),
     (E(<<"ext.16_entries.accept">>, [<<"D8">>], Ping(#{<<"ext">> => Ext(16)}),
        Accept#{<<"count">> => <<"wire_ext_ignored">>}))#{<<"bound">> => <<"upper">>},
     E(<<"ext.17_entries.refused">>, [<<"D8">>], Ping(#{<<"ext">> => Ext(17)}), Malformed),
     (E(<<"ext.name_64.accept">>, [<<"D8">>], Ping(#{<<"ext">> => ext(Name(64), 0)}),
        Accept#{<<"count">> => <<"wire_ext_ignored">>}))#{<<"bound">> => <<"upper">>},
     E(<<"ext.name_65.refused">>, [<<"D8">>], Ping(#{<<"ext">> => ext(Name(65), 0)}), Malformed),
     E(<<"ext.name_uppercase.refused">>, [<<"D8">>], Ping(#{<<"ext">> => ext(<<"Example">>, 0)}), Malformed),
     length_case(<<"length.too_large.refused">>, <<16#01000000:32>>,
                 #{<<"outcome">> => <<"refuse">>, <<"reason">> => <<"frame_too_large">>}),
     (length_case(<<"length.at_cap.header_accepted">>, <<16#00FFFFFF:32>>,
                  #{<<"outcome">> => <<"accept">>, <<"note">> => <<"the header alone passes; the body is awaited">>}))
         #{<<"bound">> => <<"upper">>}].

envelope_case(Id, Covers, Position, Frame, Expect) ->
    #{<<"id">> => <<"envelope.", Id/binary>>, <<"profile">> => <<"both">>, <<"covers">> => Covers,
      <<"lands_with">> => <<"macula#99">>,
      <<"input">> => #{<<"frame">> => hex(encode(Frame)), <<"position">> => Position, <<"after_hello">> => 1},
      <<"expect">> => Expect}.

length_case(Id, Header, Expect) ->
    #{<<"id">> => <<"envelope.", Id/binary>>, <<"profile">> => <<"both">>, <<"covers">> => [<<"D5">>],
      <<"lands_with">> => <<"macula#99">>,
      <<"input">> => #{<<"length_header">> => hex(Header), <<"position">> => <<"control_stream">>},
      <<"expect">> => Expect}.

%%------------------------------------------------------------------
%% Unknown enum values (WIRE_1_0.md section 3.4)

%% Per hop: the field reads as its fallback and the frame is read on; no signature is involved except EVENT's
%% publication, which is per profile.
enum_hop_cases() ->
    Neighbor = fun(P) -> frame(<<"hyparview_neighbor">>, #{<<"realm">> => <<1:256>>, <<"priority">> => {text, P}}) end,
    Entry = fun(State) -> #{<<"target">> => <<7:256>>, <<"state">> => {text, State}, <<"incarnation">> => 3,
                             <<"observed_at">> => ?T0, <<"by">> => <<8:256>>} end,
    Swim = frame(<<"swim_ping">>, #{<<"round">> => 1, <<"incarnation">> => 1,
                                     <<"piggyback">> => [Entry(<<"alive">>), Entry(<<"example_state">>)]}),
    Refused = fun(Code) -> frame(<<"hello">>, #{<<"accepted">> => 0, <<"refusal_code">> => {text, Code},
                                                 <<"capabilities">> => 0}) end,
    Early = frame(<<"hello">>, #{<<"accepted">> => 0, <<"refusal_code">> => {text, <<"profile_unsupported">>},
                                  <<"versions">> => [5, 5], <<"profiles">> => [{text, <<"pq_pure">>}]}),
    H = fun(Id, Lands, Frame, AfterHello, Expect) ->
                enum_case(Id, <<"both">>, Lands, Frame, <<"per_hop">>, AfterHello, [], Expect)
        end,
    [H(<<"priority.known.accept">>, <<"macula#99">>, Neighbor(<<"high">>), 1,
       #{<<"outcome">> => <<"accept">>, <<"field">> => <<"priority">>, <<"value">> => <<"high">>}),
     H(<<"priority.unknown.fallback">>, <<"macula#99">>, Neighbor(<<"example_priority">>), 1,
       fallback(<<"priority">>, <<"low">>)),
     H(<<"swim_state.unknown.entry_ignored">>, <<"macula#99">>, Swim, 1,
       (fallback(<<"state">>, <<"entry_ignored">>))#{<<"entries_read">> => 1}),
     H(<<"refusal_code.unknown.fallback">>, <<"macula#98">>, Refused(<<"example_future_code">>), 0,
       fallback(<<"refusal_code">>, <<"not_accepted">>)),
     H(<<"refusal_code.profile_unsupported.accept">>, <<"macula#98">>, Early, 0,
       #{<<"outcome">> => <<"accept">>, <<"field">> => <<"refusal_code">>, <<"value">> => <<"profile_unsupported">>})].

fallback(Field, Value) ->
    #{<<"outcome">> => <<"fallback">>, <<"field">> => Field, <<"value">> => Value,
      <<"count">> => <<"wire_enum_fallback">>}.

%% Signed: EVENT's per-hop delivered_via beside a signed publication, and the enums inside a request, relay error and
%% caller stream tbs, each refused by name without touching the link. A stream or relay frame carries, as context, the
%% STREAM_OPEN it belongs to.
enum_signed_cases(P, Keys) ->
    Client = key(Keys, P, client_identity),
    Station = key(Keys, P, station_identity),
    Publication = signed(?PUBLICATION_LABEL, Client,
                         #{<<"alg">> => alg(P), <<"payload">> => #{}, <<"published_at">> => ?T0,
                           <<"publisher">> => key_id(Client), <<"realm">> => <<1:256>>, <<"seq">> => 1,
                           <<"topic">> => {text, <<"example.topic">>}}),
    Event = fun(Via) -> frame(<<"event">>, #{<<"publication">> => Publication, <<"delivered_via">> => {text, Via}}) end,
    OpenTbs = fun(Mode) -> #{<<"alg">> => alg(P), <<"caller">> => key_id(Client), <<"deadline">> => ?T0 + ?MIN,
                             <<"frame_type">> => {text, <<"stream_open">>}, <<"mode">> => {text, Mode},
                             <<"payload">> => #{}, <<"procedure">> => {text, <<"example.procedure">>},
                             <<"realm">> => <<1:256>>, <<"request_id">> => <<1:128>>, <<"target">> => key_id(Station)}
              end,
    Open = fun(Mode) -> frame(<<"stream_open">>, #{<<"request">> => signed(?REQUEST_LABEL, Client, OpenTbs(Mode))}) end,
    RequestHash = crypto:hash(sha384, encode(OpenTbs(<<"bidi">>))),
    Stream = fun(Type, Fields) ->
                     Tbs = maps:merge(#{<<"alg">> => alg(P), <<"frame_type">> => {text, Type},
                                        <<"request_id">> => <<1:128>>, <<"request_hash">> => RequestHash,
                                        <<"signer">> => key_id(Client), <<"seq">> => 1}, Fields),
                     frame(Type, #{<<"caller_stream">> => maps:remove(<<"key">>,
                                                                      signed(?CALLER_STREAM_LABEL, Client, Tbs))})
             end,
    Relay = fun(Code) ->
                    frame(<<"error">>, #{<<"relay_error">> =>
                                             signed(?RELAY_ERROR_LABEL, Station,
                                                    #{<<"alg">> => alg(P), <<"frame_type">> => {text, <<"error">>},
                                                      <<"request_id">> => <<1:128>>, <<"request_hash">> => RequestHash,
                                                      <<"reported_by">> => key_id(Station),
                                                      <<"code">> => {text, Code}})})
            end,
    Context = [hex(encode(Open(<<"bidi">>)))],
    Refused = fun(Field) -> #{<<"outcome">> => <<"refuse">>, <<"reason">> => <<"enum_unknown">>,
                              <<"field">> => Field, <<"link">> => <<"stays_up">>} end,
    Accept = #{<<"outcome">> => <<"accept">>},
    S = fun(Id, Lands, Frame, Position, Ctx, Expect) -> enum_case(Id, atom_to_binary(P), Lands, Frame, Position, 1, Ctx,
                                                                   Expect) end,
    [S(<<"delivered_via.unknown.fallback">>, <<"macula#99">>, Event(<<"example_via">>), <<"per_hop">>, [],
       fallback(<<"delivered_via">>, <<"direct">>)),
     S(<<"mode.known.accept">>, <<"macula#99">>, Open(<<"bidi">>), <<"tbs">>, [], Accept),
     S(<<"mode.unknown.refused">>, <<"macula#99">>, Open(<<"example_mode">>), <<"tbs">>, [], Refused(<<"mode">>)),
     S(<<"relay_code.known.accept">>, <<"macula#99">>, Relay(<<"overloaded">>), <<"tbs">>, Context, Accept),
     S(<<"relay_code.unknown.refused">>, <<"macula#99">>, Relay(<<"example_code">>), <<"tbs">>, Context,
       Refused(<<"code">>)),
     S(<<"encoding.known.accept">>, <<"macula#99">>,
       Stream(<<"stream_data">>, #{<<"encoding">> => {text, <<"raw">>}, <<"body">> => <<"chunk">>}), <<"tbs">>, Context,
       Accept),
     S(<<"encoding.unknown.refused">>, <<"macula#99">>,
       Stream(<<"stream_data">>, #{<<"encoding">> => {text, <<"example_encoding">>}, <<"body">> => <<"chunk">>}),
       <<"tbs">>, Context, Refused(<<"encoding">>)),
     S(<<"role.known.accept">>, <<"macula#99">>, Stream(<<"stream_end">>, #{<<"role">> => {text, <<"send">>}}),
       <<"tbs">>, Context, Accept),
     S(<<"role.unknown.refused">>, <<"macula#99">>,
       Stream(<<"stream_end">>, #{<<"role">> => {text, <<"example_role">>}}), <<"tbs">>, Context, Refused(<<"role">>))].

enum_case(Id, Profile, Lands, Frame, Position, AfterHello, Context, Expect) ->
    #{<<"id">> => <<"enum.", Id/binary, ".", Profile/binary>>, <<"profile">> => Profile,
      <<"covers">> => [<<"D6">>], <<"lands_with">> => Lands,
      <<"input">> => #{<<"frame">> => hex(encode(Frame)), <<"position">> => Position, <<"after_hello">> => AfterHello,
                       <<"context">> => Context, <<"now_ms">> => ?T0 + 1000},
      <<"expect">> => Expect}.

%% A wire 1.0 frame: version 5, its frame_type, and its fields.
frame(Type, Fields) ->
    Fields#{<<"version">> => 5, <<"frame_type">> => {text, Type}}.

%% A signed object {key, tbs, signature} as a map, signed with the bare primitive.
signed(Label, Signer, TbsFields) ->
    Tbs = encode(TbsFields),
    #{<<"key">> => carried(Signer), <<"tbs">> => Tbs,
      <<"signature">> => macula_node_keys:sign(signed_bytes(Label, carried(Signer), Tbs), Signer)}.

alg(P) -> {text, macula_signed_object:alg(P)}.

%%------------------------------------------------------------------
%% _dht.* procedures (WIRE_1_0.md section 6)

dht_cases(Keys) ->
    Client = key(Keys, pq_pure, client_identity),
    Node = payload(macula_record:node_record(key_id(Client), [], 0)),
    Record = record(pq_pure, Client, 16#01, Node, []),
    V1 = fun(M) -> M#{<<"v">> => 1} end,
    Args = fun(Proc, Id, A, Expect) -> dht_case(<<"args">>, Proc, Id, A, Expect) end,
    Reply = fun(Proc, Id, R, Expect) -> dht_case(<<"reply">>, Proc, Id, R, Expect) end,
    Ok = #{<<"outcome">> => <<"accept">>},
    BadArgs = #{<<"outcome">> => <<"refuse">>, <<"reason">> => <<"malformed_args">>},
    BadReply = #{<<"outcome">> => <<"refuse">>, <<"reason">> => <<"malformed_reply">>},
    Res = fun(R) -> V1(#{<<"result">> => {text, R}}) end,
    Put = <<"_dht.put_record">>, Find = <<"_dht.find_record">>, Many = <<"_dht.find_records">>,
    ByType = <<"_dht.find_records_by_type">>,
    [Args(Put, <<"ok">>, V1(#{<<"record">> => Record}), Ok),
     Args(Put, <<"version_2.refused">>, #{<<"v">> => 2, <<"record">> => Record}, BadArgs),
     Args(Put, <<"no_version.refused">>, #{<<"record">> => Record}, BadArgs),
     Args(Put, <<"missing_record.refused">>, V1(#{}), BadArgs),
     Args(Put, <<"extra_key.refused">>, V1(#{<<"record">> => Record, <<"ttl">> => 5}), BadArgs),
     Args(Put, <<"record_as_text.refused">>, V1(#{<<"record">> => {text, <<"not bytes">>}}), BadArgs),
     Args(Find, <<"ok">>, V1(#{<<"key">> => <<9:256>>}), Ok),
     Args(Find, <<"key_31_bytes.refused">>, V1(#{<<"key">> => <<9:248>>}), BadArgs),
     Args(Find, <<"key_as_text.refused">>, V1(#{<<"key">> => {text, binary:copy(<<"k">>, 32)}}), BadArgs),
     Args(Find, <<"integer_map_key.refused">>, V1(#{<<"key">> => <<9:256>>, 1 => 1}), BadArgs),
     Args(Many, <<"ok">>, V1(#{<<"key">> => <<9:256>>}), Ok),
     Args(ByType, <<"ok">>, V1(#{<<"type">> => 16#12}), Ok),
     (Args(ByType, <<"type_255.ok">>, V1(#{<<"type">> => 255}), Ok))#{<<"bound">> => <<"upper">>},
     Args(ByType, <<"type_256.refused">>, V1(#{<<"type">> => 256}), BadArgs),
     Args(ByType, <<"type_negative.refused">>, V1(#{<<"type">> => -1}), BadArgs),
     Reply(Put, <<"ok">>, Res(<<"ok">>), Ok#{<<"result">> => <<"ok">>}),
     Reply(Put, <<"refused.ok">>, (Res(<<"refused">>))#{<<"reason">> => {text, <<"lifetime_too_long">>}},
           Ok#{<<"result">> => <<"refused">>, <<"reason">> => <<"lifetime_too_long">>}),
     (Reply(Put, <<"refused.reason_64.ok">>, (Res(<<"refused">>))#{<<"reason">> => {text, binary:copy(<<"r">>, 64)}},
            Ok#{<<"result">> => <<"refused">>}))#{<<"bound">> => <<"upper">>},
     Reply(Put, <<"refused.reason_65.refused">>,
           (Res(<<"refused">>))#{<<"reason">> => {text, binary:copy(<<"r">>, 65)}}, BadReply),
     Reply(Put, <<"legacy_bare_ok.refused">>, {text, <<"ok">>}, BadReply),
     Reply(Find, <<"record.ok">>, (Res(<<"record">>))#{<<"record">> => Record}, Ok#{<<"result">> => <<"record">>}),
     Reply(Find, <<"not_found.ok">>, Res(<<"not_found">>), Ok#{<<"result">> => <<"not_found">>}),
     Reply(Find, <<"version_2.refused">>, #{<<"v">> => 2, <<"result">> => {text, <<"not_found">>}},
           #{<<"outcome">> => <<"refuse">>, <<"reason">> => <<"reply_version_unsupported">>}),
     Reply(Find, <<"extra_key.refused">>, (Res(<<"not_found">>))#{<<"hint">> => 1}, BadReply),
     Reply(Find, <<"records_shape.refused">>, (Res(<<"records">>))#{<<"records">> => []}, BadReply),
     Reply(Find, <<"unknown_result.refused">>, Res(<<"example_result">>), BadReply),
     Reply(Find, <<"legacy_bare_not_found.refused">>, {text, <<"not_found">>}, BadReply),
     Reply(Many, <<"empty.ok">>, (Res(<<"records">>))#{<<"records">> => []}, Ok#{<<"result">> => <<"records">>}),
     Reply(Many, <<"one.ok">>, (Res(<<"records">>))#{<<"records">> => [Record]},
           Ok#{<<"result">> => <<"records">>, <<"records_kept">> => 1}),
     Reply(Many, <<"records_not_a_list.refused">>, (Res(<<"records">>))#{<<"records">> => Record}, BadReply),
     Reply(Many, <<"missing_records.refused">>, Res(<<"records">>), BadReply),
     Reply(ByType, <<"empty.ok">>, (Res(<<"records">>))#{<<"records">> => []}, Ok#{<<"result">> => <<"records">>})].

dht_case(Side, Proc, Id, Value, Expect) ->
    <<"_dht.", Short/binary>> = Proc,
    #{<<"id">> => <<"dht.", Short/binary, ".", Side/binary, ".", Id/binary>>, <<"profile">> => <<"pq_pure">>,
      <<"covers">> => [<<"D1">>], <<"lands_with">> => <<"macula#102">>,
      <<"input">> => #{<<"procedure">> => Proc, Side => hex(encode(Value)), <<"now_ms">> => ?T0 + 1000},
      <<"expect">> => Expect}.

%%------------------------------------------------------------------
%% Encoding and files

%% Text keys of the maps built above are binaries; macula_record_cbor takes them as {text, K}. Byte strings stay
%% binaries, text values are {text, V} already.
encode(V) -> macula_record_cbor:encode(cbor(V)).

cbor(M) when is_map(M) -> maps:from_list([{key(K), cbor(V)} || K := V <- M]);
cbor(L) when is_list(L) -> [cbor(V) || V <- L];
cbor(V) -> V.

key(K) when is_binary(K) -> {text, K};
key(K) -> K.

hex(Bin) -> binary:encode_hex(Bin, lowercase).

write_cases(Class, Cases) ->
    Ids = [maps:get(<<"id">>, C) || C <- Cases],
    true = length(Ids) =:= length(lists:usort(Ids)),
    write(Class, #{<<"wire">> => <<"1.0">>, <<"class">> => Class, <<"format">> => 1, <<"design">> => <<"macula#83">>,
                   <<"spec">> => <<"docs/design/WIRE_1_0.md">>, <<"generator">> => ?GENERATOR,
                   <<"cases">> => Cases}).

write(Class, Term) ->
    Path = filename:join(?DIR, <<Class/binary, ".json">>),
    ok = file:write_file(Path, [json:format(Term), "\n"]),
    io:format("wrote ~s~n", [Path]).
