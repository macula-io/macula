%% Writes the wire 1.0 conformance vectors in priv/vectors/wire_1_0/, shipped in the hex package so every stack runs
%% the vectors of the macula release it pins, (docs/design/WIRE_1_0.md, macula#97). Run through
%% scripts/generate-wire-1-0-vectors.sh with the compiled tree on the code path.
%%
%% This is a reference encoder for wire 1.0, independent of the code that will implement it: it builds every object
%% from the deterministic CBOR encoder and the bare signing primitive, taking only payloads from today's record
%% constructors. ML-DSA-87 signing is hedged and pq_hybrid's RSA-PSS half is randomized, so a second run signs other
%% bytes: the committed files are the vectors, generated once. The keys are fixed (test/fixtures/wire_1_0_private_test_keys.etf, test only)
%% so a regeneration keeps keys.json.
-module(wire_1_0_vectors).

-export([main/0]).

-define(DIR, "priv/vectors/wire_1_0").
%% Test-only private keys, kept out of the hex package (priv ships, test does not).
-define(KEYS_FILE, "test/fixtures/wire_1_0_private_test_keys.etf").
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
-define(REPLY_LABEL, <<"MACULA-PQ-REPLY-V1">>).
-define(STREAM_LABEL, <<"MACULA-PQ-STREAM-V1">>).
-define(PROOF_V3_LABEL, <<"MACULA-PQ-CONNECT-PROOF-V3">>).
-define(PROOF_V2_LABEL, <<"MACULA-PQ-CONNECT-PROOF-V2">>).
-define(SESSION_PROOF_LABEL, <<"MACULA-PQ-SESSION-PROOF-V2">>).
-define(LEAF, <<"wire 1.0 vector: the station leaf DER as presented (opaque to the handshake)">>).
-define(EXPORTER, <<16#E5E5E5E5:32, 0:192, 16#0001:32>>).
-define(NONCE, <<16#A1:8, 0:240, 16#7E:8>>).
-define(STATION_CAPS, 1).
-define(CLIENT_CAPS, 6).
-define(PROFILES, [pq_pure, pq_hybrid]).
-define(ROLES, [station_identity, client_identity, client_connect, org, realm, foundation]).
-define(STATION, <<"macula-station#94">>).

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
    write_cases(<<"handshake">>, lists:append([handshake_cases(P, Keys) || P <- ?PROFILES])),
    write_cases(<<"tbs">>, lists:append([tbs_cases(P, Keys) || P <- ?PROFILES])),
    write_cases(<<"frames">>, frame_cases(Keys)),
    halt(0).

%%------------------------------------------------------------------
%% Keys

keys() ->
    keys(file:read_file(?KEYS_FILE)).

%% Keys already in the file are kept; a role added since is generated once and written back.
keys({ok, Bin}) -> with_every_role(binary_to_term(Bin));
keys({error, enoent}) -> with_every_role(#{}).

with_every_role(Known) ->
    Keys = maps:merge(maps:from_list([{{P, R}, generated(P, R)} || P <- ?PROFILES, R <- ?ROLES,
                                                                   not is_map_key({P, R}, Known)]), Known),
    ok = file:write_file(?KEYS_FILE, term_to_binary(Keys)),
    Keys.

generated(P, station_identity) -> ok_value(macula_node_keys:generate(identity, P));
generated(P, client_identity) -> ok_value(macula_node_keys:generate(identity, P, #{puzzle_difficulty => 8}));
generated(P, client_connect) -> ok_value(macula_node_keys:generate(connect, P));
generated(P, org) -> ok_value(macula_node_keys:generate(org, P));
generated(P, realm) -> ok_value(macula_node_keys:generate(realm, P));
generated(P, foundation) -> ok_value(macula_node_keys:generate(foundation, P)).

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

%% WIRE_1_0.md section 5.1: the row each known type names in the frozen lifetime table.
class_of(T) when T =:= 16#01; T =:= 16#11 -> 3;
class_of(T) when T =:= 16#06; T =:= 16#12 -> 0;
class_of(T) when T =:= 16#04; T =:= 16#15 -> 2;
class_of(T) when T =:= 16#16; T =:= 16#17 -> 1;
class_of(T) when T >= 16#20 -> 4;
class_of(_) -> 5.

class_max(0) -> 5 * ?MIN;
class_max(1) -> 30 * ?MIN;
class_max(2) -> 6 * ?HOUR;
class_max(3) -> 48 * ?HOUR;
class_max(4) -> 7 * ?DAY;
class_max(5) -> 30 * ?DAY.

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
          {Store, R(<<"mcid_tag_unknown">>)})]
    ++ lists:append([type_cases(P, Keys, Name, Type, Role, Payload, Case, Store, Accept, R)
                     || {Name, Type, Role, Payload} <- builtin_payloads(P, Keys)]).

%% Every known built-in type: a valid record, and one naming another lifetime row (stored, refused by a reader).
type_cases(P, Keys, Name, Type, Role, Payload, Case, Store, Accept, R) ->
    Signer = key(Keys, P, Role),
    Row = class_of(row_type(Type, Payload)),
    Lifetime = min(?HOUR, class_max(Row)),
    Expires = {set, <<"expires_at">>, ?T0 + Lifetime},
    Wrong = case Row of 5 -> 4; _ -> 5 end,
    Covers = [<<"D4">>],
    ok = accepted_today(P, Signer, Type, Payload, ?T0 + Lifetime, Name),
    [Case(<<"type.", Name/binary, ".ok">>, Covers, Role,
          record(P, Signer, Type, Payload, [Expires, {set, <<"lifetime_class">>, Row}]), {Store, Accept}),
     Case(<<"type.", Name/binary, ".other_row.stored">>, Covers, Role,
          record(P, Signer, Type, Payload, [Expires, {set, <<"lifetime_class">>, Wrong}]),
          {Store, R(<<"lifetime_class_mismatch">>)})].

%% The payload passes today's reader (macula_record:verify/3) in today's tbs, without schema and lifetime_class, so an
%% "accept" here claims nothing about payload rules that wire 1.0 does not inherit unchanged.
accepted_today(P, Signer, Type, Payload, Expires, Name) ->
    Tbs = encode(#{<<"type">> => Type, <<"alg">> => {text, macula_signed_object:alg(P)},
                   <<"version">> => macula_record_uuid:v7(), <<"created_at">> => ?T0, <<"expires_at">> => Expires,
                   <<"payload">> => Payload}),
    Bytes = macula_signed_object:encode(#{key => carried(Signer), tbs => Tbs,
                                          signature => macula_node_keys:sign(signed_bytes(?RECORD_LABEL,
                                                                                          carried(Signer), Tbs),
                                                                             Signer)}),
    accepted(macula_record:verify(Bytes, P, ?T0 + 1000), Name).

accepted({ok, _}, _Name) -> ok;
accepted(Refused, Name) -> error({today_refuses, Name, Refused}).

%% A tombstone names the row of the type it withdraws.
row_type(16#0C, #{{text, <<"withdrawn_type">>} := Withdrawn}) -> Withdrawn;
row_type(Type, _Payload) -> Type.

%% {Name, Type, Signer role, Payload} for each built-in type, payloads from today's constructors; windows inside a
%% payload are set around the vectors' clock.
builtin_payloads(P, Keys) ->
    Id = fun(Role) -> key_id(key(Keys, P, Role)) end,
    Realm = <<16#AA:256>>,
    Window = #{valid_from => ?T0 - ?HOUR, valid_until => ?T0 + ?DAY},
    NodeRecord = macula_record:node_record(Id(client_identity), [Realm], 1),
    OrgDirectory = record(P, key(Keys, P, realm), 16#15,
                          payload(macula_record:org_directory(Realm, <<"example-org">>, Id(org))),
                          [{set, <<"expires_at">>, ?T0 + ?HOUR}]),
    [{<<"node_record">>, 16#01, client_identity, payload(NodeRecord)},
     {<<"realm_directory">>, 16#03, realm, payload(macula_record:realm_directory(Realm, <<"example-realm">>, Id(realm)))},
     {<<"realm_stations">>, 16#04, realm,
      payload(macula_record:realm_stations(Realm, [#{station_id => Id(station_identity), roles => [<<"seed">>]}]))},
     {<<"realm_member_endorsement">>, 16#05, realm,
      payload(macula_record:realm_member_endorsement(Realm, (maps:merge(#{realm => Realm,
                                                                           member_node => Id(client_identity),
                                                                           roles => [<<"member">>]}, #{})),
                                                     Window))},
     {<<"procedure_advertisement">>, 16#06, client_identity,
      payload(macula_record:procedure_advertisement(Id(client_identity), Realm, <<"example.procedure">>,
                                                    Id(station_identity)))},
     {<<"tombstone">>, 16#0C, client_identity, payload(macula_record:tombstone(NodeRecord, shutdown))},
     {<<"foundation_seed_list">>, 16#0D, foundation,
      payload(macula_record:foundation_seed_list([#{node_id => Id(station_identity), tier => 3,
                                                    addresses => [#{host => <<"2001:db8::1">>, port => 4433}]}],
                                                 Window))},
     {<<"foundation_parameter">>, 16#0E, foundation,
      payload(macula_record:foundation_parameter(<<"example_parameter">>, 7, Window))},
     {<<"foundation_realm_trust_list">>, 16#0F, foundation,
      payload(macula_record:foundation_realm_trust_list([#{realm_id => Realm, realm_key_id => Id(realm)}]))},
     {<<"foundation_t3_attestation">>, 16#10, foundation,
      payload(macula_record:foundation_t3_attestation(Id(station_identity), ?T0 - ?DAY,
                                                      #{valid_until => ?T0 + ?DAY}))},
     {<<"content_announcement">>, 16#11, client_identity, content_payload(key(Keys, P, client_identity), 2)},
     {<<"station_endpoint">>, 16#12, station_identity, payload(macula_record:station_endpoint(4433))},
     {<<"org_directory">>, 16#15, realm, payload(macula_record:org_directory(Realm, <<"example-org">>, Id(org)))},
     {<<"procedure_delegation">>, 16#16, org,
      payload(macula_record:procedure_delegation(Id(org), Id(client_identity)))},
     {<<"app_record">>, 16#17, org,
      payload(macula_record:app_record(Id(org), Realm, <<"example-org">>, <<"example-app">>,
                                       #{version => <<"1.0.0">>,
                                         services => [#{name => <<"svc">>,
                                                        procedures => [<<"example-org/example.procedure">>]}],
                                         org_directory => OrgDirectory}))}].

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
      <<"covers">> => Covers, <<"lands_with">> => [Lands, ?STATION],
      <<"input">> => #{<<"object_of">> => <<Id/binary, ".reader.", Profile/binary>>,
                       <<"signer">> => atom_to_binary(Signer), <<"now_ms">> => Now},
      <<"expect">> => Cust}.

record_case(Id, P, Covers, Lands, Bytes, Signer, Now, Expect) ->
    #{<<"id">> => <<Id/binary, ".", (atom_to_binary(P))/binary>>, <<"profile">> => atom_to_binary(P),
      <<"covers">> => Covers, <<"lands_with">> => lands(Lands),
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
                record_case(<<"check_order.", Name/binary, ".", Role/binary>>, P, [<<"D9">>], station_too(Role),
                            Bytes, Signer, Now, #{<<"outcome">> => <<"refuse">>, <<"reason">> => Reason,
                                                  <<"role">> => Role})
        end,
    [C(<<"alg_before_signature">>, <<"custodian">>, client_identity,
       record(P, Client, 16#01, Node, [{set, <<"alg">>, {text, <<"ML-DSA-65">>}}, corrupt_signature]),
       <<"alg_unsupported">>),
     C(<<"signature_before_tbs_keys">>, <<"custodian">>, client_identity,
       record(P, Client, 16#01, Node, [{set, <<"colour">>, 1}, corrupt_signature]), <<"signature_invalid">>),
     C(<<"signature_before_schema">>, <<"custodian">>, client_identity,
       record(P, Client, 16#01, Node, [{set, <<"schema">>, 0}, corrupt_signature]), <<"signature_invalid">>),
     C(<<"signature_before_lifetime_class">>, <<"custodian">>, client_identity,
       record(P, Client, 16#01, Node, [{set, <<"lifetime_class">>, 9}, corrupt_signature]), <<"signature_invalid">>),
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
      <<"lands_with">> => envelope_lands(Id),
      <<"input">> => #{<<"frame">> => hex(encode(Frame)), <<"position">> => Position, <<"after_hello">> => 1},
      <<"expect">> => Expect}.

length_case(Id, Header, Expect) ->
    #{<<"id">> => <<"envelope.", Id/binary>>, <<"profile">> => <<"both">>, <<"covers">> => [<<"D5">>],
      <<"lands_with">> => lands(<<"macula#99">>),
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
      <<"covers">> => [<<"D6">>], <<"lands_with">> => lands(Lands),
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
      <<"covers">> => [<<"D1">>], <<"lands_with">> => lands(<<"macula#102">>),
      <<"input">> => #{<<"procedure">> => Proc, Side => hex(encode(Value)), <<"now_ms">> => ?T0 + 1000},
      <<"expect">> => Expect}.

%%------------------------------------------------------------------
%% Handshake transcripts (WIRE_1_0.md section 4)

%% Each case is a transcript, opener to HELLO, judged step by step by the receiver of each step. A runner holds no
%% private key: it plays the receiver with the case's inputs (the TLS leaf and exporter, the fixed nonce, the ranges,
%% the puzzle and the floor) and the frames it "sent" earlier in the transcript.
handshake_cases(P, Keys) ->
    Station = key(Keys, P, station_identity),
    Client = key(Keys, P, client_identity),
    Connect = key(Keys, P, client_connect),
    StationId = key_id(Station),
    ClientId = key_id(Client),
    Prof = atom_to_binary(P),
    Other = atom_to_binary(other_profile(P)),
    TlsBinding = macula_key_bindings:tls_binding(Station, ?LEAF, ?T0 - ?HOUR, ?T0 + 6 * ?DAY),
    ConnectBinding = macula_key_bindings:connect_binding(Client, carried(Connect), ?T0 - ?HOUR, ?T0 + ?DAY),
    W = #{profile => P, prof => Prof, station => Station, client => Client, connect => Connect,
          station_id => StationId, client_id => ClientId,
          tls_binding => env(TlsBinding),
          tls_status => env(macula_key_bindings:status_statement(Station, TlsBinding, ?T0 - ?MIN, ?T0 + 59 * ?MIN)),
          connect_binding => env(ConnectBinding),
          connect_status => env(macula_key_bindings:status_statement(Client, ConnectBinding, ?T0 - ?MIN,
                                                                     ?T0 + 59 * ?MIN))},
    Opener = opener([5, 5], [Prof]),
    Challenge = challenge(W, 5, Prof, [5, 5], [Prof]),
    {ConnectFrame, ProofMsg} = connect(W, Challenge, [5, 5], [Prof], Prof, #{}),
    {Hello, SessionMsg} = hello_accepted(W, Challenge, ConnectFrame),
    StationIn = fun(Extra) -> maps:merge(#{<<"versions">> => [5, 5], <<"profiles">> => [Prof],
                                           <<"capabilities">> => ?STATION_CAPS,
                                           <<"puzzle">> => #{<<"difficulty">> => 8, <<"mode">> => <<"enforce">>},
                                           <<"floor">> => []}, Extra) end,
    ClientIn = fun(Extra) -> maps:merge(#{<<"versions">> => [5, 5], <<"profiles">> => [Prof],
                                          <<"capabilities">> => ?CLIENT_CAPS,
                                          <<"expected_station_node_id">> => hex(StationId), <<"floor">> => []},
                                        Extra) end,
    Ok = #{<<"outcome">> => <<"accept">>},
    Refuse = fun(Reason, Wire) -> #{<<"outcome">> => <<"refuse">>, <<"reason">> => Reason,
                                    <<"wire_refusal">> => Wire} end,
    Closes = fun(Reason) -> #{<<"outcome">> => <<"refuse">>, <<"reason">> => Reason} end,
    Step = fun(From, Frame, Expect) -> step(From, Frame, Expect) end,
    Proof = fun(Msg) -> #{<<"proof_message">> => hex(Msg)} end,
    Full = [Step(<<"client">>, Opener, Ok), Step(<<"station">>, Challenge, Ok),
            maps:merge(Step(<<"client">>, ConnectFrame, Ok), Proof(ProofMsg)),
            (Step(<<"station">>, Hello, Ok))#{<<"session_proof_message">> => hex(SessionMsg)}],
    Station1 = fun(Frame, Expect) -> [Step(<<"client">>, Opener, Ok), Step(<<"station">>, Challenge, Ok),
                                      Step(<<"client">>, Frame, Expect)] end,
    Tampered = fun(Override) -> element(1, connect(W, Challenge, [5, 5], [Prof], Prof, Override)) end,
    Early = fun(Code) -> frame(<<"hello">>, #{<<"accepted">> => 0, <<"refusal_code">> => {text, Code},
                                              <<"versions">> => [5, 5], <<"profiles">> => [{text, Prof}]}) end,
    EarlyExpect = fun(Code) -> (Refuse(Code, Code))#{<<"hello">> => #{<<"versions">> => [5, 5],
                                                                      <<"profiles">> => [Prof]}} end,
    H = fun(Id, Covers, Station0, Client0, Steps) -> hs_case(Id, P, Covers, StationIn(Station0), ClientIn(Client0),
                                                             Steps) end,
    {OtherHello, _} = hello_accepted(W, challenge(W, 5, Prof, [5, 5], [Prof, <<"example_profile">>]), ConnectFrame),
    [H(<<"complete">>, [<<"D2">>, <<"D3">>, <<"A1">>, <<"A2">>], #{}, #{}, Full),
     %% Refused at the opener: the station names its ranges and sends no challenge.
     H(<<"opener.version_4.refused">>, [<<"D2">>, <<"A2">>], #{}, #{},
       [Step(<<"client">>, #{<<"version">> => 4, <<"frame_type">> => {text, <<"opener">>}},
             EarlyExpect(<<"unsupported_version">>))]),
     H(<<"opener.no_common_version.refused">>, [<<"D3">>, <<"A2">>], #{}, #{<<"versions">> => [6, 7]},
       [Step(<<"client">>, opener([6, 7], [Prof]), EarlyExpect(<<"unsupported_version">>))]),
     H(<<"opener.unknown_profile.refused">>, [<<"D7">>, <<"A2">>], #{}, #{<<"profiles">> => [<<"example_profile">>]},
       [Step(<<"client">>, opener([5, 5], [<<"example_profile">>]), EarlyExpect(<<"profile_unsupported">>))]),
     H(<<"opener.other_profile_only.refused">>, [<<"D7">>, <<"A2">>], #{}, #{<<"profiles">> => [Other]},
       [Step(<<"client">>, opener([5, 5], [Other]), EarlyExpect(<<"profile_unsupported">>))]),
     H(<<"opener.versions_missing.refused">>, [<<"A2">>], #{}, #{},
       [Step(<<"client">>, frame(<<"opener">>, #{<<"profiles">> => [{text, Prof}]}), Closes(<<"malformed_frame">>))]),
     H(<<"opener.versions_reversed.refused">>, [<<"A2">>], #{}, #{},
       [Step(<<"client">>, opener([6, 5], [Prof]), Closes(<<"malformed_frame">>))]),
     %% The station judges CONNECT.
     H(<<"connect.ranges_changed.refused">>, [<<"A2">>], #{}, #{},
       Station1(element(1, connect(W, Challenge, [4, 5], [Prof], Prof, #{})),
                Refuse(<<"negotiation_mismatch">>, <<"not_accepted">>))),
     H(<<"connect.profile_not_chosen.refused">>, [<<"D7">>], #{}, #{},
       Station1(element(1, connect(W, Challenge, [5, 5], [Prof], Other, #{})),
                Refuse(<<"profile_unsupported">>, <<"not_accepted">>))),
     H(<<"connect.proof_v2.refused">>, [<<"D2">>, <<"A1">>], #{}, #{},
       Station1(Tampered(#{proof => v2}), Refuse(<<"proof_invalid">>, <<"not_accepted">>))),
     H(<<"connect.capabilities_changed_after_signing.refused">>, [<<"D2">>, <<"A1">>], #{}, #{},
       Station1(Tampered(#{after_signing => #{<<"capabilities">> => 7}}),
                Refuse(<<"proof_invalid">>, <<"not_accepted">>))),
     H(<<"connect.endorsement_changed_after_signing.refused">>, [<<"A1">>], #{}, #{},
       Station1(Tampered(#{after_signing => #{<<"member_endorsement">> => <<"swapped in">>}}),
                Refuse(<<"proof_invalid">>, <<"not_accepted">>))),
     H(<<"connect.version_4.refused">>, [<<"D2">>, <<"D3">>], #{}, #{},
       Station1(Tampered(#{frame_version => 4}), Refuse(<<"unsupported_version">>, <<"unsupported_version">>))),
     H(<<"connect.capabilities_2_53.refused">>, [<<"D2">>], #{}, #{},
       Station1(Tampered(#{fields => #{<<"capabilities">> => 1 bsl 53}}),
                Refuse(<<"malformed_frame">>, <<"not_accepted">>))),
     (H(<<"connect.capabilities_bit_52.accept">>, [<<"D2">>], #{}, #{},
        Station1(Tampered(#{fields => #{<<"capabilities">> => 1 bsl 52}}), Ok)))#{<<"bound">> => <<"upper">>},
     H(<<"connect.puzzle_unsolved.refused">>, [<<"D9">>], #{<<"puzzle">> => #{<<"difficulty">> => 24,
                                                                            <<"mode">> => <<"enforce">>}}, #{},
       Station1(ConnectFrame, Refuse(<<"puzzle_invalid">>, <<"puzzle_invalid">>))),
     H(<<"connect.below_floor.refused">>, [<<"D3">>],
       #{<<"floor">> => [#{<<"node_id">> => hex(ClientId), <<"version">> => 6}]}, #{},
       Station1(ConnectFrame, Refuse(<<"version_below_floor">>, <<"not_accepted">>))),
     %% The client judges the challenge and HELLO.
     H(<<"challenge.profile_downgrade.refused">>, [<<"A2">>], #{<<"profiles">> => [Other, Prof]},
       #{<<"profiles">> => [Other, Prof]},
       [Step(<<"client">>, opener([5, 5], [Other, Prof]), Ok),
        Step(<<"station">>, challenge(W, 5, Prof, [5, 5], [Other, Prof]), Closes(<<"downgrade_detected">>))]),
     H(<<"challenge.ranges_missing.refused">>, [<<"A2">>], #{}, #{},
       [Step(<<"client">>, Opener, Ok),
        Step(<<"station">>, maps:without([<<"versions">>, <<"profiles">>], Challenge), Closes(<<"malformed_frame">>))]),
     H(<<"challenge.below_floor.refused">>, [<<"D3">>], #{},
       #{<<"floor">> => [#{<<"node_id">> => hex(StationId), <<"version">> => 6}]},
       [Step(<<"client">>, Opener, Ok), Step(<<"station">>, Challenge, Closes(<<"version_below_floor">>))]),
     H(<<"hello.session_proof_other_challenge.refused">>, [<<"A2">>], #{}, #{},
       lists:droplast(Full) ++ [Step(<<"station">>, OtherHello, Closes(<<"session_proof_invalid">>))]),
     H(<<"hello.capabilities_changed_after_signing.refused">>, [<<"D2">>], #{}, #{},
       lists:droplast(Full) ++ [Step(<<"station">>, Hello#{<<"capabilities">> => ?STATION_CAPS bor (1 bsl 40)},
                                     Closes(<<"session_proof_invalid">>))]),
     H(<<"hello.session_proof_missing.refused">>, [<<"D2">>], #{}, #{},
       lists:droplast(Full) ++ [Step(<<"station">>, maps:remove(<<"session_proof">>, Hello),
                                     Closes(<<"session_proof_missing">>))]),
     H(<<"hello.refusal_code_unknown.reads_not_accepted">>, [<<"D6">>], #{}, #{},
       lists:droplast(Full) ++ [Step(<<"station">>, frame(<<"hello">>, #{<<"accepted">> => 0, <<"capabilities">> => 0,
                                                                         <<"refusal_code">> => {text, <<"example_code">>}}),
                                     Closes(<<"refused:not_accepted">>))]),
     H(<<"hello.early_refusal_names_ranges">>, [<<"A2">>], #{}, #{<<"versions">> => [6, 7]},
       [Step(<<"client">>, opener([6, 7], [Prof]), Ok),
        Step(<<"station">>, Early(<<"unsupported_version">>),
             (Closes(<<"refused:unsupported_version">>))#{<<"station_ranges">> => #{<<"versions">> => [5, 5],
                                                                                    <<"profiles">> => [Prof]}})])].

other_profile(pq_pure) -> pq_hybrid;
other_profile(pq_hybrid) -> pq_pure.

env(#{tbs := Tbs, signature := Sig}) -> #{<<"tbs">> => Tbs, <<"signature">> => Sig}.

opener(Versions, Profiles) ->
    frame(<<"opener">>, #{<<"versions">> => Versions, <<"profiles">> => [{text, X} || X <- Profiles]}).

challenge(#{station := Station, tls_binding := B, tls_status := S}, Version, Chosen, Versions, Profiles) ->
    (frame(<<"challenge">>, #{<<"nonce">> => ?NONCE, <<"profile">> => {text, Chosen},
                              <<"identity_key">> => carried(Station), <<"tls_binding">> => B, <<"tls_status">> => S,
                              <<"versions">> => Versions, <<"profiles">> => [{text, X} || X <- Profiles]}))
        #{<<"version">> => Version}.

%% CONNECT and the bytes its proof V3 signs. Override: proof => v2 signs the old V2 message instead;
%% after_signing => Fields changes fields after the proof is made; fields => Fields changes them before;
%% frame_version => V sets the frame's version.
connect(#{station_id := StationId, client_id := ClientId, client := Client, connect := ConnectKey,
          connect_binding := B, connect_status := S}, Challenge, Versions, Profiles, Chosen, Override) ->
    Fields0 = maps:merge(frame(<<"connect">>, #{<<"identity_key">> => carried(Client),
                                                <<"connect_key">> => carried(ConnectKey),
                                                <<"connect_binding">> => B, <<"connect_status">> => S,
                                                <<"capabilities">> => ?CLIENT_CAPS,
                                                <<"member_endorsement">> => <<>>, <<"versions">> => Versions,
                                                <<"profiles">> => [{text, X} || X <- Profiles],
                                                <<"profile">> => {text, Chosen}}),
                         maps:get(fields, Override, #{})),
    Fields = Fields0#{<<"version">> => maps:get(frame_version, Override, 5)},
    ConnectTbs = encode(Fields#{<<"proof">> => <<>>}),
    Msg = proof_message(maps:get(proof, Override, v3), StationId, ClientId, encode(Challenge), ConnectTbs),
    Signed = Fields#{<<"proof">> => macula_node_keys:sign(Msg, ConnectKey)},
    {maps:merge(Signed, maps:get(after_signing, Override, #{})), Msg}.

proof_message(v3, StationId, ClientId, Challenge, ConnectTbs) ->
    <<?PROOF_V3_LABEL/binary, 0, ?NONCE/binary, StationId/binary, ClientId/binary,
      (crypto:hash(sha384, ?LEAF))/binary, (crypto:hash(sha384, Challenge))/binary, ?EXPORTER/binary,
      (crypto:hash(sha384, ConnectTbs))/binary>>;
proof_message(v2, StationId, ClientId, Challenge, _ConnectTbs) ->
    <<?PROOF_V2_LABEL/binary, 0, ?NONCE/binary, StationId/binary, ClientId/binary,
      (crypto:hash(sha384, ?LEAF))/binary, (crypto:hash(sha384, Challenge))/binary, ?EXPORTER/binary,
      ?CLIENT_CAPS:64>>.

%% An accepted HELLO and the bytes its session proof V2 signs: over SHA-384 of the HELLO tbs (session_proof empty),
%% which covers the station's capabilities with no fixed width (WIRE_1_0.md 4.3).
hello_accepted(#{station := Station, station_id := StationId, client_id := ClientId}, Challenge, Connect) ->
    Fields = frame(<<"hello">>, #{<<"accepted">> => 1, <<"capabilities">> => ?STATION_CAPS}),
    HelloTbs = encode(Fields#{<<"session_proof">> => <<>>}),
    Msg = <<?SESSION_PROOF_LABEL/binary, 0, ?EXPORTER/binary, (crypto:hash(sha384, encode(Challenge)))/binary,
            (crypto:hash(sha384, encode(Connect)))/binary, StationId/binary, ClientId/binary,
            (crypto:hash(sha384, HelloTbs))/binary>>,
    {Fields#{<<"session_proof">> => macula_node_keys:sign(Msg, Station)}, Msg}.

step(From, Frame, Expect) ->
    #{<<"from">> => From, <<"frame">> => hex(encode(Frame)), <<"expect">> => Expect}.

hs_case(Id, P, Covers, Station, Client, Steps) ->
    Prof = atom_to_binary(P),
    #{<<"id">> => <<"handshake.", Id/binary, ".", Prof/binary>>, <<"profile">> => Prof, <<"covers">> => Covers,
      <<"lands_with">> => handshake_lands(Id),
      <<"input">> => #{<<"tls">> => #{<<"leaf_der">> => hex(?LEAF), <<"exporter">> => hex(?EXPORTER)},
                       <<"now_ms">> => ?T0 + 1000, <<"station">> => Station, <<"client">> => Client,
                       <<"steps">> => Steps},
      <<"expect">> => maps:get(<<"expect">>, lists:last(Steps))}.

%%------------------------------------------------------------------
%% Canonical tbs bytes and signing inputs (WIRE_1_0.md sections 3.2, 5.1; the labels of
%% DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md and DESIGN_PQ_HANDSHAKE_FRAMES.md)

%% A stack encodes `fields' to exactly `tbs' (shortest integers and lengths, keys sorted by their encoded bytes), and
%% builds exactly `signing_input' from it: label || 0x00 || SHA-384(key as carried) || tbs for signed objects, and
%% label || 0x00 || tbs for the handshake's bindings and statements.
tbs_cases(P, Keys) ->
    Client = key(Keys, P, client_identity),
    Station = key(Keys, P, station_identity),
    Hash = crypto:hash(sha384, <<"wire 1.0 vector request tbs">>),
    Request = fun(Type, Extra) -> maps:merge(#{<<"alg">> => alg(P), <<"caller">> => key_id(Client),
                                               <<"deadline">> => ?T0 + ?MIN, <<"frame_type">> => {text, Type},
                                               <<"payload">> => #{{text, <<"n">>} => 1},
                                               <<"procedure">> => {text, <<"example.procedure">>},
                                               <<"realm">> => <<1:256>>, <<"request_id">> => <<1:128>>,
                                               <<"target">> => key_id(Station)}, Extra) end,
    Stream = fun(Type, Signer, Extra) -> maps:merge(#{<<"alg">> => alg(P), <<"frame_type">> => {text, Type},
                                                      <<"request_id">> => <<1:128>>, <<"request_hash">> => Hash,
                                                      <<"signer">> => key_id(Signer), <<"seq">> => 0}, Extra) end,
    Binding = fun(Use, Label, Subject) ->
                      #{<<"label">> => {text, Label}, <<"node_id">> => key_id(Station), <<"use">> => {text, Use},
                        <<"subject_hash">> => crypto:hash(sha384, Subject), <<"binding_id">> => <<16#B1:128>>,
                        <<"not_before">> => ?T0, <<"not_after">> => ?T0 + 7 * ?DAY,
                        <<"hash_alg">> => {text, <<"SHA-384">>}, <<"sig_alg">> => alg(P)}
              end,
    TlsBinding = Binding(<<"tls">>, <<"MACULA-PQ-BINDING-TLS-V1">>, ?LEAF),
    Record = fun(Type, Extra) -> maps:merge(#{<<"type">> => Type, <<"alg">> => alg(P),
                                              <<"version">> => <<16#0190:16, 16#7A:8, 0:104>>,
                                              <<"created_at">> => ?T0, <<"expires_at">> => ?T0 + ?HOUR,
                                              <<"schema">> => 1, <<"lifetime_class">> => class_of(Type),
                                              <<"payload">> => #{{text, <<"quic_port">>} => 4433}}, Extra) end,
    T = fun(Id, Covers, Label, Role, Fields) -> tbs_case(Id, P, Keys, Covers, Label, Role, Fields, keyed) end,
    [T(<<"record.station_endpoint">>, [<<"D4">>], ?RECORD_LABEL, station_identity, Record(16#12, #{})),
     T(<<"record.domain_type.subject_and_ext">>, [<<"D4">>, <<"D8">>], ?RECORD_LABEL, org,
       Record(16#20, #{<<"subject">> => <<"subject-1">>, <<"lifetime_class">> => 4,
                       <<"ext">> => ext(<<"example.e">>, 0)})),
     T(<<"request.call">>, [<<"D1">>], ?REQUEST_LABEL, client_identity, Request(<<"call">>, #{})),
     T(<<"request.stream_open">>, [<<"D1">>], ?REQUEST_LABEL, client_identity,
       Request(<<"stream_open">>, #{<<"mode">> => {text, <<"bidi">>}})),
     T(<<"request.call.with_ext">>, [<<"D8">>], ?REQUEST_LABEL, client_identity,
       Request(<<"call">>, #{<<"ext">> => ext(<<"example.e">>, 1)})),
     T(<<"reply.result">>, [<<"D1">>], ?REPLY_LABEL, station_identity,
       #{<<"alg">> => alg(P), <<"frame_type">> => {text, <<"result">>}, <<"request_id">> => <<1:128>>,
         <<"request_hash">> => Hash, <<"responded_by">> => key_id(Station),
         <<"payload">> => #{{text, <<"ok">>} => 1}}),
     T(<<"reply.error">>, [<<"D1">>], ?REPLY_LABEL, station_identity,
       #{<<"alg">> => alg(P), <<"frame_type">> => {text, <<"error">>}, <<"request_id">> => <<1:128>>,
         <<"request_hash">> => Hash, <<"responded_by">> => key_id(Station),
         <<"code">> => {text, <<"example_code">>}, <<"detail">> => {text, <<"for people">>}}),
     T(<<"relay_error">>, [<<"D1">>], ?RELAY_ERROR_LABEL, station_identity,
       #{<<"alg">> => alg(P), <<"frame_type">> => {text, <<"error">>}, <<"request_id">> => <<1:128>>,
         <<"request_hash">> => Hash, <<"reported_by">> => key_id(Station),
         <<"code">> => {text, <<"unknown_next_peer">>}}),
     T(<<"stream.provider.stream_data">>, [<<"D1">>], ?STREAM_LABEL, station_identity,
       Stream(<<"stream_data">>, Station, #{<<"encoding">> => {text, <<"raw">>}, <<"body">> => <<"chunk">>})),
     T(<<"stream.caller.stream_end">>, [<<"D1">>], ?CALLER_STREAM_LABEL, client_identity,
       Stream(<<"stream_end">>, Client, #{<<"role">> => {text, <<"send">>}})),
     T(<<"publication">>, [<<"D1">>], ?PUBLICATION_LABEL, client_identity,
       #{<<"alg">> => alg(P), <<"payload">> => #{}, <<"published_at">> => ?T0, <<"publisher">> => key_id(Client),
         <<"realm">> => <<1:256>>, <<"seq">> => 1, <<"topic">> => {text, <<"example.topic">>}}),
     tbs_case(<<"handshake.tls_binding">>, P, Keys, [<<"D1">>], <<"MACULA-PQ-BINDING-TLS-V1">>, station_identity, TlsBinding,
              unkeyed),
     tbs_case(<<"handshake.connect_binding">>, P, Keys, [<<"D1">>], <<"MACULA-PQ-BINDING-CONNECT-V1">>, client_identity,
              (Binding(<<"connect">>, <<"MACULA-PQ-BINDING-CONNECT-V1">>, <<"connect key">>))
                  #{<<"node_id">> => key_id(Client), <<"not_after">> => ?T0 + ?DAY}, unkeyed),
     tbs_case(<<"handshake.status_statement">>, P, Keys, [<<"D1">>], <<"MACULA-PQ-STATUS-V1">>, station_identity,
              #{<<"label">> => {text, <<"MACULA-PQ-STATUS-V1">>}, <<"node_id">> => key_id(Station),
                <<"binding_hash">> => crypto:hash(sha384, encode(TlsBinding)),
                <<"issued_at">> => ?T0, <<"expires_at">> => ?T0 + ?HOUR, <<"sig_alg">> => alg(P)},
              unkeyed)].

tbs_case(Id, P, Keys, Covers, Label, Role, Fields, Form) ->
    Signer = key(Keys, P, Role),
    Tbs = encode(Fields),
    Input = signing_input(Form, Label, carried(Signer), Tbs),
    Prof = atom_to_binary(P),
    #{<<"id">> => <<"tbs.", Id/binary, ".", Prof/binary>>, <<"profile">> => Prof, <<"covers">> => Covers,
      <<"lands_with">> => lands(lands_with(Id)),
      <<"input">> => #{<<"label">> => Label, <<"signer">> => atom_to_binary(Role), <<"fields">> => tj(cbor(Fields)),
                       <<"form">> => atom_to_binary(Form)},
      <<"expect">> => #{<<"outcome">> => <<"accept">>, <<"tbs">> => hex(Tbs), <<"signing_input">> => hex(Input),
                        <<"signature">> => hex(macula_node_keys:sign(Input, Signer))}}.

signing_input(keyed, Label, Carried, Tbs) -> signed_bytes(Label, Carried, Tbs);
signing_input(unkeyed, Label, _Carried, Tbs) -> <<Label/binary, 0, Tbs/binary>>.

lands_with(<<"record.", _/binary>>) -> <<"macula#100">>;
lands_with(<<"handshake.", _/binary>>) -> <<"macula#98">>;
lands_with(_) -> <<"macula#99">>.

%% Typed JSON (FORMAT.md, Values): text as a string, bytes as {"$hex": ...}, maps with text keys as objects.
tj({text, B}) -> B;
tj(B) when is_binary(B) -> #{<<"$hex">> => hex(B)};
tj(L) when is_list(L) -> [tj(V) || V <- L];
tj(M) when is_map(M) -> maps:from_list([{tj_key(K), tj(V)} || K := V <- M]);
tj(I) when is_integer(I) -> I;
tj(null) -> null.

tj_key({text, K}) -> K.

%%------------------------------------------------------------------
%% One frame per frame type (D9): today's constructors (scripts/wire_1_0_frame_specs.erl) build a valid frame, which
%% is rewritten to wire 1.0: version 5 and no capabilities header (WIRE_1_0.md sections 2 and 3.1).

frame_cases(Keys) ->
    [frame_case(Type, Frame) || {Type, Frame} <- wire_1_0_frame_specs:frames(Keys)].

frame_case(Type, Frame) ->
    <<_Len:32, Cbor/binary>> = iolist_to_binary(macula_frame:encode(Frame)),
    {ok, Map} = macula_record_cbor:decode_strict(Cbor),
    Wire = maps:remove({text, <<"capabilities">>}, Map#{{text, <<"version">>} => 5}),
    #{<<"id">> => <<"frames.", Type/binary>>, <<"profile">> => <<"pq_pure">>, <<"covers">> => [<<"D1">>, <<"D9">>],
      <<"lands_with">> => lands(<<"macula#99">>),
      <<"input">> => #{<<"frame_type">> => Type, <<"frame">> => hex(macula_record_cbor:encode(Wire)),
                       <<"position">> => position(Type), <<"after_hello">> => 1},
      <<"expect">> => #{<<"outcome">> => <<"accept">>}}.

%% The control stream carries what macula_frame:control_frame/1 names; every other frame type a dedicated stream.
position(Type) ->
    case macula_frame:control_frame(binary_to_atom(Type)) of
        true -> <<"control_stream">>;
        false -> <<"dedicated_stream">>
    end.

%% The station's own behaviour (macula-station#94): what a custodian stores, unknown frame types on a station link,
%% and the persisted floor.
station_too(<<"custodian">>) -> [<<"macula#100">>, ?STATION];
station_too(_Reader) -> [<<"macula#100">>].

envelope_lands(<<"unknown_frame_type.", _/binary>>) -> [<<"macula#99">>, ?STATION];
envelope_lands(<<"retired_", _/binary>>) -> [<<"macula#99">>, ?STATION];
envelope_lands(_) -> [<<"macula#99">>].

handshake_lands(Id) ->
    case binary:match(Id, <<"below_floor">>) of
        nomatch -> [<<"macula#98">>];
        _ -> [<<"macula#98">>, ?STATION]
    end.

%% lands_with is a list: the issues whose code makes a case pass.
lands(L) when is_list(L) -> L;
lands(B) when is_binary(B) -> [B].

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
