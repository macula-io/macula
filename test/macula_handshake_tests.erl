%% EUnit tests for macula_handshake: the wire 1.0 connection handshake (docs/design/WIRE_1_0.md section 4), built and
%% checked as bytes. The client checks the challenge before it signs the proof. The station checks CONNECT, the puzzle
%% before any signature, and answers with the HELLO bytes to send. The transcripts of the conformance vectors run in
%% macula_wire_1_0_handshake_tests; these cover what the vectors do not pin.
-module(macula_handshake_tests).

-include_lib("eunit/include/eunit.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).
-define(NOW, 1789000000000).
-define(MINUTE, 60000).
-define(HOUR, 3600000).
-define(DAY, 86400000).
-define(LEAF, <<"the strict DER leaf the station presented">>).
-define(PROOF_LABEL, "MACULA-PQ-CONNECT-PROOF-V3").
-define(SESSION_PROOF_LABEL, "MACULA-PQ-SESSION-PROOF-V2").
-define(EXPORTER_LABEL, <<"EXPORTER-macula-session-v1">>).
-define(MLDSA87_SIGNATURE_BYTES, 4627).
-define(STATION_CAPABILITIES, 5).
-define(CLIENT_CAPABILITIES, 3).

pq_pure_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun() -> world(pq_pure) end, fun all_cases/1}}.

pq_hybrid_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun() -> world(pq_hybrid) end, fun all_cases/1}}.

all_cases(World) ->
    handshake_cases(World) ++ proof_cases(World) ++ client_refusal_cases(World) ++ decoding_cases(World)
        ++ station_refusal_cases(World) ++ puzzle_cases(World) ++ hello_cases(World) ++ status_cases(World)
        ++ negotiation_cases(World) ++ session_refusal_cases(World) ++ session_proof_cases(World)
        ++ ml_dsa_broken_cases(World).

%%------------------------------------------------------------------
%% The whole handshake
%%------------------------------------------------------------------

handshake_cases(#{profile := Profile, station_public := StationPublic, client_public := ClientPublic} = World) ->
    Challenge = challenge(World),
    {ok, Connect, Station, Expect} = macula_handshake:answer_challenge(Challenge, client_session(World)),
    {accepted, Client, Hello} = macula_handshake:accept_connect(Connect, station_session(World, Challenge)),
    [?_assertEqual({ok, choice(World)},
                   macula_handshake:read_opener(macula_handshake:opener(macula_handshake:ranges(Profile)),
                                                macula_handshake:ranges(Profile))),
     ?_assertEqual(macula_node_keys:node_id(StationPublic, Profile), maps:get(node_id, Station)),
     ?_assertEqual(?NOW + ?HOUR, maps:get(status_expires_at, Station)),
     ?_assertEqual(?NOW + 7 * ?DAY, maps:get(binding_not_after, Station)),
     ?_assertEqual(macula_node_keys:node_id(ClientPublic, Profile), maps:get(node_id, Client)),
     ?_assertEqual(?CLIENT_CAPABILITIES, maps:get(capabilities, Client)),
     ?_assertEqual(?NOW + ?HOUR, maps:get(status_expires_at, Client)),
     ?_assertEqual(?NOW + ?DAY, maps:get(binding_not_after, Client)),
     ?_assertEqual({ok, #{capabilities => ?STATION_CAPABILITIES, version => 5}},
                   macula_handshake:read_hello(Hello, Expect)),
     ?_assertEqual(5, maps:get(version, Station)),
     ?_assertEqual(5, maps:get(version, Client)),
     ?_assertEqual([<<"capabilities">>, <<"connect_binding">>, <<"connect_key">>, <<"connect_status">>,
                    <<"frame_type">>, <<"identity_key">>, <<"member_endorsement">>, <<"profile">>, <<"profiles">>,
                    <<"proof">>, <<"version">>, <<"versions">>], frame_keys(Connect)),
     ?_assertEqual([<<"accepted">>, <<"capabilities">>, <<"frame_type">>, <<"session_proof">>, <<"version">>],
                   frame_keys(Hello)),
     %% Every frame carries version 5: the frame version is the handshake version.
     ?_assertEqual([5, 5, 5, 5], [field(F, <<"version">>) || F <- [macula_handshake:opener(ranges(World)), Challenge,
                                                                  Connect, Hello]]),
     %% Every challenge carries a fresh nonce.
     ?_assertNotEqual(Challenge, challenge(World))].

%% Proof V3: label || 0x00 || nonce || station node_id || client node_id || SHA-384(leaf) || SHA-384(challenge) || E
%% || SHA-384(CONNECT tbs), the CONNECT with its proof empty. Session proof V2: label || 0x00 || E || SHA-384(challenge)
%% || SHA-384(CONNECT) || station node_id || client node_id || SHA-384(HELLO tbs), the HELLO with its proof empty.
proof_cases(#{profile := Profile, station_public := StationPublic, client_public := ClientPublic,
              connect_public := ConnectPublic} = World) ->
    Challenge = challenge(World),
    {ok, Connect, _Station, _Expect} = macula_handshake:answer_challenge(Challenge, client_session(World)),
    {accepted, _Client, Hello} = macula_handshake:accept_connect(Connect, station_session(World, Challenge)),
    StationId = macula_node_keys:node_id(StationPublic, Profile),
    ClientId = macula_node_keys:node_id(ClientPublic, Profile),
    E = exported(session_a, <<ClientId/binary, StationId/binary>>),
    ConnectMessage = [<<?PROOF_LABEL, 0>>, field(Challenge, <<"nonce">>), StationId, ClientId,
                      crypto:hash(sha384, ?LEAF), crypto:hash(sha384, Challenge), E,
                      crypto:hash(sha384, rebuilt(Connect, #{<<"proof">> => <<>>}))],
    SessionMessage = [<<?SESSION_PROOF_LABEL, 0>>, E, crypto:hash(sha384, Challenge), crypto:hash(sha384, Connect),
                      StationId, ClientId, crypto:hash(sha384, rebuilt(Hello, #{<<"session_proof">> => <<>>}))],
    [?_assertEqual(32, byte_size(E)),
     ?_assert(macula_node_keys:verify(ConnectMessage, field(Connect, <<"proof">>), ConnectPublic, Profile)),
     ?_assert(macula_node_keys:verify(SessionMessage, field(Hello, <<"session_proof">>), StationPublic, Profile))].

%%------------------------------------------------------------------
%% The client refuses a challenge, and sends no CONNECT
%%------------------------------------------------------------------

client_refusal_cases(#{profile := Profile, station_public := StationPublic, connect_public := ConnectPublic,
                       client_id := ClientId} = World) ->
    Challenge = challenge(World),
    Answer = fun(Changes) ->
        macula_handshake:answer_challenge(Challenge, maps:merge(client_session(World), Changes))
    end,
    Derived = macula_node_keys:node_id(StationPublic, Profile),
    [?_assertEqual({error, {peer_identity_mismatch, #{expected => <<1:256>>, derived => Derived}}},
                   Answer(#{expected_node_id => <<1:256>>})),
     ?_assertEqual({error, binding_key_mismatch}, Answer(#{leaf => <<"a leaf this handshake never saw">>})),
     ?_assertEqual({error, status_expired}, Answer(#{now => ?NOW + 2 * ?HOUR})),
     ?_assertEqual({error, binding_not_yet_valid}, Answer(#{now => ?NOW - ?HOUR})),
     %% A key serves one purpose: the leaf's key is neither the station's identity key nor this client's CONNECT key.
     ?_assertEqual({error, key_purpose_reuse}, Answer(#{leaf => leaf_with(StationPublic)})),
     ?_assertEqual({error, key_purpose_reuse}, Answer(#{leaf => leaf_with(ConnectPublic)})),
     %% A misconfigured node fails locally: its own CONNECT key is its identity key.
     ?_assertEqual({error, key_purpose_reuse}, Answer(#{connect_key => ClientId})),
     %% Keys are compared whole: a leaf key that differs from the identity key only in its last byte is no reuse.
     ?_assertEqual({error, binding_key_mismatch},
                   Answer(#{leaf => leaf_with(near_copy(StationPublic, ConnectPublic))})),
     ?_assertEqual({error, unexpected_frame},
                   macula_handshake:answer_challenge(macula_handshake:opener(ranges(World)), client_session(World))),
     %% Its own floor for the station: a station it completed a higher version with is never completed below it.
     ?_assertEqual({error, version_below_floor}, Answer(#{floor => 6})),
     ?_assertMatch({ok, _, _, _}, Answer(#{floor => 5}))].

decoding_cases(#{profile := Profile, station_public := StationPublic} = World) ->
    Challenge = challenge(World),
    Answer = fun(Bytes) -> macula_handshake:answer_challenge(Bytes, client_session(World)) end,
    [?_assertEqual({error, malformed_frame}, Answer(<<Challenge/binary, 0>>)),
     ?_assertEqual({error, malformed_frame}, Answer(<<"not cbor">>)),
     ?_assertEqual({error, malformed_frame}, Answer(duplicate_key(Challenge, <<"nonce">>))),
     ?_assertEqual({error, malformed_frame}, Answer(rebuilt(Challenge, #{<<"comment">> => {text, <<"x">>}}))),
     ?_assertEqual({error, malformed_frame}, Answer(without(Challenge, <<"tls_status">>))),
     ?_assertEqual({error, malformed_frame}, Answer(rebuilt(Challenge, #{<<"nonce">> => <<0:248>>}))),
     ?_assertEqual({error, malformed_frame}, Answer(without(Challenge, <<"versions">>))),
     ?_assertEqual({error, malformed_frame}, Answer(rebuilt(Challenge, #{<<"versions">> => [5]}))),
     ?_assertEqual({error, malformed_frame}, Answer(rebuilt(Challenge, #{<<"profiles">> => []}))),
     ?_assertEqual({error, malformed_frame},
                   Answer(rebuilt(Challenge, #{<<"profiles">> => [{text, <<"a">>}, {text, <<"a">>}]}))),
     ?_assertEqual({error, malformed_frame},
                   Answer(rebuilt(Challenge, #{<<"profiles">> => [{text, integer_to_binary(N)} || N <- lists:seq(1, 9)]}))),
     %% One encoding: bytes that decode but are not deterministic CBOR are refused.
     ?_assertEqual({error, malformed_frame}, Answer(non_minimal_length(Challenge))),
     ?_assertEqual({error, malformed_frame}, Answer(rebuilt(Challenge, #{<<"profile">> => atom_to_binary(Profile)}))),
     ?_assertEqual({error, malformed_frame},
                   Answer(rebuilt(Challenge, #{<<"tls_binding">> => #{{text, <<"tbs">>} => <<>>}}))),
     ?_assertEqual({error, malformed_frame},
                   Answer(rebuilt(Challenge, #{<<"identity_key">> => binary:part(StationPublic, 0, 2591)}))),
     ?_assertEqual({error, unsupported_version}, Answer(rebuilt(Challenge, #{<<"version">> => 4}))),
     ?_assertEqual({error, downgrade_detected},
                   Answer(rebuilt(Challenge, #{<<"profile">> => {text, atom_to_binary(other_profile(Profile))}})))]
        ++ carried_key_cases(Challenge, Answer, StationPublic, Profile).

%% A pq_hybrid carried key has exactly one encoding: bytes that are not a canonical DER RSAPublicKey are refused.
carried_key_cases(Challenge, Answer, <<MlDsa:2592/binary, Der/binary>>, pq_hybrid) ->
    Garbage = <<MlDsa/binary, (binary:copy(<<16#30>>, byte_size(Der)))/binary>>,
    [?_assertEqual({error, malformed_frame}, Answer(rebuilt(Challenge, #{<<"identity_key">> => Garbage})))];
carried_key_cases(_Challenge, _Answer, _StationPublic, pq_pure) ->
    [].

%%------------------------------------------------------------------
%% The station refuses CONNECT, and sends only a refusing HELLO
%%------------------------------------------------------------------

station_refusal_cases(#{profile := Profile, client_public := ClientPublic, station_public := StationPublic,
                        connect_public := ConnectPublic} = World) ->
    Challenge = challenge(World),
    {ok, Connect, _Station, _Expect} = macula_handshake:answer_challenge(Challenge, client_session(World)),
    Accept = fun(Bytes, Changes) ->
        macula_handshake:accept_connect(Bytes, maps:merge(station_session(World, Challenge), Changes))
    end,
    OtherChallenge = challenge(World),
    Version4 = rebuilt(Connect, #{<<"version">> => 4}),
    [?_assertMatch({refused, proof_invalid, _}, Accept(Connect, #{leaf => <<"a leaf never presented">>})),
     ?_assertMatch({refused, proof_invalid, _}, Accept(Connect, #{challenge => OtherChallenge})),
     ?_assertMatch({refused, binding_key_mismatch, _},
                   Accept(rebuilt(Connect, #{<<"connect_key">> => StationPublic}), #{})),
     %% A key serves one purpose: a CONNECT key that is the identity key, or the key of the presented leaf, is refused.
     ?_assertMatch({refused, key_purpose_reuse, _},
                   Accept(rebuilt(Connect, #{<<"connect_key">> => ClientPublic}), #{})),
     ?_assertMatch({refused, key_purpose_reuse, _}, Accept(Connect, #{leaf => leaf_with(ConnectPublic)})),
     ?_assertMatch({refused, binding_key_mismatch, _},
                   Accept(rebuilt(Connect, #{<<"connect_key">> => near_copy(ClientPublic, ConnectPublic)}), #{})),
     ?_assertEqual({error, {refused, not_accepted}},
                   hello_of(Accept(rebuilt(Connect, #{<<"connect_key">> => ClientPublic}), #{}))),
     ?_assertMatch({refused, status_expired, _}, Accept(Connect, #{now => ?NOW + 2 * ?HOUR})),
     ?_assertMatch({refused, unexpected_frame, _}, Accept(macula_handshake:opener(ranges(World)), #{})),
     ?_assertMatch({refused, unsupported_version, _}, Accept(Version4, #{})),
     %% The station's floor for the client.
     ?_assertMatch({refused, version_below_floor, _}, Accept(Connect, #{floor => fun(_NodeId) -> 6 end})),
     ?_assertMatch({accepted, _, _}, Accept(Connect, #{floor => fun(_NodeId) -> 5 end})),
     ?_assertMatch({refused, malformed_frame, _}, Accept(<<"not cbor">>, #{})),
     ?_assertMatch({refused, malformed_frame, _}, Accept(rebuilt(Connect, #{<<"proof">> => <<0:64>>}), #{})),
     %% On the wire, one coarse refusal code.
     ?_assertEqual({error, {refused, not_accepted}}, hello_of(Accept(Connect, #{challenge => OtherChallenge}))),
     ?_assertEqual({error, {refused, not_accepted}}, hello_of(Accept(<<"not cbor">>, #{}))),
     ?_assertEqual({error, {refused, unsupported_version}}, hello_of(Accept(Version4, #{})))]
        ++ missing_key_cases(Accept, Connect)
        ++ shared_half_cases(Accept, Connect, ConnectPublic, ClientPublic, Profile).

%% A CONNECT that lacks one of its keys beside version and frame_type is malformed, whichever key it lacks.
missing_key_cases(Accept, Connect) ->
    [?_assertMatch({refused, malformed_frame, _}, Accept(without(Connect, Key), #{}))
     || Key <- frame_keys(Connect), Key =/= <<"version">>, Key =/= <<"frame_type">>].

%% In pq_hybrid, a CONNECT key that shares either half with the identity key serves two purposes too.
shared_half_cases(Accept, Connect, <<ConnectMlDsa:2592/binary, ConnectRsa/binary>>,
                  <<IdentityMlDsa:2592/binary, IdentityRsa/binary>>, pq_hybrid) ->
    [?_assertMatch({refused, key_purpose_reuse, _},
                   Accept(rebuilt(Connect, #{<<"connect_key">> => <<ConnectMlDsa/binary, IdentityRsa/binary>>}), #{})),
     ?_assertMatch({refused, key_purpose_reuse, _},
                   Accept(rebuilt(Connect, #{<<"connect_key">> => <<IdentityMlDsa/binary, ConnectRsa/binary>>}), #{}))];
shared_half_cases(_Accept, _Connect, _ConnectPublic, _ClientPublic, pq_pure) ->
    [].

%%------------------------------------------------------------------
%% The puzzle, checked by the station before any signature
%%------------------------------------------------------------------

puzzle_cases(#{profile := Profile, client_public := ClientPublic} = World) ->
    Challenge = challenge(World),
    {ok, Connect, _Station, _Expect} = macula_handshake:answer_challenge(Challenge, client_session(World)),
    Solved = leading_zero_bits(macula_node_keys:node_id(ClientPublic, Profile)),
    Unsolved = Solved + 1,
    Accept = fun(Puzzle, Changes) ->
        Session = maps:merge(station_session(World, Challenge), Changes),
        macula_handshake:accept_connect(Connect, Session#{puzzle := Puzzle})
    end,
    Enforced = Accept(#{difficulty => Unsolved, mode => enforce}, #{}),
    [%% Under enforce, the station refuses: the only HELLO it produces refuses with puzzle_invalid.
     ?_assertMatch({refused, puzzle_invalid, _}, Enforced),
     ?_assertEqual({error, {refused, puzzle_invalid}}, hello_of(Enforced)),
     %% The puzzle comes before the proof: a CONNECT that also fails its proof is still refused for the puzzle.
     ?_assertMatch({refused, puzzle_invalid, _},
                   Accept(#{difficulty => Unsolved, mode => enforce}, #{leaf => <<"a leaf never presented">>})),
     ?_assertMatch({accepted, #{puzzle := solved}, _}, Accept(#{difficulty => Solved, mode => enforce}, #{})),
     ?_assertMatch({accepted, #{puzzle := unsolved}, _}, Accept(#{difficulty => Unsolved, mode => log_only}, #{})),
     ?_assertMatch({accepted, #{puzzle := not_checked}, _}, Accept(#{difficulty => Unsolved, mode => off}, #{}))].

%%------------------------------------------------------------------
%% HELLO, read by the client
%%------------------------------------------------------------------

%% A refusing HELLO, read by the client: an unknown code reads as not_accepted and says it fell back.
hello_cases(World) ->
    Challenge = challenge(World),
    {ok, _Connect, _Station, Expect} = macula_handshake:answer_challenge(Challenge, client_session(World)),
    Base = #{{text, <<"version">>} => 5, {text, <<"frame_type">>} => {text, <<"hello">>},
             {text, <<"capabilities">>} => 7},
    Hello = fun(Fields) -> macula_record_cbor:encode(maps:merge(Base, Fields)) end,
    Refused = fun(Code) -> #{{text, <<"accepted">>} => 0, {text, <<"refusal_code">>} => {text, Code}} end,
    Read = fun(Bytes) -> macula_handshake:read_hello(Bytes, Expect) end,
    [?_assertEqual({error, {refused, puzzle_invalid}}, Read(Hello(Refused(<<"puzzle_invalid">>)))),
     ?_assertEqual({refused, puzzle_invalid, known}, macula_handshake:read_refusal(Hello(Refused(<<"puzzle_invalid">>)))),
     ?_assertEqual({error, {refused, not_accepted}}, Read(Hello(Refused(<<"proof_invalid">>)))),
     ?_assertEqual({refused, not_accepted, fallback},
                   macula_handshake:read_refusal(Hello(Refused(<<"proof_invalid">>)))),
     ?_assertEqual({error, malformed_frame}, Read(Hello(#{{text, <<"accepted">>} => 0}))),
     ?_assertEqual({error, malformed_frame}, Read(Hello((Refused(<<"not_accepted">>))#{{text, <<"accepted">>} => 1}))),
     ?_assertEqual({error, malformed_frame}, Read(Hello(#{{text, <<"accepted">>} => 2}))),
     ?_assertEqual({error, session_proof_missing}, Read(Hello(#{{text, <<"accepted">>} => 1}))),
     ?_assertEqual({error, malformed_frame},
                   Read(Hello((Refused(<<"not_accepted">>))#{{text, <<"capabilities">>} => 1 bsl 53}))),
     ?_assertEqual({error, unsupported_version},
                   Read(Hello((Refused(<<"not_accepted">>))#{{text, <<"version">>} => 4}))),
     ?_assertEqual({error, unexpected_frame}, Read(macula_handshake:opener(ranges(World))))].

%%------------------------------------------------------------------
%% Status frames on an open connection
%%------------------------------------------------------------------

status_cases(#{profile := Profile, station_id := StationId, station_public := StationPublic,
               tls_binding := Binding} = World) ->
    Statement = macula_key_bindings:status_statement(StationId, Binding, ?NOW + 15 * ?MINUTE, ?NOW + 75 * ?MINUTE),
    Frame = macula_handshake:status(Statement),
    Peer = #{profile => Profile, identity_key => StationPublic, binding => Binding, now => ?NOW + 16 * ?MINUTE},
    OtherBinding = macula_key_bindings:tls_binding(StationId, ?LEAF, ?NOW, ?NOW + ?DAY),
    [?_assertEqual({ok, ?NOW + 75 * ?MINUTE}, macula_handshake:read_status(Frame, Peer)),
     ?_assertEqual({error, status_binding_mismatch},
                   macula_handshake:read_status(Frame, Peer#{binding := OtherBinding})),
     ?_assertEqual({error, unexpected_frame}, macula_handshake:read_status(macula_handshake:opener(ranges(World)), Peer)),
     ?_assertEqual(5, field(Frame, <<"version">>)),
     %% Its issued_at, read before any verification, so a connection can drop a statement that is not newer than the
     %% last one it accepted without paying for the check.
     ?_assertEqual({ok, ?NOW + 15 * ?MINUTE}, macula_handshake:status_issued_at(wire(Frame))),
     ?_assertEqual(error, macula_handshake:status_issued_at(
                            wire(macula_handshake:status(Statement#{tbs := <<"not a tbs">>})))),
     ?_assertEqual(error, macula_handshake:status_issued_at(wire(macula_handshake:opener(ranges(World)))))].

wire(Bytes) ->
    {ok, Wire} = macula_record_cbor:decode_strict(Bytes),
    Wire.

%%------------------------------------------------------------------
%% Negotiation (section 4.1): the station chooses, the client checks the choice, CONNECT repeats the ranges
%%------------------------------------------------------------------

negotiation_cases(#{profile := Profile} = World) ->
    Other = atom_to_binary(other_profile(Profile)),
    Own = atom_to_binary(Profile),
    Read = fun(Versions, Profiles) ->
                   macula_handshake:read_opener(opener_frame(Versions, Profiles), macula_handshake:ranges(Profile))
           end,
    Challenge = challenge(World),
    {ok, Connect, _Station, _Expect} = macula_handshake:answer_challenge(Challenge, client_session(World)),
    Accept = fun(Bytes, Changes) ->
        macula_handshake:accept_connect(Bytes, maps:merge(station_session(World, Challenge), Changes))
    end,
    [%% The highest common version, and the first of the client's profiles the station supports.
     ?_assertMatch({ok, #{version := 5, profile := Profile}}, Read([4, 7], [<<"example">>, Other, Own])),
     ?_assertMatch({refused, unsupported_version, _}, Read([6, 7], [Own])),
     ?_assertMatch({refused, unsupported_version, _}, Read([3, 4], [Own])),
     ?_assertMatch({refused, profile_unsupported, _}, Read([5, 5], [Other])),
     ?_assertEqual({error, malformed_frame}, Read([5, 4], [Own])),
     ?_assertEqual({error, malformed_frame}, Read([5, 5], [])),
     %% The early refusal names the station's ranges, and its client reads them.
     ?_assertEqual({error, {refused, unsupported_version, #{versions => [5, 5], profiles => [Own]}}},
                   macula_handshake:answer_challenge(element(3, Read([6, 7], [Own])), client_session(World))),
     %% CONNECT repeats the opener's ranges and names the profile chosen.
     ?_assertMatch({refused, negotiation_mismatch, _},
                   Accept(Connect, #{choice => (choice(World))#{client_ranges => #{versions => [4, 5],
                                                                                   profiles => [Own]}}})),
     ?_assertMatch({refused, profile_unsupported, _},
                   Accept(rebuilt(Connect, #{<<"profile">> => {text, Other}}), #{})),
     %% A changed range breaks the proof too: the CONNECT tbs covers it.
     ?_assertMatch({refused, proof_invalid, _},
                   Accept(rebuilt(Connect, #{<<"capabilities">> => ?CLIENT_CAPABILITIES + 1}), #{}))].

opener_frame(Versions, Profiles) ->
    macula_record_cbor:encode(#{{text, <<"version">>} => 5, {text, <<"frame_type">>} => {text, <<"opener">>},
                                {text, <<"versions">>} => Versions,
                                {text, <<"profiles">>} => [{text, P} || P <- Profiles]}).

%%------------------------------------------------------------------
%% Proofs bound to the TLS session's exporter, signed only after every check
%%------------------------------------------------------------------

session_refusal_cases(#{profile := Profile} = World) ->
    Challenge = challenge(World),
    {ok, Connect, _Station, Expect} = macula_handshake:answer_challenge(Challenge, client_session(World)),
    Accept = fun(Bytes, Changes) ->
        macula_handshake:accept_connect(Bytes, maps:merge(station_session(World, Challenge), Changes))
    end,
    Rated = fun(_ClientNodeId, _Message) -> {error, session_proof_rate} end,
    Unavailable = fun(_Label, _Context, _Length) -> {error, closed} end,
    [%% A CONNECT proof over another session's exporter is refused.
     ?_assertMatch({refused, proof_invalid, _}, Accept(Connect, #{export => exporter(session_b)})),
     ?_assertMatch({refused, exporter_unavailable, _}, Accept(Connect, #{export => Unavailable})),
     ?_assertEqual({error, exporter_unavailable},
                   macula_handshake:answer_challenge(Challenge, (client_session(World))#{export => Unavailable})),
     %% Past the rate limit the station refuses and signs nothing, and says so on the wire.
     ?_assertMatch({refused, session_proof_rate, _}, Accept(Connect, #{sign_session_proof => Rated})),
     ?_assertEqual({error, {refused, session_proof_rate}},
                   macula_handshake:read_hello(element(3, Accept(Connect, #{sign_session_proof => Rated})), Expect)),
     ?_test(signs_only_after_every_check(Connect, Accept, Profile))].

%% Sign after verify: a CONNECT that fails any check never reaches the signer.
signs_only_after_every_check(Connect, Accept, _Profile) ->
    Self = self(),
    Watched = fun(_ClientNodeId, Message) -> Self ! {signed, Message}, {error, session_proof_rate} end,
    {refused, proof_invalid, _} = Accept(Connect, #{export => exporter(session_b), sign_session_proof => Watched}),
    {refused, status_expired, _} = Accept(Connect, #{now => ?NOW + 2 * ?HOUR, sign_session_proof => Watched}),
    ?assertEqual(nothing, receive {signed, _} -> signed after 0 -> nothing end),
    {refused, session_proof_rate, _} = Accept(Connect, #{sign_session_proof => Watched}),
    ?assertEqual(signed, receive {signed, _} -> signed after 0 -> nothing end).

session_proof_cases(#{station_id := StationKey} = World) ->
    Challenge = challenge(World),
    {ok, Connect, _Station, Expect} = macula_handshake:answer_challenge(Challenge, client_session(World)),
    {accepted, _Client, Hello} = macula_handshake:accept_connect(Connect, station_session(World, Challenge)),
    Read = fun(Bytes) -> macula_handshake:read_hello(Bytes, Expect) end,
    OtherSession = rebuilt(Hello, #{<<"session_proof">> =>
                                        macula_node_keys:sign(session_message(World, Challenge, Connect, Hello,
                                                                              session_b),
                                                              StationKey)}),
    OtherChallenge = challenge(World),
    {ok, OtherConnect, _, _} = macula_handshake:answer_challenge(OtherChallenge, client_session(World)),
    {accepted, _, OtherConnection} = macula_handshake:accept_connect(OtherConnect,
                                                                     station_session(World, OtherChallenge)),
    {ok, OtherKey} = macula_node_keys:generate(identity, maps:get(profile, World)),
    SignedByOther = rebuilt(Hello, #{<<"session_proof">> =>
                                         macula_node_keys:sign(session_message(World, Challenge, Connect, Hello,
                                                                               session_a), OtherKey)}),
    [?_assertEqual({error, session_proof_invalid}, Read(OtherSession)),
     ?_assertEqual({error, session_proof_invalid}, Read(OtherConnection)),
     ?_assertEqual({error, session_proof_invalid}, Read(SignedByOther)),
     %% The HELLO tbs covers the station's capabilities, at no fixed width.
     ?_assertEqual({error, session_proof_invalid},
                   Read(rebuilt(Hello, #{<<"capabilities">> => ?STATION_CAPABILITIES bor (1 bsl 40)}))),
     ?_assertEqual({error, session_proof_missing}, Read(without(Hello, <<"session_proof">>))),
     ?_assertEqual({error, malformed_frame}, Read(rebuilt(Hello, #{<<"session_proof">> => <<0:64>>})))].

%% The case D17 exists for: an attacker who can forge ML-DSA-87 but not RSA-PSS-4096. A session proof whose
%% ML-DSA half is the station's own, valid one, and whose RSA half comes from another key, is refused.
ml_dsa_broken_cases(#{profile := pq_hybrid} = World) ->
    Challenge = challenge(World),
    {ok, Connect, _Station, Expect} = macula_handshake:answer_challenge(Challenge, client_session(World)),
    {accepted, _Client, Hello} = macula_handshake:accept_connect(Connect, station_session(World, Challenge)),
    {ok, OtherKey} = macula_node_keys:generate(identity, pq_hybrid),
    <<MlDsaHalf:?MLDSA87_SIGNATURE_BYTES/binary, _Rsa/binary>> = field(Hello, <<"session_proof">>),
    <<_:?MLDSA87_SIGNATURE_BYTES/binary, OtherRsa/binary>> =
        macula_node_keys:sign(session_message(World, Challenge, Connect, Hello, session_a), OtherKey),
    Spliced = rebuilt(Hello, #{<<"session_proof">> => <<MlDsaHalf/binary, OtherRsa/binary>>}),
    [?_assertEqual({error, session_proof_invalid}, macula_handshake:read_hello(Spliced, Expect))];
ml_dsa_broken_cases(#{profile := pq_pure}) ->
    [].

%% A stand-in for one TLS session's exporter: the same label, context and length give the same bytes on both ends
%% of the session, and another session gives other bytes.
exporter(Session) ->
    fun(Label, Context, Length) when Label =:= ?EXPORTER_LABEL -> {ok, exported(Session, Context, Length)} end.

exported(Session, Context) ->
    exported(Session, Context, 32).

exported(Session, Context, Length) ->
    binary:part(crypto:mac(hmac, sha256, atom_to_binary(Session), [?EXPORTER_LABEL, Context]), 0, Length).

%% The session proof's message for this challenge, CONNECT and HELLO under Session's exporter, as the station signs it.
session_message(#{profile := Profile, station_public := StationPublic, client_public := ClientPublic},
                Challenge, Connect, Hello, Session) ->
    StationId = macula_node_keys:node_id(StationPublic, Profile),
    ClientId = macula_node_keys:node_id(ClientPublic, Profile),
    [<<?SESSION_PROOF_LABEL, 0>>, exported(Session, <<ClientId/binary, StationId/binary>>),
     crypto:hash(sha384, Challenge), crypto:hash(sha384, Connect), StationId, ClientId,
     crypto:hash(sha384, rebuilt(Hello, #{<<"session_proof">> => <<>>}))].

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

world(Profile) ->
    {ok, StationId} = macula_node_keys:generate(identity, Profile),
    {ok, ClientId} = macula_node_keys:generate(identity, Profile),
    {ok, ClientConnect} = macula_node_keys:generate(connect, Profile),
    ConnectPublic = macula_node_keys:public_key(ClientConnect),
    TlsBinding = macula_key_bindings:tls_binding(StationId, ?LEAF, ?NOW, ?NOW + 7 * ?DAY),
    ConnectBinding = macula_key_bindings:connect_binding(ClientId, ConnectPublic, ?NOW, ?NOW + ?DAY),
    #{profile => Profile,
      station_id => StationId, station_public => macula_node_keys:public_key(StationId),
      client_id => ClientId, client_public => macula_node_keys:public_key(ClientId),
      client_connect => ClientConnect, connect_public => ConnectPublic,
      tls_binding => TlsBinding,
      tls_status => macula_key_bindings:status_statement(StationId, TlsBinding, ?NOW, ?NOW + ?HOUR),
      connect_binding => ConnectBinding,
      connect_status => macula_key_bindings:status_statement(ClientId, ConnectBinding, ?NOW, ?NOW + ?HOUR)}.

station_material(#{profile := Profile, station_public := StationPublic, tls_binding := Binding,
                   tls_status := Status}) ->
    #{profile => Profile, identity_key => StationPublic, tls_binding => Binding, tls_status => Status}.

ranges(#{profile := Profile}) -> macula_handshake:ranges(Profile).

%% What the station chooses from this world's opener.
choice(#{profile := Profile}) ->
    #{version => 5, profile => Profile, client_ranges => #{versions => [5, 5], profiles => [atom_to_binary(Profile)]}}.

challenge(World) ->
    macula_handshake:challenge(station_material(World), choice(World), ranges(World)).

client_session(#{profile := Profile, station_public := StationPublic, client_public := ClientPublic,
                 client_connect := ClientConnect, connect_binding := Binding, connect_status := Status} = World) ->
    maps:merge(ranges(World),
               #{profile => Profile, expected_node_id => macula_node_keys:node_id(StationPublic, Profile),
                 leaf => ?LEAF, identity_key => ClientPublic, connect_key => ClientConnect, connect_binding => Binding,
                 connect_status => Status, capabilities => ?CLIENT_CAPABILITIES, now => ?NOW + ?MINUTE,
                 export => exporter(session_a)}).

station_session(#{profile := Profile, station_id := StationKey} = World, Challenge) ->
    #{profile => Profile, choice => choice(World), challenge => Challenge, leaf => ?LEAF,
      puzzle => #{difficulty => 0, mode => enforce}, capabilities => ?STATION_CAPABILITIES, now => ?NOW + ?MINUTE,
      export => exporter(session_a),
      sign_session_proof => fun(_ClientNodeId, Message) -> {ok, macula_node_keys:sign(Message, StationKey)} end}.

%% What the client reads from a refusing HELLO.
hello_of({refused, _Reason, Hello}) -> refusal_read(macula_handshake:read_refusal(Hello)).

refusal_read({refused, Code, _Known}) -> {error, {refused, Code}};
refusal_read({error, _} = Error) -> Error.

%% The frame with its first map header's length written in two bytes instead of one: it decodes, and is not the
%% deterministic encoding.
non_minimal_length(<<Header, Rest/binary>>) when Header >= 16#A0, Header < 16#B8 ->
    <<16#B8, (Header - 16#A0), Rest/binary>>.

%% A key that differs from Key only in the last byte of its ML-DSA-87 half, with Other's classical half.
near_copy(<<MlDsa:2591/binary, Last, _/binary>>, <<_:2592/binary, OtherClassical/binary>>) ->
    <<MlDsa/binary, (Last bxor 1), OtherClassical/binary>>.

%% A stand-in for a certificate whose SubjectPublicKeyInfo holds a key's ML-DSA-87 half.
leaf_with(<<MlDsa:2592/binary, _Classical/binary>>) ->
    <<"certificate before the key", MlDsa/binary, "certificate after the key">>.

other_profile(pq_pure) -> pq_hybrid;
other_profile(pq_hybrid) -> pq_pure.

leading_zero_bits(NodeId) ->
    length(lists:takewhile(fun(Bit) -> Bit =:= 0 end, [Bit || <<Bit:1>> <= NodeId])).

decoded(Bytes) ->
    {ok, Map} = macula_record_cbor:decode_strict(Bytes),
    Map.

field(Bytes, Key) ->
    maps:get({text, Key}, decoded(Bytes)).

frame_keys(Bytes) ->
    lists:sort([Key || {text, Key} <- maps:keys(decoded(Bytes))]).

%% The frame with fields replaced or added, keyed by their text names.
rebuilt(Bytes, Changes) ->
    macula_record_cbor:encode(maps:merge(decoded(Bytes), #{{text, Key} => Value || Key := Value <- Changes})).

without(Bytes, Key) ->
    macula_record_cbor:encode(maps:remove({text, Key}, decoded(Bytes))).

%% The frame with a second copy of one key, which only a hand-built encoding can carry.
duplicate_key(Bytes, Key) ->
    Map = decoded(Bytes),
    Pairs = << <<(macula_record_cbor:encode(K))/binary, (macula_record_cbor:encode(V))/binary>> || K := V <- Map >>,
    Copy = <<(macula_record_cbor:encode({text, Key}))/binary,
             (macula_record_cbor:encode(maps:get({text, Key}, Map)))/binary>>,
    <<(16#A0 + map_size(Map) + 1), Pairs/binary, Copy/binary>>.
