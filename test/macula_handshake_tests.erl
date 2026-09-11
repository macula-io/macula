%% EUnit tests for macula_handshake: the post-quantum connection handshake of plans/DESIGN_PQ_HANDSHAKE_FRAMES.md,
%% built and checked as bytes. The client checks the challenge before it signs the proof. The station checks CONNECT,
%% the puzzle before any signature, and answers with the HELLO bytes to send.
-module(macula_handshake_tests).

-include_lib("eunit/include/eunit.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).
-define(NOW, 1789000000000).
-define(MINUTE, 60000).
-define(HOUR, 3600000).
-define(DAY, 86400000).
-define(LEAF, <<"the strict DER leaf the station presented">>).
-define(PROOF_LABEL, "MACULA-PQ-CONNECT-PROOF-V1").
-define(STATION_CAPABILITIES, 5).
-define(CLIENT_CAPABILITIES, 3).

pq_pure_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun() -> world(pq_pure) end, fun all_cases/1}}.

pq_hybrid_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun() -> world(pq_hybrid) end, fun all_cases/1}}.

all_cases(World) ->
    handshake_cases(World) ++ proof_cases(World) ++ client_refusal_cases(World) ++ decoding_cases(World)
        ++ station_refusal_cases(World) ++ puzzle_cases(World) ++ hello_cases() ++ status_cases(World).

%%------------------------------------------------------------------
%% The whole handshake
%%------------------------------------------------------------------

handshake_cases(#{profile := Profile, station_public := StationPublic, client_public := ClientPublic} = World) ->
    Challenge = macula_handshake:challenge(station_material(World)),
    {ok, Connect, Station} = macula_handshake:answer_challenge(Challenge, client_session(World)),
    {accepted, Client, Hello} = macula_handshake:accept_connect(Connect, station_session(World, Challenge)),
    [?_assertEqual(ok, macula_handshake:read_opener(macula_handshake:opener())),
     ?_assertEqual(macula_node_keys:node_id(StationPublic, Profile), maps:get(node_id, Station)),
     ?_assertEqual(?NOW + ?HOUR, maps:get(status_expires_at, Station)),
     ?_assertEqual(macula_node_keys:node_id(ClientPublic, Profile), maps:get(node_id, Client)),
     ?_assertEqual(?CLIENT_CAPABILITIES, maps:get(capabilities, Client)),
     ?_assertEqual(?NOW + ?HOUR, maps:get(status_expires_at, Client)),
     ?_assertEqual({ok, #{capabilities => ?STATION_CAPABILITIES}}, macula_handshake:read_hello(Hello)),
     ?_assertEqual([<<"capabilities">>, <<"connect_binding">>, <<"connect_key">>, <<"connect_status">>,
                    <<"frame_type">>, <<"identity_key">>, <<"proof">>, <<"version">>], frame_keys(Connect)),
     %% Every challenge carries a fresh nonce.
     ?_assertNotEqual(Challenge, macula_handshake:challenge(station_material(World)))].

%% The proof signs label || 0x00 || nonce || station node_id || client node_id || SHA-384(leaf) || SHA-384(challenge).
proof_cases(#{profile := Profile, station_public := StationPublic, client_public := ClientPublic,
              connect_public := ConnectPublic} = World) ->
    Challenge = macula_handshake:challenge(station_material(World)),
    {ok, Connect, _Station} = macula_handshake:answer_challenge(Challenge, client_session(World)),
    Message = [<<?PROOF_LABEL, 0>>, field(Challenge, <<"nonce">>), macula_node_keys:node_id(StationPublic, Profile),
               macula_node_keys:node_id(ClientPublic, Profile), crypto:hash(sha384, ?LEAF),
               crypto:hash(sha384, Challenge)],
    [?_assert(macula_node_keys:verify(Message, field(Connect, <<"proof">>), ConnectPublic, Profile))].

%%------------------------------------------------------------------
%% The client refuses a challenge, and sends no CONNECT
%%------------------------------------------------------------------

client_refusal_cases(#{profile := Profile, station_public := StationPublic, connect_public := ConnectPublic,
                       client_id := ClientId} = World) ->
    Challenge = macula_handshake:challenge(station_material(World)),
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
                   macula_handshake:answer_challenge(macula_handshake:opener(), client_session(World)))].

decoding_cases(#{profile := Profile, station_public := StationPublic} = World) ->
    Challenge = macula_handshake:challenge(station_material(World)),
    Answer = fun(Bytes) -> macula_handshake:answer_challenge(Bytes, client_session(World)) end,
    [?_assertEqual({error, malformed_frame}, Answer(<<Challenge/binary, 0>>)),
     ?_assertEqual({error, malformed_frame}, Answer(<<"not cbor">>)),
     ?_assertEqual({error, malformed_frame}, Answer(duplicate_key(Challenge, <<"nonce">>))),
     ?_assertEqual({error, malformed_frame}, Answer(rebuilt(Challenge, #{<<"comment">> => {text, <<"x">>}}))),
     ?_assertEqual({error, malformed_frame}, Answer(without(Challenge, <<"tls_status">>))),
     ?_assertEqual({error, malformed_frame}, Answer(rebuilt(Challenge, #{<<"nonce">> => <<0:248>>}))),
     ?_assertEqual({error, malformed_frame}, Answer(rebuilt(Challenge, #{<<"profile">> => atom_to_binary(Profile)}))),
     ?_assertEqual({error, malformed_frame},
                   Answer(rebuilt(Challenge, #{<<"tls_binding">> => #{{text, <<"tbs">>} => <<>>}}))),
     ?_assertEqual({error, malformed_frame},
                   Answer(rebuilt(Challenge, #{<<"identity_key">> => binary:part(StationPublic, 0, 2591)}))),
     ?_assertEqual({error, unsupported_version}, Answer(rebuilt(Challenge, #{<<"version">> => 2}))),
     ?_assertEqual({error, profile_mismatch},
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
    Challenge = macula_handshake:challenge(station_material(World)),
    {ok, Connect, _Station} = macula_handshake:answer_challenge(Challenge, client_session(World)),
    Accept = fun(Bytes, Changes) ->
        macula_handshake:accept_connect(Bytes, maps:merge(station_session(World, Challenge), Changes))
    end,
    OtherChallenge = macula_handshake:challenge(station_material(World)),
    Version2 = rebuilt(Connect, #{<<"version">> => 2}),
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
     ?_assertMatch({refused, unexpected_frame, _}, Accept(macula_handshake:opener(), #{})),
     ?_assertMatch({refused, unsupported_version, _}, Accept(Version2, #{})),
     ?_assertMatch({refused, malformed_frame, _}, Accept(<<"not cbor">>, #{})),
     ?_assertMatch({refused, malformed_frame, _}, Accept(rebuilt(Connect, #{<<"proof">> => <<0:64>>}), #{})),
     %% On the wire, one coarse refusal code.
     ?_assertEqual({error, {refused, not_accepted}}, hello_of(Accept(Connect, #{challenge => OtherChallenge}))),
     ?_assertEqual({error, {refused, not_accepted}}, hello_of(Accept(<<"not cbor">>, #{}))),
     ?_assertEqual({error, {refused, unsupported_version}}, hello_of(Accept(Version2, #{})))]
        ++ shared_half_cases(Accept, Connect, ConnectPublic, ClientPublic, Profile).

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
    Challenge = macula_handshake:challenge(station_material(World)),
    {ok, Connect, _Station} = macula_handshake:answer_challenge(Challenge, client_session(World)),
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

hello_cases() ->
    Base = #{{text, <<"version">>} => 3, {text, <<"frame_type">>} => {text, <<"hello">>},
             {text, <<"capabilities">>} => 7},
    Hello = fun(Fields) -> macula_record_cbor:encode(maps:merge(Base, Fields)) end,
    Refused = fun(Code) -> #{{text, <<"accepted">>} => 0, {text, <<"refusal_code">>} => {text, Code}} end,
    [?_assertEqual({ok, #{capabilities => 7}}, macula_handshake:read_hello(Hello(#{{text, <<"accepted">>} => 1}))),
     ?_assertEqual({error, {refused, puzzle_invalid}},
                   macula_handshake:read_hello(Hello(Refused(<<"puzzle_invalid">>)))),
     ?_assertEqual({error, malformed_frame}, macula_handshake:read_hello(Hello(Refused(<<"proof_invalid">>)))),
     ?_assertEqual({error, malformed_frame}, macula_handshake:read_hello(Hello(#{{text, <<"accepted">>} => 0}))),
     ?_assertEqual({error, malformed_frame},
                   macula_handshake:read_hello(Hello((Refused(<<"not_accepted">>))#{{text, <<"accepted">>} => 1}))),
     ?_assertEqual({error, malformed_frame}, macula_handshake:read_hello(Hello(#{{text, <<"accepted">>} => 2}))),
     ?_assertEqual({ok, #{capabilities => (1 bsl 53) - 1}},
                   macula_handshake:read_hello(Hello(#{{text, <<"accepted">>} => 1,
                                                       {text, <<"capabilities">>} => (1 bsl 53) - 1}))),
     ?_assertEqual({error, malformed_frame},
                   macula_handshake:read_hello(Hello(#{{text, <<"accepted">>} => 1,
                                                       {text, <<"capabilities">>} => 1 bsl 53}))),
     ?_assertEqual({error, unexpected_frame}, macula_handshake:read_hello(macula_handshake:opener()))].

%%------------------------------------------------------------------
%% Status frames on an open connection
%%------------------------------------------------------------------

status_cases(#{profile := Profile, station_id := StationId, station_public := StationPublic,
               tls_binding := Binding}) ->
    Statement = macula_key_bindings:status_statement(StationId, Binding, ?NOW + 15 * ?MINUTE, ?NOW + 75 * ?MINUTE),
    Frame = macula_handshake:status(Statement),
    Peer = #{profile => Profile, identity_key => StationPublic, binding => Binding, now => ?NOW + 16 * ?MINUTE},
    OtherBinding = macula_key_bindings:tls_binding(StationId, ?LEAF, ?NOW, ?NOW + ?DAY),
    [?_assertEqual({ok, ?NOW + 75 * ?MINUTE}, macula_handshake:read_status(Frame, Peer)),
     ?_assertEqual({error, status_binding_mismatch},
                   macula_handshake:read_status(Frame, Peer#{binding := OtherBinding})),
     ?_assertEqual({error, unexpected_frame}, macula_handshake:read_status(macula_handshake:opener(), Peer))].

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

client_session(#{profile := Profile, station_public := StationPublic, client_public := ClientPublic,
                 client_connect := ClientConnect, connect_binding := Binding, connect_status := Status}) ->
    #{profile => Profile, expected_node_id => macula_node_keys:node_id(StationPublic, Profile), leaf => ?LEAF,
      identity_key => ClientPublic, connect_key => ClientConnect, connect_binding => Binding,
      connect_status => Status, capabilities => ?CLIENT_CAPABILITIES, now => ?NOW + ?MINUTE}.

station_session(#{profile := Profile}, Challenge) ->
    #{profile => Profile, challenge => Challenge, leaf => ?LEAF, puzzle => #{difficulty => 0, mode => enforce},
      capabilities => ?STATION_CAPABILITIES, now => ?NOW + ?MINUTE}.

hello_of({refused, _Reason, Hello}) -> macula_handshake:read_hello(Hello).

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
