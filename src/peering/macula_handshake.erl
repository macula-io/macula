%% @doc The post-quantum connection handshake, as plans/DESIGN_PQ_HANDSHAKE_FRAMES.md lays it out: the opener,
%% challenge, CONNECT, HELLO and status frames, built as deterministic CBOR bytes and checked as received.
%%
%% The client opens the control stream with an opener. The station answers with a challenge: its carried identity
%% key, its TLS binding and status statement, all precomputed, and a fresh nonce. The client checks the challenge
%% against the node_id it dialed and the leaf it received, before it signs anything, and answers with CONNECT: its
%% identity and CONNECT keys, the CONNECT binding and status statement, and a proof by the CONNECT key over the nonce,
%% both node_ids, the leaf hash and the challenge hash. The station checks CONNECT, the puzzle before any signature,
%% and answers with HELLO. Status frames renew a peer's statement on the open connection.
%%
%% Handshake version 5 (plans/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md) binds both ends to this TLS session. The opener
%% and the challenge stay version 4; the client picks 4 or 5 in CONNECT, and the station answers HELLO in the same
%% version. In version 5 the client's proof (V2) also covers E, the session's TLS exporter value, and the client's
%% capabilities, and HELLO carries the station's session proof, signed by its identity key over E, both frames and
%% both node_ids, only after every check on CONNECT has passed. A station with no exporter answers a v5 CONNECT as an
%% old station does, with unsupported_version.
%%
%% Frames here are CBOR bytes without the length prefix: macula_frame:encode_bytes/1 frames them for the stream, and
%% macula_frame:parse_stream_bytes/1 reads them back. A frame is decoded with macula_record_cbor:decode_strict/1 and
%% must hold exactly the keys of its type, each with its type and length. Close reasons are local. On the wire, a
%% refusing station sends only HELLO with one coarse refusal code.
-module(macula_handshake).

-export([
    opener/0,
    read_opener/1,
    challenge/1,
    answer_challenge/2,
    accept_connect/2,
    read_hello/2,
    status/1,
    read_status/2,
    read_status_wire/2,
    open_frame_kind/1
]).

-export_type([close_reason/0, refusal_code/0, puzzle_mode/0, version/0, exporter/0, expect_hello/0,
              station_material/0, client_session/0, station_session/0, station/0, client/0, peer/0]).

-type profile()      :: macula_crypto_profile:profile().
-type envelope()     :: macula_key_bindings:envelope().
-type puzzle_mode()  :: off | log_only | enforce.
-type refusal_code() :: unsupported_version | puzzle_invalid | not_accepted.
-type close_reason() :: unexpected_frame
                      | unsupported_version
                      | malformed_frame
                      | profile_mismatch
                      | key_purpose_reuse
                      | {peer_identity_mismatch, #{expected := <<_:256>>, derived := <<_:256>>}}
                      | puzzle_invalid
                      | proof_invalid
                      | macula_key_bindings:refusal()
                      | session_proof_invalid
                      | session_proof_missing
                      | session_proof_rate
                      | v4_hello_to_v5_connect
                      | exporter_unavailable
                      | {refused, refusal_code()}.

-type version() :: 4 | 5.
%% The session's TLS exporter (RFC 8446 section 7.5): label, context and length to bytes.
-type exporter() :: fun((binary(), binary(), pos_integer()) -> {ok, binary()} | {error, term()}).
%% What the client needs to read HELLO: the version its CONNECT carried and, in version 5, what the session proof
%% covers.
-type expect_hello() :: #{version := 4}
                      | #{version := 5, profile := profile(), exporter_value := <<_:256>>, challenge := binary(),
                          connect := binary(), station_node_id := <<_:256>>, client_node_id := <<_:256>>,
                          station_key := binary()}.

-type station_material() :: #{profile := profile(), identity_key := binary(),
                              tls_binding := envelope(), tls_status := envelope()}.
%% `version' is 4 when absent; version 5 needs `export'.
-type client_session() :: #{profile := profile(), expected_node_id := <<_:256>>, leaf := binary(),
                            identity_key := binary(), connect_key := macula_node_keys:node_key(),
                            connect_binding := envelope(), connect_status := envelope(),
                            capabilities := non_neg_integer(), now := integer(),
                            version => version(), export => exporter(), member_endorsement => binary()}.
%% Without both `export' and `sign_session_proof' the station answers only version 4.
-type station_session() :: #{profile := profile(), challenge := binary(), leaf := binary(),
                             puzzle := #{difficulty := 0..256, mode := puzzle_mode()},
                             capabilities := non_neg_integer(), now := integer(),
                             export => exporter(),
                             sign_session_proof => fun((iodata()) -> {ok, binary()} | {error, session_proof_rate})}.
-type station() :: #{node_id := <<_:256>>, identity_key := binary(), tls_binding := envelope(),
                     status_expires_at := non_neg_integer(), binding_not_after := non_neg_integer(),
                     version := version()}.
-type client() :: #{node_id := <<_:256>>, version := version(), identity_key := binary(), connect_key := binary(),
                    connect_binding := envelope(), capabilities := non_neg_integer(),
                    member_endorsement := binary(),
                    status_expires_at := non_neg_integer(), binding_not_after := non_neg_integer(),
                    puzzle := solved | unsolved | not_checked}.
-type peer() :: #{profile := profile(), identity_key := binary(), binding := envelope(), now := integer()}.

%% 4, not 3, because D31's `member_endorsement' changes the CONNECT key set and
%% `frame_type/3' matches key sets EXACTLY. Without the bump an older peer
%% answers `malformed_frame', which reads as corruption and sends someone
%% hunting a codec bug; version 3 already did that to every published client
%% once. `unsupported_version' says the true thing and tells an operator what
%% to do. D31: "the wire breaks once, in 12.0.0, not twice."
-define(VERSION, 4).
-define(NONCE_BYTES, 32).
%% A protocol integer in a signed structure stays below 2^53 (the decoding rule).
-define(MAX_PROTOCOL_INT, 1 bsl 53).
-define(MLDSA87_PUBLIC_BYTES, 2592).
-define(VERSION_5, 5).
-define(PROOF_LABEL, "MACULA-PQ-CONNECT-PROOF-V1").
-define(PROOF_LABEL_V2, "MACULA-PQ-CONNECT-PROOF-V2").
-define(SESSION_PROOF_LABEL, "MACULA-PQ-SESSION-PROOF-V1").
-define(EXPORTER_LABEL, <<"EXPORTER-macula-session-v1">>).
-define(EXPORTER_BYTES, 32).
-define(OPENER_KEYS, [<<"frame_type">>, <<"version">>]).
-define(CHALLENGE_KEYS, [<<"frame_type">>, <<"identity_key">>, <<"nonce">>, <<"profile">>, <<"tls_binding">>,
                         <<"tls_status">>, <<"version">>]).
%% ⚠ `member_endorsement' is ALWAYS present, empty when the node has none, so a
%% peer cannot tell from the wire whether this node holds an endorsement nor
%% whether the station asks for one (D31). One layout, not two: a second layout
%% without the key would make its absence observable, which is the property
%% always carrying it exists to remove.
-define(CONNECT_KEYS, [<<"capabilities">>, <<"connect_binding">>, <<"connect_key">>, <<"connect_status">>,
                       <<"frame_type">>, <<"identity_key">>, <<"member_endorsement">>, <<"proof">>,
                       <<"version">>]).
-define(HELLO_ACCEPTED_KEYS, [<<"accepted">>, <<"capabilities">>, <<"frame_type">>, <<"version">>]).
-define(HELLO_REFUSED_KEYS, [<<"accepted">>, <<"capabilities">>, <<"frame_type">>, <<"refusal_code">>,
                             <<"version">>]).
-define(HELLO_PROVED_KEYS, [<<"accepted">>, <<"capabilities">>, <<"frame_type">>, <<"session_proof">>,
                            <<"version">>]).
-define(STATUS_KEYS, [<<"frame_type">>, <<"statement">>, <<"version">>]).

%%------------------------------------------------------------------
%% Opener
%%------------------------------------------------------------------

%% @doc The client's first frame on the control stream. It carries nothing that relates to identity.
-spec opener() -> binary().
opener() ->
    frame(?VERSION, <<"opener">>, #{}).

%% @doc The station's check of the first frame.
-spec read_opener(binary()) -> ok | {error, close_reason()}.
read_opener(Bytes) ->
    opener_verdict(decode(Bytes, <<"opener">>, #{?VERSION => [?OPENER_KEYS]})).

opener_verdict({ok, _Fields}) -> ok;
opener_verdict({error, _} = Error) -> Error.

%%------------------------------------------------------------------
%% Challenge, built by the station and answered by the client
%%------------------------------------------------------------------

%% @doc A station's challenge: its precomputed identity key, TLS binding and status statement, and a fresh nonce. The
%% station keeps the bytes it sends, for the proof check.
-spec challenge(station_material()) -> binary().
challenge(#{profile := Profile, identity_key := IdentityKey, tls_binding := Binding, tls_status := Status}) ->
    frame(?VERSION, <<"challenge">>, #{<<"nonce">> => crypto:strong_rand_bytes(?NONCE_BYTES),
                             <<"profile">> => {text, atom_to_binary(Profile)},
                             <<"identity_key">> => IdentityKey,
                             <<"tls_binding">> => envelope_value(Binding),
                             <<"tls_status">> => envelope_value(Status)}).

%% @doc The client's check of a challenge and, when every check passes, its CONNECT. The client checks the frame, the
%% profile, the station's carried key, that each key in view serves one purpose, the station's node_id against the one
%% dialed, the TLS binding against the leaf received in this TLS handshake, and the status statement, before it signs
%% the proof. On a refusal it closes without CONNECT.
-spec answer_challenge(binary(), client_session()) ->
        {ok, binary(), station(), expect_hello()} | {error, close_reason()}.
answer_challenge(Bytes, #{profile := _, expected_node_id := <<_:256>>, leaf := Leaf, identity_key := _,
                          connect_key := _, connect_binding := _, connect_status := _, capabilities := _,
                          now := _} = Session) when is_binary(Leaf) ->
    challenge_verdict(steps(#{bytes => Bytes, session => Session},
                            [fun decoded_challenge/1, fun challenge_profile/1, fun station_key/1,
                             fun keys_in_view_distinct/1, fun station_identity/1, fun station_binding/1,
                             fun station_status/1, fun session_exported/1])).

challenge_verdict({ok, State}) -> connect_frame(State);
challenge_verdict({error, _} = Error) -> Error.

decoded_challenge(#{bytes := Bytes} = State) ->
    with_fields(decode(Bytes, <<"challenge">>, #{?VERSION => [?CHALLENGE_KEYS]}), State).

challenge_profile(#{fields := #{<<"profile">> := Name}, session := #{profile := Profile}} = State) ->
    expect(Name =:= atom_to_binary(Profile), profile_mismatch, State).

station_key(#{fields := #{<<"identity_key">> := Key}, session := #{profile := Profile}} = State) ->
    expect(macula_node_keys:carried_key_well_formed(Key, Profile), malformed_frame, State).

%% A key serves one purpose (D6, D16): this client's CONNECT key shares no half with its identity key, and the key in
%% the leaf is neither the station's identity key nor this client's CONNECT key.
keys_in_view_distinct(#{fields := #{<<"identity_key">> := StationKey},
                        session := #{leaf := Leaf, identity_key := IdentityKey, connect_key := ConnectKey}} = State) ->
    ConnectPublic = macula_node_keys:public_key(ConnectKey),
    expect(not (shares_a_half(IdentityKey, ConnectPublic) orelse in_leaf(StationKey, Leaf)
                orelse in_leaf(ConnectPublic, Leaf)),
           key_purpose_reuse, State).

station_identity(#{fields := #{<<"identity_key">> := Key},
                   session := #{profile := Profile, expected_node_id := Expected}} = State) ->
    station_node_id(macula_node_keys:node_id(Key, Profile), Expected, State).

station_node_id(Expected, Expected, State) ->
    {ok, State#{station_node_id => Expected}};
station_node_id(Derived, Expected, _State) ->
    {error, {peer_identity_mismatch, #{expected => Expected, derived => Derived}}}.

station_binding(#{fields := #{<<"identity_key">> := Key, <<"tls_binding">> := Binding},
                  session := #{profile := Profile, leaf := Leaf, now := Now}} = State) ->
    with_not_after(macula_key_bindings:verify_tls_binding(Binding, Key, Profile, Leaf, Now), State).

station_status(#{fields := #{<<"identity_key">> := Key, <<"tls_binding">> := Binding, <<"tls_status">> := Status},
                 session := #{profile := Profile, now := Now}} = State) ->
    with_expiry(macula_key_bindings:verify_status(Status, Binding, Key, Profile, Now), State).

%% Version 5 only: E for this session, with the client's node_id first in the context (the initiator's).
session_exported(#{session := #{version := ?VERSION_5, export := Export, profile := Profile, identity_key := Key},
                   station_node_id := StationNodeId} = State) ->
    with_exported(Export(?EXPORTER_LABEL, <<(macula_node_keys:node_id(Key, Profile))/binary, StationNodeId/binary>>,
                         ?EXPORTER_BYTES), State);
session_exported(#{session := #{version := ?VERSION_5}}) ->
    {error, exporter_unavailable};
session_exported(State) ->
    {ok, State}.

with_exported({ok, <<_:?EXPORTER_BYTES/binary>> = E}, State) -> {ok, State#{exporter_value => E}};
with_exported(_Unavailable, _State) -> {error, exporter_unavailable}.

connect_frame(#{bytes := ChallengeBytes, fields := Fields, station_node_id := StationNodeId,
                status_expires_at := ExpiresAt, binding_not_after := NotAfter,
                session := #{profile := Profile, leaf := Leaf, identity_key := IdentityKey, connect_key := ConnectKey,
                             connect_binding := Binding, connect_status := Status,
                             capabilities := Capabilities} = Session} = State) ->
    #{<<"nonce">> := Nonce, <<"identity_key">> := StationKey, <<"tls_binding">> := TlsBinding} = Fields,
    %% Empty when the session carries none, so the field is on the wire either
    %% way. NOT covered by the proof below: `proof_message/5' signs a
    %% fixed-length concatenation ending in the hash of the CHALLENGE frame,
    %% never of this one, which is what let WP 1.5 ship before D31.
    Endorsement = maps:get(member_endorsement, Session, <<>>),
    Version = maps:get(version, Session, ?VERSION),
    ClientNodeId = macula_node_keys:node_id(IdentityKey, Profile),
    E = maps:get(exporter_value, State, none),
    Proof = macula_node_keys:sign(proof_message(Version, Nonce, StationNodeId, ClientNodeId, Leaf, ChallengeBytes, E,
                                                Capabilities),
                                  ConnectKey),
    Connect = frame(Version, <<"connect">>, #{<<"identity_key">> => IdentityKey,
                                     <<"connect_key">> => macula_node_keys:public_key(ConnectKey),
                                     <<"connect_binding">> => envelope_value(Binding),
                                     <<"connect_status">> => envelope_value(Status),
                                     <<"proof">> => Proof,
                                     <<"member_endorsement">> => Endorsement,
                                     <<"capabilities">> => Capabilities}),
    {ok, Connect, #{node_id => StationNodeId, identity_key => StationKey, tls_binding => TlsBinding,
                    status_expires_at => ExpiresAt, binding_not_after => NotAfter, version => Version},
     expect_hello(Version, Profile, E, ChallengeBytes, Connect, StationNodeId, ClientNodeId, StationKey)}.

expect_hello(?VERSION_5, Profile, E, Challenge, Connect, StationNodeId, ClientNodeId, StationKey) ->
    #{version => ?VERSION_5, profile => Profile, exporter_value => E, challenge => Challenge, connect => Connect,
      station_node_id => StationNodeId, client_node_id => ClientNodeId, station_key => StationKey};
expect_hello(?VERSION, _Profile, _E, _Challenge, _Connect, _StationNodeId, _ClientNodeId, _StationKey) ->
    #{version => ?VERSION}.

%%------------------------------------------------------------------
%% CONNECT, checked by the station
%%------------------------------------------------------------------

%% @doc The station's check of CONNECT, and the HELLO bytes to send. The station checks the frame, the carried keys and
%% the proof length, that each key serves one purpose, the puzzle on the derived node_id, the CONNECT binding and
%% status statement, and the proof against the challenge bytes it sent and the leaf this connection presented. The
%% puzzle comes before any signature: under enforce an unsolved puzzle is refused with puzzle_invalid, under log_only it
%% is accepted and reported, and off skips it. A refusal carries a HELLO with accepted 0 and one coarse refusal code.
-spec accept_connect(binary(), station_session()) ->
        {accepted, client(), binary()} | {refused, close_reason(), binary()}.
accept_connect(Bytes, #{profile := _, challenge := Challenge, leaf := Leaf, now := _,
                        capabilities := Capabilities, puzzle := #{mode := Mode, difficulty := Difficulty}} = Session)
  when is_binary(Challenge), is_binary(Leaf), (Mode =:= off orelse Mode =:= log_only orelse Mode =:= enforce),
       is_integer(Difficulty), Difficulty >= 0, Difficulty =< 256 ->
    connect_verdict(versioned_steps(#{bytes => Bytes, session => Session, version => ?VERSION},
                                    [fun decoded_connect/1, fun client_keys/1, fun client_keys_distinct/1,
                                     fun client_identity/1, fun client_puzzle/1, fun client_binding/1,
                                     fun client_status/1, fun connect_exported/1, fun client_proof/1,
                                     fun session_proved/1]),
                    Capabilities).

connect_verdict({ok, #{version := Version} = State}, Capabilities) ->
    {accepted, client(State), hello_accepted(Version, Capabilities, maps:get(session_proof, State, none))};
connect_verdict({error, Reason, Version}, Capabilities) ->
    {refused, Reason, hello_refused(Version, wire_refusal(Reason), Capabilities)}.

%% A station with no exporter decodes only version 4, so it refuses a v5 CONNECT with unsupported_version, in
%% version 4, as an old station does.
decoded_connect(#{bytes := Bytes, session := Session} = State) ->
    connect_fields(decode(Bytes, <<"connect">>, connect_layouts(Session)), State).

connect_layouts(#{export := _, sign_session_proof := _}) ->
    #{?VERSION => [?CONNECT_KEYS], ?VERSION_5 => [?CONNECT_KEYS]};
connect_layouts(_Version4Only) ->
    #{?VERSION => [?CONNECT_KEYS]}.

connect_fields({ok, #{<<"version">> := Version} = Fields}, State) -> {ok, State#{fields => Fields, version => Version}};
connect_fields({error, _} = Error, _State) -> Error.

connect_exported(#{version := ?VERSION_5, client_node_id := ClientNodeId,
                   session := #{export := Export, challenge := Challenge, profile := Profile}} = State) ->
    with_exported(Export(?EXPORTER_LABEL, <<ClientNodeId/binary, (challenge_node_id(Challenge, Profile))/binary>>,
                         ?EXPORTER_BYTES), State);
connect_exported(State) ->
    {ok, State}.

%% Sign after verify: reached only once every check on CONNECT has passed.
session_proved(#{version := ?VERSION_5, exporter_value := E, client_node_id := ClientNodeId, bytes := Connect,
                 session := #{sign_session_proof := Sign, challenge := Challenge, profile := Profile,
                              capabilities := Capabilities}} = State) ->
    Message = session_proof_message(E, Challenge, Connect, challenge_node_id(Challenge, Profile), ClientNodeId,
                                    Capabilities),
    with_session_proof(Sign(Message), State);
session_proved(State) ->
    {ok, State}.

with_session_proof({ok, Signature}, State) -> {ok, State#{session_proof => Signature}};
with_session_proof({error, session_proof_rate}, _State) -> {error, session_proof_rate}.

client_keys(#{fields := #{<<"identity_key">> := IdentityKey, <<"connect_key">> := ConnectKey, <<"proof">> := Proof},
              session := #{profile := Profile}} = State) ->
    expect(macula_node_keys:carried_key_well_formed(IdentityKey, Profile)
               andalso macula_node_keys:carried_key_well_formed(ConnectKey, Profile)
               andalso byte_size(Proof) =:= macula_node_keys:signature_bytes(Profile),
           malformed_frame, State).

%% A key serves one purpose (D6, D16): the CONNECT key shares no half with the identity key, and is not the key in
%% the leaf this connection presented.
client_keys_distinct(#{fields := #{<<"identity_key">> := IdentityKey, <<"connect_key">> := ConnectKey},
                       session := #{leaf := Leaf}} = State) ->
    expect(not (shares_a_half(IdentityKey, ConnectKey) orelse in_leaf(ConnectKey, Leaf)), key_purpose_reuse, State).

client_identity(#{fields := #{<<"identity_key">> := IdentityKey}, session := #{profile := Profile}} = State) ->
    {ok, State#{client_node_id => macula_node_keys:node_id(IdentityKey, Profile)}}.

client_puzzle(#{client_node_id := NodeId, session := #{puzzle := Puzzle}} = State) ->
    puzzle_verdict(puzzle_result(NodeId, Puzzle), Puzzle, State).

puzzle_result(_NodeId, #{mode := off}) ->
    not_checked;
puzzle_result(NodeId, #{difficulty := Difficulty}) ->
    solved_or_not(macula_node_keys:puzzle_solved(NodeId, Difficulty)).

solved_or_not(true) -> solved;
solved_or_not(false) -> unsolved.

puzzle_verdict(unsolved, #{mode := enforce}, _State) -> {error, puzzle_invalid};
puzzle_verdict(Result, _Puzzle, State) -> {ok, State#{puzzle => Result}}.

client_binding(#{fields := #{<<"identity_key">> := IdentityKey, <<"connect_key">> := ConnectKey,
                             <<"connect_binding">> := Binding},
                 session := #{profile := Profile, now := Now}} = State) ->
    with_not_after(macula_key_bindings:verify_connect_binding(Binding, IdentityKey, Profile, ConnectKey, Now),
                   State).

client_status(#{fields := #{<<"identity_key">> := IdentityKey, <<"connect_binding">> := Binding,
                            <<"connect_status">> := Status},
                session := #{profile := Profile, now := Now}} = State) ->
    with_expiry(macula_key_bindings:verify_status(Status, Binding, IdentityKey, Profile, Now), State).

%% The station's own challenge bytes decode: it built them.
client_proof(#{fields := #{<<"connect_key">> := ConnectKey, <<"proof">> := Proof,
                            <<"capabilities">> := Capabilities},
               client_node_id := ClientNodeId, version := Version,
               session := #{profile := Profile, challenge := Challenge, leaf := Leaf}} = State) ->
    {ok, #{<<"nonce">> := Nonce}} = decode(Challenge, <<"challenge">>, #{?VERSION => [?CHALLENGE_KEYS]}),
    Message = proof_message(Version, Nonce, challenge_node_id(Challenge, Profile), ClientNodeId, Leaf, Challenge,
                            maps:get(exporter_value, State, none), Capabilities),
    expect(macula_node_keys:verify(Message, Proof, ConnectKey, Profile), proof_invalid, State).

%% The station's node_id, from its own challenge bytes, which decode: it built them.
challenge_node_id(Challenge, Profile) ->
    {ok, #{<<"identity_key">> := StationKey}} = decode(Challenge, <<"challenge">>, #{?VERSION => [?CHALLENGE_KEYS]}),
    macula_node_keys:node_id(StationKey, Profile).

client(#{fields := Fields, client_node_id := NodeId, puzzle := Puzzle, status_expires_at := ExpiresAt,
         binding_not_after := NotAfter, version := Version}) ->
    #{node_id => NodeId,
      version => Version,
      identity_key => maps:get(<<"identity_key">>, Fields),
      connect_key => maps:get(<<"connect_key">>, Fields),
      connect_binding => maps:get(<<"connect_binding">>, Fields),
      capabilities => maps:get(<<"capabilities">>, Fields),
      %% Handed over, never checked here. Whether a station REQUIRES one is
      %% D31's `invite_only' setting and belongs to the station's CONNECT
      %% check, after the proof has established this node_id: the verifier
      %% binds `member_node' to it, so checked any earlier it would be matching
      %% a claim against a name nothing has proved.
      member_endorsement => maps:get(<<"member_endorsement">>, Fields),
      status_expires_at => ExpiresAt,
      binding_not_after => NotAfter,
      puzzle => Puzzle}.

%% Version 4 (V1): label || 0x00 || nonce || station node_id || client node_id || SHA-384(leaf DER) ||
%% SHA-384(challenge bytes). Version 5 (V2) appends E and the client's capabilities, 8 bytes big-endian. Every field
%% has a fixed width, so no two field sequences encode to the same bytes.
proof_message(?VERSION, Nonce, StationNodeId, ClientNodeId, Leaf, ChallengeBytes, _E, _Capabilities) ->
    [<<?PROOF_LABEL, 0>>, Nonce, StationNodeId, ClientNodeId, crypto:hash(sha384, Leaf),
     crypto:hash(sha384, ChallengeBytes)];
proof_message(?VERSION_5, Nonce, StationNodeId, ClientNodeId, Leaf, ChallengeBytes, <<_:?EXPORTER_BYTES/binary>> = E,
              Capabilities) ->
    [<<?PROOF_LABEL_V2, 0>>, Nonce, StationNodeId, ClientNodeId, crypto:hash(sha384, Leaf),
     crypto:hash(sha384, ChallengeBytes), E, <<Capabilities:64>>].

%% label || 0x00 || E || SHA-384(challenge) || SHA-384(CONNECT) || station node_id || client node_id ||
%% station capabilities, 8 bytes big-endian. SHA-384(CONNECT) covers the client's capabilities.
session_proof_message(E, Challenge, Connect, StationNodeId, ClientNodeId, Capabilities) ->
    [<<?SESSION_PROOF_LABEL, 0>>, E, crypto:hash(sha384, Challenge), crypto:hash(sha384, Connect), StationNodeId,
     ClientNodeId, <<Capabilities:64>>].

wire_refusal(unsupported_version) -> unsupported_version;
wire_refusal(puzzle_invalid) -> puzzle_invalid;
wire_refusal(_Other) -> not_accepted.

%%------------------------------------------------------------------
%% HELLO
%%------------------------------------------------------------------

hello_accepted(?VERSION, Capabilities, none) ->
    frame(?VERSION, <<"hello">>, #{<<"accepted">> => 1, <<"capabilities">> => Capabilities});
hello_accepted(?VERSION_5, Capabilities, SessionProof) ->
    frame(?VERSION_5, <<"hello">>, #{<<"accepted">> => 1, <<"capabilities">> => Capabilities,
                                     <<"session_proof">> => SessionProof}).

hello_refused(Version, Code, Capabilities) ->
    frame(Version, <<"hello">>, #{<<"accepted">> => 0, <<"refusal_code">> => {text, atom_to_binary(Code)},
                                  <<"capabilities">> => Capabilities}).

%% @doc The client's reading of HELLO, against the version its CONNECT carried: the station's capabilities and the
%% version, or why not. After a v5 CONNECT an accepted HELLO must be version 5, with a session proof that verifies
%% under the station's identity key. A v4 refusal is how an old station answers; a v4 acceptance is never taken as
%% a v4 connection.
-spec read_hello(binary(), expect_hello()) ->
        {ok, #{capabilities := non_neg_integer(), version := version()}} | {error, close_reason()}.
read_hello(Bytes, #{version := ?VERSION}) ->
    hello_verdict(decode(Bytes, <<"hello">>, #{?VERSION => [?HELLO_ACCEPTED_KEYS, ?HELLO_REFUSED_KEYS]}),
                  #{version => ?VERSION});
read_hello(Bytes, #{version := ?VERSION_5} = Expect) ->
    hello_verdict(decode(Bytes, <<"hello">>, #{?VERSION => [?HELLO_ACCEPTED_KEYS, ?HELLO_REFUSED_KEYS],
                                               ?VERSION_5 => [?HELLO_PROVED_KEYS, ?HELLO_ACCEPTED_KEYS,
                                                              ?HELLO_REFUSED_KEYS]}),
                  Expect).

hello_verdict({ok, #{<<"accepted">> := 0, <<"refusal_code">> := Code}}, _Expect) ->
    {error, {refused, Code}};
hello_verdict({ok, #{<<"version">> := ?VERSION, <<"accepted">> := 1, <<"capabilities">> := Capabilities} = Fields},
              #{version := ?VERSION})
  when not is_map_key(<<"refusal_code">>, Fields) ->
    {ok, #{capabilities => Capabilities, version => ?VERSION}};
hello_verdict({ok, #{<<"version">> := ?VERSION, <<"accepted">> := 1} = Fields}, #{version := ?VERSION_5})
  when not is_map_key(<<"refusal_code">>, Fields) ->
    {error, v4_hello_to_v5_connect};
hello_verdict({ok, #{<<"version">> := ?VERSION_5, <<"accepted">> := 1, <<"session_proof">> := Proof,
                     <<"capabilities">> := Capabilities}}, #{version := ?VERSION_5, profile := Profile} = Expect) ->
    session_proof_checked(byte_size(Proof) =:= macula_node_keys:signature_bytes(Profile), Proof, Capabilities, Expect);
hello_verdict({ok, #{<<"version">> := ?VERSION_5, <<"accepted">> := 1} = Fields}, #{version := ?VERSION_5})
  when not is_map_key(<<"refusal_code">>, Fields) ->
    {error, session_proof_missing};
hello_verdict({ok, _Inconsistent}, _Expect) ->
    {error, malformed_frame};
hello_verdict({error, _} = Error, _Expect) ->
    Error.

session_proof_valid(Proof, Capabilities, #{profile := Profile, exporter_value := E, challenge := Challenge,
                                           connect := Connect, station_node_id := StationNodeId,
                                           client_node_id := ClientNodeId, station_key := StationKey}) ->
    Message = session_proof_message(E, Challenge, Connect, StationNodeId, ClientNodeId, Capabilities),
    macula_node_keys:verify(Message, Proof, StationKey, Profile).

%% A session proof of the wrong length is a malformed frame, as a CONNECT proof of the wrong length is.
session_proof_checked(true, Proof, Capabilities, Expect) ->
    session_proof_verdict(session_proof_valid(Proof, Capabilities, Expect), Capabilities);
session_proof_checked(false, _Proof, _Capabilities, _Expect) ->
    {error, malformed_frame}.

session_proof_verdict(true, Capabilities) -> {ok, #{capabilities => Capabilities, version => ?VERSION_5}};
session_proof_verdict(false, _Capabilities) -> {error, session_proof_invalid}.

%%------------------------------------------------------------------
%% Status frames
%%------------------------------------------------------------------

%% @doc A status frame carrying a fresh status statement, sent at every reissue.
-spec status(envelope()) -> binary().
status(Statement) ->
    frame(?VERSION, <<"status">>, #{<<"statement">> => envelope_value(Statement)}).

%% @doc A peer's status frame, checked against the binding and identity key the handshake verified, and when the new
%% statement expires.
-spec read_status(binary(), peer()) -> {ok, non_neg_integer()} | {error, close_reason()}.
read_status(Bytes, #{profile := Profile, identity_key := IdentityKey, binding := Binding, now := Now}) ->
    status_verdict(decode(Bytes, <<"status">>, #{?VERSION => [?STATUS_KEYS]}), Binding, IdentityKey, Profile, Now).

status_verdict({ok, #{<<"statement">> := Statement}}, Binding, IdentityKey, Profile, Now) ->
    status_expiry(macula_key_bindings:verify_status(Statement, Binding, IdentityKey, Profile, Now));
status_verdict({error, _} = Error, _Binding, _IdentityKey, _Profile, _Now) ->
    Error.

status_expiry({ok, #{expires_at := ExpiresAt}}) -> {ok, ExpiresAt};
status_expiry({error, _} = Error) -> Error.

%% @doc read_status/2 on a frame already decoded from CBOR, for a connection that decodes each frame once to route
%% it.
-spec read_status_wire(term(), peer()) -> {ok, non_neg_integer()} | {error, close_reason()}.
read_status_wire(Wire, #{profile := Profile, identity_key := IdentityKey, binding := Binding, now := Now}) ->
    status_verdict(frame_version(text_keyed({ok, Wire}), <<"status">>, #{?VERSION => [?STATUS_KEYS]}), Binding,
                   IdentityKey, Profile, Now).

%% @doc What a decoded frame on an open connection is, by its frame_type: a status frame, a handshake frame, which
%% has no place after HELLO, or any other frame, which macula_frame reads.
-spec open_frame_kind(term()) -> status | handshake | other.
open_frame_kind(#{{text, <<"frame_type">>} := {text, <<"status">>}}) ->
    status;
open_frame_kind(#{{text, <<"frame_type">>} := {text, Type}})
  when Type =:= <<"opener">>; Type =:= <<"challenge">>; Type =:= <<"connect">>; Type =:= <<"hello">> ->
    handshake;
open_frame_kind(_Other) ->
    other.

%%------------------------------------------------------------------
%% Encoding
%%------------------------------------------------------------------

%% Opener, challenge and status frames are always version 4; CONNECT and HELLO carry the version the client chose.
frame(Version, Type, Fields) ->
    Header = #{{text, <<"version">>} => Version, {text, <<"frame_type">>} => {text, Type}},
    macula_record_cbor:encode(maps:fold(fun(Key, Value, Acc) -> Acc#{{text, Key} => Value} end, Header, Fields)).

envelope_value(#{tbs := Tbs, signature := Signature}) ->
    #{{text, <<"tbs">>} => Tbs, {text, <<"signature">>} => Signature}.

%%------------------------------------------------------------------
%% Strict decoding: the version, then the frame type, then exactly the keys of one of the frame's layouts, then
%% the type and length of every field
%%------------------------------------------------------------------

decode(Bytes, Type, Layouts) when is_binary(Bytes) ->
    frame_version(text_keyed(macula_record_cbor:decode_strict(Bytes)), Type, Layouts).

text_keyed({ok, Map}) when is_map(Map) -> maps:fold(fun text_key/3, #{}, Map);
text_keyed(_NotAMap) -> not_a_frame.

text_key({text, Key}, Value, Acc) -> Acc#{Key => Value};
text_key(Key, Value, Acc) -> Acc#{{not_text, Key} => Value}.

%% Layouts by version: a version the reader has no layouts for is unsupported_version.
frame_version(#{<<"version">> := Version} = Fields, Type, Layouts) when is_map_key(Version, Layouts) ->
    frame_type(Fields, Type, maps:get(Version, Layouts));
frame_version(#{<<"version">> := Version}, _Type, _Layouts) when is_integer(Version) ->
    {error, unsupported_version};
frame_version(_Fields, _Type, _Layouts) ->
    {error, malformed_frame}.

frame_type(#{<<"frame_type">> := {text, Type}} = Fields, Type, Layouts) ->
    frame_layout(lists:member(lists:sort(maps:keys(Fields)), Layouts), Fields);
frame_type(#{<<"frame_type">> := {text, _Other}}, _Type, _Layouts) ->
    {error, unexpected_frame};
frame_type(_Fields, _Type, _Layouts) ->
    {error, malformed_frame}.

frame_layout(true, Fields) -> maps:fold(fun typed_field/3, {ok, #{}}, Fields);
frame_layout(false, _Fields) -> {error, malformed_frame}.

typed_field(_Key, _Value, {error, _} = Error) -> Error;
typed_field(Key, Value, {ok, Acc}) -> added_field(field_value(Key, Value), Key, Acc).

added_field({ok, Value}, Key, Acc) -> {ok, Acc#{Key => Value}};
added_field(error, _Key, _Acc) -> {error, malformed_frame}.

field_value(<<"version">>, Version) when Version =:= ?VERSION; Version =:= ?VERSION_5 -> {ok, Version};
field_value(<<"frame_type">>, {text, Type}) -> {ok, Type};
field_value(<<"nonce">>, <<_:?NONCE_BYTES/binary>> = Nonce) -> {ok, Nonce};
field_value(<<"profile">>, {text, Name}) -> {ok, Name};
field_value(<<"identity_key">>, Key) -> bytes(Key);
field_value(<<"connect_key">>, Key) -> bytes(Key);
field_value(<<"proof">>, Proof) -> bytes(Proof);
field_value(<<"session_proof">>, Proof) -> bytes(Proof);
field_value(<<"tls_binding">>, Envelope) -> envelope(Envelope);
field_value(<<"tls_status">>, Envelope) -> envelope(Envelope);
field_value(<<"connect_binding">>, Envelope) -> envelope(Envelope);
field_value(<<"connect_status">>, Envelope) -> envelope(Envelope);
field_value(<<"statement">>, Envelope) -> envelope(Envelope);
%% Bytes, and an EMPTY one is valid and ordinary: a node with no endorsement
%% still carries the field so its absence cannot be observed (D31). The SDK
%% does not parse the record here -- whether it is a well-formed
%% `realm_member_endorsement' is for the station's check, which runs after the
%% proof establishes this node_id.
field_value(<<"member_endorsement">>, Endorsement) -> bytes(Endorsement);
field_value(<<"capabilities">>, Capabilities)
  when is_integer(Capabilities), Capabilities >= 0, Capabilities < ?MAX_PROTOCOL_INT ->
    {ok, Capabilities};
field_value(<<"accepted">>, Accepted) when Accepted =:= 0; Accepted =:= 1 -> {ok, Accepted};
field_value(<<"refusal_code">>, {text, Code}) -> refusal_code(Code);
field_value(_Key, _Value) -> error.

bytes(Bytes) when is_binary(Bytes) -> {ok, Bytes};
bytes(_NotBytes) -> error.

envelope(#{{text, <<"tbs">>} := Tbs, {text, <<"signature">>} := Signature} = Envelope)
  when map_size(Envelope) =:= 2, is_binary(Tbs), is_binary(Signature) ->
    {ok, #{tbs => Tbs, signature => Signature}};
envelope(_NotAnEnvelope) ->
    error.

refusal_code(<<"unsupported_version">>) -> {ok, unsupported_version};
refusal_code(<<"puzzle_invalid">>) -> {ok, puzzle_invalid};
refusal_code(<<"not_accepted">>) -> {ok, not_accepted};
refusal_code(_Unknown) -> error.

%%------------------------------------------------------------------
%% Steps
%%------------------------------------------------------------------

%% Run checks in order; each takes the state and returns it, extended, or the first refusal.
steps(State, []) -> {ok, State};
steps(State, [Step | Rest]) -> next_step(Step(State), Rest).

next_step({ok, State}, Rest) -> steps(State, Rest);
next_step({error, _} = Error, _Rest) -> Error.

%% steps/2 for the station, whose refusal names the version it answers in: the CONNECT's once decoded, else 4.
versioned_steps(State, []) -> {ok, State};
versioned_steps(#{version := Version} = State, [Step | Rest]) -> next_versioned_step(Step(State), Rest, Version).

next_versioned_step({ok, State}, Rest, _Version) -> versioned_steps(State, Rest);
next_versioned_step({error, Reason}, _Rest, Version) -> {error, Reason, Version}.

with_fields({ok, Fields}, State) -> {ok, State#{fields => Fields}};
with_fields({error, _} = Error, _State) -> Error.

with_expiry({ok, #{expires_at := ExpiresAt}}, State) -> {ok, State#{status_expires_at => ExpiresAt}};
with_expiry({error, _} = Error, _State) -> Error.

with_not_after({ok, #{not_after := NotAfter}}, State) -> {ok, State#{binding_not_after => NotAfter}};
with_not_after({error, _} = Error, _State) -> Error.

expect(true, _Refusal, State) -> {ok, State};
expect(false, Refusal, _State) -> {error, Refusal}.

%%------------------------------------------------------------------
%% Keys in view, compared whole, half by half
%%------------------------------------------------------------------

%% Carried keys are well formed by now: the ML-DSA-87 key first, then the classical half in pq_hybrid.
in_leaf(<<MlDsa:?MLDSA87_PUBLIC_BYTES/binary, _Classical/binary>>, Leaf) ->
    binary:match(Leaf, MlDsa) =/= nomatch.

shares_a_half(<<MlDsaA:?MLDSA87_PUBLIC_BYTES/binary, ClassicalA/binary>>,
              <<MlDsaB:?MLDSA87_PUBLIC_BYTES/binary, ClassicalB/binary>>) ->
    MlDsaA =:= MlDsaB orelse (ClassicalA =/= <<>> andalso ClassicalA =:= ClassicalB).
