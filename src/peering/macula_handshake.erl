%% @doc The post-quantum connection handshake of wire 1.0 (docs/design/WIRE_1_0.md section 4, on
%% docs/design/DESIGN_PQ_HANDSHAKE_FRAMES.md and docs/design/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md): the opener,
%% challenge, CONNECT, HELLO and status frames, built as deterministic CBOR bytes and checked as received.
%%
%% Handshake version 5 is the only version, and every frame carries it. The client opens the control stream with an
%% opener naming its ranges, the versions and profiles it speaks. The station chooses the highest common version and
%% the first of the client's profiles it supports, or answers at once with a HELLO refusal naming its own ranges. Its
%% challenge names the choice, repeats the station's ranges, and carries its identity key, TLS binding and status
%% statement and a fresh nonce. The client checks that the choice is the one its own ranges and the station's give,
%% so nobody on the path can downgrade it, then the station's identity, binding and status, and answers with CONNECT:
%% its keys, binding and status, its ranges again, the chosen profile, and a proof V3 by its CONNECT key over the nonce,
%% both node_ids, the leaf hash, the challenge hash, E (the session's TLS exporter value) and the hash of the CONNECT
%% tbs, the frame with the proof empty. The station checks CONNECT, the puzzle before any signature, and answers HELLO
%% with a session proof V2 by its identity key over E, both frames, both node_ids and the HELLO tbs. A node never
%% completes below the version floor it holds for the peer (section 4.6). Status frames renew a peer's statement on
%% the open connection.
%%
%% Frames here are CBOR bytes without the length prefix: macula_frame:encode_bytes/1 frames them for the stream, and
%% macula_frame:parse_stream_bytes/1 reads them back. A frame must be deterministic CBOR and hold exactly the keys of
%% its type, each with its type and length. Close reasons are local. On the wire, a refusing station sends only HELLO
%% with one coarse refusal code.
-module(macula_handshake).

-export([
    ranges/1,
    opener/1,
    read_opener/2,
    challenge/3,
    answer_challenge/2,
    accept_connect/2,
    read_hello/2,
    read_refusal/1,
    status/1,
    read_status/2,
    read_status_wire/2,
    open_frame_kind/1
]).

-export_type([close_reason/0, refusal_code/0, puzzle_mode/0, version/0, exporter/0, expect_hello/0, ranges/0,
              wire_ranges/0, choice/0, floor/0, station_material/0, client_session/0, station_session/0, station/0,
              client/0, peer/0]).

-type profile()      :: macula_crypto_profile:profile().
-type envelope()     :: macula_key_bindings:envelope().
-type puzzle_mode()  :: off | log_only | enforce.
-type refusal_code() :: unsupported_version | profile_unsupported | puzzle_invalid | session_proof_rate
                      | not_accepted.
%% The ranges a node speaks: [Min, Max] of handshake versions, and its profiles in its order of preference.
-type ranges() :: #{versions := [non_neg_integer()], profiles := [profile()]}.
%% Ranges as a peer sent them: profile names stay text, since a peer may name a profile this node does not know.
-type wire_ranges() :: #{versions := [non_neg_integer()], profiles := [binary()]}.
%% The station's choice from an opener, kept for the CONNECT check.
-type choice() :: #{version := version(), profile := profile(), client_ranges := wire_ranges()}.
%% The version floor held for a peer: the highest version completed with it, or none.
-type floor() :: version() | none.
-type close_reason() :: unexpected_frame
                      | unsupported_version
                      | malformed_frame
                      | profile_mismatch
                      | profile_unsupported
                      | downgrade_detected
                      | negotiation_mismatch
                      | version_below_floor
                      | key_purpose_reuse
                      | {peer_identity_mismatch, #{expected := <<_:256>>, derived := <<_:256>>}}
                      | puzzle_invalid
                      | proof_invalid
                      | macula_key_bindings:refusal()
                      | session_proof_invalid
                      | session_proof_missing
                      | session_proof_rate
                      | exporter_unavailable
                      | {refused, refusal_code()}
                      | {refused, refusal_code(), wire_ranges()}.

-type version() :: 5.
%% The session's TLS exporter (RFC 8446 section 7.5): label, context and length to bytes.
-type exporter() :: fun((binary(), binary(), pos_integer()) -> {ok, binary()} | {error, term()}).
%% What the client needs to read HELLO: what the session proof covers.
-type expect_hello() :: #{version := version(), profile := profile(), exporter_value := <<_:256>>,
                          challenge := binary(), connect := binary(), station_node_id := <<_:256>>,
                          client_node_id := <<_:256>>, station_key := binary()}.

-type station_material() :: #{profile := profile(), identity_key := binary(),
                              tls_binding := envelope(), tls_status := envelope()}.
%% `versions' and `profiles' are the client's ranges, as its opener sent them; `floor' is the floor it holds for the
%% station it dialled.
-type client_session() :: #{profile := profile(), expected_node_id := <<_:256>>, leaf := binary(),
                            identity_key := binary(), connect_key := macula_node_keys:node_key(),
                            connect_binding := envelope(), connect_status := envelope(),
                            capabilities := non_neg_integer(), now := integer(), export := exporter(),
                            versions := [non_neg_integer()], profiles := [profile()],
                            floor => floor(), member_endorsement => binary()}.
%% `floor' gives the floor the station holds for a client's node_id.
-type station_session() :: #{profile := profile(), choice := choice(), challenge := binary(), leaf := binary(),
                             puzzle := #{difficulty := 0..256, mode := puzzle_mode()},
                             capabilities := non_neg_integer(), now := integer(), export := exporter(),
                             sign_session_proof := fun((<<_:256>>, iodata()) ->
                                                               {ok, binary()} | {error, session_proof_rate}),
                             floor => fun((<<_:256>>) -> floor())}.
-type station() :: #{node_id := <<_:256>>, identity_key := binary(), tls_binding := envelope(),
                     status_expires_at := non_neg_integer(), binding_not_after := non_neg_integer(),
                     version := version()}.
-type client() :: #{node_id := <<_:256>>, version := version(), identity_key := binary(), connect_key := binary(),
                    connect_binding := envelope(), capabilities := non_neg_integer(),
                    member_endorsement := binary(),
                    status_expires_at := non_neg_integer(), binding_not_after := non_neg_integer(),
                    puzzle := solved | unsolved | not_checked}.
-type peer() :: #{profile := profile(), identity_key := binary(), binding := envelope(), now := integer()}.

-define(VERSION, 5).
-define(NONCE_BYTES, 32).
%% A protocol integer in a signed structure stays below 2^53 (the decoding rule).
-define(MAX_PROTOCOL_INT, 1 bsl 53).
-define(MAX_PROFILES, 8).
-define(MAX_PROFILE_NAME, 64).
-define(MLDSA87_PUBLIC_BYTES, 2592).
-define(PROOF_LABEL, "MACULA-PQ-CONNECT-PROOF-V3").
-define(SESSION_PROOF_LABEL, "MACULA-PQ-SESSION-PROOF-V2").
-define(EXPORTER_LABEL, <<"EXPORTER-macula-session-v1">>).
-define(EXPORTER_BYTES, 32).
-define(OPENER_KEYS, [<<"frame_type">>, <<"profiles">>, <<"version">>, <<"versions">>]).
-define(CHALLENGE_KEYS, [<<"frame_type">>, <<"identity_key">>, <<"nonce">>, <<"profile">>, <<"profiles">>,
                         <<"tls_binding">>, <<"tls_status">>, <<"version">>, <<"versions">>]).
%% ⚠ `member_endorsement' is ALWAYS present, empty when the node has none, so a
%% peer cannot tell from the wire whether this node holds an endorsement nor
%% whether the station asks for one (D31). One layout, not two: a second layout
%% without the key would make its absence observable, which is the property
%% always carrying it exists to remove.
-define(CONNECT_KEYS, [<<"capabilities">>, <<"connect_binding">>, <<"connect_key">>, <<"connect_status">>,
                       <<"frame_type">>, <<"identity_key">>, <<"member_endorsement">>, <<"profile">>,
                       <<"profiles">>, <<"proof">>, <<"version">>, <<"versions">>]).
-define(HELLO_PROVED_KEYS, [<<"accepted">>, <<"capabilities">>, <<"frame_type">>, <<"session_proof">>,
                            <<"version">>]).
%% An accepted HELLO without its session proof: read only to name what is missing.
-define(HELLO_UNPROVED_KEYS, [<<"accepted">>, <<"capabilities">>, <<"frame_type">>, <<"version">>]).
-define(HELLO_REFUSED_KEYS, [<<"accepted">>, <<"capabilities">>, <<"frame_type">>, <<"refusal_code">>,
                             <<"version">>]).
%% The station's answer to an opener it cannot serve, naming its ranges (section 4.4).
-define(HELLO_EARLY_KEYS, [<<"accepted">>, <<"frame_type">>, <<"profiles">>, <<"refusal_code">>, <<"version">>,
                           <<"versions">>]).
-define(STATUS_KEYS, [<<"frame_type">>, <<"statement">>, <<"version">>]).

%%------------------------------------------------------------------
%% Ranges
%%------------------------------------------------------------------

%% @doc The ranges a node of Profile speaks: a node supports N and N-1 of the versions it knows, and in wire 1.0 that
%% is 5 alone (section 4.1).
-spec ranges(profile()) -> ranges().
ranges(Profile) ->
    #{versions => [?VERSION, ?VERSION], profiles => [Profile]}.

%%------------------------------------------------------------------
%% Opener, sent by the client and read by the station
%%------------------------------------------------------------------

%% @doc The client's first frame on the control stream: its ranges, and nothing that relates to identity.
-spec opener(ranges()) -> binary().
opener(Ranges) ->
    frame(<<"opener">>, wire(Ranges)).

%% @doc The station's reading of the first frame, against its own ranges: its choice, or a HELLO refusal naming its
%% ranges to send before closing (unsupported_version or profile_unsupported), or a reason to close without one.
-spec read_opener(binary(), ranges()) ->
        {ok, choice()} | {refused, unsupported_version | profile_unsupported, binary()} | {error, close_reason()}.
read_opener(Bytes, StationRanges) ->
    opener_verdict(decode(Bytes, <<"opener">>, [?OPENER_KEYS]), StationRanges).

opener_verdict({ok, #{<<"versions">> := Versions, <<"profiles">> := Profiles}}, StationRanges) ->
    ClientRanges = #{versions => Versions, profiles => Profiles},
    chosen(choose(ClientRanges, StationRanges), ClientRanges, StationRanges);
opener_verdict({error, unsupported_version}, StationRanges) ->
    early_refusal(unsupported_version, StationRanges);
opener_verdict({error, _} = Error, _StationRanges) ->
    Error.

chosen({ok, Version, Profile}, ClientRanges, _StationRanges) ->
    {ok, #{version => Version, profile => Profile, client_ranges => ClientRanges}};
chosen({error, Code}, _ClientRanges, StationRanges) ->
    early_refusal(Code, StationRanges).

early_refusal(Code, StationRanges) ->
    {refused, Code, frame(<<"hello">>, maps:merge(wire(StationRanges),
                                                  #{<<"accepted">> => 0,
                                                    <<"refusal_code">> => {text, atom_to_binary(Code)}}))}.

%% The highest version in both ranges, and the first of the client's profiles the station supports (section 4.1).
choose(#{versions := ClientVersions, profiles := ClientProfiles}, #{versions := StationVersions,
                                                                   profiles := StationProfiles}) ->
    profile_chosen(highest_common(ClientVersions, StationVersions), ClientProfiles, StationProfiles).

profile_chosen(none, _ClientProfiles, _StationProfiles) ->
    {error, unsupported_version};
profile_chosen(Version, ClientProfiles, StationProfiles) ->
    first_supported(ClientProfiles, StationProfiles, Version).

first_supported([], _StationProfiles, _Version) ->
    {error, profile_unsupported};
first_supported([Name | Rest], StationProfiles, Version) ->
    supported([P || P <- StationProfiles, atom_to_binary(P) =:= Name], Rest, StationProfiles, Version).

supported([Profile], _Rest, _StationProfiles, Version) -> {ok, Version, Profile};
supported([], Rest, StationProfiles, Version) -> first_supported(Rest, StationProfiles, Version).

highest_common([MinA, MaxA], [MinB, MaxB]) when max(MinA, MinB) =< min(MaxA, MaxB) -> min(MaxA, MaxB);
highest_common(_A, _B) -> none.

%% Ranges as they travel: profile names as text.
wire(#{versions := Versions, profiles := Profiles}) ->
    #{<<"versions">> => Versions, <<"profiles">> => [{text, profile_name(P)} || P <- Profiles]}.

profile_name(Profile) when is_atom(Profile) -> atom_to_binary(Profile);
profile_name(Name) when is_binary(Name) -> Name.

%%------------------------------------------------------------------
%% Challenge, built by the station and answered by the client
%%------------------------------------------------------------------

%% @doc A station's challenge for its choice: the chosen profile, its own ranges, its precomputed identity key, TLS
%% binding and status statement, and a fresh nonce. The station keeps the bytes it sends, for the proof check.
-spec challenge(station_material(), choice(), ranges()) -> binary().
challenge(#{profile := Profile, identity_key := IdentityKey, tls_binding := Binding, tls_status := Status},
          #{profile := Profile}, StationRanges) ->
    frame(<<"challenge">>, maps:merge(wire(StationRanges),
                                      #{<<"nonce">> => crypto:strong_rand_bytes(?NONCE_BYTES),
                                        <<"profile">> => {text, atom_to_binary(Profile)},
                                        <<"identity_key">> => IdentityKey,
                                        <<"tls_binding">> => envelope_value(Binding),
                                        <<"tls_status">> => envelope_value(Status)})).

%% @doc The client's check of a challenge and, when every check passes, its CONNECT. The client checks the frame, that
%% the choice is the one its own ranges and the station's give, its floor for the station, the profile, the
%% station's carried key, that each key in view serves one purpose, the station's node_id against the one dialled,
%% the TLS binding against the leaf received in this TLS handshake, and the status statement, before it signs the
%% proof. A station that cannot serve the opener answers HELLO instead, read as {refused, Code, StationRanges}. On
%% any refusal the client closes without CONNECT.
-spec answer_challenge(binary(), client_session()) ->
        {ok, binary(), station(), expect_hello()} | {error, close_reason()}.
answer_challenge(Bytes, #{profile := _, expected_node_id := <<_:256>>, leaf := Leaf, identity_key := _,
                          connect_key := _, connect_binding := _, connect_status := _, capabilities := _,
                          now := _, export := _, versions := _, profiles := _} = Session) when is_binary(Leaf) ->
    challenge_verdict(steps(#{bytes => Bytes, session => Session},
                            [fun decoded_challenge/1, fun negotiation_checked/1, fun station_floor/1,
                             fun challenge_profile/1, fun station_key/1, fun keys_in_view_distinct/1,
                             fun station_identity/1, fun station_binding/1, fun station_status/1,
                             fun session_exported/1])).

challenge_verdict({ok, State}) -> connect_frame(State);
challenge_verdict({error, _} = Error) -> Error.

%% A HELLO in place of the challenge is the station's early refusal.
decoded_challenge(#{bytes := Bytes} = State) ->
    challenge_or_refusal(frame_type_of(Bytes), Bytes, State).

challenge_or_refusal(<<"hello">>, Bytes, _State) ->
    early_refusal_read(decode(Bytes, <<"hello">>, [?HELLO_EARLY_KEYS]));
challenge_or_refusal(_Challenge, Bytes, State) ->
    with_fields(decode(Bytes, <<"challenge">>, [?CHALLENGE_KEYS]), State).

early_refusal_read({ok, #{<<"accepted">> := 0, <<"refusal_code">> := {Code, _Known}, <<"versions">> := Versions,
                          <<"profiles">> := Profiles}}) ->
    {error, {refused, Code, #{versions => Versions, profiles => Profiles}}};
early_refusal_read({ok, _Accepted}) ->
    {error, malformed_frame};
early_refusal_read({error, _} = Error) ->
    Error.

%% The station's choice must be the one the client's ranges and the station's repeated ranges give: anything else is
%% a downgrade on the path or by the station (section 4.5).
negotiation_checked(#{fields := #{<<"version">> := Version, <<"profile">> := Profile, <<"versions">> := Versions,
                                  <<"profiles">> := Profiles},
                      session := #{versions := Own, profiles := OwnProfiles}} = State) ->
    Expected = choose(#{versions => Own, profiles => [atom_to_binary(P) || P <- OwnProfiles]},
                      #{versions => Versions, profiles => known_profiles(Profiles)}),
    expect(Expected =:= {ok, Version, profile_atom(Profile, OwnProfiles)}, downgrade_detected, State).

%% Of the station's profiles, those this node knows: an unknown name never becomes an atom.
known_profiles(Names) ->
    [P || P <- macula_crypto_profile:profiles(), lists:member(atom_to_binary(P), Names)].

profile_atom(Name, Known) ->
    first_named([P || P <- Known, atom_to_binary(P) =:= Name]).

first_named([Profile]) -> Profile;
first_named([]) -> unknown.

station_floor(#{fields := #{<<"version">> := Version}, session := Session} = State) ->
    expect(above_floor(Version, maps:get(floor, Session, none)), version_below_floor, State).

above_floor(_Version, none) -> true;
above_floor(Version, Floor) -> Version >= Floor.

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

%% E for this session, with the client's node_id first in the context (the initiator's).
session_exported(#{session := #{export := Export, profile := Profile, identity_key := Key},
                   station_node_id := StationNodeId} = State) ->
    with_exported(Export(?EXPORTER_LABEL, <<(macula_node_keys:node_id(Key, Profile))/binary, StationNodeId/binary>>,
                         ?EXPORTER_BYTES), State).

with_exported({ok, <<_:?EXPORTER_BYTES/binary>> = E}, State) -> {ok, State#{exporter_value => E}};
with_exported(_Unavailable, _State) -> {error, exporter_unavailable}.

connect_frame(#{bytes := ChallengeBytes, fields := Fields, station_node_id := StationNodeId,
                status_expires_at := ExpiresAt, binding_not_after := NotAfter, exporter_value := E,
                session := #{profile := Profile, leaf := Leaf, identity_key := IdentityKey, connect_key := ConnectKey,
                             connect_binding := Binding, connect_status := Status,
                             capabilities := Capabilities} = Session}) ->
    #{<<"nonce">> := Nonce, <<"identity_key">> := StationKey, <<"tls_binding">> := TlsBinding} = Fields,
    %% Empty when the session carries none, so the field is on the wire either way. Covered by the proof through the
    %% CONNECT tbs.
    Endorsement = maps:get(member_endorsement, Session, <<>>),
    ClientNodeId = macula_node_keys:node_id(IdentityKey, Profile),
    Unproved = maps:merge(wire(maps:with([versions, profiles], Session)),
                          #{<<"identity_key">> => IdentityKey,
                            <<"connect_key">> => macula_node_keys:public_key(ConnectKey),
                            <<"connect_binding">> => envelope_value(Binding),
                            <<"connect_status">> => envelope_value(Status),
                            <<"member_endorsement">> => Endorsement,
                            <<"capabilities">> => Capabilities,
                            <<"profile">> => {text, atom_to_binary(Profile)}}),
    ConnectTbs = frame(<<"connect">>, Unproved#{<<"proof">> => <<>>}),
    Proof = macula_node_keys:sign(proof_message(Nonce, StationNodeId, ClientNodeId, Leaf, ChallengeBytes, E,
                                                ConnectTbs),
                                  ConnectKey),
    Connect = frame(<<"connect">>, Unproved#{<<"proof">> => Proof}),
    {ok, Connect, #{node_id => StationNodeId, identity_key => StationKey, tls_binding => TlsBinding,
                    status_expires_at => ExpiresAt, binding_not_after => NotAfter, version => ?VERSION},
     #{version => ?VERSION, profile => Profile, exporter_value => E, challenge => ChallengeBytes, connect => Connect,
       station_node_id => StationNodeId, client_node_id => ClientNodeId, station_key => StationKey}}.

%%------------------------------------------------------------------
%% CONNECT, checked by the station
%%------------------------------------------------------------------

%% @doc The station's check of CONNECT, and the HELLO bytes to send. The station checks the frame, that the client's
%% ranges are the ones its opener sent and the profile the one chosen, the carried keys and the proof length, that each
%% key serves one purpose, the puzzle on the derived node_id, the CONNECT binding and status statement, the proof V3
%% against the challenge bytes it sent, the leaf this connection presented and the CONNECT tbs, and then its floor for
%% the client, so whether it holds one is told only to the node that proved the identity. The puzzle comes before any signature: under enforce an unsolved puzzle is refused with
%% puzzle_invalid, under log_only it is accepted and reported, and off skips it. A refusal carries a HELLO with
%% accepted 0 and one coarse refusal code.
-spec accept_connect(binary(), station_session()) ->
        {accepted, client(), binary()} | {refused, close_reason(), binary()}.
accept_connect(Bytes, #{profile := _, choice := _, challenge := Challenge, leaf := Leaf, now := _, export := _,
                        sign_session_proof := _, capabilities := Capabilities,
                        puzzle := #{mode := Mode, difficulty := Difficulty}} = Session)
  when is_binary(Challenge), is_binary(Leaf), (Mode =:= off orelse Mode =:= log_only orelse Mode =:= enforce),
       is_integer(Difficulty), Difficulty >= 0, Difficulty =< 256 ->
    connect_verdict(steps(#{bytes => Bytes, session => Session},
                          [fun decoded_connect/1, fun negotiation_repeated/1, fun profile_chosen/1,
                           fun client_keys/1, fun client_keys_distinct/1, fun client_identity/1,
                           fun client_puzzle/1, fun client_binding/1, fun client_status/1,
                           fun connect_exported/1, fun client_proof/1, fun client_floor/1, fun session_proved/1]),
                    Capabilities).

connect_verdict({ok, #{hello := Hello} = State}, _Capabilities) ->
    {accepted, client(State), Hello};
connect_verdict({error, Reason}, Capabilities) ->
    {refused, Reason, hello_refused(wire_refusal(Reason), Capabilities)}.

decoded_connect(#{bytes := Bytes} = State) ->
    with_fields(decode_raw(Bytes, <<"connect">>, [?CONNECT_KEYS]), State).

%% CONNECT repeats the ranges the opener sent, unchanged (section 4.5).
negotiation_repeated(#{fields := #{<<"versions">> := Versions, <<"profiles">> := Profiles},
                       session := #{choice := #{client_ranges := Ranges}}} = State) ->
    expect(#{versions => Versions, profiles => Profiles} =:= Ranges, negotiation_mismatch, State).

profile_chosen(#{fields := #{<<"profile">> := Name}, session := #{choice := #{profile := Chosen}}} = State) ->
    expect(Name =:= atom_to_binary(Chosen), profile_unsupported, State).

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

client_floor(#{client_node_id := NodeId, session := Session} = State) ->
    Floor = maps:get(floor, Session, fun(_NodeId) -> none end),
    expect(above_floor(?VERSION, Floor(NodeId)), version_below_floor, State).

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

connect_exported(#{client_node_id := ClientNodeId,
                   session := #{export := Export, challenge := Challenge, profile := Profile}} = State) ->
    with_exported(Export(?EXPORTER_LABEL, <<ClientNodeId/binary, (challenge_node_id(Challenge, Profile))/binary>>,
                         ?EXPORTER_BYTES), State).

%% The station's own challenge bytes decode: it built them. The CONNECT tbs is rebuilt from the frame as received,
%% the proof set empty; the frame is deterministic CBOR, so the rebuild is exact (section 4.2).
client_proof(#{fields := #{<<"connect_key">> := ConnectKey, <<"proof">> := Proof}, raw := Raw,
               client_node_id := ClientNodeId, exporter_value := E,
               session := #{profile := Profile, challenge := Challenge, leaf := Leaf}} = State) ->
    {ok, #{<<"nonce">> := Nonce}} = decode(Challenge, <<"challenge">>, [?CHALLENGE_KEYS]),
    Message = proof_message(Nonce, challenge_node_id(Challenge, Profile), ClientNodeId, Leaf, Challenge, E,
                            with_empty(<<"proof">>, Raw)),
    expect(macula_node_keys:verify(Message, Proof, ConnectKey, Profile), proof_invalid, State).

%% The station's node_id, from its own challenge bytes, which decode: it built them.
challenge_node_id(Challenge, Profile) ->
    {ok, #{<<"identity_key">> := StationKey}} = decode(Challenge, <<"challenge">>, [?CHALLENGE_KEYS]),
    macula_node_keys:node_id(StationKey, Profile).

%% Sign after verify: reached only once every check on CONNECT has passed. The session proof covers the HELLO tbs, so
%% the station's capabilities, at no fixed width (section 4.3).
session_proved(#{exporter_value := E, client_node_id := ClientNodeId, bytes := Connect,
                 session := #{sign_session_proof := Sign, challenge := Challenge, profile := Profile,
                              capabilities := Capabilities}} = State) ->
    Unproved = #{<<"accepted">> => 1, <<"capabilities">> => Capabilities},
    Message = session_proof_message(E, Challenge, Connect, challenge_node_id(Challenge, Profile), ClientNodeId,
                                    frame(<<"hello">>, Unproved#{<<"session_proof">> => <<>>})),
    with_session_proof(Sign(ClientNodeId, Message), Unproved, State).

with_session_proof({ok, Signature}, Unproved, State) ->
    {ok, State#{hello => frame(<<"hello">>, Unproved#{<<"session_proof">> => Signature})}};
with_session_proof({error, session_proof_rate}, _Unproved, _State) ->
    {error, session_proof_rate}.

client(#{fields := Fields, client_node_id := NodeId, puzzle := Puzzle, status_expires_at := ExpiresAt,
         binding_not_after := NotAfter}) ->
    #{node_id => NodeId,
      version => ?VERSION,
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

%% Proof V3: label || 0x00 || nonce || station node_id || client node_id || SHA-384(leaf DER) || SHA-384(challenge
%% bytes) || E || SHA-384(CONNECT tbs). Every field has a fixed width, so no two field sequences encode to the same
%% bytes.
proof_message(Nonce, StationNodeId, ClientNodeId, Leaf, ChallengeBytes, <<_:?EXPORTER_BYTES/binary>> = E,
              ConnectTbs) ->
    [<<?PROOF_LABEL, 0>>, Nonce, StationNodeId, ClientNodeId, crypto:hash(sha384, Leaf),
     crypto:hash(sha384, ChallengeBytes), E, crypto:hash(sha384, ConnectTbs)].

%% Session proof V2: label || 0x00 || E || SHA-384(challenge) || SHA-384(CONNECT) || station node_id ||
%% client node_id || SHA-384(HELLO tbs).
session_proof_message(E, Challenge, Connect, StationNodeId, ClientNodeId, HelloTbs) ->
    [<<?SESSION_PROOF_LABEL, 0>>, E, crypto:hash(sha384, Challenge), crypto:hash(sha384, Connect), StationNodeId,
     ClientNodeId, crypto:hash(sha384, HelloTbs)].

wire_refusal(unsupported_version) -> unsupported_version;
wire_refusal(puzzle_invalid) -> puzzle_invalid;
wire_refusal(session_proof_rate) -> session_proof_rate;
wire_refusal(_Other) -> not_accepted.

%%------------------------------------------------------------------
%% HELLO
%%------------------------------------------------------------------

hello_refused(Code, Capabilities) ->
    frame(<<"hello">>, #{<<"accepted">> => 0, <<"refusal_code">> => {text, atom_to_binary(Code)},
                         <<"capabilities">> => Capabilities}).

%% @doc The client's reading of HELLO: the station's capabilities, once the session proof verifies under the
%% station's identity key over the HELLO tbs, or why not. An unknown refusal code reads as not_accepted.
-spec read_hello(binary(), expect_hello()) ->
        {ok, #{capabilities := non_neg_integer(), version := version()}} | {error, close_reason()}.
read_hello(Bytes, #{version := ?VERSION} = Expect) ->
    hello_verdict(decode_raw(Bytes, <<"hello">>, [?HELLO_PROVED_KEYS, ?HELLO_UNPROVED_KEYS, ?HELLO_REFUSED_KEYS]),
                  Expect).

hello_verdict({ok, #{<<"accepted">> := 0, <<"refusal_code">> := {Code, _Known}}, _Raw}, _Expect) ->
    {error, {refused, Code}};
hello_verdict({ok, #{<<"accepted">> := 1, <<"session_proof">> := Proof, <<"capabilities">> := Capabilities}, Raw},
              #{profile := Profile} = Expect) ->
    session_proof_checked(byte_size(Proof) =:= macula_node_keys:signature_bytes(Profile), Proof, Capabilities,
                          with_empty(<<"session_proof">>, Raw), Expect);
hello_verdict({ok, #{<<"accepted">> := 1} = Fields, _Raw}, _Expect) when not is_map_key(<<"refusal_code">>, Fields) ->
    {error, session_proof_missing};
hello_verdict({ok, _Inconsistent, _Raw}, _Expect) ->
    {error, malformed_frame};
hello_verdict({error, _} = Error, _Expect) ->
    Error.

%% A session proof of the wrong length is a malformed frame, as a CONNECT proof of the wrong length is.
session_proof_checked(true, Proof, Capabilities, HelloTbs, Expect) ->
    session_proof_verdict(session_proof_valid(Proof, HelloTbs, Expect), Capabilities);
session_proof_checked(false, _Proof, _Capabilities, _HelloTbs, _Expect) ->
    {error, malformed_frame}.

session_proof_valid(Proof, HelloTbs, #{profile := Profile, exporter_value := E, challenge := Challenge,
                                       connect := Connect, station_node_id := StationNodeId,
                                       client_node_id := ClientNodeId, station_key := StationKey}) ->
    Message = session_proof_message(E, Challenge, Connect, StationNodeId, ClientNodeId, HelloTbs),
    macula_node_keys:verify(Message, Proof, StationKey, Profile).

session_proof_verdict(true, Capabilities) -> {ok, #{capabilities => Capabilities, version => ?VERSION}};
session_proof_verdict(false, _Capabilities) -> {error, session_proof_invalid}.

%% @doc A HELLO refusal as received, early or after CONNECT: its code, and whether that code is one this node knows
%% (`known') or an unknown one read as not_accepted (`fallback'), which the caller counts as wire_enum_fallback.
-spec read_refusal(binary()) -> {refused, refusal_code(), known | fallback} | {error, close_reason()}.
read_refusal(Bytes) ->
    refusal_verdict(decode(Bytes, <<"hello">>, [?HELLO_REFUSED_KEYS, ?HELLO_EARLY_KEYS])).

refusal_verdict({ok, #{<<"accepted">> := 0, <<"refusal_code">> := {Code, Known}}}) -> {refused, Code, Known};
refusal_verdict({ok, _Accepted}) -> {error, malformed_frame};
refusal_verdict({error, _} = Error) -> Error.

%%------------------------------------------------------------------
%% Status frames
%%------------------------------------------------------------------

%% @doc A status frame carrying a fresh status statement, sent at every reissue.
-spec status(envelope()) -> binary().
status(Statement) ->
    frame(<<"status">>, #{<<"statement">> => envelope_value(Statement)}).

%% @doc A peer's status frame, checked against the binding and identity key the handshake verified, and when the new
%% statement expires. STATUS travels after HELLO, so it is an envelope frame: it may carry frame_id, sent_at_ms and
%% ext (section 3.1). An unknown critical extension refuses it with extension_unsupported, and the caller drops it.
-spec read_status(binary(), peer()) -> {ok, non_neg_integer()} | {error, close_reason() | extension_unsupported}.
read_status(Bytes, Peer) when is_binary(Bytes) ->
    status_bytes(deterministic(macula_record_cbor:decode_strict(Bytes), Bytes), Peer).

status_bytes({ok, Wire}, Peer) -> read_status_wire(Wire, Peer);
status_bytes(not_a_frame, _Peer) -> {error, malformed_frame}.

status_verdict({ok, #{<<"statement">> := Statement}}, Binding, IdentityKey, Profile, Now) ->
    status_expiry(macula_key_bindings:verify_status(Statement, Binding, IdentityKey, Profile, Now));
status_verdict({error, _} = Error, _Binding, _IdentityKey, _Profile, _Now) ->
    Error.

status_expiry({ok, #{expires_at := ExpiresAt}}) -> {ok, ExpiresAt};
status_expiry({error, _} = Error) -> Error.

%% @doc read_status/2 on a frame already decoded from CBOR, for a connection that decodes each frame once to route
%% it.
-spec read_status_wire(term(), peer()) -> {ok, non_neg_integer()} | {error, close_reason() | extension_unsupported}.
read_status_wire(Wire, #{profile := Profile, identity_key := IdentityKey, binding := Binding, now := Now}) ->
    status_verdict(status_frame(text_keyed({ok, Wire})), Binding, IdentityKey, Profile, Now).

%% The optional envelope keys a STATUS may carry are read first, then exactly the frame's own keys.
status_frame(not_a_frame) ->
    {error, malformed_frame};
status_frame(Fields) ->
    Envelope = maps:with([<<"frame_id">>, <<"sent_at_ms">>, <<"ext">>], Fields),
    envelope_read(maps:fold(fun envelope_field/3, ok, Envelope), maps:without(maps:keys(Envelope), Fields)).

envelope_field(_Key, _Value, {error, _} = Error) -> Error;
envelope_field(<<"frame_id">>, <<_:128>>, ok) -> ok;
envelope_field(<<"sent_at_ms">>, At, ok) when is_integer(At), At >= 0, At < ?MAX_PROTOCOL_INT -> ok;
envelope_field(<<"ext">>, Ext, ok) -> extensions_read(macula_frame:read_hop_extensions(Ext));
envelope_field(_Key, _Value, ok) -> {error, malformed_frame}.

extensions_read({ok, _Ignored}) -> ok;
extensions_read({error, _} = Refused) -> Refused.

envelope_read(ok, Core) -> frame_version(Core, <<"status">>, [?STATUS_KEYS]);
envelope_read({error, _} = Error, _Core) -> Error.

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

%% Every frame carries version 5: the frame version is the handshake version (section 2).
frame(Type, Fields) ->
    Header = #{{text, <<"version">>} => ?VERSION, {text, <<"frame_type">>} => {text, Type}},
    macula_record_cbor:encode(maps:fold(fun(Key, Value, Acc) -> Acc#{{text, Key} => Value} end, Header, Fields)).

envelope_value(#{tbs := Tbs, signature := Signature}) ->
    #{{text, <<"tbs">>} => Tbs, {text, <<"signature">>} => Signature}.

%% A frame as received with one key's value set to the empty byte string, re-encoded: the tbs a proof covers.
with_empty(Key, Raw) ->
    macula_record_cbor:encode(Raw#{{text, Key} => <<>>}).

%%------------------------------------------------------------------
%% Strict decoding: deterministic CBOR, the version, then the frame type, then exactly the keys of one of the frame's
%% layouts, then the type and length of every field
%%------------------------------------------------------------------

decode(Bytes, Type, Layouts) ->
    fields_only(decode_raw(Bytes, Type, Layouts)).

fields_only({ok, Fields, _Raw}) -> {ok, Fields};
fields_only({error, _} = Error) -> Error.

%% The fields, and the frame as decoded for a tbs rebuild.
decode_raw(Bytes, Type, Layouts) when is_binary(Bytes) ->
    raw_verdict(deterministic(macula_record_cbor:decode_strict(Bytes), Bytes), Type, Layouts).

deterministic({ok, Map} = Decoded, Bytes) when is_map(Map) ->
    one_encoding(macula_record_cbor:encode(Map) =:= Bytes, Decoded);
deterministic(_NotAMap, _Bytes) ->
    not_a_frame.

one_encoding(true, Decoded) -> Decoded;
one_encoding(false, _Decoded) -> not_a_frame.

raw_verdict({ok, Raw}, Type, Layouts) ->
    with_raw(frame_version(text_keyed({ok, Raw}), Type, Layouts), Raw);
raw_verdict(not_a_frame, _Type, _Layouts) ->
    {error, malformed_frame}.

with_raw({ok, Fields}, Raw) -> {ok, Fields, Raw};
with_raw({error, _} = Error, _Raw) -> Error.

%% The frame_type of bytes that may be any handshake frame, before its layout is known.
frame_type_of(Bytes) ->
    type_named(macula_record_cbor:decode_strict(Bytes)).

type_named({ok, #{{text, <<"frame_type">>} := {text, Type}}}) -> Type;
type_named(_Other) -> undefined.

text_keyed({ok, Map}) when is_map(Map) -> maps:fold(fun text_key/3, #{}, Map);
text_keyed(_NotAMap) -> not_a_frame.

text_key({text, Key}, Value, Acc) -> Acc#{Key => Value};
text_key(Key, Value, Acc) -> Acc#{{not_text, Key} => Value}.

%% Any version but 5 is unsupported_version.
frame_version(#{<<"version">> := ?VERSION} = Fields, Type, Layouts) ->
    frame_type(Fields, Type, Layouts);
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

field_value(<<"version">>, ?VERSION) -> {ok, ?VERSION};
field_value(<<"frame_type">>, {text, Type}) -> {ok, Type};
field_value(<<"nonce">>, <<_:?NONCE_BYTES/binary>> = Nonce) -> {ok, Nonce};
field_value(<<"profile">>, {text, Name}) -> profile_text(Name);
field_value(<<"versions">>, Versions) -> versions(Versions);
field_value(<<"profiles">>, Profiles) -> profiles(Profiles);
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
field_value(<<"refusal_code">>, {text, Code}) -> {ok, refusal_code(Code)};
field_value(_Key, _Value) -> error.

bytes(Bytes) when is_binary(Bytes) -> {ok, Bytes};
bytes(_NotBytes) -> error.

profile_text(Name) when is_binary(Name), byte_size(Name) >= 1, byte_size(Name) =< ?MAX_PROFILE_NAME -> {ok, Name};
profile_text(_Name) -> error.

%% [Min, Max], Min =< Max, each a protocol integer.
versions([Min, Max]) when is_integer(Min), is_integer(Max), Min >= 0, Min =< Max, Max < ?MAX_PROTOCOL_INT ->
    {ok, [Min, Max]};
versions(_Other) ->
    error.

%% 1 to 8 distinct profile names.
profiles(Profiles) when is_list(Profiles), Profiles =/= [], length(Profiles) =< ?MAX_PROFILES ->
    Names = [Name || {text, Name} <- Profiles, profile_text(Name) =:= {ok, Name}],
    distinct(length(Names) =:= length(Profiles) andalso length(lists:usort(Names)) =:= length(Names), Names);
profiles(_Other) ->
    error.

distinct(true, Names) -> {ok, Names};
distinct(false, _Names) -> error.

envelope(#{{text, <<"tbs">>} := Tbs, {text, <<"signature">>} := Signature} = Envelope)
  when map_size(Envelope) =:= 2, is_binary(Tbs), is_binary(Signature) ->
    {ok, #{tbs => Tbs, signature => Signature}};
envelope(_NotAnEnvelope) ->
    error.

%% An unknown refusal code reads as not_accepted (section 3.4), marked as a fallback for the caller to count.
refusal_code(<<"unsupported_version">>) -> {unsupported_version, known};
refusal_code(<<"profile_unsupported">>) -> {profile_unsupported, known};
refusal_code(<<"puzzle_invalid">>) -> {puzzle_invalid, known};
refusal_code(<<"session_proof_rate">>) -> {session_proof_rate, known};
refusal_code(<<"not_accepted">>) -> {not_accepted, known};
refusal_code(_Unknown) -> {not_accepted, fallback}.

%%------------------------------------------------------------------
%% Steps
%%------------------------------------------------------------------

%% Run checks in order; each takes the state and returns it, extended, or the first refusal.
steps(State, []) -> {ok, State};
steps(State, [Step | Rest]) -> next_step(Step(State), Rest).

next_step({ok, State}, Rest) -> steps(State, Rest);
next_step({error, _} = Error, _Rest) -> Error.

with_fields({ok, Fields, Raw}, State) -> {ok, State#{fields => Fields, raw => Raw}};
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
