%% The wire 1.0 handshake vectors (priv/vectors/wire_1_0/handshake.json, docs/design/WIRE_1_0.md section 4,
%% macula#98): every transcript replayed against macula_handshake, each step judged by its receiver with the case's
%% inputs, and the HELLO refusal_code enum cases of enums.json that land with #98.
%%
%% The runner plays the receiver of each step. The station's checks need no private key except to sign the session
%% proof; the client's need its CONNECT key to answer a challenge. Both come from the test-only keys the vectors were
%% generated with (test/fixtures/wire_1_0_private_test_keys.etf), never from the vectors.
-module(macula_wire_1_0_handshake_tests).

-include_lib("eunit/include/eunit.hrl").

-define(KEYS_FILE, "test/fixtures/wire_1_0_private_test_keys.etf").
-define(DAY, 86400000).
-define(HOUR, 3600000).

handshake_test_() ->
    Keys = private_keys(),
    [{binary_to_list(Id), {timeout, 60, fun() -> transcript(Keys, Case) end}}
     || #{<<"id">> := Id, <<"lands_with">> := Lands} = Case <- cases(<<"handshake">>),
        lists:member(<<"macula#98">>, Lands)].

%% Each step: the receiver judges the frame, holding what it sent before. The state carries the station's choice
%% from the opener, and the challenge and CONNECT as they travelled.
transcript(Keys, #{<<"profile">> := ProfileName, <<"input">> := #{<<"steps">> := Steps} = Input}) ->
    Profile = binary_to_atom(ProfileName),
    lists:foldl(fun(Step, State) -> step(Keys, Profile, Input, Step, State) end, #{}, numbered(Steps)).

numbered(Steps) -> lists:zip(lists:seq(0, length(Steps) - 1), Steps).

%% Step 0: the station reads the opener.
step(_Keys, Profile, Input, {0, #{<<"frame">> := Hex, <<"expect">> := Expect}}, State) ->
    opener_judged(macula_handshake:read_opener(hex(Hex), ranges(Profile, station(Input))), Expect, State);
%% Step 1: the client reads the challenge, or the station's early HELLO.
step(Keys, Profile, Input, {1, #{<<"frame">> := Hex, <<"expect">> := Expect}}, State) ->
    challenge_judged(macula_handshake:answer_challenge(hex(Hex), client_session(Keys, Profile, Input)), Expect,
                     State#{challenge => hex(Hex)});
%% Step 2: the station reads CONNECT, against the challenge it sent.
step(Keys, Profile, Input, {2, #{<<"frame">> := Hex, <<"expect">> := Expect}}, #{choice := Choice,
                                                                                  challenge := Challenge} = State) ->
    connect_judged(macula_handshake:accept_connect(hex(Hex), station_session(Keys, Profile, Input, Choice, Challenge)),
                   Expect, State#{connect => hex(Hex)});
%% Step 3: the client reads HELLO, against the challenge and CONNECT as they travelled.
step(Keys, Profile, Input, {3, #{<<"frame">> := Hex, <<"expect">> := Expect}}, State) ->
    hello_judged(macula_handshake:read_hello(hex(Hex), expect_hello(Keys, Profile, Input, State)), Expect),
    State.

opener_judged({ok, Choice}, #{<<"outcome">> := <<"accept">>}, State) ->
    State#{choice => Choice};
opener_judged({refused, Reason, Hello}, #{<<"outcome">> := <<"refuse">>, <<"reason">> := Expected,
                                          <<"wire_refusal">> := Wire, <<"hello">> := Ranges}, State) ->
    ?assertEqual(Expected, atom_to_binary(Reason)),
    ?assertEqual(early_hello(Wire, Ranges), decoded(Hello)),
    State;
opener_judged({error, Reason}, #{<<"outcome">> := <<"refuse">>, <<"reason">> := Expected} = Expect, State) ->
    ?assertNot(is_map_key(<<"wire_refusal">>, Expect)),
    ?assertEqual(Expected, atom_to_binary(Reason)),
    State;
opener_judged(Got, Expect, _State) ->
    ?assertEqual(Expect, Got).

challenge_judged({ok, _Connect, _Station, _Expect}, #{<<"outcome">> := <<"accept">>}, State) ->
    State;
challenge_judged({error, {refused, Code, Ranges}}, #{<<"outcome">> := <<"refuse">>, <<"reason">> := Expected,
                                                    <<"station_ranges">> := StationRanges}, State) ->
    ?assertEqual(Expected, <<"refused:", (atom_to_binary(Code))/binary>>),
    ?assertEqual(StationRanges, json_ranges(Ranges)),
    State;
challenge_judged({error, Reason}, #{<<"outcome">> := <<"refuse">>, <<"reason">> := Expected}, State) ->
    ?assertEqual(Expected, reason(Reason)),
    State;
challenge_judged(Got, Expect, _State) ->
    ?assertEqual(Expect, Got).

connect_judged({accepted, _Client, Hello}, #{<<"outcome">> := <<"accept">>}, State) ->
    ?assertMatch(#{{text, <<"accepted">>} := 1}, decoded(Hello)),
    State;
connect_judged({refused, Reason, Hello}, #{<<"outcome">> := <<"refuse">>, <<"reason">> := Expected,
                                           <<"wire_refusal">> := Wire}, State) ->
    ?assertEqual(Expected, atom_to_binary(Reason)),
    ?assertMatch(#{{text, <<"accepted">>} := 0, {text, <<"refusal_code">>} := {text, Wire}}, decoded(Hello)),
    State;
connect_judged(Got, Expect, _State) ->
    ?assertEqual(Expect, Got).

hello_judged({ok, #{version := 5}}, #{<<"outcome">> := <<"accept">>}) ->
    ok;
hello_judged({error, Reason}, #{<<"outcome">> := <<"refuse">>, <<"reason">> := Expected}) ->
    ?assertEqual(Expected, reason(Reason));
hello_judged(Got, Expect) ->
    ?assertEqual(Expect, Got).

reason({refused, Code}) -> <<"refused:", (atom_to_binary(Code))/binary>>;
reason(Reason) when is_atom(Reason) -> atom_to_binary(Reason).

%%------------------------------------------------------------------
%% The HELLO refusal_code enum (WIRE_1_0.md 3.4): an unknown code reads as not_accepted.

refusal_code_enum_test_() ->
    [{binary_to_list(Id), fun() -> refusal_read(hex(Hex), Expect) end}
     || #{<<"id">> := Id, <<"lands_with">> := Lands, <<"input">> := #{<<"frame">> := Hex},
          <<"expect">> := #{<<"field">> := <<"refusal_code">>} = Expect} <- cases(<<"enums">>),
        lists:member(<<"macula#98">>, Lands)].

%% read_refusal/1 says whether the code fell back, so the connection counts wire_enum_fallback.
refusal_read(Hello, #{<<"outcome">> := <<"fallback">>, <<"value">> := Value}) ->
    ?assertEqual({refused, binary_to_atom(Value), fallback}, macula_handshake:read_refusal(Hello));
refusal_read(Hello, #{<<"outcome">> := <<"accept">>, <<"value">> := Value}) ->
    ?assertEqual({refused, binary_to_atom(Value), known}, macula_handshake:read_refusal(Hello)).

%%------------------------------------------------------------------
%% STATUS after HELLO is an envelope frame (WIRE_1_0.md 3.1): envelope.json's status cases.

status_test_() ->
    {setup, fun() -> {ok, _} = application:ensure_all_started(macula) end, fun(_) -> ok end,
     [{binary_to_list(Id), fun() -> status_read(Case) end}
      || #{<<"id">> := Id, <<"input">> := #{<<"status_peer">> := _}} = Case <- cases(<<"envelope">>)]}.

status_read(#{<<"profile">> := P, <<"input">> := #{<<"frame">> := Hex, <<"now_ms">> := Now,
                                                   <<"status_peer">> := #{<<"identity">> := Role,
                                                                          <<"binding">> := Binding}},
              <<"expect">> := Expect}) ->
    Profile = binary_to_atom(P),
    Peer = #{profile => Profile, identity_key => public_key(Profile, binary_to_atom(Role)), now => Now,
             binding => #{tbs => hex(maps:get(<<"tbs">>, Binding)), signature => hex(maps:get(<<"signature">>, Binding))}},
    Before = macula_wire_tolerance:counters(),
    Read = macula_handshake:read_status(hex(Hex), Peer),
    status_judged(Read, Expect),
    counted(maps:get(<<"count">>, Expect, none), Before).

status_judged({ok, _ExpiresAt}, #{<<"outcome">> := <<"accept">>}) -> ok;
status_judged({error, Reason}, #{<<"outcome">> := <<"refuse">>, <<"reason">> := Expected}) ->
    ?assertEqual(Expected, atom_to_binary(Reason));
status_judged(Got, Expect) ->
    ?assertEqual(Expect, Got).

counted(none, _Before) -> ok;
counted(Counter, Before) ->
    Name = binary_to_atom(Counter),
    Moved = [K || K := V <- macula_wire_tolerance:counters(), counter_named(K, Name), V > maps:get(K, Before, 0)],
    ?assertNotEqual([], Moved).

counter_named({Name, _Label}, Name) -> true;
counter_named(Name, Name) -> true;
counter_named(_Key, _Name) -> false.

public_key(Profile, Role) ->
    macula_node_keys:public_key(key(private_keys(), Profile, Role)).

%%------------------------------------------------------------------
%% Sessions from a case's inputs

station(#{<<"station">> := Station}) -> Station.

client(#{<<"client">> := Client}) -> Client.

%% The receiver's own ranges; profile names it knows become atoms.
ranges(_Profile, #{<<"versions">> := [Min, Max], <<"profiles">> := Profiles}) ->
    #{versions => [Min, Max], profiles => [binary_to_atom(P) || P <- Profiles]}.

json_ranges(#{versions := Versions, profiles := Profiles}) ->
    #{<<"versions">> => Versions, <<"profiles">> => Profiles}.

client_session(Keys, Profile, #{<<"now_ms">> := Now, <<"tls">> := Tls} = Input) ->
    Client = client(Input),
    Identity = key(Keys, Profile, client_identity),
    Connect = key(Keys, Profile, client_connect),
    T0 = Now - 1000,
    Binding = macula_key_bindings:connect_binding(Identity, macula_node_keys:public_key(Connect), T0 - ?HOUR,
                                                  T0 + ?DAY),
    Status = macula_key_bindings:status_statement(Identity, Binding, T0 - 60000, T0 + 59 * 60000),
    Expected = hex(maps:get(<<"expected_station_node_id">>, Client)),
    maps:merge(ranges(Profile, Client),
               #{profile => Profile, expected_node_id => Expected, leaf => leaf(Tls),
                 identity_key => macula_node_keys:public_key(Identity), connect_key => Connect,
                 connect_binding => Binding, connect_status => Status,
                 capabilities => maps:get(<<"capabilities">>, Client), now => Now, export => exporter(Tls),
                 floor => floor_for(Expected, maps:get(<<"floor">>, Client))}).

station_session(Keys, Profile, #{<<"now_ms">> := Now, <<"tls">> := Tls} = Input, Choice, Challenge) ->
    Station = station(Input),
    #{<<"puzzle">> := #{<<"difficulty">> := Difficulty, <<"mode">> := Mode}} = Station,
    Identity = key(Keys, Profile, station_identity),
    Floor = maps:get(<<"floor">>, Station),
    #{profile => Profile, choice => Choice, challenge => Challenge, leaf => leaf(Tls),
      puzzle => #{difficulty => Difficulty, mode => binary_to_atom(Mode)},
      capabilities => maps:get(<<"capabilities">>, Station), now => Now, export => exporter(Tls),
      sign_session_proof => fun(_ClientNodeId, Message) -> {ok, macula_node_keys:sign(Message, Identity)} end,
      floor => fun(NodeId) -> floor_for(NodeId, Floor) end}.

expect_hello(Keys, Profile, #{<<"tls">> := #{<<"exporter">> := E}}, #{challenge := Challenge, connect := Connect}) ->
    Station = macula_node_keys:public_key(key(Keys, Profile, station_identity)),
    Client = macula_node_keys:public_key(key(Keys, Profile, client_identity)),
    #{version => 5, profile => Profile, exporter_value => hex(E), challenge => Challenge, connect => Connect,
      station_node_id => macula_node_keys:node_id(Station, Profile),
      client_node_id => macula_node_keys:node_id(Client, Profile), station_key => Station}.

%% A case's floor: [{node_id, version}] as recorded for each peer.
floor_for(NodeId, Floor) ->
    case [V || #{<<"node_id">> := N, <<"version">> := V} <- Floor, hex(N) =:= NodeId] of
        [Version] -> Version;
        [] -> none
    end.

leaf(#{<<"leaf_der">> := Leaf}) -> hex(Leaf).

exporter(#{<<"exporter">> := E}) ->
    Value = hex(E),
    fun(_Label, _Context, 32) -> {ok, Value} end.

early_hello(Wire, #{<<"versions">> := Versions, <<"profiles">> := Profiles}) ->
    #{{text, <<"version">>} => 5, {text, <<"frame_type">>} => {text, <<"hello">>}, {text, <<"accepted">>} => 0,
      {text, <<"refusal_code">>} => {text, Wire}, {text, <<"versions">>} => Versions,
      {text, <<"profiles">>} => [{text, P} || P <- Profiles]}.

%%------------------------------------------------------------------
%% Files

private_keys() ->
    {ok, Bin} = file:read_file(?KEYS_FILE),
    binary_to_term(Bin).

key(Keys, Profile, Role) -> maps:get({Profile, Role}, Keys).

decoded(Bytes) ->
    {ok, Map} = macula_record_cbor:decode_strict(Bytes),
    Map.

hex(Hex) -> binary:decode_hex(Hex).

cases(Class) ->
    {ok, Bytes} = file:read_file(filename:join([code:priv_dir(macula), "vectors", "wire_1_0",
                                                <<Class/binary, ".json">>])),
    maps:get(<<"cases">>, json:decode(Bytes)).
