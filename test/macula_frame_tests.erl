%% EUnit tests for hecate_frame.
-module(macula_frame_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% Constructors — header invariants
%%------------------------------------------------------------------

connect_has_required_header_fields_test() ->
    Frame = build_connect(),
    ?assertEqual(connect, macula_frame:frame_type(Frame)),
    ?assertEqual(2, macula_frame:version(Frame)),
    ?assertEqual(16, byte_size(macula_frame:frame_id(Frame))),
    ?assert(macula_frame:sent_at_ms(Frame) > 0).

hello_has_required_header_fields_test() ->
    Frame = build_hello(),
    ?assertEqual(hello, macula_frame:frame_type(Frame)),
    ?assertEqual(2, macula_frame:version(Frame)).

goodbye_has_required_header_fields_test() ->
    F = macula_frame:goodbye(operator_stop, undefined),
    ?assertEqual(goodbye, macula_frame:frame_type(F)),
    ?assertEqual(operator_stop, maps:get(reason, F)),
    ?assertEqual(undefined, maps:get(detail, F)).

goodbye_with_detail_test() ->
    F = macula_frame:goodbye(draining, <<"shutting down">>),
    ?assertEqual(<<"shutting down">>, maps:get(detail, F)).

%%------------------------------------------------------------------
%% Sign / verify
%%------------------------------------------------------------------

sign_attaches_64_byte_signature_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(build_connect(Kp), Kp),
    ?assertEqual(64, byte_size(macula_frame:signature(F))).

verify_signed_connect_with_node_id_pubkey_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(build_connect(Kp), Kp),
    Pub = macula_identity:public(Kp),
    ?assertMatch({ok, _}, macula_frame:verify(F, Pub)).

verify_rejects_tampered_frame_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(build_connect(Kp), Kp),
    Tampered = F#{capabilities => 999},
    ?assertEqual({error, signature_invalid},
                 macula_frame:verify(Tampered, macula_identity:public(Kp))).

verify_rejects_wrong_pubkey_test() ->
    Kp1 = macula_identity:generate(),
    Kp2 = macula_identity:generate(),
    F = macula_frame:sign(build_connect(Kp1), Kp1),
    ?assertEqual({error, signature_invalid},
                 macula_frame:verify(F, macula_identity:public(Kp2))).

verify_rejects_unsigned_frame_test() ->
    F = build_connect(),
    Pub = crypto:strong_rand_bytes(32),
    ?assertEqual({error, bad_frame}, macula_frame:verify(F, Pub)).

%%------------------------------------------------------------------
%% Wire codec — single-frame round-trip
%%------------------------------------------------------------------

encode_prepends_4_byte_length_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(build_connect(Kp), Kp),
    Wire = macula_frame:encode(F),
    <<Len:32/big, Body/binary>> = Wire,
    ?assertEqual(byte_size(Body), Len).

encode_decode_roundtrip_connect_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(build_connect(Kp), Kp),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, Decoded),
    ?assertMatch({ok, _}, macula_frame:verify(Decoded, macula_identity:public(Kp))).

encode_decode_roundtrip_hello_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(build_hello(Kp), Kp),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertMatch({ok, _}, macula_frame:verify(Decoded, macula_identity:public(Kp))).

encode_decode_roundtrip_goodbye_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:goodbye(draining, <<"bye">>), Kp),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertMatch({ok, _}, macula_frame:verify(Decoded, macula_identity:public(Kp))).

%%------------------------------------------------------------------
%% Wire codec — partial / streaming
%%------------------------------------------------------------------

decode_returns_more_for_short_length_prefix_test() ->
    ?assertEqual({more, 4}, macula_frame:decode(<<>>)),
    ?assertEqual({more, 1}, macula_frame:decode(<<1, 2, 3>>)).

decode_returns_more_for_short_body_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(build_connect(Kp), Kp),
    Wire = macula_frame:encode(F),
    %% Truncate to 10 bytes (4-byte len + 6 bytes of body).
    Short = binary:part(Wire, 0, 10),
    ?assertMatch({more, _}, macula_frame:decode(Short)).

decode_rejects_oversized_length_test() ->
    %% 32 MiB declared — exceeds 16 MiB cap.
    Bogus = <<(32 * 1024 * 1024):32/big>>,
    ?assertEqual({error, frame_too_large}, macula_frame:decode(Bogus)).

decode_rejects_garbage_body_test() ->
    Garbage = <<10:32/big, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>,
    ?assertEqual({error, bad_frame}, macula_frame:decode(Garbage)).

%%------------------------------------------------------------------
%% Stream parser
%%------------------------------------------------------------------

parse_stream_drains_multiple_frames_test() ->
    Kp = macula_identity:generate(),
    F1 = macula_frame:sign(build_connect(Kp), Kp),
    F2 = macula_frame:sign(build_hello(Kp), Kp),
    Buf = <<(macula_frame:encode(F1))/binary, (macula_frame:encode(F2))/binary>>,
    {Frames, <<>>} = macula_frame:parse_stream(Buf),
    ?assertEqual(2, length(Frames)),
    [D1, D2] = Frames,
    ?assertEqual(connect, macula_frame:frame_type(D1)),
    ?assertEqual(hello, macula_frame:frame_type(D2)).

parse_stream_returns_unconsumed_tail_test() ->
    Kp = macula_identity:generate(),
    F1 = macula_frame:sign(build_connect(Kp), Kp),
    Wire1 = macula_frame:encode(F1),
    %% Append a partial second frame: first 6 bytes of length+body.
    F2 = macula_frame:sign(build_hello(Kp), Kp),
    Wire2Partial = binary:part(macula_frame:encode(F2), 0, 6),
    Buf = <<Wire1/binary, Wire2Partial/binary>>,
    {Frames, Rest} = macula_frame:parse_stream(Buf),
    ?assertEqual(1, length(Frames)),
    ?assertEqual(Wire2Partial, Rest).

%%------------------------------------------------------------------
%% Determinism
%%------------------------------------------------------------------

encode_is_deterministic_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(build_connect(Kp), Kp),
    ?assertEqual(macula_frame:encode(F), macula_frame:encode(F)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

build_connect() ->
    build_connect(macula_identity:generate()).

build_connect(Kp) ->
    Pub = macula_identity:public(Kp),
    macula_frame:connect(#{
        node_id          => Pub,
        station_id       => Pub,
        realms           => [crypto:strong_rand_bytes(32)],
        capabilities     => 16#FF,
        puzzle_evidence  => macula_identity:puzzle_evidence(Pub)
    }).

build_hello() ->
    build_hello(macula_identity:generate()).

build_hello(Kp) ->
    Pub = macula_identity:public(Kp),
    macula_frame:hello(#{
        node_id                 => Pub,
        station_id              => Pub,
        realms                  => [crypto:strong_rand_bytes(32)],
        capabilities            => 16#FF,
        accepted                => true,
        negotiated_capabilities => 16#0F
    }).

%%------------------------------------------------------------------
%% SWIM frames
%%------------------------------------------------------------------

swim_ping_has_expected_shape_test() ->
    Kp = macula_identity:generate(),
    Pub = macula_identity:public(Kp),
    U   = signed_update(Pub, alive, Kp),
    F = macula_frame:swim_ping(#{round => 3, incarnation => 7, piggyback => [U]}),
    ?assertEqual(swim_ping, macula_frame:frame_type(F)),
    ?assertEqual(3, maps:get(round, F)),
    ?assertEqual(7, maps:get(incarnation, F)),
    ?assertMatch([#{signature := _}], maps:get(piggyback, F)).

swim_ack_carries_responder_test() ->
    Kp = macula_identity:generate(),
    Pub = macula_identity:public(Kp),
    F = macula_frame:swim_ack(#{
        round => 3, responder => Pub, incarnation => 7, piggyback => []
    }),
    ?assertEqual(swim_ack, macula_frame:frame_type(F)),
    ?assertEqual(Pub, maps:get(responder, F)).

swim_suspect_and_confirm_share_shape_test() ->
    Target = crypto:strong_rand_bytes(32),
    By     = crypto:strong_rand_bytes(32),
    Spec = #{target => Target, target_incarnation => 2,
             suspected_by => By, ttl => 5},
    S = macula_frame:swim_suspect(Spec),
    C = macula_frame:swim_confirm(Spec),
    ?assertEqual(swim_suspect, macula_frame:frame_type(S)),
    ?assertEqual(swim_confirm, macula_frame:frame_type(C)),
    ?assertEqual(Target, maps:get(target, S)),
    ?assertEqual(Target, maps:get(target, C)),
    ?assertEqual(5, maps:get(ttl, S)).

swim_ping_sign_verify_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(
        macula_frame:swim_ping(#{round => 1, incarnation => 0, piggyback => []}),
        Kp),
    ?assertMatch({ok, _},
                 macula_frame:verify(F, macula_identity:public(Kp))).

swim_ping_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(
        macula_frame:swim_ping(#{round => 42, incarnation => 1, piggyback => []}),
        Kp),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, Decoded),
    ?assertMatch({ok, _},
                 macula_frame:verify(Decoded, macula_identity:public(Kp))).

%%------------------------------------------------------------------
%% SWIM piggyback updates — signed independently by observer
%%------------------------------------------------------------------

swim_update_sign_verify_roundtrip_test() ->
    Kp = macula_identity:generate(),
    U = signed_update(macula_identity:public(Kp), alive, Kp),
    ?assertMatch({ok, _}, macula_frame:verify_swim_update(U)).

swim_update_rejects_tamper_test() ->
    Kp = macula_identity:generate(),
    U = signed_update(macula_identity:public(Kp), alive, Kp),
    Tampered = U#{state => suspect},
    ?assertEqual({error, signature_invalid},
                 macula_frame:verify_swim_update(Tampered)).

swim_update_rejects_wrong_observer_test() ->
    Kp1 = macula_identity:generate(),
    Kp2 = macula_identity:generate(),
    %% Claim "by = Kp1" but sign with Kp2.
    Unsigned = macula_frame:swim_update(#{
        target      => crypto:strong_rand_bytes(32),
        state       => alive,
        incarnation => 0,
        observed_at => erlang:system_time(millisecond),
        by          => macula_identity:public(Kp1)
    }),
    Signed = macula_frame:sign_swim_update(Unsigned, Kp2),
    ?assertEqual({error, signature_invalid},
                 macula_frame:verify_swim_update(Signed)).

swim_update_domain_is_distinct_from_frame_domain_test() ->
    %% Signing a swim_update with the swim-update domain should not verify
    %% if the frame sig-verify path (different domain) is tried.
    Kp = macula_identity:generate(),
    U = signed_update(macula_identity:public(Kp), alive, Kp),
    %% Attempt frame-verify on a signed update — must fail (bad_frame
    %% because it's missing frame fields, or signature_invalid).
    Result = macula_frame:verify(U, macula_identity:public(Kp)),
    ?assertNotMatch({ok, _}, Result).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

signed_update(Target, State, Kp) ->
    By = macula_identity:public(Kp),
    U = macula_frame:swim_update(#{
        target      => Target,
        state       => State,
        incarnation => 0,
        observed_at => erlang:system_time(millisecond),
        by          => By
    }),
    macula_frame:sign_swim_update(U, Kp).

%%------------------------------------------------------------------
%% DHT frames — Part 6 §7
%%------------------------------------------------------------------

%% -- PING / PONG --------------------------------------------------

ping_has_16_byte_nonce_test() ->
    Nonce = crypto:strong_rand_bytes(16),
    F = macula_frame:ping(#{nonce => Nonce}),
    ?assertEqual(ping, macula_frame:frame_type(F)),
    ?assertEqual(Nonce, maps:get(nonce, F)).

ping_rejects_wrong_nonce_size_test() ->
    ?assertError(function_clause,
                 macula_frame:ping(#{nonce => <<0:64>>})).

pong_has_16_byte_nonce_test() ->
    Nonce = crypto:strong_rand_bytes(16),
    F = macula_frame:pong(#{nonce => Nonce}),
    ?assertEqual(pong, macula_frame:frame_type(F)),
    ?assertEqual(Nonce, maps:get(nonce, F)).

ping_pong_share_nonce_in_roundtrip_test() ->
    Nonce = crypto:strong_rand_bytes(16),
    Ping  = macula_frame:ping(#{nonce => Nonce}),
    Pong  = macula_frame:pong(#{nonce => Nonce}),
    ?assertEqual(maps:get(nonce, Ping), maps:get(nonce, Pong)).

ping_sign_verify_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F  = macula_frame:sign(
           macula_frame:ping(#{nonce => crypto:strong_rand_bytes(16)}),
           Kp),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, Decoded),
    ?assertMatch({ok, _}, macula_frame:verify(Decoded,
                                              macula_identity:public(Kp))).

%% -- FIND_NODE / NODES --------------------------------------------

find_node_has_key_origin_depth_test() ->
    Key    = crypto:strong_rand_bytes(32),
    Origin = crypto:strong_rand_bytes(32),
    F = macula_frame:find_node(#{key => Key, origin => Origin, depth => 2}),
    ?assertEqual(find_node, macula_frame:frame_type(F)),
    ?assertEqual(Key, maps:get(key, F)),
    ?assertEqual(Origin, maps:get(origin, F)),
    ?assertEqual(2, maps:get(depth, F)).

find_node_rejects_non_32_byte_key_test() ->
    ?assertError(function_clause,
                 macula_frame:find_node(#{key    => <<0:128>>,
                                          origin => crypto:strong_rand_bytes(32),
                                          depth  => 0})).

nodes_carries_station_refs_test() ->
    Ref1 = sample_station_ref(),
    Ref2 = sample_station_ref(),
    Key  = crypto:strong_rand_bytes(32),
    F    = macula_frame:nodes(#{key => Key, nodes => [Ref1, Ref2]}),
    ?assertEqual(nodes, macula_frame:frame_type(F)),
    ?assertEqual(2, length(maps:get(nodes, F))),
    ?assertEqual(Key, maps:get(key, F)).

nodes_accepts_empty_list_test() ->
    F = macula_frame:nodes(#{key => crypto:strong_rand_bytes(32),
                             nodes => []}),
    ?assertEqual([], maps:get(nodes, F)).

nodes_validates_each_station_ref_test() ->
    Bad = #{node_id => <<0:256>>, station_id => <<0:256>>,
            tier => 99,   %% invalid — tier 0..4
            country => <<"BE">>, last_seen_at => 1},
    ?assertError(function_clause,
                 macula_frame:nodes(#{key   => <<0:256>>,
                                      nodes => [Bad]})).

find_node_nodes_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    Req = macula_frame:sign(
           macula_frame:find_node(#{key    => crypto:strong_rand_bytes(32),
                                    origin => crypto:strong_rand_bytes(32),
                                    depth  => 1}),
           Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(Req)),
    ?assertEqual(Req, D),
    Resp = macula_frame:sign(
             macula_frame:nodes(#{key => crypto:strong_rand_bytes(32),
                                  nodes => [sample_station_ref()]}),
             Kp),
    {ok, D2, <<>>} = macula_frame:decode(macula_frame:encode(Resp)),
    ?assertEqual(Resp, D2),
    ?assertMatch({ok, _},
                 macula_frame:verify(D2, macula_identity:public(Kp))).

%% -- FIND_VALUE / VALUE -------------------------------------------

find_value_has_key_and_origin_test() ->
    Key    = crypto:strong_rand_bytes(32),
    Origin = crypto:strong_rand_bytes(32),
    F = macula_frame:find_value(#{key => Key, origin => Origin}),
    ?assertEqual(find_value, macula_frame:frame_type(F)),
    ?assertEqual(Key, maps:get(key, F)),
    ?assertEqual(Origin, maps:get(origin, F)).

value_carries_records_test() ->
    Rec = sample_record(),
    F = macula_frame:value(#{key => crypto:strong_rand_bytes(32),
                             records => [Rec]}),
    ?assertEqual(value, macula_frame:frame_type(F)),
    ?assertEqual([Rec], maps:get(records, F)).

value_rejects_malformed_record_test() ->
    BadRec = #{type => 1, key => <<0:256>>, payload => not_a_map},
    ?assertError(function_clause,
                 macula_frame:value(#{key     => crypto:strong_rand_bytes(32),
                                      records => [BadRec]})).

find_value_value_wire_roundtrip_test() ->
    Kp  = macula_identity:generate(),
    Req = macula_frame:sign(
            macula_frame:find_value(#{key    => crypto:strong_rand_bytes(32),
                                      origin => crypto:strong_rand_bytes(32)}),
            Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(Req)),
    ?assertEqual(Req, D),
    Rsp = macula_frame:sign(
            macula_frame:value(#{key => crypto:strong_rand_bytes(32),
                                 records => [sample_record()]}),
            Kp),
    {ok, D2, <<>>} = macula_frame:decode(macula_frame:encode(Rsp)),
    ?assertEqual(Rsp, D2).

%% -- STORE / STORE_ACK --------------------------------------------

store_carries_record_test() ->
    Rec = sample_record(),
    F = macula_frame:store(#{record => Rec}),
    ?assertEqual(store, macula_frame:frame_type(F)),
    ?assertEqual(Rec, maps:get(record, F)).

store_rejects_bad_record_test() ->
    ?assertError(function_clause,
                 macula_frame:store(#{record => not_a_record})).

store_ack_positive_test() ->
    Key = crypto:strong_rand_bytes(32),
    F = macula_frame:store_ack(#{key => Key, stored => true}),
    ?assertEqual(store_ack, macula_frame:frame_type(F)),
    ?assertEqual(true, maps:get(stored, F)),
    ?assertEqual(undefined, maps:get(reason, F)).

store_ack_with_rejection_reason_test() ->
    F = macula_frame:store_ack(#{key => crypto:strong_rand_bytes(32),
                                 stored => false,
                                 reason => quota}),
    ?assertEqual(false, maps:get(stored, F)),
    ?assertEqual(quota, maps:get(reason, F)).

store_ack_rejects_non_atom_reason_test() ->
    ?assertError(function_clause,
                 macula_frame:store_ack(#{key    => crypto:strong_rand_bytes(32),
                                          stored => false,
                                          reason => <<"bad">>})).

store_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F  = macula_frame:sign(macula_frame:store(#{record => sample_record()}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D),
    ?assertMatch({ok, _},
                 macula_frame:verify(D, macula_identity:public(Kp))).

%% -- REPLICATE / REPLICATE_ACK ------------------------------------

replicate_carries_record_and_custodian_flag_test() ->
    F = macula_frame:replicate(#{record => sample_record(),
                                 new_custodian => true}),
    ?assertEqual(replicate, macula_frame:frame_type(F)),
    ?assertEqual(true, maps:get(new_custodian, F)).

replicate_rejects_non_boolean_flag_test() ->
    ?assertError(function_clause,
                 macula_frame:replicate(#{record => sample_record(),
                                          new_custodian => perhaps})).

replicate_ack_test() ->
    Key = crypto:strong_rand_bytes(32),
    F = macula_frame:replicate_ack(#{key => Key, accepted => true}),
    ?assertEqual(replicate_ack, macula_frame:frame_type(F)),
    ?assertEqual(Key, maps:get(key, F)),
    ?assertEqual(true, maps:get(accepted, F)).

replicate_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F  = macula_frame:sign(
           macula_frame:replicate(#{record => sample_record(),
                                    new_custodian => false}),
           Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D),
    ?assertMatch({ok, _},
                 macula_frame:verify(D, macula_identity:public(Kp))).

%% -- station_ref helper -------------------------------------------

station_ref_populates_defaults_test() ->
    Ref = macula_frame:station_ref(#{
        node_id      => <<0:256>>,
        station_id   => <<0:256>>,
        tier         => 1,
        country      => <<"BE">>,
        last_seen_at => 1
    }),
    ?assertEqual([], maps:get(addresses, Ref)),
    ?assertEqual(undefined, maps:get(asn, Ref)).

station_ref_rejects_tier_above_4_test() ->
    ?assertError(function_clause,
                 macula_frame:station_ref(#{
                     node_id      => <<0:256>>,
                     station_id   => <<0:256>>,
                     tier         => 5,
                     country      => <<"BE">>,
                     last_seen_at => 1
                 })).

station_ref_rejects_wrong_country_size_test() ->
    ?assertError(function_clause,
                 macula_frame:station_ref(#{
                     node_id      => <<0:256>>,
                     station_id   => <<0:256>>,
                     tier         => 1,
                     country      => <<"BEL">>,
                     last_seen_at => 1
                 })).

%%------------------------------------------------------------------
%% DHT helpers
%%------------------------------------------------------------------

sample_station_ref() ->
    macula_frame:station_ref(#{
        node_id      => crypto:strong_rand_bytes(32),
        station_id   => crypto:strong_rand_bytes(32),
        addresses    => [],
        tier         => 2,
        asn          => 64512,
        country      => <<"BE">>,
        last_seen_at => erlang:system_time(millisecond)
    }).

sample_record() ->
    Kp = macula_identity:generate(),
    Node = macula_record:node_record(macula_identity:public(Kp), [], 0),
    macula_record:sign(Node, Kp).

%%------------------------------------------------------------------
%% CALL / RESULT / ERROR frames — Part 6 §5
%%------------------------------------------------------------------

%% -- CALL ---------------------------------------------------------

call_has_required_header_fields_test() ->
    F = sample_call(),
    ?assertEqual(call, macula_frame:frame_type(F)),
    ?assertEqual(16, byte_size(maps:get(call_id, F))),
    ?assertEqual(32, byte_size(maps:get(realm, F))),
    ?assertEqual(32, byte_size(maps:get(caller, F))),
    ?assertEqual(<<>>, maps:get(source_route, F)),
    ?assertEqual(0,    maps:get(retry_budget, F)).

call_with_optional_fields_test() ->
    Caller = crypto:strong_rand_bytes(32),
    F = macula_frame:call(#{
        call_id      => macula_record_uuid:v7(),
        procedure    => <<"realm/acme/weather/forecast/get_v1">>,
        realm        => crypto:strong_rand_bytes(32),
        payload      => #{city => <<"Brussels">>},
        deadline_ms  => erlang:system_time(millisecond) + 5_000,
        caller       => Caller,
        source_route => <<1, 2, 3>>,
        retry_budget => 3
    }),
    ?assertEqual(<<1, 2, 3>>, maps:get(source_route, F)),
    ?assertEqual(3, maps:get(retry_budget, F)).

call_rejects_wrong_call_id_size_test() ->
    ?assertError(function_clause,
                 macula_frame:call(spec_with(call_id, <<0:64>>))).

call_rejects_wrong_realm_size_test() ->
    ?assertError(function_clause,
                 macula_frame:call(spec_with(realm, <<0:128>>))).

call_sign_verify_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F  = macula_frame:sign(sample_call(macula_identity:public(Kp)), Kp),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, Decoded),
    ?assertMatch({ok, _},
                 macula_frame:verify(Decoded, macula_identity:public(Kp))).

%% -- RESULT -------------------------------------------------------

result_has_required_fields_test() ->
    Responded = crypto:strong_rand_bytes(32),
    F = macula_frame:result(#{
        call_id      => macula_record_uuid:v7(),
        payload      => #{ok => true},
        responded_by => Responded
    }),
    ?assertEqual(result,    macula_frame:frame_type(F)),
    ?assertEqual(Responded, maps:get(responded_by, F)),
    ?assertEqual(<<>>,      maps:get(source_route_reverse, F)).

result_rejects_wrong_responded_by_size_test() ->
    ?assertError(function_clause,
                 macula_frame:result(#{
                     call_id      => macula_record_uuid:v7(),
                     payload      => ok,
                     responded_by => <<0:64>>})).

result_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:result(#{
            call_id      => macula_record_uuid:v7(),
            payload      => <<"hello">>,
            responded_by => macula_identity:public(Kp)
        }), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

%% -- ERROR --------------------------------------------------------

call_error_carries_code_and_derived_name_test() ->
    F = macula_frame:call_error(#{
        call_id     => macula_record_uuid:v7(),
        code        => 16#01,
        reported_by => crypto:strong_rand_bytes(32)
    }),
    ?assertEqual(error,             macula_frame:frame_type(F)),
    ?assertEqual(16#01,             maps:get(code, F)),
    ?assertEqual(unknown_next_peer, maps:get(name, F)),
    ?assertEqual(undefined,         maps:get(detail, F)),
    ?assertEqual(undefined,         maps:get(offending_hop, F)).

call_error_with_offending_hop_and_detail_test() ->
    Hop = crypto:strong_rand_bytes(32),
    F = macula_frame:call_error(#{
        call_id       => macula_record_uuid:v7(),
        code          => 16#02,
        reported_by   => crypto:strong_rand_bytes(32),
        detail        => <<"backend down">>,
        offending_hop => Hop,
        source_route_partial => <<9, 8, 7>>
    }),
    ?assertEqual(temporary_relay_failure, maps:get(name, F)),
    ?assertEqual(<<"backend down">>,      maps:get(detail, F)),
    ?assertEqual(Hop,                     maps:get(offending_hop, F)),
    ?assertEqual(<<9, 8, 7>>,             maps:get(source_route_partial, F)).

call_error_rejects_unknown_code_test() ->
    %% macula_bolt4:name/1 raises on unknown code; ensure that
    %% propagates out of the constructor.
    ?assertError({bolt4_unknown, _, _},
                 macula_frame:call_error(#{
                     call_id => macula_record_uuid:v7(),
                     code    => 99,
                     reported_by => crypto:strong_rand_bytes(32)})).

call_error_rejects_wrong_offending_hop_size_test() ->
    ?assertError(function_clause,
                 macula_frame:call_error(#{
                     call_id     => macula_record_uuid:v7(),
                     code        => 16#01,
                     reported_by => crypto:strong_rand_bytes(32),
                     offending_hop => <<0:64>>})).

call_error_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:call_error(#{
            call_id     => macula_record_uuid:v7(),
            code        => 16#0E,
            reported_by => macula_identity:public(Kp)
        }), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D),
    ?assertEqual(signature_invalid, maps:get(name, D)),
    ?assertMatch({ok, _},
                 macula_frame:verify(D, macula_identity:public(Kp))).

%%------------------------------------------------------------------
%% CALL helpers
%%------------------------------------------------------------------

sample_call() ->
    sample_call(crypto:strong_rand_bytes(32)).

sample_call(Caller) ->
    macula_frame:call(#{
        call_id     => macula_record_uuid:v7(),
        procedure   => <<"realm/acme/weather/forecast/get_v1">>,
        realm       => crypto:strong_rand_bytes(32),
        payload     => #{city => <<"Brussels">>},
        deadline_ms => erlang:system_time(millisecond) + 5_000,
        caller      => Caller
    }).

%% Build a CALL spec replacing one field with a bad value, used to
%% test guard rejection.
spec_with(Field, BadValue) ->
    Base = #{
        call_id     => macula_record_uuid:v7(),
        procedure   => <<"foo">>,
        realm       => crypto:strong_rand_bytes(32),
        payload     => ok,
        deadline_ms => erlang:system_time(millisecond) + 1_000,
        caller      => crypto:strong_rand_bytes(32)
    },
    maps:put(Field, BadValue, Base).

%%------------------------------------------------------------------
%% HyParView frames — Part 3 §7.1
%%------------------------------------------------------------------

hyparview_join_shape_test() ->
    Realm = crypto:strong_rand_bytes(32),
    Member = crypto:strong_rand_bytes(32),
    F = macula_frame:hyparview_join(#{realm => Realm, new_member => Member}),
    ?assertEqual(hyparview_join, macula_frame:frame_type(F)),
    ?assertEqual(Realm,  maps:get(realm, F)),
    ?assertEqual(Member, maps:get(new_member, F)).

hyparview_join_rejects_short_realm_test() ->
    ?assertError(function_clause,
                 macula_frame:hyparview_join(#{realm => <<0:128>>,
                                               new_member => <<0:256>>})).

hyparview_forward_join_carries_ttl_arwl_prwl_test() ->
    F = macula_frame:hyparview_forward_join(#{
        realm      => crypto:strong_rand_bytes(32),
        new_member => crypto:strong_rand_bytes(32),
        ttl        => 6,
        arwl       => 6,
        prwl       => 3
    }),
    ?assertEqual(hyparview_forward_join, macula_frame:frame_type(F)),
    ?assertEqual(6, maps:get(ttl, F)),
    ?assertEqual(6, maps:get(arwl, F)),
    ?assertEqual(3, maps:get(prwl, F)).

hyparview_neighbor_high_priority_test() ->
    F = macula_frame:hyparview_neighbor(#{
        realm    => crypto:strong_rand_bytes(32),
        priority => high
    }),
    ?assertEqual(hyparview_neighbor, macula_frame:frame_type(F)),
    ?assertEqual(high, maps:get(priority, F)).

hyparview_neighbor_low_priority_test() ->
    F = macula_frame:hyparview_neighbor(#{
        realm    => crypto:strong_rand_bytes(32),
        priority => low
    }),
    ?assertEqual(low, maps:get(priority, F)).

hyparview_neighbor_rejects_unknown_priority_test() ->
    ?assertError(function_clause,
                 macula_frame:hyparview_neighbor(#{
                     realm    => crypto:strong_rand_bytes(32),
                     priority => medium})).

hyparview_disconnect_carries_realm_only_test() ->
    R = crypto:strong_rand_bytes(32),
    F = macula_frame:hyparview_disconnect(#{realm => R}),
    ?assertEqual(hyparview_disconnect, macula_frame:frame_type(F)),
    ?assertEqual(R, maps:get(realm, F)).

hyparview_shuffle_carries_origin_ttl_and_sample_test() ->
    Origin = crypto:strong_rand_bytes(32),
    Sample = [crypto:strong_rand_bytes(32) || _ <- lists:seq(1, 4)],
    F = macula_frame:hyparview_shuffle(#{
        realm => crypto:strong_rand_bytes(32),
        origin => Origin,
        ttl    => 4,
        peer_sample => Sample
    }),
    ?assertEqual(hyparview_shuffle, macula_frame:frame_type(F)),
    ?assertEqual(Origin, maps:get(origin, F)),
    ?assertEqual(Sample, maps:get(peer_sample, F)).

hyparview_shuffle_rejects_non_pubkey_in_sample_test() ->
    ?assertError(function_clause,
                 macula_frame:hyparview_shuffle(#{
                     realm => crypto:strong_rand_bytes(32),
                     origin => crypto:strong_rand_bytes(32),
                     ttl    => 4,
                     peer_sample => [<<"too short">>]})).

hyparview_shuffle_reply_carries_sample_test() ->
    Sample = [crypto:strong_rand_bytes(32) || _ <- lists:seq(1, 3)],
    F = macula_frame:hyparview_shuffle_reply(#{
        realm => crypto:strong_rand_bytes(32),
        peer_sample => Sample
    }),
    ?assertEqual(hyparview_shuffle_reply, macula_frame:frame_type(F)),
    ?assertEqual(Sample, maps:get(peer_sample, F)).

%% -- sign + wire roundtrip cover all 6 frame types --

hyparview_join_sign_verify_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:hyparview_join(#{
            realm => crypto:strong_rand_bytes(32),
            new_member => crypto:strong_rand_bytes(32)}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D),
    ?assertMatch({ok, _},
                 macula_frame:verify(D, macula_identity:public(Kp))).

hyparview_forward_join_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:hyparview_forward_join(#{
            realm => crypto:strong_rand_bytes(32),
            new_member => crypto:strong_rand_bytes(32),
            ttl => 6, arwl => 6, prwl => 3}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

hyparview_neighbor_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:hyparview_neighbor(#{
            realm => crypto:strong_rand_bytes(32),
            priority => low}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

hyparview_shuffle_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    Sample = [crypto:strong_rand_bytes(32) || _ <- lists:seq(1, 3)],
    F = macula_frame:sign(macula_frame:hyparview_shuffle(#{
            realm => crypto:strong_rand_bytes(32),
            origin => crypto:strong_rand_bytes(32),
            ttl => 4, peer_sample => Sample}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

hyparview_shuffle_reply_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:hyparview_shuffle_reply(#{
            realm => crypto:strong_rand_bytes(32),
            peer_sample => [crypto:strong_rand_bytes(32)]}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

hyparview_disconnect_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:hyparview_disconnect(#{
            realm => crypto:strong_rand_bytes(32)}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

%%------------------------------------------------------------------
%% Plumtree frames — Part 3 §7.2
%%------------------------------------------------------------------

plumtree_gossip_carries_msg_id_round_payload_test() ->
    MsgId = crypto:strong_rand_bytes(16),
    F = macula_frame:plumtree_gossip(#{
        realm   => crypto:strong_rand_bytes(32),
        msg_id  => MsgId,
        round   => 3,
        payload => #{event => <<"hello">>}
    }),
    ?assertEqual(plumtree_gossip, macula_frame:frame_type(F)),
    ?assertEqual(MsgId, maps:get(msg_id, F)),
    ?assertEqual(3, maps:get(round, F)),
    ?assertEqual(#{event => <<"hello">>}, maps:get(payload, F)).

plumtree_gossip_rejects_short_msg_id_test() ->
    ?assertError(function_clause,
                 macula_frame:plumtree_gossip(#{
                     realm   => crypto:strong_rand_bytes(32),
                     msg_id  => <<0:64>>,
                     round   => 0,
                     payload => ok})).

plumtree_ihave_round_trip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:plumtree_ihave(#{
            realm  => crypto:strong_rand_bytes(32),
            msg_id => crypto:strong_rand_bytes(16),
            round  => 1}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D),
    ?assertEqual(plumtree_ihave, macula_frame:frame_type(D)).

plumtree_graft_round_trip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:plumtree_graft(#{
            realm  => crypto:strong_rand_bytes(32),
            msg_id => crypto:strong_rand_bytes(16),
            round  => 2}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

plumtree_prune_carries_realm_only_test() ->
    R = crypto:strong_rand_bytes(32),
    F = macula_frame:plumtree_prune(#{realm => R}),
    ?assertEqual(plumtree_prune, macula_frame:frame_type(F)),
    ?assertEqual(R, maps:get(realm, F)).

plumtree_gossip_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:plumtree_gossip(#{
            realm   => crypto:strong_rand_bytes(32),
            msg_id  => crypto:strong_rand_bytes(16),
            round   => 0,
            payload => <<"data">>}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D),
    ?assertMatch({ok, _},
                 macula_frame:verify(D, macula_identity:public(Kp))).

%%------------------------------------------------------------------
%% PubSub frames — Part 6 §6
%%------------------------------------------------------------------

publish_carries_topic_and_payload_test() ->
    Pub = crypto:strong_rand_bytes(32),
    F = macula_frame:publish(#{
        topic           => <<"realm/acme/news/headlines">>,
        realm           => crypto:strong_rand_bytes(32),
        publisher       => Pub,
        seq             => 42,
        payload         => #{headline => <<"breaking">>},
        published_at_ms => erlang:system_time(millisecond)
    }),
    ?assertEqual(publish, macula_frame:frame_type(F)),
    ?assertEqual(Pub,     maps:get(publisher, F)),
    ?assertEqual(42,      maps:get(seq, F)),
    ?assertEqual(undefined, maps:get(ttl_ms, F)).

publish_with_ttl_test() ->
    F = macula_frame:publish(#{
        topic           => <<"t">>,
        realm           => crypto:strong_rand_bytes(32),
        publisher       => crypto:strong_rand_bytes(32),
        seq             => 0,
        payload         => ok,
        published_at_ms => 1,
        ttl_ms          => 60_000
    }),
    ?assertEqual(60_000, maps:get(ttl_ms, F)).

subscribe_carries_subscriber_and_options_test() ->
    Sub = crypto:strong_rand_bytes(32),
    F = macula_frame:subscribe(#{
        topic      => <<"t">>,
        realm      => crypto:strong_rand_bytes(32),
        subscriber => Sub,
        options    => #{qos => 1}
    }),
    ?assertEqual(subscribe, macula_frame:frame_type(F)),
    ?assertEqual(Sub,       maps:get(subscriber, F)),
    ?assertEqual(#{qos => 1}, maps:get(options, F)),
    ?assertEqual(undefined, maps:get(filter, F)).

unsubscribe_carries_subscriber_test() ->
    Sub = crypto:strong_rand_bytes(32),
    F = macula_frame:unsubscribe(#{
        topic      => <<"t">>,
        realm      => crypto:strong_rand_bytes(32),
        subscriber => Sub
    }),
    ?assertEqual(unsubscribe, macula_frame:frame_type(F)),
    ?assertEqual(Sub,         maps:get(subscriber, F)).

event_carries_delivery_channel_test() ->
    F = macula_frame:event(#{
        topic         => <<"t">>,
        realm         => crypto:strong_rand_bytes(32),
        publisher     => crypto:strong_rand_bytes(32),
        seq           => 7,
        payload       => <<"data">>,
        delivered_via => plumtree
    }),
    ?assertEqual(event, macula_frame:frame_type(F)),
    ?assertEqual(plumtree, maps:get(delivered_via, F)).

event_rejects_unknown_delivery_channel_test() ->
    ?assertError(function_clause,
                 macula_frame:event(#{
                     topic         => <<"t">>,
                     realm         => crypto:strong_rand_bytes(32),
                     publisher     => crypto:strong_rand_bytes(32),
                     seq           => 0,
                     payload       => ok,
                     delivered_via => carrier_pigeon})).

publish_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:publish(#{
            topic           => <<"t">>,
            realm           => crypto:strong_rand_bytes(32),
            publisher       => macula_identity:public(Kp),
            seq             => 3,
            payload         => <<"hi">>,
            published_at_ms => 1_000}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D),
    ?assertMatch({ok, _},
                 macula_frame:verify(D, macula_identity:public(Kp))).

subscribe_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:subscribe(#{
            topic      => <<"t">>,
            realm      => crypto:strong_rand_bytes(32),
            subscriber => macula_identity:public(Kp)}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

event_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:event(#{
            topic         => <<"t">>,
            realm         => crypto:strong_rand_bytes(32),
            publisher     => macula_identity:public(Kp),
            seq           => 0,
            payload       => ok,
            delivered_via => direct}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

%%------------------------------------------------------------------
%% Content transfer frames — Part 6 §9
%%------------------------------------------------------------------

mcid()     -> <<1, 1, (crypto:strong_rand_bytes(32))/binary>>.

want_carries_blocks_with_priority_test() ->
    M1 = mcid(), M2 = mcid(),
    F = macula_frame:want(#{blocks => [
        #{mcid => M1, priority => 200},
        #{mcid => M2}
    ]}),
    ?assertEqual(want, macula_frame:frame_type(F)),
    [B1, B2] = maps:get(blocks, F),
    ?assertEqual(M1,  maps:get(mcid, B1)),
    ?assertEqual(200, maps:get(priority, B1)),
    ?assertEqual(M2,  maps:get(mcid, B2)),
    ?assertEqual(128, maps:get(priority, B2)).

want_rejects_bad_priority_test() ->
    ?assertError(function_clause,
                 macula_frame:want(#{blocks => [
                     #{mcid => mcid(), priority => 999}
                 ]})).

want_rejects_short_mcid_test() ->
    ?assertError(function_clause,
                 macula_frame:want(#{blocks => [
                     #{mcid => <<"too short">>}
                 ]})).

have_carries_size_per_block_test() ->
    M = mcid(),
    F = macula_frame:have(#{blocks => [#{mcid => M, size => 4096}]}),
    ?assertEqual(have, macula_frame:frame_type(F)),
    [B] = maps:get(blocks, F),
    ?assertEqual(M,    maps:get(mcid, B)),
    ?assertEqual(4096, maps:get(size, B)).

have_rejects_negative_size_test() ->
    ?assertError(function_clause,
                 macula_frame:have(#{blocks => [
                     #{mcid => mcid(), size => -1}
                 ]})).

block_carries_payload_test() ->
    M = mcid(),
    F = macula_frame:block(#{mcid => M, payload => <<"data">>}),
    ?assertEqual(block, macula_frame:frame_type(F)),
    ?assertEqual(M,           maps:get(mcid, F)),
    ?assertEqual(<<"data">>,  maps:get(payload, F)).

block_rejects_non_binary_payload_test() ->
    ?assertError(function_clause,
                 macula_frame:block(#{mcid => mcid(), payload => 42})).

manifest_req_carries_mcid_test() ->
    M = mcid(),
    F = macula_frame:manifest_req(#{mcid => M}),
    ?assertEqual(manifest_req, macula_frame:frame_type(F)),
    ?assertEqual(M,            maps:get(mcid, F)).

manifest_res_carries_manifest_test() ->
    M = mcid(),
    Manifest = #{name => <<"hello.txt">>, size => 1024},
    F = macula_frame:manifest_res(#{mcid => M, manifest => Manifest}),
    ?assertEqual(manifest_res, macula_frame:frame_type(F)),
    ?assertEqual(Manifest,     maps:get(manifest, F)).

manifest_res_accepts_not_found_test() ->
    M = mcid(),
    F = macula_frame:manifest_res(#{mcid => M, manifest => not_found}),
    ?assertEqual(not_found, maps:get(manifest, F)).

cancel_carries_mcid_list_test() ->
    M1 = mcid(), M2 = mcid(),
    F = macula_frame:cancel(#{blocks => [M1, M2]}),
    ?assertEqual(cancel, macula_frame:frame_type(F)),
    ?assertEqual([M1, M2], maps:get(blocks, F)).

cancel_rejects_short_mcid_in_list_test() ->
    ?assertError(function_clause,
                 macula_frame:cancel(#{blocks => [mcid(), <<"short">>]})).

want_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:want(#{
            blocks => [#{mcid => mcid(), priority => 99}]}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D),
    ?assertMatch({ok, _},
                 macula_frame:verify(D, macula_identity:public(Kp))).

block_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:block(#{
            mcid => mcid(), payload => <<"chunk-bytes">>}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

manifest_res_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    F = macula_frame:sign(macula_frame:manifest_res(#{
            mcid     => mcid(),
            manifest => #{name => <<"x">>, size => 42}}), Kp),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).
