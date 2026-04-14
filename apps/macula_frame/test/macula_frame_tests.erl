%% EUnit tests for macula_frame.
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
