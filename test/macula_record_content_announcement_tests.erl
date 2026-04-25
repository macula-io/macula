-module(macula_record_content_announcement_tests).
-include_lib("eunit/include/eunit.hrl").

mcid()     -> <<1, 16#56, (crypto:strong_rand_bytes(32))/binary>>.
keypair()  -> macula_identity:generate().
station()  -> Kp = keypair(), {Kp, macula_identity:public(Kp)}.

constructor_3_arg_carries_announcer_mcid_endpoint_test() ->
    {_Kp, Pub} = station(),
    M = mcid(),
    R = macula_record:content_announcement(Pub, M, <<"quic://h:4">>),
    ?assertEqual(16#11, macula_record:type(R)),
    ?assertEqual(Pub,   macula_record:key(R)).

constructor_4_arg_with_metadata_test() ->
    {_Kp, Pub} = station(),
    M = mcid(),
    R = macula_record:content_announcement(Pub, M, <<"quic://h:4">>,
                                            #{name => <<"file.txt">>,
                                              size => 4096,
                                              chunk_count => 2}),
    Payload = macula_record:payload(R),
    ?assertEqual({text, <<"file.txt">>},
                 maps:get({text, <<"name">>}, Payload)),
    ?assertEqual(4096,
                 maps:get({text, <<"size">>}, Payload)),
    ?assertEqual(2,
                 maps:get({text, <<"chunk_count">>}, Payload)).

constructor_rejects_short_mcid_test() ->
    {_Kp, Pub} = station(),
    ?assertError(function_clause,
                 macula_record:content_announcement(
                   Pub, <<"too short">>, <<"quic://h:4">>)).

sign_verify_roundtrip_test() ->
    {Kp, Pub} = station(),
    R = macula_record:content_announcement(Pub, mcid(), <<"e">>),
    Signed = macula_record:sign(R, Kp),
    ?assertMatch({ok, _}, macula_record:verify(Signed)).

encode_decode_roundtrip_test() ->
    {Kp, Pub} = station(),
    R = macula_record:content_announcement(
          Pub, mcid(), <<"quic://h:4">>,
          #{name => <<"x">>, size => 1, chunk_count => 1}),
    Signed = macula_record:sign(R, Kp),
    Bin = macula_record:encode(Signed),
    {ok, Decoded} = macula_record:decode(Bin),
    ?assertEqual(macula_record:type(Signed),    macula_record:type(Decoded)),
    ?assertEqual(macula_record:key(Signed),     macula_record:key(Decoded)),
    ?assertEqual(macula_record:payload(Signed), macula_record:payload(Decoded)),
    ?assertMatch({ok, _}, macula_record:verify(Decoded)).
