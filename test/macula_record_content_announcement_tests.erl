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

%%------------------------------------------------------------------
%% storage_key/1 — was a `function_clause' crash for every
%% content_announcement (type 0x11 is below DOMAIN_TYPE_MIN, so it
%% never reached the generic domain-type clauses). Fixed to key by
%% SHA-256(MCID), matching content_key/1 and macula-station's
%% independent macula_content_dht:dht_key/1.
%%------------------------------------------------------------------

storage_key_matches_content_key_test() ->
    {Kp, Pub} = station(),
    M = mcid(),
    R = macula_record:sign(
          macula_record:content_announcement(Pub, M, <<"quic://h:4">>), Kp),
    ?assertEqual(macula_record:content_key(M), macula_record:storage_key(R)).

%% Two different hosts announcing the SAME MCID must land in the SAME
%% bag slot — storage_key keys on the MCID, not the announcer (the
%% envelope `key'), so `find_records' can see every provider.
storage_key_same_for_different_announcers_test() ->
    {KpA, PubA} = station(),
    {KpB, PubB} = station(),
    M = mcid(),
    RA = macula_record:sign(
           macula_record:content_announcement(PubA, M, <<"quic://a:4">>), KpA),
    RB = macula_record:sign(
           macula_record:content_announcement(PubB, M, <<"quic://b:4">>), KpB),
    ?assertEqual(macula_record:storage_key(RA), macula_record:storage_key(RB)).

content_key_rejects_wrong_size_test() ->
    ?assertError(function_clause, macula_record:content_key(<<"too short">>)).

%%------------------------------------------------------------------
%% read_content_announcement/1
%%------------------------------------------------------------------

read_content_announcement_canonical_test() ->
    {_Kp, Pub} = station(),
    M = mcid(),
    R = macula_record:content_announcement(
          Pub, M, <<"quic://h:4">>,
          #{name => <<"file.txt">>, size => 4096, chunk_count => 2}),
    ?assertEqual(#{announcer_node => Pub, mcid => M,
                   endpoint => <<"quic://h:4">>, name => <<"file.txt">>,
                   size => 4096, chunk_count => 2},
                 macula_record:read_content_announcement(R)).

%% Unset opts read back as `undefined', not a KeyError.
read_content_announcement_no_metadata_test() ->
    {_Kp, Pub} = station(),
    M = mcid(),
    R = macula_record:content_announcement(Pub, M, <<"quic://h:4">>),
    ?assertEqual(#{announcer_node => Pub, mcid => M,
                   endpoint => <<"quic://h:4">>, name => undefined,
                   size => undefined, chunk_count => undefined},
                 macula_record:read_content_announcement(R)).

%% The frame decoder atomises payload keys when a record is returned
%% inside an RPC result (the SDK `find_records/2' path) — same shape
%% that broke the procedure_advertisement reader before it was made
%% robust. Prove content_announcement's reader handles it too.
read_content_announcement_atom_keys_test() ->
    {_Kp, Pub} = station(),
    M = mcid(),
    Rec = #{type    => 16#11,
            payload => #{announcer_node => Pub,
                         mcid           => {text, M},
                         endpoint       => <<"quic://h:4">>,
                         name           => {text, <<"x">>},
                         size           => 10,
                         chunk_count    => 1}},
    ?assertEqual(#{announcer_node => Pub, mcid => M,
                   endpoint => <<"quic://h:4">>, name => <<"x">>,
                   size => 10, chunk_count => 1},
                 macula_record:read_content_announcement(Rec)).
