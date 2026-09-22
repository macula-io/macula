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
%% Wire codec — single-frame round-trip
%%------------------------------------------------------------------

encode_prepends_4_byte_length_test() ->
    F = build_connect(),
    Wire = macula_frame:encode(F),
    <<Len:32/big, Body/binary>> = Wire,
    ?assertEqual(byte_size(Body), Len).

encode_decode_roundtrip_connect_test() ->
    F = build_connect(),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, Decoded).

encode_decode_roundtrip_hello_test() ->
    F = build_hello(),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, Decoded).

%% A GOODBYE's reason is text on the wire and comes back as text, so the decoded frame is compared by its bytes.
encode_decode_roundtrip_goodbye_test() ->
    F = macula_frame:goodbye(draining, <<"bye">>),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(macula_frame:encode(F), macula_frame:encode(Decoded)).

%%------------------------------------------------------------------
%% Wire codec — partial / streaming
%%------------------------------------------------------------------

decode_returns_more_for_short_length_prefix_test() ->
    ?assertEqual({more, 4}, macula_frame:decode(<<>>)),
    ?assertEqual({more, 1}, macula_frame:decode(<<1, 2, 3>>)).

decode_returns_more_for_short_body_test() ->
    F = build_connect(),
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

parse_received_drains_multiple_frames_test() ->
    F1 = build_connect(),
    F2 = build_hello(),
    Buf = <<(macula_frame:encode(F1))/binary, (macula_frame:encode(F2))/binary>>,
    {ok, Frames, <<>>} = macula_frame:parse_received(Buf),
    ?assertEqual(2, length(Frames)),
    [D1, D2] = Frames,
    ?assertEqual(connect, macula_frame:frame_type(D1)),
    ?assertEqual(hello, macula_frame:frame_type(D2)).

parse_received_returns_unconsumed_tail_test() ->
    F1 = build_connect(),
    Wire1 = macula_frame:encode(F1),
    %% Append a partial second frame: first 6 bytes of length+body.
    F2 = build_hello(),
    Wire2Partial = binary:part(macula_frame:encode(F2), 0, 6),
    Buf = <<Wire1/binary, Wire2Partial/binary>>,
    {ok, Frames, Rest} = macula_frame:parse_received(Buf),
    ?assertEqual(1, length(Frames)),
    ?assertEqual(Wire2Partial, Rest).

%%------------------------------------------------------------------
%% Determinism
%%------------------------------------------------------------------

encode_is_deterministic_test() ->
    F = build_connect(),
    ?assertEqual(macula_frame:encode(F), macula_frame:encode(F)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

build_connect() ->
    NodeId = macula_test_identity:node_id(),
    macula_frame:connect(#{
        node_id          => NodeId,
        station_id       => NodeId,
        realms           => [crypto:strong_rand_bytes(32)],
        capabilities     => 16#FF,
        puzzle_evidence  => crypto:hash(sha256, NodeId)
    }).

build_hello() ->
    NodeId = macula_test_identity:node_id(),
    macula_frame:hello(#{
        node_id                 => NodeId,
        station_id              => NodeId,
        realms                  => [crypto:strong_rand_bytes(32)],
        capabilities            => 16#FF,
        accepted                => true,
        negotiated_capabilities => 16#0F
    }).

%%------------------------------------------------------------------
%% SWIM frames
%%------------------------------------------------------------------

swim_ping_has_expected_shape_test() ->
    Pub = macula_test_identity:node_id(),
    U   = observed_update(Pub, alive, Pub),
    F = macula_frame:swim_ping(#{round => 3, incarnation => 7, piggyback => [U]}),
    ?assertEqual(swim_ping, macula_frame:frame_type(F)),
    ?assertEqual(3, maps:get(round, F)),
    ?assertEqual(7, maps:get(incarnation, F)),
    ?assertMatch([#{by := Pub, state := alive}], maps:get(piggyback, F)).

swim_ack_carries_responder_test() ->
    Pub = macula_test_identity:node_id(),
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


swim_ping_wire_roundtrip_test() ->
    F = macula_frame:swim_ping(#{round => 42, incarnation => 1, piggyback => []}),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, Decoded).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

observed_update(Target, State, By) ->
    macula_frame:swim_update(#{
        target      => Target,
        state       => State,
        incarnation => 0,
        observed_at => erlang:system_time(millisecond),
        by          => By
    }).

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

ping_wire_roundtrip_test() ->
    F  = macula_frame:ping(#{nonce => crypto:strong_rand_bytes(16)}),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, Decoded).

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
    Req = macula_frame:find_node(#{key    => crypto:strong_rand_bytes(32),
                                    origin => crypto:strong_rand_bytes(32),
                                    depth  => 1}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(Req)),
    ?assertEqual(Req, D),
    Resp = macula_frame:nodes(#{key => crypto:strong_rand_bytes(32),
                                  nodes => [sample_station_ref()]}),
    {ok, D2, <<>>} = macula_frame:decode(macula_frame:encode(Resp)),
    ?assertEqual(Resp, D2).

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
    Req = macula_frame:find_value(#{key    => crypto:strong_rand_bytes(32),
                                      origin => crypto:strong_rand_bytes(32)}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(Req)),
    ?assertEqual(Req, D),
    Rsp = macula_frame:value(#{key => crypto:strong_rand_bytes(32),
                                 records => [sample_record()]}),
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
    F = macula_frame:store_ack(#{key => Key, signer => crypto:strong_rand_bytes(32),
                                 record_version => crypto:strong_rand_bytes(16), stored => true}),
    ?assertEqual(store_ack, macula_frame:frame_type(F)),
    ?assertEqual(true, maps:get(stored, F)),
    ?assertNot(maps:is_key(reason, F)).

store_ack_refusal_carries_no_reason_test() ->
    F = macula_frame:store_ack(#{key => crypto:strong_rand_bytes(32),
                                 signer => crypto:strong_rand_bytes(32),
                                 record_version => crypto:strong_rand_bytes(16), stored => false}),
    ?assertEqual(false, maps:get(stored, F)),
    ?assertNot(maps:is_key(reason, F)).

store_ack_refuses_a_reason_test() ->
    ?assertError(function_clause,
                 macula_frame:store_ack(#{key    => crypto:strong_rand_bytes(32),
                                          signer => crypto:strong_rand_bytes(32),
                                          record_version => crypto:strong_rand_bytes(16),
                                          stored => false,
                                          reason => quota})).

store_wire_roundtrip_test() ->
    F  = macula_frame:store(#{record => sample_record()}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

%% -- No REPLICATE / REPLICATE_ACK ---------------------------------

%% Replication runs as STOREs, so the codec has no REPLICATE or REPLICATE_ACK.
replicate_frames_are_not_frame_types_test() ->
    {module, macula_frame} = code:ensure_loaded(macula_frame),
    ?assertNot(erlang:function_exported(macula_frame, replicate, 1)),
    ?assertNot(erlang:function_exported(macula_frame, replicate_ack, 1)),
    [?assertEqual({error, bad_frame}, macula_frame:decode(macula_frame:encode(#{frame_type => Type})))
     || Type <- [replicate, replicate_ack]].

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

%% A signed record in its wire form, as a frame carries it.
sample_record() ->
    {ok, Id} = macula_node_keys:generate(identity, pq_pure),
    Node = macula_record:node_record(macula_node_keys:key_id(Id), [], 0),
    macula_record:encode(macula_record:sign(Node, Id)).

%% A realm's signed member endorsement in its wire form.
endorsement(RealmId, Member) ->
    {ok, Realm} = macula_node_keys:generate(realm, pq_pure),
    Unsigned = macula_record:realm_member_endorsement(RealmId, #{realm => RealmId, member_node => Member,
                                                                 roles => [<<"member">>]}),
    macula_record:encode(macula_record:sign(Unsigned, Realm)).

%%------------------------------------------------------------------
%% CALL / RESULT / ERROR frames — Part 6 §5
%%------------------------------------------------------------------

%% -- CALL, RESULT and ERROR: signed objects, in macula_frame_request_tests ------

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

%% -- the wire roundtrip covers all 6 frame types --

hyparview_join_wire_roundtrip_test() ->
    F = macula_frame:hyparview_join(#{
            realm => crypto:strong_rand_bytes(32),
            new_member => crypto:strong_rand_bytes(32)}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

hyparview_forward_join_wire_roundtrip_test() ->
    F = macula_frame:hyparview_forward_join(#{
            realm => crypto:strong_rand_bytes(32),
            new_member => crypto:strong_rand_bytes(32),
            ttl => 6, arwl => 6, prwl => 3}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

%% The `record' field (realm-admin-signed endorsement) must survive a
%% full wire round trip byte for byte, and verify at the receiver --
%% this is exactly the gap that shipped broken (the endorsement was
%% computed and then discarded, never attached to the frame at all).
hyparview_join_carries_endorsement_through_wire_roundtrip_test() ->
    RealmId = crypto:strong_rand_bytes(32),
    Member  = crypto:strong_rand_bytes(32),
    Endorsement = endorsement(RealmId, Member),
    F = macula_frame:hyparview_join(#{
            realm => RealmId, new_member => Member,
            record => Endorsement}),
    ?assertEqual(Endorsement, maps:get(record, F)),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D),
    ?assertEqual(Endorsement, maps:get(record, D)),
    ?assertMatch({ok, _}, macula_record:verify(maps:get(record, D), pq_pure)).

hyparview_join_without_endorsement_omits_record_field_test() ->
    F = macula_frame:hyparview_join(#{
            realm => crypto:strong_rand_bytes(32),
            new_member => crypto:strong_rand_bytes(32)}),
    ?assertNot(maps:is_key(record, F)).

%% NEIGHBOR can arrive unsolicited (shuffle-driven promotion) and
%% results in active-view admission the same as JOIN/FORWARD_JOIN --
%% it needs the same endorsement-carrying capability, not just a
%% frame shaped like an ack.
hyparview_neighbor_carries_endorsement_through_wire_roundtrip_test() ->
    RealmId = crypto:strong_rand_bytes(32),
    Sender  = crypto:strong_rand_bytes(32),
    Endorsement = endorsement(RealmId, Sender),
    F = macula_frame:hyparview_neighbor(#{
            realm => RealmId, priority => high, record => Endorsement}),
    ?assertEqual(Endorsement, maps:get(record, F)),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(Endorsement, maps:get(record, D)).

hyparview_neighbor_wire_roundtrip_test() ->
    F = macula_frame:hyparview_neighbor(#{
            realm => crypto:strong_rand_bytes(32),
            priority => low}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

hyparview_shuffle_wire_roundtrip_test() ->
    Sample = [crypto:strong_rand_bytes(32) || _ <- lists:seq(1, 3)],
    F = macula_frame:hyparview_shuffle(#{
            realm => crypto:strong_rand_bytes(32),
            origin => crypto:strong_rand_bytes(32),
            ttl => 4, peer_sample => Sample}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

hyparview_shuffle_reply_wire_roundtrip_test() ->
    F = macula_frame:hyparview_shuffle_reply(#{
            realm => crypto:strong_rand_bytes(32),
            peer_sample => [crypto:strong_rand_bytes(32)]}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

hyparview_disconnect_wire_roundtrip_test() ->
    F = macula_frame:hyparview_disconnect(#{
            realm => crypto:strong_rand_bytes(32)}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

%%------------------------------------------------------------------
%% Plumtree frames — Part 3 §7.2
%%------------------------------------------------------------------

plumtree_ihave_round_trip_test() ->
    F = macula_frame:plumtree_ihave(#{
            realm  => crypto:strong_rand_bytes(32),
            msg_id => crypto:strong_rand_bytes(48),
            round  => 1}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D),
    ?assertEqual(plumtree_ihave, macula_frame:frame_type(D)).

plumtree_graft_round_trip_test() ->
    F = macula_frame:plumtree_graft(#{
            realm  => crypto:strong_rand_bytes(32),
            msg_id => crypto:strong_rand_bytes(48),
            round  => 2}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

plumtree_prune_carries_realm_only_test() ->
    R = crypto:strong_rand_bytes(32),
    F = macula_frame:plumtree_prune(#{realm => R}),
    ?assertEqual(plumtree_prune, macula_frame:frame_type(F)),
    ?assertEqual(R, maps:get(realm, F)).

%% GOSSIP carries a signed publication: macula_frame_publication_tests.

%%------------------------------------------------------------------
%% Overlay relay envelope — Phase 3.5
%%------------------------------------------------------------------

overlay_relay_carries_peer_and_payload_test() ->
    Peer = crypto:strong_rand_bytes(32),
    Inner = macula_frame:encode(macula_frame:plumtree_prune(#{
                realm => crypto:strong_rand_bytes(32)})),
    F = macula_frame:overlay_relay(#{peer => Peer, payload => Inner}),
    ?assertEqual(overlay_relay, macula_frame:frame_type(F)),
    ?assertEqual(Peer, maps:get(peer, F)),
    ?assertEqual(Inner, maps:get(payload, F)).

overlay_relay_wire_roundtrip_preserves_wrapped_frame_bytes_test() ->
    Peer = crypto:strong_rand_bytes(32),
    Inner = macula_frame:encode(macula_frame:hyparview_disconnect(
                #{realm => crypto:strong_rand_bytes(32)})),
    F = macula_frame:overlay_relay(
            #{peer => Peer, payload => Inner}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D),
    %% The wrapped bytes decode back to the exact original inner frame.
    {ok, InnerDecoded, <<>>} = macula_frame:decode(maps:get(payload, D)),
    {ok, InnerExpected, <<>>} = macula_frame:decode(Inner),
    ?assertEqual(InnerExpected, InnerDecoded),
    ?assertEqual(hyparview_disconnect, macula_frame:frame_type(InnerDecoded)).

%%------------------------------------------------------------------
%% PubSub frames — Part 6 §6
%%------------------------------------------------------------------

%% PUBLISH and EVENT carry a signed publication: macula_frame_publication_tests.

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
    ?assertNot(maps:is_key(filter, F)).

unsubscribe_carries_subscriber_test() ->
    Sub = crypto:strong_rand_bytes(32),
    F = macula_frame:unsubscribe(#{
        topic      => <<"t">>,
        realm      => crypto:strong_rand_bytes(32),
        subscriber => Sub
    }),
    ?assertEqual(unsubscribe, macula_frame:frame_type(F)),
    ?assertEqual(Sub,         maps:get(subscriber, F)).

%% An EVENT names how its publication was delivered, from a closed set.
event_refuses_an_unknown_or_dht_delivery_channel_test() ->
    Publication = #{key => <<1>>, tbs => <<2>>, signature => <<3>>},
    [?assertError(function_clause, macula_frame:event(#{publication => Publication, delivered_via => Via}))
     || Via <- [carrier_pigeon, dht]].

subscribe_wire_roundtrip_test() ->
    F = macula_frame:subscribe(#{
            topic      => <<"t">>,
            realm      => crypto:strong_rand_bytes(32),
            subscriber => macula_test_identity:node_id()}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

%%------------------------------------------------------------------
%% ADVERTISE and UNADVERTISE carry the advertisement record and its tombstone: macula_frame_control_fields_tests
%%------------------------------------------------------------------

%%------------------------------------------------------------------
%% Content transfer frames — Part 6 §9
%%------------------------------------------------------------------

mcid()     -> <<2, 16#55, (crypto:strong_rand_bytes(48))/binary>>.

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

manifest_res_refuses_not_found_test() ->
    ?assertError(function_clause, macula_frame:manifest_res(#{mcid => mcid(), manifest => not_found})).

cancel_carries_mcid_list_test() ->
    M1 = mcid(), M2 = mcid(),
    F = macula_frame:cancel(#{blocks => [M1, M2]}),
    ?assertEqual(cancel, macula_frame:frame_type(F)),
    ?assertEqual([M1, M2], maps:get(blocks, F)).

cancel_rejects_short_mcid_in_list_test() ->
    ?assertError(function_clause,
                 macula_frame:cancel(#{blocks => [mcid(), <<"short">>]})).

want_wire_roundtrip_test() ->
    F = macula_frame:want(#{
            blocks => [#{mcid => mcid(), priority => 99}]}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

block_wire_roundtrip_test() ->
    F = macula_frame:block(#{
            mcid => mcid(), payload => <<"chunk-bytes">>}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F, D).

manifest_res_wire_roundtrip_test() ->
    F = macula_frame:manifest_res(#{
            mcid     => mcid(),
            manifest => #{name => <<"x">>, size => 42}}),
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(F#{manifest := #{{text, <<"name">>} => <<"x">>, {text, <<"size">>} => 42}}, D).

%%------------------------------------------------------------------
%% Streaming RPC frames: STREAM_OPEN in macula_frame_request_tests, and STREAM_DATA, STREAM_END,
%% STREAM_ERROR and STREAM_REPLY in macula_frame_stream_tests
%%------------------------------------------------------------------

%%------------------------------------------------------------------
%% check_payload/1 — wire admissibility
%%
%% The agreement test at the bottom is the one that matters. Everything
%% above it documents intent; that one stops the checker and the encoder
%% from drifting apart, which is the only way this guard can rot.
%%------------------------------------------------------------------

check_payload_accepts_scalars_test() ->
    ?assertEqual(ok, macula_frame:check_payload(0)),
    ?assertEqual(ok, macula_frame:check_payload(42)),
    ?assertEqual(ok, macula_frame:check_payload(-1)),
    ?assertEqual(ok, macula_frame:check_payload(-42)),
    ?assertEqual(ok, macula_frame:check_payload(<<"bytes">>)),
    ?assertEqual(ok, macula_frame:check_payload({text, <<"utf8">>})),
    ?assertEqual(ok, macula_frame:check_payload(undefined)),
    ?assertEqual(ok, macula_frame:check_payload(an_atom)),
    ?assertEqual(ok, macula_frame:check_payload(true)),
    ?assertEqual(ok, macula_frame:check_payload([])),
    ?assertEqual(ok, macula_frame:check_payload(#{})).

check_payload_accepts_nested_structures_test() ->
    Payload = #{<<"type">> => observation,
                <<"rows">> => [#{<<"seq">> => 1}, #{<<"seq">> => -2}],
                42         => [<<"int keys are legal">>]},
    ?assertEqual(ok, macula_frame:check_payload(Payload)).

%% Floats are CARRIED now. This test used to assert the opposite, which was
%% correct only while the canonical encoder had no float clause.
check_payload_accepts_float_test() ->
    ?assertEqual(ok, macula_frame:check_payload(52.34)),
    ?assertEqual(ok, macula_frame:check_payload(#{<<"v">> => -1234.5})).

%% The whole point of the error shape: say WHERE, so a service that
%% publishes one bad field in a large map is told which field.
check_payload_reports_path_to_offender_test() ->
    Payload = #{<<"battery">> => #{<<"voltage">> => {volts, 52}}},
    ?assertEqual({error, {unsupported_payload_type, tuple,
                          [<<"battery">>, <<"voltage">>]}},
                 macula_frame:check_payload(Payload)),
    ?assertEqual({error, {unsupported_payload_type, tuple, [1, 0]}},
                 macula_frame:check_payload([#{}, [{a, b}]])).

check_payload_rejects_tuple_test() ->
    ?assertMatch({error, {unsupported_payload_type, tuple, []}},
                 macula_frame:check_payload({a, b})),
    %% {text, _} is the codec's own marker and stays legal; a lookalike
    %% carrying a non-binary is not.
    ?assertMatch({error, {unsupported_payload_type, tuple, []}},
                 macula_frame:check_payload({text, not_a_binary})).

check_payload_rejects_out_of_range_integer_test() ->
    TooBig = 16#FFFFFFFFFFFFFFFF + 1,
    ?assertMatch({error, {unsupported_payload_type, integer_out_of_range, []}},
                 macula_frame:check_payload(TooBig)),
    ?assertMatch({error, {unsupported_payload_type, integer_out_of_range, []}},
                 macula_frame:check_payload(-TooBig - 1)).

check_payload_rejects_improper_list_test() ->
    ?assertMatch({error, {unsupported_payload_type, improper_list, []}},
                 macula_frame:check_payload([1 | 2])).

check_payload_rejects_unsupported_terms_test() ->
    ?assertMatch({error, {unsupported_payload_type, unsupported_term, []}},
                 macula_frame:check_payload(self())),
    ?assertMatch({error, {unsupported_payload_type, unsupported_term, []}},
                 macula_frame:check_payload(make_ref())).

check_payload_rejects_bad_map_key_test() ->
    ?assertMatch({error, {unsupported_payload_type, unsupported_map_key, []}},
                 macula_frame:check_payload(#{1.5 => ok})),
    ?assertMatch({error, {unsupported_payload_type, unsupported_map_key, []}},
                 macula_frame:check_payload(#{#{} => ok})).

%% AGREEMENT, in the two directions that matter. Note these are NOT
%% "check_payload says ok exactly when encode/1 does not raise". The
%% first draft of this test asserted that and went red on 52.34,
%% correctly: the encoder does not raise on a float, it silently
%% rewrites it as text. "Did not crash" is precisely the wrong
%% definition of agreement for a bug about silent corruption.
sample_terms() ->
    [0, 42, -1, -42, 16#FFFFFFFFFFFFFFFF, 16#FFFFFFFFFFFFFFFF + 1,
     -16#10000000000000000, -16#10000000000000001,
     <<>>, <<"bytes">>, {text, <<"utf8">>}, {text, not_a_binary},
     undefined, an_atom, true, false,
     [], [1, 2, 3], [1 | 2], #{}, #{<<"k">> => 1}, #{1.5 => ok},
     #{<<"nested">> => #{<<"deep">> => [1, <<"two">>]}},
     52.34, {a, b}, self(), make_ref(),
     #{<<"battery">> => #{<<"voltage">> => 52.34}},
     %% Edge cases from walking to_wire/1 clause by clause. A bitstring
     %% is NOT a binary, so is_binary/1 rejects it and the encoder has no
     %% clause; an Erlang string is a list of integers and legitimately
     %% becomes an array; nesting hides an improper tail one level down;
     %% and {text, _} with a non-binary slips past wire_key/1's missing
     %% guard, so only the checker stops it.
     <<1:3>>, "a string", [[1 | 2]], #{<<"k">> => [1 | 2]},
     {text, <<>>}, #{{text, not_a_binary} => ok}, #{[] => ok}].

%% SOUNDNESS, the safety-critical direction: anything the checker
%% green-lights must encode. A false ok here is a killed peering
%% connection in production.
check_payload_never_admits_an_unencodable_term_test() ->
    Admitted = [T || T <- sample_terms(), macula_frame:check_payload(T) =:= ok],
    [?assert(survives(T)) || T <- Admitted],
    ?assert(length(Admitted) > 0).

%% COMPLETENESS: anything the checker rejects must genuinely be unable to
%% survive. There is no longer a policy exemption, because the float
%% restriction it existed for is gone.
check_payload_rejects_only_what_cannot_survive_test() ->
    Rejected = [T || T <- sample_terms(), macula_frame:check_payload(T) =/= ok],
    [?assert(not survives(T)) || T <- Rejected],
    ?assert(length(Rejected) > 0).

%% This test used to pin the corruption: a float went in and
%% {text, <<"52.34">>} came out. It was written to go red the moment that
%% stopped being true, and it did. Now it pins the fix.
float_payload_round_trips_exactly_test() ->
    [begin
         ?assertEqual({ok, F}, decode_request_payload(F))
     end || F <- [52.34, -1234.5, 0.0, 1.0e300, 1.0e-300, 3.141592653589793]].


%% A payload travels in the tbs of a signed CALL. The request builder consults check_payload/1, so a payload the
%% checker rejects cannot be sent at all; the generated soundness test below guards the unsafe direction.
decode_request_payload(Term) ->
    Frame = macula_frame:call(#{request_id => <<1:128>>, realm => <<1:256>>, procedure => <<"p">>, target => <<2:256>>,
                                deadline => 1, payload => Term}, request_key()),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    {ok, #{payload := Payload}} = macula_frame:verify_request(Decoded, pq_pure),
    {ok, Payload}.

%% A payload whose keys collide as text, carried past the builder's check in a CALL tbs signed by hand.
colliding_payload(Colliding) ->
    Key = request_key(),
    Wire = maps:from_list([{{text, atom_or_binary(K)}, V} || K := V <- Colliding]),
    Tbs = #{{text, <<"frame_type">>} => {text, <<"call">>}, {text, <<"caller">>} => macula_node_keys:key_id(Key),
            {text, <<"request_id">>} => <<1:128>>, {text, <<"realm">>} => <<1:256>>,
            {text, <<"procedure">>} => {text, <<"p">>}, {text, <<"target">>} => <<2:256>>,
            {text, <<"deadline">>} => 1, {text, <<"payload">>} => Wire},
    Frame = #{version => macula_frame:version(macula_frame:ping(#{nonce => <<0:128>>})), frame_type => call,
              request => macula_signed_object:sign(<<"MACULA-PQ-REQUEST-V1">>, Tbs, Key)},
    {ok, #{payload := Payload}} = macula_frame:verify_request(Frame, pq_pure),
    Payload.

atom_or_binary(A) when is_atom(A) -> atom_to_binary(A);
atom_or_binary(B) when is_binary(B) -> B.

%% One caller key for every payload round trip in this module, made once.
request_key() ->
    request_key(persistent_term:get({?MODULE, request_key}, undefined)).

request_key(undefined) ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    persistent_term:put({?MODULE, request_key}, Key),
    Key;
request_key(Key) ->
    Key.

%% SURVIVES = encodes, decodes, and comes back with its structure
%% intact. NOT "did not raise": that predicate is blind to silent
%% corruption, which is the entire bug class here, and it let two false
%% oks (oversized payloads, colliding wire keys) sit in a green suite.
%%
%% Leaf identity is deliberately NOT asserted, because the wire aliases
%% leaves by design: an atom, and a `{text, Binary}' of the same name,
%% are the same bytes, and which one you get back depends on whether the
%% receiving node knows the atom. Node count is invariant under that
%% aliasing and still changes the moment a map pair is swallowed.
survives(Term) ->
    try decode_request_payload(Term) of
        {ok, Payload} -> intact(Term, Payload);
        _Other        -> false
    catch
        _:_ -> false
    end.

%% Structure AND leaves. Node count alone was not enough: it cannot see
%% 52.34 arrive as {text, <<"52.34">>}, so survives(52.34) was true and
%% the generated suite would never have caught the float rewrite that
%% started all of this. Leaves are canonicalised first, because the wire
%% aliases an atom and a {text, Binary} of the same name deliberately,
%% and that aliasing is not corruption.
intact(Sent, Got) ->
    node_count(Sent) =:= node_count(Got) andalso leaves(Sent) =:= leaves(Got).

leaves(Term) -> lists:sort(collect(Term, [])).

collect(M, Acc) when is_map(M) ->
    maps:fold(fun(K, V, A) -> collect(K, collect(V, A)) end, Acc, M);
collect([], Acc)      -> Acc;
collect([H | T], Acc) -> collect(H, collect(T, Acc));
collect(Leaf, Acc)    -> [canon(Leaf) | Acc].

canon(A) when is_atom(A)           -> atom_to_binary(A, utf8);
canon({text, B}) when is_binary(B) -> B;
canon(Other)                       -> Other.

node_count(M) when is_map(M) ->
    maps:fold(fun(K, V, Acc) -> Acc + node_count(K) + node_count(V) end, 1, M);
node_count([])      -> 1;
node_count([H | T]) -> node_count(H) + node_count(T);
node_count(_Leaf)   -> 1.

%%------------------------------------------------------------------
%% Generated terms. A curated list cannot stop drift — it only contains
%% the mistakes already known. Both false oks Fable found were absent
%% from the curated list and are reachable by this generator.
%%------------------------------------------------------------------

check_payload_soundness_holds_on_generated_terms_test_() ->
    {timeout, 120,
     fun() ->
         _ = rand:seed(exsss, {20260726, 1, 1}),
         [?assert(sound(gen_term(3))) || _ <- lists:seq(1, 3000)],
         ok
     end}.

%% The only invariant that must never break: admitted implies survives.
sound(Term) ->
    macula_frame:check_payload(Term) =/= ok orelse survives(Term).

gen_term(0) -> gen_leaf();
gen_term(D) -> gen_node(rand:uniform(6), D).

gen_node(1, D) -> [gen_term(D - 1) || _ <- lists:seq(1, rand:uniform(4) - 1)];
gen_node(2, D) -> maps:from_list([{gen_key(), gen_term(D - 1)}
                                  || _ <- lists:seq(1, rand:uniform(4))]);
gen_node(_N, _D) -> gen_leaf().

%% Drawn from a deliberately tiny pool so that `foo', `<<"foo">>' and
%% `{text, <<"foo">>}' collide often — that is the point.
gen_key() ->
    lists:nth(rand:uniform(6),
              [foo, <<"foo">>, {text, <<"foo">>}, bar, <<"bar">>, 1]).

gen_leaf() ->
    lists:nth(rand:uniform(12),
              [0, 42, -7, 16#FFFFFFFFFFFFFFFF + 1, <<>>, <<"bytes">>,
               {text, <<"t">>}, an_atom, undefined, 52.34, {a, b}, <<1:3>>]).

%%------------------------------------------------------------------
%% Regressions for the two false oks. Both passed a green suite before
%% the predicate was fixed: the checker said ok and the connection died
%% (oversize) or the data was silently swallowed (key collision).
%%------------------------------------------------------------------

check_payload_rejects_oversized_payload_test() ->
    TooBig = binary:copy(<<"x">>, 16#FFFFFF + 1),
    ?assertMatch({error, {unsupported_payload_type, payload_too_large, []}},
                 macula_frame:check_payload(TooBig)).

%% #{foo => 1, <<"foo">> => 2} both project onto {text, <<"foo">>}, so
%% the frame ships one pair and the loser vanishes by sort order.
check_payload_rejects_colliding_wire_keys_test() ->
    ?assertMatch({error, {unsupported_payload_type, duplicate_wire_key, []}},
                 macula_frame:check_payload(#{foo => 1, <<"foo">> => 2})),
    ?assertMatch({error, {unsupported_payload_type, duplicate_wire_key, []}},
                 macula_frame:check_payload(#{bar => 1, {text, <<"bar">>} => 2})),
    ?assertEqual(ok, macula_frame:check_payload(#{foo => 1, bar => 2})).

collision_actually_loses_data_test() ->
    Colliding = #{foo => 1, <<"foo">> => 2},
    ?assertEqual(2, maps:size(Colliding)),
    ?assertEqual(1, maps:size(colliding_payload(Colliding))).

%% check_frame/1 guards the whole frame at the send_frame seam. Records travel as
%% their wire bytes, so every field is judged the same way.
check_frame_judges_every_field_test() ->
    ?assertEqual(ok, macula_frame:check_frame(#{frame_type => store, record => <<"record bytes">>})),
    ?assertMatch({error, {unsupported_payload_type, tuple, [payload]}},
                 macula_frame:check_frame(
                   #{frame_type => publish, payload => {a, b}})).

explain_names_the_offender_and_its_place_test() ->
    Sentence = iolist_to_binary(
                 macula_frame:explain(
                   {unsupported_payload_type, tuple,
                    [<<"battery">>, <<"voltage">>]})),
    ?assert(binary:match(Sentence, <<"battery.voltage">>) =/= nomatch),
    ?assert(binary:match(Sentence, <<"tuple">>) =/= nomatch),
    Collision = iolist_to_binary(
                  macula_frame:explain(
                    {unsupported_payload_type, duplicate_wire_key, []})),
    ?assert(binary:match(Collision, <<"same wire key">>) =/= nomatch).

%% Regression for Fable's finding that the old predicate was blind to
%% leaf rewrites: the float bug itself must be detectable by the test
%% machinery, not only fenced off by the policy clause in check_value/2.
survives_detects_the_float_rewrite_test() ->
    ?assert(survives(52.34)),
    ?assert(survives(#{<<"v">> => 52.34})),
    %% ...while the wire's deliberate atom/text aliasing is NOT flagged.
    ?assert(survives(an_atom)),
    ?assert(survives(#{<<"k">> => [1, <<"two">>, undefined]})).

%% The post-quantum format has only tag 2, SHA-384 (D24): a BLAKE3 id, tag 1, is refused in content frames.
content_frames_reject_a_blake3_mcid_test_() ->
    Blake3 = <<1, 16#55, 0:256>>,
    [?_assertError(function_clause, macula_frame:block(#{mcid => Blake3, payload => <<"x">>})),
     ?_assertError(function_clause, macula_frame:manifest_req(#{mcid => Blake3}))].
