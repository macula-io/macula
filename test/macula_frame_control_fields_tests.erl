%% EUnit tests for the typed fields of step 2f (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md: Frame signatures,
%% Advertisements, the decoding rule). ADVERTISE carries the provider's signed procedure advertisement record and
%% UNADVERTISE a tombstone for it, both as record bytes. Every field that named an Ed25519 key names a 32-byte key id,
%% and every protocol integer in a control frame, and so in a neighbour tbs, is an unsigned integer below 2^53.
-module(macula_frame_control_fields_tests).

-include_lib("eunit/include/eunit.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).
-define(REALM, <<1:256>>).
-define(TOO_BIG, 1 bsl 53).
-define(NEIGHBOUR_LABEL, <<"MACULA-PQ-NEIGHBOUR-V1">>).

%%------------------------------------------------------------------
%% ADVERTISE and UNADVERTISE
%%------------------------------------------------------------------

advertise_carries_the_signed_advertisement_record_test() ->
    {Provider, Bytes} = advertisement(),
    Frame = wire(macula_frame:advertise(#{advertisement => Bytes})),
    ?assertEqual(Bytes, maps:get(advertisement, Frame)),
    {ok, Record} = macula_record:verify(maps:get(advertisement, Frame), pq_pure),
    ?assertEqual(macula_record:type_procedure_advertisement(), macula_record:type(Record)),
    ?assertEqual(macula_node_keys:key_id(Provider), macula_record:key_id(Record)).

unadvertise_carries_a_tombstone_for_the_advertisement_test() ->
    {Provider, Bytes} = advertisement(),
    {ok, Advertisement} = macula_record:verify(Bytes, pq_pure),
    Withdrawal = macula_record:encode(macula_record:sign(macula_record:tombstone(Advertisement, shutdown), Provider)),
    Frame = wire(macula_frame:unadvertise(#{withdrawal => Withdrawal})),
    ?assertEqual(Withdrawal, maps:get(withdrawal, Frame)),
    {ok, Tombstone} = macula_record:verify(maps:get(withdrawal, Frame), pq_pure),
    ?assertMatch(#{withdrawn_type := 16#06}, macula_record:read_tombstone(Tombstone)).

the_old_advertise_fields_are_refused_test() ->
    {_Provider, Bytes} = advertisement(),
    Frame = macula_frame:advertise(#{advertisement => Bytes}),
    [?assertEqual({error, bad_frame}, decode(Frame#{Field => Value}))
     || {Field, Value} <- [{procedure, <<"p">>}, {advertiser, <<0:256>>}, {options, #{}}]],
    ?assertEqual({error, bad_frame}, decode(Frame#{advertisement := {text, <<"not bytes">>}})),
    ?assertError(function_clause,
                 macula_frame:advertise(#{realm => ?REALM, procedure => <<"p">>, advertiser => <<0:256>>})),
    ?assertError(function_clause, macula_frame:unadvertise(#{withdrawal => not_bytes})).

%%------------------------------------------------------------------
%% Key ids
%%------------------------------------------------------------------

key_id_fields_are_32_bytes_test_() ->
    [{atom_to_list(Name), fun() ->
         ?assertMatch({ok, _, <<>>}, decode(Frame)),
         ?assertEqual({error, bad_frame}, decode(Short))
     end} || {Name, Frame, Short} <- key_id_cases()].

key_id_cases() ->
    Short = <<0:248>>,
    Update = swim_update(),
    Ping = macula_frame:swim_ping(#{round => 1, incarnation => 0, piggyback => [Update]}),
    Ack = macula_frame:swim_ack(#{round => 1, responder => id(3), incarnation => 0}),
    Suspect = macula_frame:swim_suspect(#{target => id(1), target_incarnation => 0, suspected_by => id(2), ttl => 3}),
    Confirm = macula_frame:swim_confirm(#{target => id(1), target_incarnation => 0, suspected_by => id(2), ttl => 3}),
    FindNode = macula_frame:find_node(#{key => id(4), origin => id(5), depth => 1}),
    FindValue = macula_frame:find_value(#{key => id(4), origin => id(5)}),
    Ref = macula_frame:station_ref(#{node_id => id(6), station_id => id(7), tier => 1, country => <<"BE">>,
                                     last_seen_at => 1}),
    Nodes = macula_frame:nodes(#{key => id(4), nodes => [Ref]}),
    Join = macula_frame:hyparview_join(#{realm => ?REALM, new_member => id(8)}),
    Forward = macula_frame:hyparview_forward_join(#{realm => ?REALM, new_member => id(8), ttl => 2, arwl => 3,
                                                    prwl => 1}),
    Shuffle = macula_frame:hyparview_shuffle(#{realm => ?REALM, origin => id(9), ttl => 2, peer_sample => [id(10)]}),
    ShuffleReply = macula_frame:hyparview_shuffle_reply(#{realm => ?REALM, peer_sample => [id(10)]}),
    Relay = macula_frame:overlay_relay(#{peer => id(11), payload => <<"inner">>}),
    Subscribe = macula_frame:subscribe(#{topic => <<"t">>, realm => ?REALM, subscriber => id(12)}),
    Unsubscribe = macula_frame:unsubscribe(#{topic => <<"t">>, realm => ?REALM, subscriber => id(12)}),
    [{swim_piggyback_target, Ping, Ping#{piggyback := [Update#{target := Short}]}},
     {swim_piggyback_by, Ping, Ping#{piggyback := [Update#{by := Short}]}},
     {swim_ack_responder, Ack, Ack#{responder := Short}},
     {swim_suspect_target, Suspect, Suspect#{target := Short}},
     {swim_suspect_suspected_by, Suspect, Suspect#{suspected_by := Short}},
     {swim_confirm_target, Confirm, Confirm#{target := Short}},
     {find_node_origin, FindNode, FindNode#{origin := Short}},
     {find_value_origin, FindValue, FindValue#{origin := Short}},
     {nodes_node_id, Nodes, Nodes#{nodes := [Ref#{node_id := Short}]}},
     {nodes_station_id, Nodes, Nodes#{nodes := [Ref#{station_id := Short}]}},
     {hyparview_join_new_member, Join, Join#{new_member := Short}},
     {hyparview_forward_join_new_member, Forward, Forward#{new_member := Short}},
     {hyparview_shuffle_origin, Shuffle, Shuffle#{origin := Short}},
     {hyparview_shuffle_peer_sample, Shuffle, Shuffle#{peer_sample := [id(10), Short]}},
     {hyparview_shuffle_reply_peer_sample, ShuffleReply, ShuffleReply#{peer_sample := [Short]}},
     {overlay_relay_peer, Relay, Relay#{peer := Short}},
     {subscribe_subscriber, Subscribe, Subscribe#{subscriber := Short}},
     {unsubscribe_subscriber, Unsubscribe, Unsubscribe#{subscriber := Short}}].

%%------------------------------------------------------------------
%% Protocol integers
%%------------------------------------------------------------------

protocol_integers_are_unsigned_below_2_pow_53_test_() ->
    [{atom_to_list(Name), fun() ->
         ?assertMatch({ok, _, <<>>}, decode(Frame)),
         [?assertEqual({error, bad_frame}, decode(Place(Bad))) || Bad <- [?TOO_BIG, -1]]
     end} || {Name, Frame, Place} <- integer_cases()].

integer_cases() ->
    Update = swim_update(),
    Ping = macula_frame:swim_ping(#{round => 1, incarnation => 0, piggyback => [Update]}),
    Ack = macula_frame:swim_ack(#{round => 1, responder => id(3), incarnation => 0}),
    Suspect = macula_frame:swim_suspect(#{target => id(1), target_incarnation => 0, suspected_by => id(2), ttl => 3}),
    FindNode = macula_frame:find_node(#{key => id(4), origin => id(5), depth => 1}),
    Ref = macula_frame:station_ref(#{node_id => id(6), station_id => id(7), tier => 1, country => <<"BE">>,
                                     last_seen_at => 1, asn => 5}),
    Nodes = macula_frame:nodes(#{key => id(4), nodes => [Ref]}),
    Forward = macula_frame:hyparview_forward_join(#{realm => ?REALM, new_member => id(8), ttl => 2, arwl => 3,
                                                    prwl => 1}),
    Shuffle = macula_frame:hyparview_shuffle(#{realm => ?REALM, origin => id(9), ttl => 2, peer_sample => []}),
    Ihave = macula_frame:plumtree_ihave(#{realm => ?REALM, msg_id => <<0:384>>, round => 1}),
    Graft = macula_frame:plumtree_graft(#{realm => ?REALM, msg_id => <<0:384>>, round => 1}),
    PingDht = macula_frame:ping(#{nonce => <<0:128>>}),
    [{sent_at_ms, PingDht, fun(Bad) -> PingDht#{sent_at_ms := Bad} end},
     {capabilities, PingDht, fun(Bad) -> PingDht#{capabilities := Bad} end},
     {swim_ping_round, Ping, fun(Bad) -> Ping#{round := Bad} end},
     {swim_ping_incarnation, Ping, fun(Bad) -> Ping#{incarnation := Bad} end},
     {swim_piggyback_incarnation, Ping, fun(Bad) -> Ping#{piggyback := [Update#{incarnation := Bad}]} end},
     {swim_piggyback_observed_at, Ping, fun(Bad) -> Ping#{piggyback := [Update#{observed_at := Bad}]} end},
     {swim_ack_round, Ack, fun(Bad) -> Ack#{round := Bad} end},
     {swim_ack_incarnation, Ack, fun(Bad) -> Ack#{incarnation := Bad} end},
     {swim_suspect_target_incarnation, Suspect, fun(Bad) -> Suspect#{target_incarnation := Bad} end},
     {swim_suspect_ttl, Suspect, fun(Bad) -> Suspect#{ttl := Bad} end},
     {find_node_depth, FindNode, fun(Bad) -> FindNode#{depth := Bad} end},
     {nodes_tier, Nodes, fun(Bad) -> Nodes#{nodes := [Ref#{tier := Bad}]} end},
     {nodes_asn, Nodes, fun(Bad) -> Nodes#{nodes := [Ref#{asn := Bad}]} end},
     {nodes_last_seen_at, Nodes, fun(Bad) -> Nodes#{nodes := [Ref#{last_seen_at := Bad}]} end},
     {hyparview_forward_join_ttl, Forward, fun(Bad) -> Forward#{ttl := Bad} end},
     {hyparview_forward_join_arwl, Forward, fun(Bad) -> Forward#{arwl := Bad} end},
     {hyparview_forward_join_prwl, Forward, fun(Bad) -> Forward#{prwl := Bad} end},
     {hyparview_shuffle_ttl, Shuffle, fun(Bad) -> Shuffle#{ttl := Bad} end},
     {plumtree_ihave_round, Ihave, fun(Bad) -> Ihave#{round := Bad} end},
     {plumtree_graft_round, Graft, fun(Bad) -> Graft#{round := Bad} end}].

%% A NODES entry without an asn still reads, as null.
a_nodes_entry_without_an_asn_reads_test() ->
    Ref = macula_frame:station_ref(#{node_id => id(6), station_id => id(7), tier => 1, country => <<"BE">>,
                                     last_seen_at => 1}),
    ?assertMatch({ok, #{nodes := [#{asn := undefined}]}, <<>>},
                 decode(macula_frame:nodes(#{key => id(4), nodes => [Ref]}))).

%% The neighbour tbs is read through the frame type's table, so the same bound holds inside a neighbour signature.
a_neighbour_tbs_integer_at_2_pow_53_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun hybrid_key/0, fun neighbour_cases/1}}.

neighbour_cases(Key) ->
    Connection = crypto:hash(sha384, <<"the challenge frame bytes">>),
    Opts = #{profile => pq_hybrid, peer_key => macula_node_keys:public_key(Key), connection => Connection, seq => 0},
    Tbs = fun(SentAt) ->
        #{{text, <<"frame_type">>} => {text, <<"ping">>}, {text, <<"nonce">>} => <<0:128>>,
          {text, <<"sent_at_ms">>} => SentAt, {text, <<"connection">>} => Connection, {text, <<"seq">>} => 0}
    end,
    Frame = fun(SentAt) ->
        #{version => macula_frame:version(macula_frame:ping(#{nonce => <<0:128>>})), frame_type => ping,
          neighbour => macula_signed_object:sign_held(?NEIGHBOUR_LABEL, Tbs(SentAt), Key)}
    end,
    [?_assertMatch({ok, #{sent_at_ms := 1}}, macula_frame:verify_neighbour(wire(Frame(1)), Opts)),
     ?_assertEqual({error, malformed_frame}, macula_frame:verify_neighbour(wire(Frame(?TOO_BIG)), Opts))].

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

id(N) ->
    <<N:256>>.

swim_update() ->
    macula_frame:swim_update(#{target => id(1), state => alive, incarnation => 0, observed_at => 1, by => id(2)}).

advertisement() ->
    {ok, Provider} = macula_node_keys:generate(identity, pq_pure),
    Unsigned = macula_record:procedure_advertisement(macula_node_keys:key_id(Provider), ?REALM, <<"forecast_v1">>,
                                                     id(9)),
    {Provider, macula_record:encode(macula_record:sign(Unsigned, Provider))}.

hybrid_key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_hybrid),
    Key.

decode(Frame) ->
    macula_frame:decode(macula_frame:encode(Frame)).

%% A frame as a peer receives it: encoded and decoded.
wire(Frame) ->
    {ok, Decoded, <<>>} = decode(Frame),
    Decoded.
