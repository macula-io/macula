%% EUnit tests for neighbour signatures (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, Neighbour signatures, D17). In
%% pq_hybrid a control frame travels as {version, frame_type, neighbour}: neighbour is {tbs, signature} under
%% MACULA-PQ-NEIGHBOUR-V1, verified with the connection peer's identity key, and its tbs holds the frame's fields with
%% frame_type, alg, the connection hash and the per-direction seq. In pq_pure no frame carries one.
-module(macula_frame_neighbour_tests).

-include_lib("eunit/include/eunit.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).
-define(LABEL, <<"MACULA-PQ-NEIGHBOUR-V1">>).
-define(CONTROL, [swim_ping, swim_ack, swim_suspect, swim_confirm, ping, pong, find_node, nodes, find_value, value,
                  store, store_ack, advertise, unadvertise, subscribe, unsubscribe,
                  overlay_relay, hyparview_join, hyparview_forward_join, hyparview_neighbor, hyparview_disconnect,
                  hyparview_shuffle, hyparview_shuffle_reply, plumtree_ihave, plumtree_graft, plumtree_prune, goodbye]).
-define(DATA, [publish, event, plumtree_gossip, want, have, block, manifest_req, manifest_res, cancel, call, result,
               error, stream_open, stream_data, stream_end, stream_error, stream_reply]).

%%------------------------------------------------------------------
%% The D17 table
%%------------------------------------------------------------------

control_frames_are_neighbour_signed_in_pq_hybrid_test() ->
    [?assert(macula_frame:neighbour_signed(pq_hybrid, Type)) || Type <- ?CONTROL].

data_frames_are_never_neighbour_signed_test() ->
    [?assertNot(macula_frame:neighbour_signed(pq_hybrid, Type)) || Type <- ?DATA].

no_frame_is_neighbour_signed_in_pq_pure_test() ->
    [?assertNot(macula_frame:neighbour_signed(pq_pure, Type)) || Type <- ?CONTROL ++ ?DATA].

%%------------------------------------------------------------------
%% Signing and verifying, in pq_hybrid
%%------------------------------------------------------------------

neighbour_signatures_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun keys/0, fun cases/1}}.

%% Every case signs inside its own test, so each one passes or fails on its own.
cases(Keys) ->
    [{case_name(Case), fun() -> Case(Keys) end}
     || Case <- [fun a_signed_control_frame_verifies_with_its_connection_and_seq/1,
                 fun a_signed_store_ack_keeps_its_boolean/1,
                 fun the_outer_frame_carries_only_version_frame_type_and_neighbour/1,
                 fun fields_beside_neighbour_are_refused/1,
                 fun the_tbs_holds_the_frame_fields_with_frame_type_alg_connection_and_seq/1,
                 fun another_connection_is_refused/1,
                 fun a_repeated_or_skipped_seq_is_refused/1,
                 fun another_peer_key_is_refused/1,
                 fun a_tampered_tbs_is_refused/1,
                 fun an_outer_frame_type_other_than_the_tbs_one_is_refused/1,
                 fun a_tbs_field_the_frame_type_does_not_define_is_refused/1,
                 fun a_control_frame_without_neighbour_is_refused_in_pq_hybrid/1,
                 fun a_frame_with_neighbour_is_refused_in_pq_pure/1,
                 fun a_control_frame_without_neighbour_is_read_as_is_in_pq_pure/1,
                 fun a_data_frame_cannot_be_neighbour_signed/1]].

a_signed_control_frame_verifies_with_its_connection_and_seq(#{key := Key} = Keys) ->
    Ping = ping(),
    {ok, Opened} = macula_frame:verify_neighbour(wire(macula_frame:sign_neighbour(Ping, Key, at(Keys, 0))),
                                                 opts(Keys, 0)),
    ?assertEqual(Ping, Opened).

a_signed_store_ack_keeps_its_boolean(#{key := Key} = Keys) ->
    StoreAck = wire(macula_frame:store_ack(#{key => <<7:256>>, stored => false})),
    Signed = wire(macula_frame:sign_neighbour(StoreAck, Key, at(Keys, 0))),
    ?assertEqual({ok, StoreAck}, macula_frame:verify_neighbour(Signed, opts(Keys, 0))).

the_outer_frame_carries_only_version_frame_type_and_neighbour(#{key := Key} = Keys) ->
    Decoded = wire(macula_frame:sign_neighbour(ping(), Key, at(Keys, 0))),
    ?assertEqual([frame_type, neighbour, version], lists:sort(maps:keys(Decoded))),
    ?assertEqual([signature, tbs], lists:sort(maps:keys(maps:get(neighbour, Decoded)))).

fields_beside_neighbour_are_refused(#{key := Key} = Keys) ->
    Signed = macula_frame:sign_neighbour(ping(), Key, at(Keys, 0)),
    ?assertEqual({error, malformed_frame},
                 macula_frame:verify_neighbour(wire(Signed#{nonce => <<0:128>>}), opts(Keys, 0))).

the_tbs_holds_the_frame_fields_with_frame_type_alg_connection_and_seq(#{key := Key, connection := C} = Keys) ->
    #{neighbour := #{tbs := Tbs}} = macula_frame:sign_neighbour(ping(), Key, at(Keys, 7)),
    {ok, Fields} = macula_record_cbor:decode_strict(Tbs),
    ?assertMatch(#{{text, <<"frame_type">>} := {text, <<"ping">>},
                   {text, <<"alg">>} := {text, <<"ML-DSA-87-PS384">>},
                   {text, <<"connection">>} := C, {text, <<"seq">>} := 7, {text, <<"nonce">>} := <<_:128>>},
                 Fields),
    ?assertNot(maps:is_key({text, <<"version">>}, Fields)).

another_connection_is_refused(#{key := Key} = Keys) ->
    Frame = wire(macula_frame:sign_neighbour(ping(), Key, at(Keys, 0))),
    Other = (opts(Keys, 0))#{connection => crypto:hash(sha384, <<"another challenge">>)},
    ?assertEqual({error, malformed_frame}, macula_frame:verify_neighbour(Frame, Other)).

a_repeated_or_skipped_seq_is_refused(#{key := Key} = Keys) ->
    Frame = wire(macula_frame:sign_neighbour(ping(), Key, at(Keys, 3))),
    ?assertEqual({error, malformed_frame}, macula_frame:verify_neighbour(Frame, opts(Keys, 2))),
    ?assertEqual({error, malformed_frame}, macula_frame:verify_neighbour(Frame, opts(Keys, 4))).

another_peer_key_is_refused(#{key := Key, other := Other} = Keys) ->
    Frame = wire(macula_frame:sign_neighbour(ping(), Key, at(Keys, 0))),
    ?assertEqual({error, signature_invalid},
                 macula_frame:verify_neighbour(Frame,
                                               (opts(Keys, 0))#{peer_key => macula_node_keys:public_key(Other)})).

a_tampered_tbs_is_refused(#{key := Key} = Keys) ->
    #{neighbour := #{tbs := <<Head:20/binary, Byte, Tail/binary>>} = Neighbour} = Signed =
        macula_frame:sign_neighbour(ping(), Key, at(Keys, 0)),
    Tampered = Signed#{neighbour := Neighbour#{tbs := <<Head/binary, (Byte bxor 1), Tail/binary>>}},
    ?assertEqual({error, signature_invalid}, macula_frame:verify_neighbour(wire(Tampered), opts(Keys, 0))).

an_outer_frame_type_other_than_the_tbs_one_is_refused(#{key := Key} = Keys) ->
    Signed = macula_frame:sign_neighbour(ping(), Key, at(Keys, 0)),
    ?assertEqual({error, malformed_frame},
                 macula_frame:verify_neighbour(wire(Signed#{frame_type := pong}), opts(Keys, 0))).

a_tbs_field_the_frame_type_does_not_define_is_refused(#{key := Key, connection := C} = Keys) ->
    Fields = #{{text, <<"frame_type">>} => {text, <<"ping">>}, {text, <<"nonce">>} => <<0:128>>,
               {text, <<"connection">>} => C, {text, <<"seq">>} => 0, {text, <<"payload">>} => 1},
    Frame = #{version => 3, frame_type => ping, neighbour => macula_signed_object:sign_held(?LABEL, Fields, Key)},
    ?assertEqual({error, malformed_frame}, macula_frame:verify_neighbour(wire(Frame), opts(Keys, 0))).

a_control_frame_without_neighbour_is_refused_in_pq_hybrid(Keys) ->
    ?assertEqual({error, malformed_frame}, macula_frame:verify_neighbour(wire(ping()), opts(Keys, 0))).

a_frame_with_neighbour_is_refused_in_pq_pure(#{key := Key} = Keys) ->
    Frame = wire(macula_frame:sign_neighbour(ping(), Key, at(Keys, 0))),
    ?assertEqual({error, malformed_frame}, macula_frame:verify_neighbour(Frame, (opts(Keys, 0))#{profile => pq_pure})).

a_control_frame_without_neighbour_is_read_as_is_in_pq_pure(Keys) ->
    Ping = wire(ping()),
    ?assertEqual({ok, Ping}, macula_frame:verify_neighbour(Ping, (opts(Keys, 0))#{profile => pq_pure})).

a_data_frame_cannot_be_neighbour_signed(#{key := Key} = Keys) ->
    Publish = macula_frame:publish(#{realm => <<0:256>>, topic => <<"t">>, seq => 0, published_at => 1,
                                     payload => <<"p">>}, Key),
    ?assertError(function_clause, macula_frame:sign_neighbour(Publish, Key, at(Keys, 0))).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

keys() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_hybrid),
    {ok, Other} = macula_node_keys:generate(identity, pq_hybrid),
    #{key => Key, other => Other, connection => crypto:hash(sha384, <<"the challenge frame bytes">>)}.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

ping() ->
    wire(macula_frame:ping(#{nonce => crypto:strong_rand_bytes(16)})).

%% A frame as a peer receives it: encoded and decoded.
wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

at(#{connection := C}, Seq) ->
    #{connection => C, seq => Seq}.

opts(#{key := Key, connection := C}, Seq) ->
    #{profile => pq_hybrid, peer_key => macula_node_keys:public_key(Key), connection => C, seq => Seq}.

%% The control frames are the frame types that belong on a connection's control stream, whatever the profile.
control_frames_belong_on_the_control_stream_in_either_profile_test() ->
    ?assertEqual({?CONTROL, []},
                 {[Type || Type <- ?CONTROL, macula_frame:control_frame(Type)],
                  [Type || Type <- ?DATA, macula_frame:control_frame(Type)]}).
