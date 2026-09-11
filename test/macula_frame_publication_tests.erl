%% EUnit tests for publications (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md: Publications; D17). PUBLISH is {version,
%% frame_type, publication}, EVENT adds delivered_via, and Plumtree GOSSIP adds its round. publication is {key, tbs,
%% signature} under MACULA-PQ-PUBLICATION-V1, signed by the publisher, and the same bytes ride in every frame made from
%% it, so its tbs holds no frame_type. IHAVE and GRAFT name a publication by the SHA-384 of its tbs.
-module(macula_frame_publication_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PUBLICATION_LABEL, <<"MACULA-PQ-PUBLICATION-V1">>).
-define(NOW, 1789000000000).
-define(MINUTE, 60000).
-define(REALM, <<1:256>>).

publications_test_() ->
    {setup, fun keys/0, fun cases/1}.

%% Every case signs inside its own test, so each one passes or fails on its own.
cases(Keys) ->
    [{case_name(Case), fun() -> Case(Keys) end}
     || Case <- [fun a_publish_carries_a_signed_publication_that_verifies/1,
                 fun an_event_carries_the_same_publication_bytes/1,
                 fun a_gossip_carries_the_same_publication_bytes_and_its_round/1,
                 fun the_publication_tbs_holds_no_frame_type/1,
                 fun a_publisher_that_is_not_the_key_id_of_key_is_refused/1,
                 fun a_tampered_publication_is_refused/1,
                 fun a_publication_under_the_other_profile_is_malformed/1,
                 fun publication_fields_the_design_does_not_allow_are_malformed/1,
                 fun a_publication_more_than_5_minutes_ahead_is_refused/1,
                 fun a_publication_past_its_ttl_plus_5_minutes_is_refused/1,
                 fun without_ttl_a_publication_lives_10_minutes_plus_5/1,
                 fun ihave_and_graft_name_a_publication_by_its_hash/1,
                 fun the_old_publisher_signature_is_gone/1]].

%%------------------------------------------------------------------
%% Cases
%%------------------------------------------------------------------

a_publish_carries_a_signed_publication_that_verifies(#{publisher := Publisher}) ->
    Frame = wire(macula_frame:publish(publish_spec(?NOW), Publisher)),
    ?assertEqual([frame_type, publication, version], lists:sort(maps:keys(Frame))),
    #{publication := #{key := Key, tbs := Tbs} = Publication} = Frame,
    ?assertEqual([key, signature, tbs], lists:sort(maps:keys(Publication))),
    {ok, Verified} = macula_frame:verify_publication(Frame, pq_pure, ?NOW),
    ?assertMatch(#{realm := ?REALM, topic := <<"weather.tienen">>, seq := 7, published_at := ?NOW}, Verified),
    ?assertEqual(macula_node_keys:key_id(Publisher), maps:get(publisher, Verified)),
    ?assertEqual(Key, maps:get(key, Verified)),
    ?assertEqual(crypto:hash(sha384, Tbs), maps:get(publication_hash, Verified)),
    ?assertEqual(#{{text, <<"celsius">>} => 21}, maps:get(payload, Verified)).

an_event_carries_the_same_publication_bytes(#{publisher := Publisher}) ->
    #{publication := Publication} = Publish = wire(macula_frame:publish(publish_spec(?NOW), Publisher)),
    Event = wire(macula_frame:event(#{publication => Publication, delivered_via => plumtree})),
    ?assertEqual([delivered_via, frame_type, publication, version], lists:sort(maps:keys(Event))),
    ?assertEqual(Publication, maps:get(publication, Event)),
    ?assertEqual(plumtree, maps:get(delivered_via, Event)),
    ?assertEqual(macula_frame:verify_publication(Publish, pq_pure, ?NOW),
                 macula_frame:verify_publication(Event, pq_pure, ?NOW)).

a_gossip_carries_the_same_publication_bytes_and_its_round(#{publisher := Publisher}) ->
    #{publication := Publication} = wire(macula_frame:publish(publish_spec(?NOW), Publisher)),
    Gossip = wire(macula_frame:plumtree_gossip(#{publication => Publication, round => 2})),
    ?assertEqual([frame_type, publication, round, version], lists:sort(maps:keys(Gossip))),
    ?assertMatch({ok, #{seq := 7}}, macula_frame:verify_publication(Gossip, pq_pure, ?NOW)).

the_publication_tbs_holds_no_frame_type(#{publisher := Publisher}) ->
    #{publication := #{tbs := Tbs}} = macula_frame:publish(publish_spec(?NOW), Publisher),
    {ok, Fields} = macula_record_cbor:decode_strict(Tbs),
    ?assertNot(maps:is_key({text, <<"frame_type">>}, Fields)),
    ?assertMatch(#{{text, <<"publisher">>} := _, {text, <<"alg">>} := {text, <<"ML-DSA-87">>}}, Fields).

a_publisher_that_is_not_the_key_id_of_key_is_refused(#{publisher := Publisher, other := Other}) ->
    Tbs = (publication_tbs(Publisher, ?NOW))#{{text, <<"publisher">>} := macula_node_keys:key_id(Other)},
    ?assertEqual({error, key_id_mismatch}, verify_crafted(Tbs, Publisher)).

a_tampered_publication_is_refused(#{publisher := Publisher}) ->
    #{publication := Signed} = Frame = macula_frame:publish(publish_spec(?NOW), Publisher),
    Tampered = Frame#{publication := Signed#{tbs := flip(maps:get(tbs, Signed))}},
    ?assertEqual({error, signature_invalid}, macula_frame:verify_publication(wire(Tampered), pq_pure, ?NOW)).

a_publication_under_the_other_profile_is_malformed(#{publisher := Publisher}) ->
    Frame = wire(macula_frame:publish(publish_spec(?NOW), Publisher)),
    ?assertEqual({error, malformed_frame}, macula_frame:verify_publication(Frame, pq_hybrid, ?NOW)).

publication_fields_the_design_does_not_allow_are_malformed(#{publisher := Publisher}) ->
    Base = publication_tbs(Publisher, ?NOW),
    ?assertMatch({ok, _}, verify_crafted(Base, Publisher)),
    ?assertMatch({ok, #{ttl_ms := 60000}}, verify_crafted(Base#{{text, <<"ttl_ms">>} => 60000}, Publisher)),
    [?assertEqual({error, malformed_frame}, verify_crafted(Tbs, Publisher))
     || Tbs <- [Base#{{text, <<"extra">>} => 1},
                Base#{{text, <<"frame_type">>} => {text, <<"publish">>}},
                maps:remove({text, <<"payload">>}, Base),
                Base#{{text, <<"topic">>} := <<"weather.tienen">>},
                Base#{{text, <<"realm">>} := <<1:248>>},
                Base#{{text, <<"seq">>} := 1 bsl 53},
                Base#{{text, <<"published_at">>} := {text, <<"now">>}},
                Base#{{text, <<"ttl_ms">>} => -1}]].

a_publication_more_than_5_minutes_ahead_is_refused(#{publisher := Publisher}) ->
    Verify = fun(PublishedAt) ->
        macula_frame:verify_publication(wire(macula_frame:publish(publish_spec(PublishedAt), Publisher)), pq_pure, ?NOW)
    end,
    ?assertMatch({ok, _}, Verify(?NOW + 4 * ?MINUTE)),
    ?assertEqual({error, not_yet_valid}, Verify(?NOW + 6 * ?MINUTE)).

a_publication_past_its_ttl_plus_5_minutes_is_refused(#{publisher := Publisher}) ->
    Verify = fun(PublishedAt) ->
        Spec = (publish_spec(PublishedAt))#{ttl_ms => ?MINUTE},
        macula_frame:verify_publication(wire(macula_frame:publish(Spec, Publisher)), pq_pure, ?NOW)
    end,
    ?assertMatch({ok, _}, Verify(?NOW - 5 * ?MINUTE)),
    ?assertEqual({error, expired}, Verify(?NOW - 7 * ?MINUTE)).

without_ttl_a_publication_lives_10_minutes_plus_5(#{publisher := Publisher}) ->
    Verify = fun(PublishedAt) ->
        macula_frame:verify_publication(wire(macula_frame:publish(publish_spec(PublishedAt), Publisher)), pq_pure, ?NOW)
    end,
    ?assertMatch({ok, _}, Verify(?NOW - 14 * ?MINUTE)),
    ?assertEqual({error, expired}, Verify(?NOW - 16 * ?MINUTE)).

ihave_and_graft_name_a_publication_by_its_hash(#{publisher := Publisher}) ->
    {ok, #{publication_hash := Hash}} =
        macula_frame:verify_publication(macula_frame:publish(publish_spec(?NOW), Publisher), pq_pure, ?NOW),
    [begin
         Frame = wire(macula_frame:Builder(#{realm => ?REALM, msg_id => Hash, round => 1})),
         ?assertEqual(Hash, maps:get(msg_id, Frame)),
         ?assertError(function_clause, macula_frame:Builder(#{realm => ?REALM, msg_id => <<0:128>>, round => 1})),
         ?assertEqual({error, bad_frame}, macula_frame:decode(macula_frame:encode(Frame#{msg_id := <<0:128>>})))
     end || Builder <- [plumtree_ihave, plumtree_graft]].

the_old_publisher_signature_is_gone(#{publisher := Publisher}) ->
    {module, macula_frame} = code:ensure_loaded(macula_frame),
    ?assertNot(erlang:function_exported(macula_frame, sign_publisher, 2)),
    ?assertNot(erlang:function_exported(macula_frame, verify_publisher, 1)),
    Frame = macula_frame:publish(publish_spec(?NOW), Publisher),
    ?assertEqual({error, bad_frame}, macula_frame:decode(macula_frame:encode(Frame#{publisher_sig => <<0:512>>}))).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

keys() ->
    Generate = fun() -> {ok, Key} = macula_node_keys:generate(identity, pq_pure), Key end,
    #{publisher => Generate(), other => Generate()}.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

publish_spec(PublishedAt) ->
    #{realm => ?REALM, topic => <<"weather.tienen">>, seq => 7, published_at => PublishedAt,
      payload => #{celsius => 21}}.

%% The fields of a publication as its signer puts them in tbs; signing adds alg.
publication_tbs(Publisher, PublishedAt) ->
    #{{text, <<"publisher">>} => macula_node_keys:key_id(Publisher), {text, <<"realm">>} => ?REALM,
      {text, <<"topic">>} => {text, <<"weather.tienen">>}, {text, <<"seq">>} => 7,
      {text, <<"published_at">>} => PublishedAt, {text, <<"payload">>} => 1}.

verify_crafted(Tbs, Key) ->
    Version = macula_frame:version(macula_frame:ping(#{nonce => <<0:128>>})),
    Frame = #{version => Version, frame_type => publish,
              publication => macula_signed_object:sign(?PUBLICATION_LABEL, Tbs, Key)},
    macula_frame:verify_publication(wire(Frame), pq_pure, ?NOW).

%% A frame as a peer receives it: encoded and decoded.
wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

flip(<<Head:20/binary, Byte, Tail/binary>>) ->
    <<Head/binary, (Byte bxor 1), Tail/binary>>.
