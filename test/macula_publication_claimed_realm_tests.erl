%% EUnit tests for the realm a GOSSIP's publication claims, read without verifying it. A GOSSIP names its realm only
%% inside its signed publication, so a station link routes one to its realm's overlay subscribers by this claim, and
%% the subscriber verifies the publication before acting on it. The read checks the frame and the signed object as
%% verify_publication/3 does, and reads the tbs with the same strict decoding and field table. It never raises: a tbs
%% that does not decode, names no realm, names a realm of another length or is past the element budget, and any frame
%% that is not a well-formed GOSSIP, is no_realm.
-module(macula_publication_claimed_realm_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).
-define(PUBLICATION_LABEL, <<"MACULA-PQ-PUBLICATION-V1">>).

a_gossip_names_the_realm_its_publication_claims_test() ->
    ?assertEqual({ok, ?REALM}, macula_frame:claimed_publication_realm(gossip(publication(?REALM)))).

%% The realm is a key for routing and nothing more: a publication whose signature does not verify still names it, and
%% verify_publication/3 refuses that publication.
a_claimed_realm_is_read_without_verifying_the_publication_test() ->
    Gossip = gossip(tampered(publication(?REALM))),
    ?assertEqual({ok, ?REALM}, macula_frame:claimed_publication_realm(Gossip)),
    ?assertEqual({error, signature_invalid},
                 macula_frame:verify_publication(Gossip, profile(), erlang:system_time(millisecond))).

a_publication_that_names_no_realm_is_no_realm_test() ->
    Fields = maps:remove({text, <<"realm">>}, tbs_fields(?REALM)),
    ?assertEqual({error, no_realm}, macula_frame:claimed_publication_realm(gossip(signed(Fields)))).

a_realm_of_another_length_is_no_realm_test_() ->
    [?_assertEqual({error, no_realm}, macula_frame:claimed_publication_realm(gossip(signed(tbs_fields(Realm)))))
     || Realm <- [<<7:248>>, <<7:264>>, <<>>]].

%% An empty tbs, a lone break byte, a map cut off inside its first key, and a map with a byte after it.
a_tbs_that_is_not_one_cbor_map_is_no_realm_test_() ->
    [?_assertEqual({error, no_realm},
                   macula_frame:claimed_publication_realm(gossip(with_tbs(publication(?REALM), Tbs))))
     || Tbs <- [<<>>, <<16#FF>>, <<16#A1, 16#65, "rea">>, <<16#A0, 0>>]].

%% A tbs of more CBOR items than the decoder's element budget is refused before it is read, so reading a claim adds no
%% unbounded decode.
a_tbs_past_the_element_budget_is_no_realm_test() ->
    Items = lists:duplicate(macula_cbor_nif:element_budget(), 0),
    Tbs = macula_record_cbor:encode((tbs_fields(?REALM))#{{text, <<"payload">>} => Items}),
    ?assertEqual({error, no_realm}, macula_frame:claimed_publication_realm(gossip(with_tbs(publication(?REALM), Tbs)))).

%% A PUBLISH and an IHAVE name a realm of their own and are not read this way, and a GOSSIP with a field it does not
%% have, or whose publication is not a signed object, names no realm.
a_frame_that_is_not_a_well_formed_gossip_is_no_realm_test_() ->
    #{publication := Object} = Gossip = gossip(publication(?REALM)),
    Frames = [macula_frame:publish(#{realm => ?REALM, topic => <<"news">>, seq => 1, payload => <<"p">>,
                                     published_at => erlang:system_time(millisecond)}, key()),
              macula_frame:plumtree_ihave(#{realm => ?REALM, msg_id => <<0:384>>, round => 1}),
              Gossip#{extra => 1},
              Gossip#{publication := Object#{extra => <<>>}},
              Gossip#{publication := maps:remove(signature, Object)},
              Gossip#{publication := Object#{key := 1}},
              Gossip#{publication := not_an_object},
              maps:remove(publication, Gossip),
              #{}],
    [?_assertEqual({error, no_realm}, macula_frame:claimed_publication_realm(Frame)) || Frame <- Frames].

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

key() ->
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    Key.

%% The publication a PUBLISH for `Realm' carries, signed by a fresh publisher in the configured profile.
publication(Realm) ->
    #{publication := Publication} =
        macula_frame:publish(#{realm => Realm, topic => <<"news">>, seq => 1, payload => <<"p">>,
                               published_at => erlang:system_time(millisecond)}, key()),
    Publication.

%% The fields a publication's tbs holds, in their wire form, for a realm of any length.
tbs_fields(Realm) ->
    #{{text, <<"publisher">>} => <<1:256>>, {text, <<"realm">>} => Realm, {text, <<"topic">>} => {text, <<"news">>},
      {text, <<"seq">>} => 1, {text, <<"published_at">>} => erlang:system_time(millisecond),
      {text, <<"payload">>} => <<"p">>}.

signed(Fields) ->
    macula_signed_object:sign(?PUBLICATION_LABEL, Fields, key()).

with_tbs(Publication, Tbs) ->
    Publication#{tbs := Tbs}.

tampered(#{signature := <<Byte, Rest/binary>>} = Publication) ->
    Publication#{signature := <<(Byte bxor 1), Rest/binary>>}.

%% A GOSSIP carrying `Publication', as a link receives it.
gossip(Publication) ->
    Frame = macula_frame:plumtree_gossip(#{publication => Publication, round => 1}),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.
