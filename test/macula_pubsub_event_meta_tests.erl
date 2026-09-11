%% EUnit tests for macula_pubsub:event_meta/2, the one producer of the meta a subscriber receives with an event: the
%% verified publication's realm, publisher node_id, seq and published_at, the EVENT's delivered_via, and the
%% publication_hash and expires_at the pool delivers each publication once on.
-module(macula_pubsub_event_meta_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NOW, 1789000000000).
-define(MINUTE, 60000).
-define(REALM, <<1:256>>).

event_meta_test_() ->
    {setup, fun publisher/0,
     fun(Publisher) ->
         [{case_name(Case), fun() -> Case(Publisher) end}
          || Case <- [fun the_meta_holds_exactly_the_event_meta_keys/1,
                      fun the_meta_is_taken_from_the_verified_publication/1,
                      fun delivered_via_is_the_one_the_link_passes/1,
                      fun expires_at_is_the_verifiers_and_ttl_ms_stays_out/1,
                      fun a_delivery_channel_other_than_plumtree_or_direct_is_refused/1]]
     end}.

%%------------------------------------------------------------------
%% Cases
%%------------------------------------------------------------------

%% No payload, carried key, ttl_ms or publisher_verified: only the keys of event_meta().
the_meta_holds_exactly_the_event_meta_keys(Publisher) ->
    Meta = macula_pubsub:event_meta(verified(event(Publisher, #{}, plumtree)), plumtree),
    ?assertEqual([delivered_via, expires_at, publication_hash, published_at, publisher, realm, seq],
                 lists:sort(maps:keys(Meta))).

%% publisher is the key id of the publication's key, the publisher's node_id, and publication_hash is the SHA-384
%% of its tbs.
the_meta_is_taken_from_the_verified_publication(Publisher) ->
    #{publication := #{key := Key, tbs := Tbs}} = Frame = event(Publisher, #{}, direct),
    Meta = macula_pubsub:event_meta(verified(Frame), direct),
    ?assertEqual(macula_node_keys:node_id(Key, pq_pure), maps:get(publisher, Meta)),
    ?assertEqual(macula_node_keys:key_id(Publisher), maps:get(publisher, Meta)),
    ?assertMatch(#{realm := ?REALM, seq := 7, published_at := ?NOW}, Meta),
    ?assertEqual(crypto:hash(sha384, Tbs), maps:get(publication_hash, Meta)).

delivered_via_is_the_one_the_link_passes(Publisher) ->
    [?assertEqual(Via, maps:get(delivered_via, macula_pubsub:event_meta(verified(event(Publisher, #{}, Via)), Via)))
     || Via <- [plumtree, direct]].

%% expires_at is the moment the verifier refuses the publication: published_at plus ttl_ms plus 5 minutes, or plus 15
%% minutes without ttl_ms.
expires_at_is_the_verifiers_and_ttl_ms_stays_out(Publisher) ->
    WithTtl = macula_pubsub:event_meta(verified(event(Publisher, #{ttl_ms => 2 * ?MINUTE}, plumtree)), plumtree),
    ?assertEqual(?NOW + 7 * ?MINUTE, maps:get(expires_at, WithTtl)),
    ?assertNot(maps:is_key(ttl_ms, WithTtl)),
    Without = macula_pubsub:event_meta(verified(event(Publisher, #{}, plumtree)), plumtree),
    ?assertEqual(?NOW + 15 * ?MINUTE, maps:get(expires_at, Without)).

a_delivery_channel_other_than_plumtree_or_direct_is_refused(Publisher) ->
    Verified = verified(event(Publisher, #{}, plumtree)),
    ?assertError(function_clause, macula_pubsub:event_meta(Verified, station)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

publisher() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

%% An EVENT as a subscriber's link receives it: made from a signed PUBLISH, encoded and decoded.
event(Publisher, Extra, Via) ->
    Spec = maps:merge(#{realm => ?REALM, topic => <<"weather.tienen">>, seq => 7, published_at => ?NOW,
                        payload => #{celsius => 21}}, Extra),
    #{publication := Publication} = macula_frame:publish(Spec, Publisher),
    wire(macula_frame:event(#{publication => Publication, delivered_via => Via})).

verified(Frame) ->
    {ok, Verified} = macula_frame:verify_publication(Frame, pq_pure, ?NOW),
    Verified.

wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.
