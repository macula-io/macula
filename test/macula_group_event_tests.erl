%%%-------------------------------------------------------------------
%%% @doc An event sealed under a group epoch (plans/DESIGN_E2E_SEALED_PUBSUB.md §7; test/vectors/E2E_SEAL_V1.md): the
%%% publisher's subkey of the epoch key, a fresh random nonce, the event AAD over the publication's routing fields,
%%% and the epoch id as `key_id'. A PUBLISH carries `sealed' in place of `payload', signed like any publication.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_group_event_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NOW, 1_790_000_000_000).
-define(REALM, <<7:256>>).
-define(TOPIC, <<"io.macula/acme/chat/room/said_v1">>).

epoch() ->
    macula_group_epoch:new(?NOW, 15 * 60000).

published(Key, Epoch, Payload) ->
    Fields = #{publisher => macula_node_keys:key_id(Key), realm => ?REALM, topic => ?TOPIC, seq => 3,
               published_at => ?NOW},
    {ok, Sealed} = macula_group_event:seal(Epoch, Fields, Payload),
    Frame = macula_frame:publish(#{realm => ?REALM, topic => ?TOPIC, seq => 3, published_at => ?NOW,
                                   sealed => Sealed}, Key),
    {ok, Verified} = macula_frame:verify_publication(Frame, pq_pure, ?NOW),
    Verified.

a_sealed_publication_verifies_with_sealed_and_no_payload_test() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    #{id := Id} = Epoch = epoch(),
    Verified = published(Key, Epoch, #{{text, <<"said">>} => {text, <<"hello">>}}),
    ?assertNot(maps:is_key(payload, Verified)),
    ?assertMatch(#{sealed := #{scheme := 1, key_id := Id, nonce := <<_:96>>, ct := _}}, Verified).

a_member_holding_the_epoch_opens_it_test() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Epoch = epoch(),
    Verified = published(Key, Epoch, #{{text, <<"said">>} => {text, <<"hello">>}}),
    ?assertEqual({ok, #{{text, <<"said">>} => {text, <<"hello">>}}}, macula_group_event:open(Epoch, Verified)).

another_epochs_key_does_not_open_it_test() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Verified = published(Key, epoch(), <<"x">>),
    ?assertEqual({error, tag_invalid}, macula_group_event:open(epoch(), Verified)).

a_routing_field_moved_does_not_open_it_test() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Epoch = epoch(),
    Verified = published(Key, Epoch, <<"x">>),
    [?assertEqual({error, tag_invalid}, macula_group_event:open(Epoch, Verified#{F => V}))
     || {F, V} <- [{topic, <<"io.macula/acme/chat/other_v1">>}, {seq, 4}, {published_at, ?NOW + 1},
                   {publisher, <<1:256>>}, {realm, <<8:256>>}]].

every_seal_draws_a_fresh_nonce_test() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Epoch = epoch(),
    #{sealed := #{nonce := N1}} = published(Key, Epoch, <<"x">>),
    #{sealed := #{nonce := N2}} = published(Key, Epoch, <<"x">>),
    ?assertNotEqual(N1, N2).

a_clear_publication_is_not_opened_test() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Frame = macula_frame:publish(#{realm => ?REALM, topic => ?TOPIC, seq => 3, published_at => ?NOW,
                                   payload => <<"x">>}, Key),
    {ok, Verified} = macula_frame:verify_publication(Frame, pq_pure, ?NOW),
    ?assertEqual({error, not_sealed}, macula_group_event:open(epoch(), Verified)).

%% The subkey is the vectors' k_pub: every publisher seals under its own.
the_key_is_the_publishers_subkey_test() ->
    #{key := GroupKey} = Epoch = epoch(),
    Publisher = <<1:256>>,
    Fields = #{publisher => Publisher, realm => ?REALM, topic => ?TOPIC, seq => 0, published_at => ?NOW},
    {ok, #{nonce := Nonce, ct := Ct}} = macula_group_event:seal(Epoch, Fields, <<"x">>),
    {ok, Plain} = macula_frame:payload_plain(<<"x">>),
    ?assertEqual({ok, Plain},
                 macula_seal:open(macula_seal:event_key(GroupKey, Publisher), Nonce,
                                  macula_seal:event_aad(?REALM, ?TOPIC, Publisher, 0, ?NOW), Ct)).
