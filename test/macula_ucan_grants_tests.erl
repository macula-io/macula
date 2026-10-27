%% EUnit tests for macula_ucan:grants/4 (macula#128): a station answers each SUBSCRIBE and PUBLISH from the
%% capabilities of a membership token it verified once per link, with nothing verified again.
%%
%% A publish names one topic. A subscription pattern's `*' matches one segment, so a grant must cover every topic the
%% pattern can match: it covers the prefix of the segments before the pattern's first `*' (threat model, 4.4).
-module(macula_ucan_grants_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM_NAME, <<"io.example">>).
-define(SUBSCRIBE, <<"pubsub/subscribe">>).
-define(PUBLISH, <<"pubsub/publish">>).
-define(CALL, <<"rpc/call">>).

realm() -> crypto:hash(sha256, ?REALM_NAME).

cap(With, Can) -> #{<<"with">> => With, <<"can">> => Can}.

subscribe(Caps, Pattern) -> macula_ucan:grants(Caps, ?SUBSCRIBE, realm(), {subscription, Pattern}).

%%------------------------------------------------------------------
%% Subscribe: exact topics and patterns
%%------------------------------------------------------------------

a_subscription_is_granted_only_when_a_grant_covers_every_topic_it_matches_test_() ->
    EuOrders = [cap(<<"mri:topic:io.example/acme/_org/orders/eu/*">>, ?SUBSCRIBE)],
    [{"an exact topic below the prefix is covered",
      ?_assertEqual(ok, subscribe(EuOrders, <<"io.example/acme/_org/orders/eu/placed_v1">>))},
     {"a pattern inside the prefix is covered",
      ?_assertEqual(ok, subscribe(EuOrders, <<"io.example/acme/_org/orders/eu/*">>))},
     {"a pattern whose wildcard sits deeper inside the prefix is covered",
      ?_assertEqual(ok, subscribe(EuOrders, <<"io.example/acme/_org/orders/eu/*/placed_v1">>))},
     {"a wider pattern is not: orders.eu.* does not cover orders.*",
      ?_assertEqual({error, missing_capability}, subscribe(EuOrders, <<"io.example/acme/_org/orders/*">>))},
     {"a pattern whose wildcard comes before the prefix ends is not",
      ?_assertEqual({error, missing_capability}, subscribe(EuOrders, <<"io.example/acme/_org/*/eu/placed_v1">>))},
     {"a partial segment is no match: orders/e is not orders/eu",
      ?_assertEqual({error, missing_capability},
                    subscribe([cap(<<"mri:topic:io.example/acme/_org/orders/e/*">>, ?SUBSCRIBE)],
                              <<"io.example/acme/_org/orders/eu/placed_v1">>))},
     {"an exact grant covers its topic and no pattern over it",
      [?_assertEqual(ok, subscribe([cap(<<"mri:topic:io.example/acme/_org/orders/placed_v1">>, ?SUBSCRIBE)],
                                   <<"io.example/acme/_org/orders/placed_v1">>)),
       ?_assertEqual({error, missing_capability},
                     subscribe([cap(<<"mri:topic:io.example/acme/_org/orders/placed_v1">>, ?SUBSCRIBE)],
                               <<"io.example/acme/_org/orders/*">>))]},
     {"a pattern over the whole realm needs the realm grant",
      [?_assertEqual(ok, subscribe([cap(<<"mri:realm:io.example">>, ?SUBSCRIBE)], <<"io.example/*/_org/x/y_v1">>)),
       ?_assertEqual({error, missing_capability},
                     subscribe([cap(<<"mri:topic:io.example/acme/*">>, ?SUBSCRIBE)], <<"io.example/*/_org/x/y_v1">>))]}].

a_subscription_needs_the_subscribe_can_in_this_realm_test_() ->
    [{"a publish grant does not let a node subscribe",
      ?_assertEqual({error, missing_capability},
                    subscribe([cap(<<"mri:topic:io.example/acme/*">>, ?PUBLISH)], <<"io.example/acme/_org/x/y_v1">>))},
     {"a grant in another realm does not reach this one",
      ?_assertEqual({error, wrong_realm},
                    subscribe([cap(<<"mri:topic:io.other/acme/*">>, ?SUBSCRIBE)], <<"io.other/acme/_org/x/y_v1">>))},
     {"a pattern naming another realm is refused even with a realm-wide grant here",
      ?_assertEqual({error, wrong_realm},
                    subscribe([cap(<<"mri:realm:io.example">>, ?SUBSCRIBE)], <<"io.other/acme/*">>))},
     {"a pattern that is all wildcard names no realm",
      ?_assertEqual({error, topic_without_realm},
                    subscribe([cap(<<"mri:realm:io.example">>, ?SUBSCRIBE)], <<"*/acme/_org/x/y_v1">>))},
     {"no capabilities, no subscription",
      ?_assertEqual({error, missing_capability}, subscribe([], <<"io.example/acme/_org/x/y_v1">>))}].

%%------------------------------------------------------------------
%% Publish: one concrete topic
%%------------------------------------------------------------------

a_publish_is_granted_on_the_concrete_topic_it_routes_on_test_() ->
    Caps = [cap(<<"mri:topic:io.example/acme/_org/orders/*">>, ?PUBLISH)],
    Publish = fun(Topic) -> macula_ucan:grants(Caps, ?PUBLISH, realm(), {topic, Topic}) end,
    [?_assertEqual(ok, Publish(<<"io.example/acme/_org/orders/placed_v1">>)),
     ?_assertEqual({error, missing_capability}, Publish(<<"io.example/acme/_org/billing/paid_v1">>)),
     {"a publish never names a pattern",
      ?_assertEqual({error, malformed}, Publish(<<"io.example/acme/_org/orders/*">>))},
     {"a system topic has no realm to be granted in",
      ?_assertEqual({error, topic_without_realm}, Publish(<<"_mesh.patterns">>))}].

%%------------------------------------------------------------------
%% Procedures, as authorize/3 checks them
%%------------------------------------------------------------------

a_procedure_is_granted_as_authorize_grants_it_test_() ->
    Caps = [cap(<<"mri:proc:io.example/acme/*">>, ?CALL)],
    [?_assertEqual(ok, macula_ucan:grants(Caps, ?CALL, realm(), {procedure, <<"acme/api.users.get">>})),
     ?_assertEqual({error, missing_capability},
                   macula_ucan:grants(Caps, ?CALL, realm(), {procedure, <<"beta/api.users.get">>})),
     ?_assertEqual({error, procedure_without_org},
                   macula_ucan:grants(Caps, ?CALL, realm(), {procedure, <<"get">>}))].

%%------------------------------------------------------------------
%% The gate that keeps every capability
%%------------------------------------------------------------------

a_realm_member_gate_of_any_keeps_every_capability_test() ->
    {ok, RealmKey} = macula_node_keys:generate(realm, pq_pure),
    Member = macula_test_identity:node_id(),
    Caps = [#{with => <<"mri:topic:io.example/acme/*">>, can => ?SUBSCRIBE},
            #{with => <<"mri:topic:io.example/acme/*">>, can => ?PUBLISH}],
    Token = macula_ucan:create(RealmKey, Member, Caps, #{exp => erlang:system_time(second) + 600}),
    Policy = {realm_member_required, macula_node_keys:key_id(RealmKey), any},
    {ok, #{<<"cap">> := Kept}} =
        macula_ucan:authorize(Token, Policy, #{caller => Member, profile => pq_pure,
                                               now => erlang:system_time(second)}),
    ?assertEqual(2, length(Kept)),
    ?assertEqual(ok, macula_ucan:grants(Kept, ?PUBLISH, realm(), {topic, <<"io.example/acme/_org/x/y_v1">>})).
