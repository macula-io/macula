%% EUnit tests for topic grants and prefix grants in macula_ucan (macula#127, the grant realm RBAC enforces).
%%
%% A topic is granted as `mri:topic:' and the topic in full, its realm's name first. A procedure or topic grant whose
%% last segment is `*' covers every name below the segments before it, whole segments only. A procedure grant never
%% answers a topic request, nor a topic grant a procedure request.
-module(macula_ucan_topic_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<"io.example">>).
-define(TOPIC, <<"io.example/acme/_org/orders/placed_v1">>).
-define(TOPIC_GRANT, <<"mri:topic:io.example/acme/_org/orders/placed_v1">>).
-define(TOPIC_PREFIX, <<"mri:topic:io.example/acme/_org/orders/*">>).
-define(PROCEDURE, <<"acme/api.users.get">>).
-define(PROC_PREFIX, <<"mri:proc:io.example/acme/*">>).
-define(PUBLISH, <<"pubsub/publish">>).

%%------------------------------------------------------------------
%% The narrowing matrix
%%------------------------------------------------------------------

the_narrowing_matrix_covers_topics_and_prefixes_test_() ->
    [{lists:flatten(io_lib:format("~s covers ~s", [Parent, Child])),
      ?_assertEqual(Covered, macula_ucan:covers(Parent, Child))}
     || {Parent, Child, Covered} <-
            [%% An exact topic covers itself and nothing else.
             {?TOPIC_GRANT, ?TOPIC_GRANT, true},
             {?TOPIC_GRANT, <<"mri:topic:io.example/acme/_org/orders/shipped_v1">>, false},
             {?TOPIC_GRANT, <<"mri:topic:io.other/acme/_org/orders/placed_v1">>, false},
             {?TOPIC_GRANT, ?TOPIC_PREFIX, false},
             %% A topic prefix covers what is below it, itself, and a narrower prefix; never a wider one.
             {?TOPIC_PREFIX, ?TOPIC_GRANT, true},
             {?TOPIC_PREFIX, <<"mri:topic:io.example/acme/_org/orders/x/y_v1">>, true},
             {?TOPIC_PREFIX, ?TOPIC_PREFIX, true},
             {?TOPIC_PREFIX, <<"mri:topic:io.example/acme/_org/orders/x/*">>, true},
             {<<"mri:topic:io.example/acme/*">>, ?TOPIC_PREFIX, true},
             {?TOPIC_PREFIX, <<"mri:topic:io.example/acme/*">>, false},
             %% Whole segments only: a prefix is not a string prefix.
             {<<"mri:topic:io.example/acme/_org/ord/*">>, ?TOPIC_GRANT, false},
             {<<"mri:topic:io.example/acme/*">>, <<"mri:topic:io.example/acmex/_org/orders/placed_v1">>, false},
             %% Nor does it cover the name its segments spell.
             {?TOPIC_PREFIX, <<"mri:topic:io.example/acme/_org/orders">>, false},
             {?TOPIC_PREFIX, <<"mri:topic:io.other/acme/_org/orders/placed_v1">>, false},
             %% A realm grant covers its topics; an org grant covers none.
             {<<"mri:realm:io.example">>, ?TOPIC_GRANT, true},
             {<<"mri:realm:io.example">>, ?TOPIC_PREFIX, true},
             {<<"mri:realm:io.other">>, ?TOPIC_GRANT, false},
             {<<"mri:org:io.example/acme">>, ?TOPIC_GRANT, false},
             {<<"mri:org:io.example/acme">>, ?TOPIC_PREFIX, false},
             %% A procedure grant never covers a topic, nor a topic grant a procedure, whatever their names.
             {<<"mri:proc:io.example/acme/_org/orders/placed_v1">>, ?TOPIC_GRANT, false},
             {<<"mri:proc:io.example/acme/*">>, ?TOPIC_GRANT, false},
             {?TOPIC_GRANT, <<"mri:proc:io.example/acme/_org/orders/placed_v1">>, false},
             {<<"mri:topic:io.example/acme/*">>, <<"mri:proc:io.example/acme/api.users.get">>, false},
             %% A procedure prefix, by the same rule, inside its org.
             {?PROC_PREFIX, <<"mri:proc:io.example/acme/api.users.get">>, true},
             {?PROC_PREFIX, <<"mri:proc:io.example/acme/billing/*">>, true},
             {?PROC_PREFIX, ?PROC_PREFIX, true},
             {<<"mri:proc:io.example/acme/billing/*">>, <<"mri:proc:io.example/acme/api.users.get">>, false},
             {<<"mri:proc:io.example/acme/billing/*">>, ?PROC_PREFIX, false},
             {<<"mri:proc:io.example/acme/api/*">>, <<"mri:proc:io.example/acme/api.users.get">>, false},
             {?PROC_PREFIX, <<"mri:proc:io.example/acmex/api.users.get">>, false},
             {?PROC_PREFIX, <<"mri:proc:io.example/acme">>, false},
             {<<"mri:proc:io.example/acme/api.users.get">>, ?PROC_PREFIX, false},
             %% An org grant covers its own procedure prefixes, not another org's.
             {<<"mri:org:io.example/acme">>, ?PROC_PREFIX, true},
             {<<"mri:org:io.example/acme">>, <<"mri:proc:io.example/acme/billing/*">>, true},
             {<<"mri:org:io.example/beta">>, ?PROC_PREFIX, false},
             {<<"mri:realm:io.example">>, ?PROC_PREFIX, true},
             %% A `*' anywhere but a prefix's last segment is no grant, as a parent or as a child.
             {<<"mri:proc:io.example/*">>, <<"mri:proc:io.example/acme/api.users.get">>, false},
             {<<"mri:proc:io.example/_/*">>, <<"mri:proc:io.example/_/x">>, false},
             {<<"mri:proc:io.example/acme/*/get">>, <<"mri:proc:io.example/acme/x/get">>, false},
             {<<"mri:proc:io.example/acme//*">>, <<"mri:proc:io.example/acme//x">>, false},
             {<<"mri:proc:io.example/acme/x*">>, <<"mri:proc:io.example/acme/x*">>, true},
             {<<"mri:topic:io.example/*">>, ?TOPIC_GRANT, false},
             {<<"mri:topic:io.example/acme/*/orders/*">>, ?TOPIC_GRANT, false},
             {<<"mri:topic:io.example/">>, <<"mri:topic:io.example/">>, false},
             {<<"mri:topic:io.example">>, <<"mri:topic:io.example">>, false},
             {<<"mri:topic:IO.example/acme/*">>, <<"mri:topic:IO.example/acme/x">>, false},
             {<<"mri:realm:io.example">>, <<"mri:proc:io.example/acme/*/get">>, false},
             {<<"mri:realm:io.example">>, <<"mri:topic:io.example/*">>, false}]].

%%------------------------------------------------------------------
%% A topic request
%%------------------------------------------------------------------

an_exact_topic_grant_answers_its_topic_test() ->
    ?assertMatch({ok, _}, authorize(?TOPIC_GRANT, topic(?TOPIC))).

a_topic_prefix_answers_a_topic_below_it_test() ->
    ?assertMatch({ok, _}, authorize(?TOPIC_PREFIX, topic(?TOPIC))),
    ?assertMatch({ok, _}, authorize(<<"mri:topic:io.example/acme/*">>, topic(?TOPIC))).

a_realm_grant_answers_a_topic_of_its_realm_test() ->
    ?assertMatch({ok, _}, authorize(<<"mri:realm:io.example">>, topic(?TOPIC))).

a_prefix_sharing_part_of_a_segment_answers_nothing_test() ->
    ?assertEqual({error, missing_capability},
                 authorize(<<"mri:topic:io.example/acme/_org/ord/*">>, topic(?TOPIC))).

another_topic_is_not_answered_test() ->
    ?assertEqual({error, missing_capability},
                 authorize(?TOPIC_GRANT, topic(<<"io.example/acme/_org/orders/shipped_v1">>))).

a_procedure_grant_does_not_answer_a_topic_test() ->
    ?assertEqual({error, missing_capability},
                 authorize(<<"mri:proc:io.example/acme/_org/orders/placed_v1">>, topic(?TOPIC))),
    ?assertEqual({error, missing_capability}, authorize(?PROC_PREFIX, topic(?TOPIC))).

an_org_grant_does_not_answer_a_topic_test() ->
    ?assertEqual({error, missing_capability}, authorize(<<"mri:org:io.example/acme">>, topic(?TOPIC))).

a_topic_grant_does_not_answer_a_procedure_test() ->
    ?assertEqual({error, missing_capability},
                 authorize(<<"mri:topic:io.example/acme/api.users.get">>, procedure(?PROCEDURE))),
    ?assertEqual({error, missing_capability}, authorize(<<"mri:topic:io.example/acme/*">>, procedure(?PROCEDURE))).

%% The topic names its realm, which must be the one whose id the request carries.
a_topic_of_another_realm_is_refused_test() ->
    ?assertEqual({error, wrong_realm},
                 authorize(?TOPIC_GRANT, (topic(?TOPIC))#{realm := crypto:hash(sha256, <<"io.other">>)})),
    ?assertEqual({error, wrong_realm},
                 authorize(<<"mri:realm:io.example">>, topic(<<"io.other/acme/_org/orders/placed_v1">>))).

%% A system topic has no realm segment, so no realm's grant answers it.
a_topic_without_a_realm_is_refused_test() ->
    ?assertEqual({error, topic_without_realm}, authorize(<<"mri:realm:io.example">>, topic(<<"_mesh.presence">>))),
    ?assertEqual({error, topic_without_realm}, authorize(<<"mri:realm:io.example">>, topic(<<"io.example/">>))).

a_topic_whose_realm_name_is_not_canonical_is_refused_test() ->
    ?assertEqual({error, realm_name_not_canonical},
                 authorize(<<"mri:realm:io.example">>, topic(<<"IO.example/acme/_org/orders/placed_v1">>))).

%% A request is for one topic or one procedure, never a prefix and never both.
a_request_naming_a_prefix_is_malformed_test() ->
    ?assertEqual({error, malformed}, authorize(<<"mri:realm:io.example">>, topic(<<"io.example/acme/*">>))),
    ?assertEqual({error, malformed}, authorize(<<"mri:realm:io.example">>, procedure(<<"acme/*">>))).

a_request_naming_a_procedure_and_a_topic_is_malformed_test() ->
    ?assertEqual({error, malformed},
                 authorize(<<"mri:realm:io.example">>, (topic(?TOPIC))#{procedure => ?PROCEDURE})).

%% A request without its realm is malformed, never a context with no request, which would check the token alone.
a_request_without_a_realm_is_malformed_test() ->
    ?assertEqual({error, malformed}, authorize(<<"mri:realm:io.example">>, #{topic => ?TOPIC})),
    ?assertEqual({error, malformed}, authorize(<<"mri:realm:io.example">>, #{procedure => ?PROCEDURE})),
    ?assertEqual({error, malformed},
                 authorize(<<"mri:realm:io.example">>, #{topic => ?TOPIC, procedure => ?PROCEDURE})),
    ?assertEqual({error, malformed}, authorize(<<"mri:realm:io.example">>, #{topic => ?TOPIC, realm => <<1, 2>>})).

a_procedure_prefix_answers_a_procedure_below_it_test() ->
    ?assertMatch({ok, _}, authorize(?PROC_PREFIX, procedure(?PROCEDURE))),
    ?assertEqual({error, missing_capability},
                 authorize(<<"mri:proc:io.example/acme/billing/*">>, procedure(?PROCEDURE))).

%%------------------------------------------------------------------
%% Delegating a prefix
%%------------------------------------------------------------------

%% The realm key grants an org key a topic prefix, which hands the caller one topic below it.
a_prefix_delegated_down_to_one_topic_authorizes_test() ->
    ?assertMatch({ok, _}, delegated(<<"mri:topic:io.example/acme/*">>, ?TOPIC_GRANT)).

a_delegate_cannot_widen_a_prefix_test() ->
    ?assertEqual({error, grants_more_than_proof}, delegated(?TOPIC_PREFIX, <<"mri:topic:io.example/acme/*">>)).

a_delegate_cannot_turn_a_procedure_prefix_into_a_topic_test() ->
    ?assertEqual({error, grants_more_than_proof}, delegated(?PROC_PREFIX, ?TOPIC_GRANT)).

%%------------------------------------------------------------------
%% The world under test
%%------------------------------------------------------------------

topic(Topic) ->
    #{topic => Topic, realm => crypto:hash(sha256, ?REALM)}.

procedure(Procedure) ->
    #{procedure => Procedure, realm => crypto:hash(sha256, ?REALM)}.

%% The realm key grants the caller one capability, authorized under the realm-member policy for it.
authorize(With, Request) ->
    RealmKey = key(realm),
    Caller = macula_test_identity:node_id(),
    Token = mint(RealmKey, Caller, With, #{}),
    macula_ucan:authorize(Token, {realm_member_required, macula_node_keys:key_id(RealmKey), ?PUBLISH},
                          maps:merge(#{caller => Caller, profile => pq_pure, now => now_s()}, Request)).

%% The realm key grants an org key ParentWith; the org key hands the caller ChildWith; the request is for ?TOPIC.
delegated(ParentWith, ChildWith) ->
    RealmKey = key(realm),
    OrgKey = key(org),
    OrgId = macula_node_keys:node_id(macula_node_keys:public_key(OrgKey), pq_pure),
    Caller = macula_test_identity:node_id(),
    Proof = mint(RealmKey, OrgId, ParentWith, #{}),
    Leaf = mint(OrgKey, Caller, ChildWith, #{prf => [macula_ucan:proof_id(Proof)]}),
    macula_ucan:authorize(Leaf, {realm_member_required, macula_node_keys:key_id(RealmKey), ?PUBLISH},
                          maps:merge(#{caller => Caller, profile => pq_pure, now => now_s(),
                                       proofs => #{macula_ucan:proof_id(Proof) => Proof}},
                                     topic(?TOPIC))).

key(Purpose) ->
    {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
    Key.

mint(Key, Audience, With, Opts) ->
    macula_ucan:create(Key, Audience, [#{with => With, can => ?PUBLISH}], maps:merge(#{exp => later()}, Opts)).

now_s() -> erlang:system_time(second).
later() -> now_s() + 3600.
