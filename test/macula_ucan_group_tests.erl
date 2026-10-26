%% EUnit tests for a group request in macula_ucan (macula#129): a sealed group's key opens the group's prefix and every
%% topic below it, so a pull of it is a request for the whole group, answered only when the token's grants cover both
%% `mri:topic:P' and `mri:topic:P/*'. Every grant that answers is checked up the chain, so a leaf cannot add the half
%% its proof never granted.
-module(macula_ucan_group_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<"io.example">>).
-define(GROUP, <<"io.example/acme/orders">>).
-define(EXACT, <<"mri:topic:io.example/acme/orders">>).
-define(BELOW, <<"mri:topic:io.example/acme/orders/*">>).
-define(SUBSCRIBE, <<"pubsub/subscribe">>).

%%------------------------------------------------------------------
%% A group request
%%------------------------------------------------------------------

the_group_and_everything_below_it_answer_test() ->
    {ok, _} = Ok = authorize([?EXACT, ?BELOW], group(?GROUP)),
    ?assertMatch({ok, #{<<"cap">> := [_, _]}}, Ok).

a_parent_prefix_answers_the_whole_group_test() ->
    ?assertMatch({ok, _}, authorize([<<"mri:topic:io.example/acme/*">>], group(?GROUP))).

a_realm_grant_answers_the_whole_group_test() ->
    ?assertMatch({ok, _}, authorize([<<"mri:realm:io.example">>], group(?GROUP))).

%% `P/*' covers below P, never P itself (macula#127).
the_prefix_below_the_group_alone_does_not_answer_test() ->
    ?assertEqual({error, missing_capability}, authorize([?BELOW], group(?GROUP))).

the_group_topic_alone_does_not_answer_test() ->
    ?assertEqual({error, missing_capability}, authorize([?EXACT], group(?GROUP))).

a_prefix_sharing_part_of_a_segment_answers_nothing_test() ->
    ?assertEqual({error, missing_capability},
                 authorize([<<"mri:topic:io.example/acme/ord/*">>, <<"mri:topic:io.example/acme/ord">>],
                           group(?GROUP))).

another_group_is_not_answered_test() ->
    ?assertEqual({error, missing_capability},
                 authorize([<<"mri:topic:io.example/acme/chat">>, <<"mri:topic:io.example/acme/chat/*">>],
                           group(?GROUP))).

a_grant_under_another_can_does_not_answer_test() ->
    ?assertEqual({error, missing_capability},
                 authorize_can(<<"pubsub/publish">>, [?EXACT, ?BELOW], group(?GROUP))).

a_group_of_another_realm_is_refused_test() ->
    ?assertEqual({error, wrong_realm},
                 authorize([?EXACT, ?BELOW], (group(?GROUP))#{realm := crypto:hash(sha256, <<"io.other">>)})).

%% A group is named as a topic is, realm first, never as a prefix, and alone.
a_group_naming_a_prefix_is_malformed_test() ->
    ?assertEqual({error, malformed}, authorize([<<"mri:realm:io.example">>], group(<<"io.example/acme/*">>))).

a_group_without_a_path_is_refused_test() ->
    ?assertEqual({error, topic_without_realm}, authorize([<<"mri:realm:io.example">>], group(<<"io.example">>))).

a_group_beside_a_topic_or_a_procedure_is_malformed_test() ->
    ?assertEqual({error, malformed},
                 authorize([<<"mri:realm:io.example">>], (group(?GROUP))#{topic => <<"io.example/acme/orders/x">>})),
    ?assertEqual({error, malformed},
                 authorize([<<"mri:realm:io.example">>], (group(?GROUP))#{procedure => <<"acme/api.get">>})).

a_group_without_a_realm_is_malformed_test() ->
    ?assertEqual({error, malformed}, authorize([<<"mri:realm:io.example">>], #{group => ?GROUP})).

%%------------------------------------------------------------------
%% Delegating a group
%%------------------------------------------------------------------

a_group_delegated_whole_authorizes_test() ->
    ?assertMatch({ok, _}, delegated([?EXACT, ?BELOW], [?EXACT, ?BELOW])),
    ?assertMatch({ok, _}, delegated([<<"mri:topic:io.example/acme/*">>], [?EXACT, ?BELOW])).

%% Both halves are checked against the proof: a leaf cannot add the one its proof never granted.
a_delegate_cannot_add_the_half_its_proof_lacks_test() ->
    ?assertEqual({error, grants_more_than_proof}, delegated([?EXACT], [?EXACT, ?BELOW])),
    ?assertEqual({error, grants_more_than_proof}, delegated([?BELOW], [?EXACT, ?BELOW])).

%%------------------------------------------------------------------
%% The world under test
%%------------------------------------------------------------------

group(Group) ->
    #{group => Group, realm => crypto:hash(sha256, ?REALM)}.

authorize(Withs, Request) ->
    authorize_can(?SUBSCRIBE, Withs, Request).

%% The realm key grants the caller Withs under Can, authorized under the realm-member policy for pubsub/subscribe.
authorize_can(Can, Withs, Request) ->
    RealmKey = key(realm),
    Caller = macula_test_identity:node_id(),
    Token = mint(RealmKey, Caller, Can, Withs, #{}),
    macula_ucan:authorize(Token, {realm_member_required, macula_node_keys:key_id(RealmKey), ?SUBSCRIBE},
                          maps:merge(#{caller => Caller, profile => pq_pure, now => now_s()}, Request)).

%% The realm key grants an org key ParentWiths; the org key hands the caller ChildWiths; the request is for ?GROUP.
delegated(ParentWiths, ChildWiths) ->
    RealmKey = key(realm),
    OrgKey = key(org),
    OrgId = macula_node_keys:node_id(macula_node_keys:public_key(OrgKey), pq_pure),
    Caller = macula_test_identity:node_id(),
    Proof = mint(RealmKey, OrgId, ?SUBSCRIBE, ParentWiths, #{}),
    Leaf = mint(OrgKey, Caller, ?SUBSCRIBE, ChildWiths, #{prf => [macula_ucan:proof_id(Proof)]}),
    macula_ucan:authorize(Leaf, {realm_member_required, macula_node_keys:key_id(RealmKey), ?SUBSCRIBE},
                          maps:merge(#{caller => Caller, profile => pq_pure, now => now_s(),
                                       proofs => #{macula_ucan:proof_id(Proof) => Proof}},
                                     group(?GROUP))).

key(Purpose) ->
    {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
    Key.

mint(Key, Audience, Can, Withs, Opts) ->
    macula_ucan:create(Key, Audience, [#{with => With, can => Can} || With <- Withs],
                       maps:merge(#{exp => later()}, Opts)).

now_s() -> erlang:system_time(second).
later() -> now_s() + 3600.
