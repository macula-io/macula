%% Call classes (macula#147): a service declares who may call each procedure it
%% serves, and the provider refuses anyone the class does not admit.
%%
%% These are the tests the threat model (issue #147, section 7) names: the class
%% resolver's own mapping, the facade refusing an undeclared procedure, the
%% `any' gap closing (a bare member token refused where the class is `role'),
%% and the two delegation edges the model states rather than hides -- a bare
%% member and a role holder each admit their delegates.
%%
%% The end-to-end path through a real link (a token on an injected CALL) lives
%% with the link's other authorization tests, in `macula_station_link_tests'.
-module(macula_call_class_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM_NAME, <<"io.example">>).
-define(REALM, crypto:hash(sha256, ?REALM_NAME)).
-define(PROCEDURE, <<"acme/realm.only">>).
-define(PROC_MRI, <<"mri:proc:io.example/acme/realm.only">>).
-define(REALM_MRI, <<"mri:realm:io.example">>).
-define(CALL, <<"rpc/call">>).

%%------------------------------------------------------------------
%% Resolving a class at the pool
%%------------------------------------------------------------------

%% `members' is the realm-key policy that admits a capability of any `can'
%% covering the procedure: a member's realm capability covers every procedure
%% in the realm, which is what membership means.
members_resolves_to_the_realm_policy_test() ->
    #{realm_key := RealmKey} = parties(),
    KeyId = macula_node_keys:key_id(RealmKey),
    ?assertEqual({ok, {realm_member_required, KeyId, any}},
                 macula_client:caller_policy({callers, members}, ?REALM, #{?REALM => carried(RealmKey)})).

%% `role' demands exactly the capability a role expansion mints, which is why it
%% is decidable from the token on the CALL with nothing looked up.
role_demands_the_call_capability_test() ->
    #{realm_key := RealmKey} = parties(),
    KeyId = macula_node_keys:key_id(RealmKey),
    ?assertEqual({ok, {realm_member_required, KeyId, ?CALL}},
                 macula_client:caller_policy({callers, role}, ?REALM, #{?REALM => carried(RealmKey)})).

%% A named-nodes class names its callers itself, so it needs no realm key.
nodes_need_no_realm_key_test() ->
    NodeIds = [<<1:256>>, <<2:256>>],
    ?assertEqual({ok, {nodes, NodeIds}},
                 macula_client:caller_policy({callers, {nodes, NodeIds}}, ?REALM, #{})).

%% A class that rests on realm membership cannot be enforced in a realm whose key
%% this pool never pinned, so it is refused rather than served open.
a_member_class_with_no_pinned_realm_key_is_refused_test() ->
    ?assertEqual({error, {call_class, no_realm_key}},
                 macula_client:caller_policy({callers, members}, ?REALM, #{})),
    ?assertEqual({error, {call_class, no_realm_key}},
                 macula_client:caller_policy({callers, role}, ?REALM, #{})).

a_class_naming_no_nodes_is_refused_test() ->
    ?assertEqual({error, {call_class, {nodes, empty}}},
                 macula_client:caller_policy({callers, {nodes, []}}, ?REALM, #{})),
    ?assertEqual({error, {call_class, {nodes, not_a_list}}},
                 macula_client:caller_policy({callers, {nodes, <<1:256>>}}, ?REALM, #{})),
    ?assertEqual({error, {call_class, {nodes, not_node_ids}}},
                 macula_client:caller_policy({callers, {nodes, [<<"not-a-node-id">>]}}, ?REALM, #{})).

%% An improper list is not a list of node ids. `[_ | _]' matches an improper
%% cons as happily as a proper one, so without the `is_list' guard this reached
%% `lists:all/2', which raises on `[a | b]' -- inside the pool process, so the
%% whole pool went down for one bad declaration.
an_improper_node_list_is_refused_test() ->
    ?assertEqual({error, {call_class, {nodes, not_a_list}}},
                 macula_client:caller_policy({callers, {nodes, [<<1:256>> | <<2:256>>]}}, ?REALM, #{})).

%% The refusal happens in the pool, so prove the pool answers it and stays up:
%% a crash here would take every subscription, link and advertisement with it.
an_improper_node_list_does_not_take_the_pool_down_test_() ->
    {timeout, 10, fun() ->
        {ok, _} = application:ensure_all_started(macula),
        {ok, Pool} = macula_client:connect([], #{}),
        ?assertEqual({error, {call_class, {nodes, not_a_list}}},
                     macula_client:advertise(Pool, ?REALM, ?PROCEDURE, fun(_) -> {ok, 1} end,
                                             {callers, {nodes, [<<1:256>> | <<2:256>>]}},
                                             undefined, all, undefined)),
        ?assertMatch({ok, _}, macula_client:status(Pool)),
        ok = macula_client:close(Pool)
    end}.

a_class_that_is_not_one_is_refused_test() ->
    ?assertEqual({error, {call_class, {unknown, elevated}}},
                 macula_client:caller_policy({callers, elevated}, ?REALM, #{})).

%% `open' is the explicit opt-out and is not a class: it passes through untouched.
an_explicit_opt_out_is_not_a_class_test() ->
    ?assertEqual({ok, open}, macula_client:caller_policy(open, ?REALM, #{})).

%%------------------------------------------------------------------
%% The facade: no class, no advertisement
%%------------------------------------------------------------------

%% There is no default class. A procedure that names none is refused at the call
%% boundary, before anything is resolved or registered -- so a service cannot
%% ship a silently open procedure (threat model section 5.4).
an_advertise_that_declares_no_class_is_refused_test() ->
    ?assertEqual({error, {auth, required}},
                 macula:advertise(self(), ?REALM, ?PROCEDURE, fun(_) -> {ok, 1} end, #{})).

%%------------------------------------------------------------------
%% What the verifier accepts at each class
%%------------------------------------------------------------------

%% THE GAP THIS CLOSES. A bare member token is a realm-signed token whose one
%% capability is `member/email-verified' on `mri:realm:<realm>', and a realm
%% capability covers every procedure in the realm. Under `any' (today's
%% `{realm_member_required, Key, any}') it is admitted to every procedure. Where
%% the class is `role' it must be refused, and where the class is `members' it
%% must still be admitted.
the_any_gap_closes_test() ->
    #{realm_key := RealmKey, holder_id := Caller} = parties(),
    KeyId = macula_node_keys:key_id(RealmKey),
    Bare = mint(RealmKey, Caller, ?REALM_MRI, <<"member/email-verified">>, #{}),
    ?assertEqual({error, missing_capability},
                 verdict(Bare, [], {realm_member_required, KeyId, ?CALL}, Caller)),
    ?assertMatch({ok, _}, verdict(Bare, [], {realm_member_required, KeyId, any}, Caller)).

%% A role holder's token carries `rpc/call' on the procedure's MRI, and is
%% admitted where the class is `role'.
a_role_holder_is_admitted_at_role_test() ->
    #{realm_key := RealmKey, holder_id := Caller} = parties(),
    KeyId = macula_node_keys:key_id(RealmKey),
    RoleToken = mint(RealmKey, Caller, ?PROC_MRI, ?CALL, #{}),
    ?assertMatch({ok, _}, verdict(RoleToken, [], {realm_member_required, KeyId, ?CALL}, Caller)).

%% The delegation edge the threat model states (section 2): a capability keeps
%% what its parent granted when the `can' matches and the `with' is covered, so a
%% role holder can hand its capability to another node, and that node is admitted
%% where the class is `role' -- as it is where the class is `members'. `{nodes, _}'
%% is the exact class, and it never reaches this verifier at all: the link compares
%% the caller the request verified against the list (macula_station_link_tests).
a_delegate_of_a_role_holder_is_admitted_at_role_test() ->
    #{realm_key := RealmKey, holder_key := HolderKey, holder_id := Holder,
      delegate_id := Delegate} = parties(),
    KeyId = macula_node_keys:key_id(RealmKey),
    Parent = mint(RealmKey, Holder, ?PROC_MRI, ?CALL, #{}),
    Child = mint(HolderKey, Delegate, ?PROC_MRI, ?CALL, #{prf => [macula_ucan:proof_id(Parent)]}),
    Proofs = [Parent],
    ?assertMatch({ok, _}, verdict(Child, Proofs, {realm_member_required, KeyId, ?CALL}, Delegate)),
    ?assertMatch({ok, _}, verdict(Child, Proofs, {realm_member_required, KeyId, any}, Delegate)).

%% A capability for another procedure's MRI does not admit a call here, at either
%% class: the class is a per-procedure declaration, not a per-realm one.
a_capability_for_another_procedure_does_not_admit_test() ->
    #{realm_key := RealmKey, holder_id := Caller} = parties(),
    KeyId = macula_node_keys:key_id(RealmKey),
    Other = mint(RealmKey, Caller, <<"mri:proc:io.example/acme/other">>, ?CALL, #{}),
    ?assertEqual({error, missing_capability},
                 verdict(Other, [], {realm_member_required, KeyId, ?CALL}, Caller)).

%%------------------------------------------------------------------
%% Fixtures
%%------------------------------------------------------------------

%% The pool's `realm_keys' holds each realm key as carried, a public-key binary,
%% which is what `macula_node_keys:key_id/2' hashes into a key id.
carried(Key) -> macula_node_keys:public_key(Key).

parties() ->
    {ok, RealmKey} = macula_node_keys:generate(realm, pq_pure),
    {ok, HolderKey} = macula_node_keys:generate(identity, pq_pure),
    {ok, HolderId} = macula_node_keys:node_id(HolderKey),
    {ok, DelegateKey} = macula_node_keys:generate(identity, pq_pure),
    {ok, DelegateId} = macula_node_keys:node_id(DelegateKey),
    _ = DelegateKey,
    #{realm_key => RealmKey, holder_key => HolderKey, holder_id => HolderId,
      delegate_id => DelegateId}.

mint(Key, Audience, With, Can, Opts) ->
    macula_ucan:create(Key, Audience, [#{with => With, can => Can}],
                       maps:merge(#{exp => erlang:system_time(second) + 3_600}, Opts)).

verdict(Token, Proofs, Policy, Caller) ->
    macula_ucan:authorize(Token, Policy,
                          #{caller => Caller, profile => pq_pure,
                            now => erlang:system_time(second),
                            realm => ?REALM, procedure => ?PROCEDURE,
                            proofs => maps:from_list([{macula_ucan:proof_id(P), P} || P <- Proofs])}).
