%% EUnit tests for macula:org_member/4,5 and macula_record:verify_membership/5:
%% whether a node is an admitted member of an org in a realm, read from the
%% same D25 chain a provider's advertisement carries (the realm-signed
%% org_directory and the org-signed procedure_delegation naming the node).
%%
%% It exists for a SUBSCRIBER. macula verifies who signed a publication, not
%% who may publish on a topic, so a service that must only hear an org's own
%% nodes asks this about the delivery's `publisher'. Every refusal below is a
%% way a node that is not a member could otherwise pass.
%%
%% The DHT calls are injected as seam entries in `Opts', as in
%% macula_provider_authorization_tests: no module is replaced, and the
%% records and signatures are real.
-module(macula_org_member_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<16#5e5f:256>>).
-define(ORG, <<"acme">>).

org_member_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(macula), setup_keys() end,
     fun(_K) -> ok end,
     fun(K) ->
         [{spawn, fun() -> Test(K) end}
          || Test <- [fun a_member_is_answered_with_the_chain_expiry/1,
                      fun a_node_without_a_delegation_is_not_a_member/1,
                      fun a_missing_org_directory_is_unavailable_not_a_refusal/1,
                      fun a_delegation_the_lookup_refused_is_not_a_member/1,
                      fun a_delegation_naming_another_node_is_refused/1,
                      fun a_delegation_signed_by_another_org_key_is_refused/1,
                      fun a_directory_for_another_org_is_refused/1,
                      fun a_directory_for_another_realm_is_refused/1,
                      fun a_directory_not_signed_by_the_realm_key_is_refused/1,
                      fun a_withdrawn_delegation_is_not_a_member/1,
                      fun a_withdrawn_org_directory_has_no_members/1,
                      fun an_expired_chain_is_refused/1,
                      fun without_a_pinned_realm_key_it_cannot_say/1,
                      fun a_failed_lookup_is_unavailable_not_a_refusal/1]]
     end}.

setup_keys() ->
    Profile = profile(),
    {ok, RealmKey} = macula_node_keys:generate(realm, Profile),
    {ok, OrgKey} = macula_node_keys:generate(org, Profile),
    {ok, IdKey} = macula_node_keys:generate(identity, Profile, #{puzzle_difficulty => 0}),
    {ok, NodeId} = macula_node_keys:node_id(IdKey),
    OrgKeyId = macula_node_keys:key_id(OrgKey),
    #{realm_key => RealmKey, org_key => OrgKey, org_key_id => OrgKeyId, node_id => NodeId,
      org_dir => macula_record:sign(macula_record:org_directory(?REALM, ?ORG, OrgKeyId), RealmKey),
      deleg => macula_record:sign(macula_record:procedure_delegation(OrgKeyId, NodeId), OrgKey),
      profile => Profile}.

profile() ->
    {ok, P} = macula_crypto_profile:configured(),
    P.

%%--------------------------------------------------------------------
%% The answer
%%--------------------------------------------------------------------

%% A member is answered with the earlier of the two records' expiries: the
%% moment after which the answer must be asked again, which is how a
%% revocation reaches a caller that caches it.
a_member_is_answered_with_the_chain_expiry(#{org_dir := Dir, deleg := Deleg, node_id := NodeId} = K) ->
    Earliest = min(macula_record:expires_at(Dir), macula_record:expires_at(Deleg)),
    ?assertEqual({ok, Earliest}, member(K, #{}, NodeId)).

a_node_without_a_delegation_is_not_a_member(K) ->
    ?assertEqual({error, {not_a_member, {procedure_delegation, not_found}}},
                 member(K, #{}, <<7:256>>)).

%% The org is the caller's own, so an empty directory slot is a station roll,
%% not an answer: a member must not be refused for the minute it lasts.
a_missing_org_directory_is_unavailable_not_a_refusal(#{node_id := NodeId} = K) ->
    ?assertEqual({error, {unavailable, {org_directory, not_found}}},
                 member(K#{org_dir => missing}, #{}, NodeId)).

%% A revoked node whose tombstone never landed: the lookup finds its delegation
%% and refuses it as expired. That is an answer, and it is no.
a_delegation_the_lookup_refused_is_not_a_member(#{node_id := NodeId} = K) ->
    Find = find_stub(K),
    DelegKey = macula_record:procedure_delegation_key(maps:get(org_key_id, K), NodeId),
    Refusing = fun(_Pool, Key) when Key =:= DelegKey -> {error, expired};
                  (Pool, Key) -> Find(Pool, Key) end,
    ?assertEqual({error, {not_a_member, {procedure_delegation, expired}}},
                 member(K, #{find_record => Refusing}, NodeId)).

%%--------------------------------------------------------------------
%% The chain comparisons that stop a stranger
%%--------------------------------------------------------------------

%% A real delegation of the org, for another node, stored under the asked
%% node's key: the chain verifies as records and must still be refused,
%% because it does not name the node being asked about.
a_delegation_naming_another_node_is_refused(#{org_key := OrgKey, org_key_id := OrgKeyId, node_id := NodeId} = K) ->
    Other = macula_record:sign(macula_record:procedure_delegation(OrgKeyId, <<9:256>>), OrgKey),
    ?assertEqual({error, {not_a_member, delegation_mismatch}}, member(K#{deleg => Other}, #{}, NodeId)).

%% A delegation naming the node, signed by an org key the directory does not
%% hold: any node can mint itself an org key, so this is the forgery a
%% stranger can actually make.
a_delegation_signed_by_another_org_key_is_refused(#{profile := Profile, node_id := NodeId} = K) ->
    {ok, Forger} = macula_node_keys:generate(org, Profile),
    Forged = macula_record:sign(
               macula_record:procedure_delegation(macula_node_keys:key_id(Forger), NodeId), Forger),
    ?assertEqual({error, {not_a_member, delegation_mismatch}}, member(K#{deleg => Forged}, #{}, NodeId)).

a_directory_for_another_org_is_refused(#{realm_key := RealmKey, org_key_id := OrgKeyId, node_id := NodeId} = K) ->
    Dir = macula_record:sign(macula_record:org_directory(?REALM, <<"other">>, OrgKeyId), RealmKey),
    ?assertEqual({error, {not_a_member, org_directory_wrong_org}}, member(K#{org_dir => Dir}, #{}, NodeId)).

a_directory_for_another_realm_is_refused(#{realm_key := RealmKey, org_key_id := OrgKeyId, node_id := NodeId} = K) ->
    Dir = macula_record:sign(macula_record:org_directory(<<1:256>>, ?ORG, OrgKeyId), RealmKey),
    ?assertEqual({error, {not_a_member, org_directory_wrong_realm}}, member(K#{org_dir => Dir}, #{}, NodeId)).

%% The forgery a stranger with a realm-shaped key of their own could make.
a_directory_not_signed_by_the_realm_key_is_refused(#{profile := Profile, org_key_id := OrgKeyId,
                                                     node_id := NodeId} = K) ->
    {ok, Forger} = macula_node_keys:generate(realm, Profile),
    Dir = macula_record:sign(macula_record:org_directory(?REALM, ?ORG, OrgKeyId), Forger),
    ?assertEqual({error, {not_a_member, org_directory_wrong_realm}}, member(K#{org_dir => Dir}, #{}, NodeId)).

%% The realm withdraws a lapsed member's delegation with a tombstone in the
%% same slot: what the lookup then finds is not a delegation.
a_withdrawn_delegation_is_not_a_member(#{org_key := OrgKey, deleg := Deleg, node_id := NodeId} = K) ->
    Tombstone = macula_record:sign(macula_record:tombstone(Deleg, revoked), OrgKey),
    ?assertEqual({error, {not_a_member, delegation_invalid}}, member(K#{deleg => Tombstone}, #{}, NodeId)).

%% The realm tombstones an org's directory when its last provider lapses: the
%% slot then holds no org key to look the delegation up under.
a_withdrawn_org_directory_has_no_members(#{realm_key := RealmKey, org_dir := Dir, node_id := NodeId} = K) ->
    Tombstone = macula_record:sign(macula_record:tombstone(Dir, revoked), RealmKey),
    ?assertEqual({error, {not_a_member, org_directory_invalid}}, member(K#{org_dir => Tombstone}, #{}, NodeId)).

%% The pure check, at a moment after the chain ends.
an_expired_chain_is_refused(#{org_dir := Dir, deleg := Deleg, node_id := NodeId, realm_key := RealmKey,
                              profile := Profile}) ->
    Later = max(macula_record:expires_at(Dir), macula_record:expires_at(Deleg))
            + macula_record:clock_tolerance_ms() + 1,
    Trust = #{profile => Profile, realm_key => macula_node_keys:public_key(RealmKey)},
    ?assertEqual({error, org_directory_invalid},
                 macula_record:verify_membership(macula_record:encode(Dir), macula_record:encode(Deleg),
                                                 #{realm_id => ?REALM, org => ?ORG, node_id => NodeId},
                                                 Trust, Later)).

%%--------------------------------------------------------------------
%% When it cannot say
%%--------------------------------------------------------------------

%% Not a refusal: the caller must be able to tell "this node is not a member"
%% from "I could not find out", or a station roll reads as a revocation.
without_a_pinned_realm_key_it_cannot_say(#{node_id := NodeId} = K) ->
    ?assertEqual({error, {unavailable, no_realm_key}},
                 member(K, #{realm_key => fun(_Pool, _Realm) -> none end}, NodeId)).

a_failed_lookup_is_unavailable_not_a_refusal(#{node_id := NodeId} = K) ->
    ?assertEqual({error, {unavailable, {org_directory, timeout}}},
                 member(K, #{find_record => fun(_Pool, _Key) -> {error, timeout} end}, NodeId)).

%%--------------------------------------------------------------------
%% The seam
%%--------------------------------------------------------------------

member(K, Overrides, NodeId) ->
    Base = #{find_record => find_stub(K),
             realm_key => fun(_Pool, _Realm) -> {ok, macula_node_keys:public_key(maps:get(realm_key, K))} end},
    macula:org_member(self(), ?REALM, ?ORG, NodeId, maps:merge(Base, Overrides)).

%% The DHT as the facade's find_record/2 answers it: a verified record, or
%% not_found. The delegation is stored under the asked node's slot, so a test
%% that swaps it in is a record a stranger could have put there.
find_stub(#{org_dir := Dir, deleg := Deleg, org_key_id := OrgKeyId, node_id := NodeId}) ->
    DirKey = macula_record:org_directory_key(?REALM, ?ORG),
    DelegKey = macula_record:procedure_delegation_key(OrgKeyId, NodeId),
    fun(_Pool, Key) -> found(Key, [{DirKey, Dir}, {DelegKey, Deleg}]) end.

found(Key, Slots) ->
    stored(proplists:get_value(Key, Slots, missing)).

stored(missing) -> {error, not_found};
stored(Record) -> macula_record:verify(macula_record:encode(Record), profile()).
