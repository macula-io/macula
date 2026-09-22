%% EUnit tests for macula:provider_authorization/3,4 — the pool's own D25
%% chain resolved from the DHT, verified against the realm key the pool
%% pins, as the `authorization' opt publish_advertisement/5 and
%% advertise_direct/6,7 embed in the direct-dial record. advertise/5
%% resolves the same chain for the wire frame; this export is what a
%% caller that publishes the record itself uses, so a provider's
%% direct-dial DHT record can carry the authorization the station
%% refuses to store it without.
%%
%% The resolution's DHT calls are injected as the `provider_io/0' seam
%% entries in `Opts' — no module is replaced, which is what
%% macula_shared_module_mocks_tests enforces. The records and
%% signatures are real.
-module(macula_provider_authorization_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<16#5e5f:256>>).
-define(ORG, <<"acme">>).
-define(PROCEDURE, <<"acme/svc.do">>).

%%--------------------------------------------------------------------
%% Setup: one real chain, injected through the seam.
%%--------------------------------------------------------------------

provider_authorization_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(macula), setup_keys() end,
     fun(_K) -> ok end,
     fun(K) ->
         [{spawn, fun() -> Test(K) end}
          || Test <- [fun resolves_the_chain_as_the_publish_opt/1,
                      fun refuses_a_procedure_without_an_org_namespace/1,
                      fun refuses_when_the_org_directory_is_missing/1,
                      fun refuses_when_the_delegation_is_missing/1,
                      fun refuses_without_a_pinned_realm_key/1,
                      fun refuses_a_chain_that_does_not_verify/1]]
     end}.

setup_keys() ->
    Profile = profile(),
    {ok, RealmKey} = macula_node_keys:generate(realm, Profile),
    {ok, OrgKey} = macula_node_keys:generate(org, Profile),
    {ok, IdKey} = macula_node_keys:generate(identity, Profile,
                                            #{puzzle_difficulty => 0}),
    {ok, NodeId} = macula_node_keys:node_id(IdKey),
    OrgKeyId = macula_node_keys:key_id(OrgKey),
    OrgDir = macula_record:sign(
               macula_record:org_directory(?REALM, ?ORG, OrgKeyId),
               RealmKey),
    Deleg = macula_record:sign(
              macula_record:procedure_delegation(OrgKeyId, NodeId), OrgKey),
    #{realm_key => RealmKey, org_key => OrgKey, id_key => IdKey,
      node_id => NodeId, org_key_id => OrgKeyId,
      org_dir => OrgDir, deleg => Deleg,
      profile => Profile}.

profile() ->
    {ok, P} = macula_crypto_profile:configured(),
    P.

%%--------------------------------------------------------------------
%% The happy path
%%--------------------------------------------------------------------

resolves_the_chain_as_the_publish_opt(K) ->
    with_io(K, #{}, fun(Opts) ->
        {ok, Authorization} =
            macula:provider_authorization(self(), ?REALM, ?PROCEDURE, Opts),
        %% The exact shape publish_advertisement/5's `authorization' opt
        %% takes: the two records' encoded wire forms, each verifying
        %% back to the record it resolved.
        #{org_directory := OrgDirWire,
          procedure_delegation := DelegWire} = Authorization,
        ?assertEqual({ok, maps:get(org_dir, K)},
                     macula_record:verify(OrgDirWire, maps:get(profile, K))),
        ?assertEqual({ok, maps:get(deleg, K)},
                     macula_record:verify(DelegWire, maps:get(profile, K)))
    end).

%%--------------------------------------------------------------------
%% The refusals
%%--------------------------------------------------------------------

refuses_a_procedure_without_an_org_namespace(K) ->
    with_io(K, #{}, fun(Opts) ->
        ?assertEqual({error, {provider_authorization, no_org_namespace}},
                     macula:provider_authorization(self(), ?REALM, <<"bare">>,
                                                   Opts))
    end).

refuses_when_the_org_directory_is_missing(K) ->
    with_io(K, #{find_record => find_stub(K, org_directory)}, fun(Opts) ->
        ?assertEqual({error, {provider_authorization, {org_directory, not_found}}},
                     macula:provider_authorization(self(), ?REALM, ?PROCEDURE,
                                                   Opts))
    end).

refuses_when_the_delegation_is_missing(K) ->
    with_io(K, #{find_record => find_stub(K, procedure_delegation)}, fun(Opts) ->
        ?assertEqual({error, {provider_authorization,
                              {procedure_delegation, not_found}}},
                     macula:provider_authorization(self(), ?REALM, ?PROCEDURE,
                                                   Opts))
    end).

refuses_without_a_pinned_realm_key(K) ->
    with_io(K, #{realm_key => fun(_Pool, _Realm) -> none end}, fun(Opts) ->
        ?assertEqual({error, {provider_authorization, no_realm_key}},
                     macula:provider_authorization(self(), ?REALM, ?PROCEDURE,
                                                   Opts))
    end).

%% A delegation naming an advertiser other than the pool's node id
%% verifies as a record but fails the chain — the advertisement never
%% leaves without a chain the pool can check. (macula_record:sign/2
%% refuses to sign a delegation whose signer key the directory does not
%% name, so the bad chain is a wrong ADVERTISER, not a wrong signer.)
refuses_a_chain_that_does_not_verify(K) ->
    {ok, Other} = macula_node_keys:generate(identity, maps:get(profile, K),
                                            #{puzzle_difficulty => 0}),
    {ok, OtherNodeId} = macula_node_keys:node_id(Other),
    Deleg = macula_record:sign(
              macula_record:procedure_delegation(maps:get(org_key_id, K),
                                                 OtherNodeId),
              maps:get(org_key, K)),
    with_io(K#{deleg => Deleg}, #{}, fun(Opts) ->
        ?assertMatch({error, {provider_authorization, {error, _}}},
                     macula:provider_authorization(self(), ?REALM, ?PROCEDURE,
                                                   Opts))
    end).

%%--------------------------------------------------------------------
%% The seam
%%--------------------------------------------------------------------

%% The `Opts' handed to provider_authorization/4: the provider_io seam
%% entries the resolution reads its DHT calls from, each overridable
%% per case.
with_io(K, Overrides, Fun) ->
    Base = #{status => fun(_Pool) ->
                           {ok, #{self_node_id => maps:get(node_id, K)}}
                       end,
             find_record => find_stub(K, none),
             %% As the pool's own bounded signing does it: one clock
             %% read stamps the record and ends it at the bound.
             sign_node_record => fun(_Pool, Unsigned, #{not_after := NotAfter}) ->
                                     macula_record:refresh(
                                       Unsigned, maps:get(id_key, K), NotAfter)
                                 end,
             realm_key => fun(_Pool, _Realm) ->
                              {ok, macula_node_keys:public_key(
                                     maps:get(realm_key, K))}
                          end},
    Fun(maps:merge(Base, Overrides)).

%% The DHT lookups underneath the resolution: the verified record (the
%% facade's own find_record/2 contract), or a not_found for a missing
%% piece (or anything else).
find_stub(K, Missing) ->
    OrgDirKey = macula_record:org_directory_key(?REALM, ?ORG),
    DelegKey = macula_record:procedure_delegation_key(maps:get(org_key_id, K),
                                                      maps:get(node_id, K)),
    fun(_Pool, Key) ->
        find_reply(Key, OrgDirKey, DelegKey, Missing, K)
    end.

find_reply(OrgDirKey, OrgDirKey, _DelegKey, org_directory, _K) ->
    {error, not_found};
find_reply(DelegKey, _OrgDirKey, DelegKey, procedure_delegation, _K) ->
    {error, not_found};
find_reply(OrgDirKey, OrgDirKey, _DelegKey, _Missing, K) ->
    verified(maps:get(org_dir, K));
find_reply(DelegKey, _OrgDirKey, DelegKey, _Missing, K) ->
    verified(maps:get(deleg, K));
find_reply(_Key, _OrgDirKey, _DelegKey, _Missing, _K) ->
    {error, not_found}.

%% The facade's find_record/2 returns records already verified under
%% the node's profile — the seam stub answers in the same shape.
verified(Record) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    macula_record:verify(macula_record:encode(Record), Profile).
