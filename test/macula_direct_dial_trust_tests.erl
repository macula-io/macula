%%% @doc Advertisement trust for macula_direct_dial.
%%%
%%% The resolve path takes a serving_station only from an advertisement that passes advertisement_trusted/2: the
%%% advertisement arrives verified under the node's crypto profile, it advertises the procedure in the realm the caller
%%% resolved, and its provider authorization verifies against the caller's realm trust (D25 item 6). Without the check,
%%% a node able to sign some record could point a caller at a real station it has no authority to name.
-module(macula_direct_dial_trust_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<16#11:256>>).
-define(PROCEDURE, <<"forecast_v1">>).
-define(STATION, <<16#33:256>>).

an_advertisement_for_the_resolved_procedure_is_trusted_test() ->
    ?assert(trusted(advertisement(?REALM, ?PROCEDURE, #{}), ?REALM, ?PROCEDURE)).

an_advertisement_for_another_procedure_is_not_trusted_test() ->
    ?assertNot(trusted(advertisement(?REALM, <<"other_v1">>, #{}), ?REALM, ?PROCEDURE)).

an_advertisement_for_another_realm_is_not_trusted_test() ->
    ?assertNot(trusted(advertisement(<<16#22:256>>, ?PROCEDURE, #{}), ?REALM, ?PROCEDURE)).

a_record_of_another_type_is_not_trusted_test() ->
    Key = identity_key(),
    NodeRecord = verified(macula_record:sign(macula_record:node_record(macula_node_keys:key_id(Key), [], 0), Key)),
    ?assertNot(trusted(NodeRecord, ?REALM, ?PROCEDURE)).

an_org_namespaced_procedure_without_an_authorization_is_not_trusted_test() ->
    Procedure = <<"acme/forecast_v1">>,
    ?assertNot(trusted(advertisement(?REALM, Procedure, #{}), ?REALM, Procedure)).

a_procedure_without_an_org_namespace_that_carries_an_authorization_is_not_trusted_test() ->
    Opts = #{authorization => #{org_directory => <<"not a record">>, procedure_delegation => <<"not a record">>}},
    ?assertNot(trusted(advertisement(?REALM, ?PROCEDURE, Opts), ?REALM, ?PROCEDURE)).

an_authorization_without_the_realm_trust_its_form_needs_is_not_trusted_test() ->
    Procedure = <<"acme/forecast_v1">>,
    Opts = #{authorization => #{org_directory => <<"not a record">>, procedure_delegation => <<"not a record">>}},
    ?assertNot(trusted(advertisement(?REALM, Procedure, Opts), ?REALM, Procedure)).

%% macula#128 (threat model T7): a caller checks that the provider's delegation names the procedure. Counting, the
%% default, keeps the candidate and counts it; refusing drops it.
a_delegation_that_omits_the_procedure_is_counted_then_refused_test_() ->
    {foreach,
     fun() -> rights_table(ets:whereis(macula_realm_rights)), ets:delete_all_objects(macula_realm_rights) end,
     fun(_) -> application:unset_env(macula, realm_rights) end,
     [{"counting keeps a provider whose delegation omits the procedure, and counts it",
       fun() ->
           {Adv, Trust} = delegated(<<"acme/forecast_v1">>, [<<"acme/other_v1">>]),
           ?assert(macula_direct_dial:advertisement_trusted(Adv, Trust)),
           ?assertEqual(1, maps:get({?REALM, serve, would_refuse}, macula_realm_rights:counters()))
       end},
      {"refusing drops it",
       fun() ->
           application:set_env(macula, realm_rights, #{?REALM => refusing}),
           {Adv, Trust} = delegated(<<"acme/forecast_v1">>, [<<"acme/other_v1">>]),
           ?assertNot(macula_direct_dial:advertisement_trusted(Adv, Trust)),
           ?assertEqual(1, maps:get({?REALM, serve, refused}, macula_realm_rights:counters()))
       end},
      {"refusing drops a delegation that names no procedures at all",
       fun() ->
           application:set_env(macula, realm_rights, #{?REALM => refusing}),
           {Adv, Trust} = delegated(<<"acme/forecast_v1">>, undefined),
           ?assertNot(macula_direct_dial:advertisement_trusted(Adv, Trust))
       end},
      {"refusing keeps a provider whose delegation names it",
       fun() ->
           application:set_env(macula, realm_rights, #{?REALM => refusing}),
           {Adv, Trust} = delegated(<<"acme/forecast_v1">>, [<<"acme/*">>]),
           ?assert(macula_direct_dial:advertisement_trusted(Adv, Trust)),
           ?assertEqual(1, maps:get({?REALM, serve, authorised}, macula_realm_rights:counters()))
       end}]}.

%%====================================================================
%% Helpers
%%====================================================================

%% The counter table outlives each test: a process that lives for the run owns it, as macula_peering_sup does.
rights_table(undefined) ->
    Self = self(),
    spawn(fun() -> ok = macula_realm_rights:new_table(), Self ! table_ready, receive never -> ok end end),
    receive table_ready -> ok end;
rights_table(_Tid) ->
    ok.

%% A provider's advertisement of Procedure under an org directory the realm signed and a delegation naming Listed
%% (none when undefined), and the caller's trust in that realm.
delegated(Procedure, Listed) ->
    {ok, RealmKey} = macula_node_keys:generate(realm, pq_pure),
    {ok, OrgKey} = macula_node_keys:generate(org, pq_pure),
    Provider = identity_key(),
    Dir = macula_record:sign(macula_record:org_directory(?REALM, <<"acme">>, macula_node_keys:key_id(OrgKey)),
                             RealmKey),
    DelOpts = maps:from_list([{procedures, Listed} || Listed =/= undefined]),
    Del = macula_record:sign(macula_record:procedure_delegation(macula_node_keys:key_id(OrgKey),
                                                                macula_node_keys:key_id(Provider), DelOpts), OrgKey),
    Authorization = #{org_directory => macula_record:encode(Dir), procedure_delegation => macula_record:encode(Del)},
    Unsigned = macula_record:procedure_advertisement(macula_node_keys:key_id(Provider), ?REALM, Procedure, [?STATION],
                                                     #{authorization => Authorization, ttl_ms => 300_000}),
    {verified(macula_record:sign(Unsigned, Provider)),
     #{realm => ?REALM, procedure => Procedure, profile => pq_pure,
       realm_key => macula_node_keys:public_key(RealmKey)}}.

trusted(Verified, Realm, Procedure) ->
    macula_direct_dial:advertisement_trusted(Verified, #{realm => Realm, procedure => Procedure, profile => pq_pure}).

identity_key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.

advertisement(Realm, Procedure, Opts) ->
    Key = identity_key(),
    Unsigned = macula_record:procedure_advertisement(macula_node_keys:key_id(Key), Realm, Procedure, [?STATION], Opts),
    verified(macula_record:sign(Unsigned, Key)).

%% A record as macula:find_records/2 hands it over: verified under the node's profile.
verified(Signed) ->
    {ok, Verified} = macula_record:verify(macula_record:encode(Signed), pq_pure),
    Verified.
