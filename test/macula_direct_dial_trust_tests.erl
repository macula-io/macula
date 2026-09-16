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

%%====================================================================
%% Helpers
%%====================================================================

trusted(Verified, Realm, Procedure) ->
    macula_direct_dial:advertisement_trusted(Verified, #{realm => Realm, procedure => Procedure, profile => pq_pure}).

identity_key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.

advertisement(Realm, Procedure, Opts) ->
    Key = identity_key(),
    Unsigned = macula_record:procedure_advertisement(macula_node_keys:key_id(Key), Realm, Procedure, ?STATION, Opts),
    verified(macula_record:sign(Unsigned, Key)).

%% A record as macula:find_records/2 hands it over: verified under the node's profile.
verified(Signed) ->
    {ok, Verified} = macula_record:verify(macula_record:encode(Signed), pq_pure),
    Verified.
