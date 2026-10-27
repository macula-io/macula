%% EUnit tests for macula_realm_rights (macula#128): the realm's RBAC checks at a station and at a caller, each in
%% counting and in refusing mode, and the token a member presents once per link.
-module(macula_realm_rights_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM_NAME, <<"io.example">>).
-define(SUBSCRIBE, <<"pubsub/subscribe">>).
-define(PUBLISH, <<"pubsub/publish">>).
-define(ORDERS, <<"io.example/acme/_org/orders/placed_v1">>).
-define(BILLING, <<"io.example/acme/_org/billing/paid_v1">>).

realm() -> crypto:hash(sha256, ?REALM_NAME).

setup() ->
    ensure_table(ets:whereis(macula_realm_rights)),
    ets:delete_all_objects(macula_realm_rights),
    application:unset_env(macula, realm_rights).

%% The table outlives each test: a process that lives for the run owns it, as macula_peering_sup does.
ensure_table(undefined) ->
    Self = self(),
    spawn(fun() -> ok = macula_realm_rights:new_table(), Self ! table_ready, receive never -> ok end end),
    receive table_ready -> ok end;
ensure_table(_Tid) -> ok.

rights(Caps) -> #{caps => Caps, until => erlang:system_time(second) + 600, member => <<0:256>>}.

cap(With, Can) -> #{<<"with">> => With, <<"can">> => Can}.

orders_rights() -> rights([cap(<<"mri:topic:io.example/acme/_org/orders/*">>, ?SUBSCRIBE),
                           cap(<<"mri:topic:io.example/acme/_org/orders/*">>, ?PUBLISH)]).

count(Check, Outcome) -> maps:get({realm(), Check, Outcome}, macula_realm_rights:counters(), 0).

%%------------------------------------------------------------------
%% The token a member presents on its link
%%------------------------------------------------------------------

presented_tokens_test_() ->
    {setup, fun() -> {ok, RealmKey} = macula_node_keys:generate(realm, pq_pure), RealmKey end,
     fun(RealmKey) ->
         Member = macula_test_identity:node_id(),
         Context = #{peer => Member, realm_key => macula_node_keys:key_id(RealmKey), profile => pq_pure,
                     now => erlang:system_time(second)},
         Token = fun(Audience, Caps) ->
                     macula_ucan:create(RealmKey, Audience, Caps, #{exp => erlang:system_time(second) + 600})
                 end,
         Subscribe = [#{with => <<"mri:topic:io.example/acme/*">>, can => ?SUBSCRIBE}],
         [{"a member's own token is admitted with every capability it grants, until its exp",
           fun() ->
               {ok, #{caps := Caps, until := Until, member := Root}} =
                   macula_realm_rights:admit(#{token => Token(Member, Subscribe), proofs => []}, Context),
               ?assertEqual([cap(<<"mri:topic:io.example/acme/*">>, ?SUBSCRIBE)], Caps),
               ?assert(Until > erlang:system_time(second)),
               ?assertEqual(Member, Root)
           end},
          {"a token whose audience is another node is refused: a station cannot present a member's token",
           ?_assertEqual({error, not_the_audience},
                         macula_realm_rights:admit(#{token => Token(<<1:256>>, Subscribe), proofs => []}, Context))},
          {"a token from another realm's key is refused",
           fun() ->
               {ok, Other} = macula_node_keys:generate(realm, pq_pure),
               Foreign = macula_ucan:create(Other, Member, Subscribe, #{exp => erlang:system_time(second) + 600}),
               ?assertEqual({error, not_the_issuer},
                            macula_realm_rights:admit(#{token => Foreign, proofs => []}, Context))
           end},
          {"a token over the size cap is refused before it is verified, though it would verify",
           fun() ->
               Many = [#{with => <<"mri:topic:io.example/acme/_org/orders/n", (integer_to_binary(N))/binary>>,
                         can => ?SUBSCRIBE} || N <- lists:seq(1, 900)],
               Large = Token(Member, Many),
               ?assert(byte_size(Large) > macula_realm_rights:max_token_bytes()),
               ?assertMatch({ok, _}, macula_ucan:authorize(Large, {realm_member_required,
                                                                   macula_node_keys:key_id(RealmKey), any},
                                                           (maps:with([profile, now], Context))
                                                               #{caller => Member})),
               ?assertEqual({error, too_large}, macula_realm_rights:admit(#{token => Large, proofs => []}, Context))
           end},
          {"more proofs than the cap are refused before anything is verified",
           ?_assertEqual({error, too_large},
                         macula_realm_rights:admit(#{token => Token(Member, Subscribe),
                                                     proofs => lists:duplicate(5, <<"x">>)}, Context))}]
     end}.

%%------------------------------------------------------------------
%% Subscribe and publish, counting and refusing
%%------------------------------------------------------------------

a_station_counts_first_and_lets_everything_through_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end,
     [{"by default a realm counts: an unauthorised subscribe passes and is counted as one it would refuse",
       fun() ->
           ?assertEqual(counting, macula_realm_rights:mode(realm())),
           ?assertEqual(allow, macula_realm_rights:subscribe(counting, realm(), orders_rights(), ?BILLING)),
           ?assertEqual(1, count(subscribe, would_refuse))
       end},
      {"an authorised subscribe and publish are counted as authorised",
       fun() ->
           ?assertEqual(allow, macula_realm_rights:subscribe(counting, realm(), orders_rights(), ?ORDERS)),
           ?assertEqual(allow, macula_realm_rights:publish(counting, realm(), orders_rights(), ?ORDERS)),
           ?assertEqual(1, count(subscribe, authorised)),
           ?assertEqual(1, count(publish, authorised))
       end},
      {"a link that presented no token is counted too",
       fun() ->
           ?assertEqual(allow, macula_realm_rights:publish(counting, realm(), none, ?ORDERS)),
           ?assertEqual(1, count(publish, would_refuse))
       end},
      {"off checks and counts nothing",
       fun() ->
           ?assertEqual(allow, macula_realm_rights:subscribe(off, realm(), none, ?BILLING)),
           ?assertEqual(#{}, macula_realm_rights:counters())
       end}]}.

a_refusing_realm_refuses_naming_what_was_lacking_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end,
     [{"subscribe outside the grant",
       fun() ->
           ?assertEqual({refuse, #{can => ?SUBSCRIBE, resource => ?BILLING, reason => missing_capability}},
                        macula_realm_rights:subscribe(refusing, realm(), orders_rights(), ?BILLING)),
           ?assertEqual(1, count(subscribe, refused))
       end},
      {"a pattern wider than the grant: orders.eu.* does not cover orders.*",
       fun() ->
           Eu = rights([cap(<<"mri:topic:io.example/acme/_org/orders/eu/*">>, ?SUBSCRIBE)]),
           ?assertEqual(allow, macula_realm_rights:subscribe(refusing, realm(), Eu,
                                                             <<"io.example/acme/_org/orders/eu/*">>)),
           ?assertMatch({refuse, #{reason := missing_capability}},
                        macula_realm_rights:subscribe(refusing, realm(), Eu, <<"io.example/acme/_org/orders/*">>))
       end},
      {"publish without the publish capability",
       fun() ->
           SubscribeOnly = rights([cap(<<"mri:topic:io.example/acme/*">>, ?SUBSCRIBE)]),
           ?assertEqual({refuse, #{can => ?PUBLISH, resource => ?ORDERS, reason => missing_capability}},
                        macula_realm_rights:publish(refusing, realm(), SubscribeOnly, ?ORDERS))
       end},
      {"a link with no token",
       ?_assertEqual({refuse, #{can => ?PUBLISH, resource => ?ORDERS, reason => no_token}},
                     macula_realm_rights:publish(refusing, realm(), none, ?ORDERS))},
      {"a system topic carries the mesh's own traffic: exempt, and counted as such",
       fun() ->
           ?assertEqual(allow, macula_realm_rights:publish(refusing, realm(), none, <<"_mesh.patterns">>)),
           ?assertEqual(1, count(publish, exempt))
       end}]}.

%%------------------------------------------------------------------
%% Serve, from the delegation's procedures
%%------------------------------------------------------------------

a_provider_serves_only_what_its_delegation_names_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end,
     [{"counting: a delegation without the procedure passes and is counted",
       fun() ->
           ?assertEqual(allow, macula_realm_rights:serve(counting, realm(), <<"acme/b">>,
                                                         {error, procedure_not_delegated})),
           ?assertEqual(1, count(serve, would_refuse))
       end},
      {"refusing: it is refused, naming rpc/serve and the procedure",
       ?_assertEqual({refuse, #{can => <<"rpc/serve">>, resource => <<"acme/b">>, reason => no_procedures_delegated}},
                     macula_realm_rights:serve(refusing, realm(), <<"acme/b">>, {error, no_procedures_delegated}))},
      {"a delegation that names it is authorised",
       fun() ->
           ?assertEqual(allow, macula_realm_rights:serve(refusing, realm(), <<"acme/a">>, ok)),
           ?assertEqual(1, count(serve, authorised))
       end}]}.

%%------------------------------------------------------------------
%% The mode, per realm
%%------------------------------------------------------------------

the_mode_is_set_per_realm_with_a_default_test_() ->
    {foreach, fun setup/0, fun(_) -> application:unset_env(macula, realm_rights) end,
     [?_assertEqual(counting, macula_realm_rights:mode(realm())),
      fun() ->
          application:set_env(macula, realm_rights, #{realm() => refusing, default => off}),
          ?assertEqual(refusing, macula_realm_rights:mode(realm())),
          ?assertEqual(off, macula_realm_rights:mode(<<1:256>>))
      end]}.
