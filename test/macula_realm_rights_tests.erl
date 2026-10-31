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

%% The counter table is macula_peering_sup's, so the app owns it: a test that owned it would leave a named table
%% behind that refuses the next start of the app (already_exists).
setup() ->
    {ok, _} = application:ensure_all_started(macula),
    ets:delete_all_objects(macula_realm_rights),
    application:unset_env(macula, realm_rights).

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
                                                     proofs => lists:duplicate(5, <<"x">>)}, Context))},
          {"a proof that fails the size check is refused before any proof is hashed",
           ?_assertEqual({error, too_large},
                         macula_realm_rights:admit(#{token => Token(Member, Subscribe), proofs => [not_a_proof]},
                                                   Context))}]
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

%%------------------------------------------------------------------
%% Members mode: a closed realm asks only for a live membership (macula-realm#175)
%%------------------------------------------------------------------

%% The membership verdict the station keeps, built from what admit/2 answered on a real minted token — never a
%% hand-built map (macula-realm#175, WP2.5).
membership_of(Token, Proofs, Context) ->
    case macula_realm_rights:admit(#{token => Token, proofs => Proofs}, Context) of
        {ok, Rights} -> {member, Rights};
        {error, Reason} -> {not_member, macula_realm_rights:membership_reason(Reason)}
    end.

members_mode_closes_the_realm_test_() ->
    {setup, fun() -> {ok, RealmKey} = macula_node_keys:generate(realm, pq_pure), RealmKey end,
     fun(RealmKey) -> members_mode_tests(RealmKey) end}.

members_mode_tests(RealmKey) ->
    {foreach, fun setup/0, fun(_) -> ok end,
     [{"each of the five links gets the reason the realm closed on it",
       fun() ->
           Now = erlang:system_time(second),
           Member = macula_test_identity:node_id(),
           Context = #{peer => Member, realm_key => macula_node_keys:key_id(RealmKey), profile => pq_pure,
                       now => Now},
           Subscribe = [#{with => <<"mri:topic:io.example/acme/*">>, can => ?SUBSCRIBE}],
           Token = fun(Audience, Exp) -> macula_ucan:create(RealmKey, Audience, Subscribe, #{exp => Exp}) end,
           {ok, ForeignRealmKey} = macula_node_keys:generate(realm, pq_pure),
           Links = [{{not_member, no_token}, none},
                    {{not_member, expired}, {Token(Member, Now - 1), []}},
                    {{not_member, not_the_audience}, {Token(<<1:256>>, Now + 600), []}},
                    {{not_member, wrong_realm}, {macula_ucan:create(ForeignRealmKey, Member, Subscribe,
                                                                    #{exp => Now + 600}), []}},
                    {member, {Token(Member, Now + 600), []}}],
           [decided_per_frame(Expected, Presented, Context) || {Expected, Presented} <- Links]
       end},
      {"counting: the same five links are all let through, and the members decision is counted by reason",
       fun() ->
           Now = erlang:system_time(second),
           Member = macula_test_identity:node_id(),
           Context = #{peer => Member, realm_key => macula_node_keys:key_id(RealmKey), profile => pq_pure,
                       now => Now},
           Subscribe = [#{with => <<"mri:topic:io.example/acme/*">>, can => ?SUBSCRIBE}],
           Token = fun(Audience, Exp) -> macula_ucan:create(RealmKey, Audience, Subscribe, #{exp => Exp}) end,
           {ok, ForeignRealmKey} = macula_node_keys:generate(realm, pq_pure),
           Links = [none,
                    {Token(Member, Now - 1), []},
                    {Token(<<1:256>>, Now + 600), []},
                    {macula_ucan:create(ForeignRealmKey, Member, Subscribe, #{exp => Now + 600}), []},
                    {Token(Member, Now + 600), []}],
           Verdicts = [presented_verdict(Presented, Context) || Presented <- Links],
           ?assertEqual([allow, allow, allow, allow, allow],
                        [macula_realm_rights:membership(counting, realm(), V) || V <- Verdicts]),
           ?assertEqual(1, count(membership, member)),
           ?assertEqual(4, count(membership, would_refuse)),
           ?assertEqual(1, count(membership, no_token)),
           ?assertEqual(1, count(membership, expired)),
           ?assertEqual(1, count(membership, not_the_audience)),
           ?assertEqual(1, count(membership, wrong_realm))
       end},
      {"the four reasons hold their boundary: a foreign realm is wrong_realm, a malformed realm name is not",
       fun() ->
           %% The token's issuer not being this realm's key is, for a membership, exactly a token for another
           %% realm; macula_ucan's own wrong_realm is a capability naming another realm. A malformed realm NAME
           %% in a capability says nothing about which realm the token belongs to, so it is invalid.
           ?assertEqual(wrong_realm, macula_realm_rights:membership_reason(not_the_issuer)),
           ?assertEqual(wrong_realm, macula_realm_rights:membership_reason(wrong_realm)),
           ?assertEqual(not_the_audience, macula_realm_rights:membership_reason(not_the_audience)),
           ?assertEqual(expired, macula_realm_rights:membership_reason(expired)),
           ?assertEqual(invalid, macula_realm_rights:membership_reason(realm_name_not_canonical)),
           ?assertEqual(invalid, macula_realm_rights:membership_reason(malformed)),
           ?assertEqual(invalid, macula_realm_rights:membership_reason(signature_invalid))
       end},
      {"a system topic stays exempt in members mode, admitted with no token at all",
       fun() ->
           ?assertEqual(allow, macula_realm_rights:publish(members, realm(), none, <<"_mesh.patterns">>)),
           ?assertEqual(1, count(publish, exempt)),
           ?assertEqual(0, count(membership, refused))
       end},
      {"off decides nothing and counts nothing",
       fun() ->
           ?assertEqual(allow, macula_realm_rights:membership(off, realm(), {not_member, no_token})),
           ?assertEqual(#{}, macula_realm_rights:counters())
       end},
      {"a delegation that names the procedure is still not enough without a live membership",
       fun() ->
           ?assertEqual({refuse, #{can => <<"realm/member">>, resource => realm(), reason => no_token}},
                        macula_realm_rights:serve(members, realm(), <<"acme/a">>, ok, {not_member, no_token})),
           ?assertEqual(1, count(serve, authorised)),
           ?assertEqual(1, count(membership, refused))
       end},
      {"a live membership does not excuse a delegation that omits the procedure",
       ?_assertEqual({refuse, #{can => <<"rpc/serve">>, resource => <<"acme/a">>,
                                reason => no_procedures_delegated}},
                     macula_realm_rights:serve(members, realm(), <<"acme/a">>, {error, no_procedures_delegated},
                                               {member, orders_rights()}))},
      {"both halves held: the advertisement is admitted",
       ?_assertEqual(allow, macula_realm_rights:serve(members, realm(), <<"acme/a">>, ok,
                                                      {member, orders_rights()}))},
      {"counting counts both halves and refuses neither",
       fun() ->
           ?assertEqual(allow, macula_realm_rights:serve(counting, realm(), <<"acme/a">>, ok,
                                                         {not_member, no_token})),
           ?assertEqual(1, count(serve, authorised)),
           ?assertEqual(1, count(membership, would_refuse))
       end}]}.

%% Each frame kind answers for one link: membership/3 is the decision the station makes, and the older entry points
%% defer to it. They can only see rights or nothing, so past the decision a token that ended reads as `no_token'
%% there — the reason is the station's to report, through membership/3.
decided_per_frame(Expected, none, _Context) ->
    ?assertEqual(refusal(Expected), macula_realm_rights:subscribe(members, realm(), none, ?ORDERS)),
    ?assertEqual(refusal(Expected), macula_realm_rights:publish(members, realm(), none, ?ORDERS));
decided_per_frame(member, Presented, Context) ->
    Verdict = presented_verdict(Presented, Context),
    ?assertEqual(allow, macula_realm_rights:membership(members, realm(), Verdict)),
    ?assertEqual(allow, macula_realm_rights:subscribe(members, realm(), rights_of(Verdict), ?ORDERS)),
    ?assertEqual(allow, macula_realm_rights:publish(members, realm(), rights_of(Verdict), ?ORDERS));
decided_per_frame(Expected, Presented, Context) ->
    Verdict = presented_verdict(Presented, Context),
    ?assertEqual(refusal(Expected), macula_realm_rights:membership(members, realm(), Verdict)),
    ?assertMatch({refuse, #{can := <<"realm/member">>}},
                 macula_realm_rights:publish(members, realm(), rights_of(Verdict), ?ORDERS)).

presented_verdict(none, _Context) -> {not_member, no_token};
presented_verdict({Token, Proofs}, Context) -> membership_of(Token, Proofs, Context).

rights_of({member, Rights}) -> Rights;
rights_of({not_member, _}) -> none.

refusal(member) -> allow;
refusal({not_member, Reason}) -> {refuse, #{can => <<"realm/member">>, resource => realm(), reason => Reason}}.

members_is_a_mode_test_() ->
    {foreach, fun setup/0, fun(_) -> application:unset_env(macula, realm_rights) end,
     [fun() ->
          application:set_env(macula, realm_rights, #{realm() => members}),
          ?assertEqual(members, macula_realm_rights:mode(realm()))
      end]}.
