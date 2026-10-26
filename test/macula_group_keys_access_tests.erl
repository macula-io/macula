%%%-------------------------------------------------------------------
%%% @doc Who a sealed group's distributor hands the group's keys to (macula#129, DESIGN_E2E_SEALED_PUBSUB.md §3):
%%% one access policy per group, chosen at start. `org_grant' admits the org's `group_keys' grant, as the advertise
%%% gate did; `{realm_capability, RealmKeyId, Mode}' admits the realm's `pubsub/subscribe' covering the whole group,
%%% refusing on it alone, or counting first: a pull it would refuse is counted and let through on the org grant.
%%% The handler checks the call's own token, which the provider link hands it (`handler_authorizes').
%%% @end
%%%-------------------------------------------------------------------
-module(macula_group_keys_access_tests).

-include_lib("eunit/include/eunit.hrl").

-define(T0, 1_800_000_000_000).
-define(MIN, 60000).
-define(ORG, <<"acme">>).
-define(REALM_NAME, <<"io.macula">>).
-define(REALM, crypto:hash(sha256, ?REALM_NAME)).
-define(GROUP, <<"io.macula/acme/orders">>).
-define(OPEN_GROUP, <<"io.macula/acme/chat">>).
-define(SUBSCRIBE, <<"pubsub/subscribe">>).
-define(WHOLE, [<<"mri:topic:io.macula/acme/orders">>, <<"mri:topic:io.macula/acme/orders/*">>]).
-define(PROFILE, pq_pure).

refusing_test_() ->
    {foreach, fun() -> setup(refusing) end, fun cleanup/1,
     [fun a_member_with_the_role_pulls/1,
      fun no_grant_is_refused/1,
      fun a_grant_on_another_group_is_refused/1,
      fun a_grant_below_the_group_only_is_refused/1,
      fun a_grant_sharing_part_of_a_segment_is_refused/1,
      fun a_parent_prefix_admits/1,
      fun a_token_for_another_node_is_refused/1,
      fun a_pull_after_exp_is_refused/1,
      fun a_past_epoch_pull_is_checked_like_a_current_one/1,
      fun the_org_grant_alone_is_refused/1,
      fun the_refusal_names_the_capability_and_resource/1,
      fun a_refused_caller_is_counted/1,
      fun an_unlisted_group_is_served_on_the_org_grant_as_today/1,
      fun removal_and_membership_still_apply/1]}.

counting_test_() ->
    {foreach, fun() -> setup(counting) end, fun cleanup/1,
     [fun counting_admits_the_role/1,
      fun counting_admits_the_org_grant_and_counts_a_would_be_refusal/1,
      fun counting_refuses_a_caller_with_neither/1,
      fun counting_refuses_a_token_for_another_node/1,
      fun counting_refuses_a_pull_after_exp/1]}.

setup(Mode) ->
    Now = counters:new(1, []),
    counters:put(Now, 1, ?T0),
    {ok, RealmKey} = macula_node_keys:generate(realm, ?PROFILE),
    {ok, OrgKey} = macula_node_keys:generate(org, ?PROFILE),
    Removed = ets:new(removed, [public, set]),
    {ok, Pid} = macula_group_keys:start_link(
                  #{org => ?ORG, policy => required, org_key_id => macula_node_keys:key_id(OrgKey),
                    realm => ?REALM, profile => ?PROFILE,
                    groups => #{?GROUP => {realm_capability, macula_node_keys:key_id(RealmKey), Mode}},
                    now => fun() -> counters:get(Now, 1) end,
                    membership => fun(_Caller) -> ok end,
                    removed => fun(Caller) -> ets:member(Removed, Caller) end}),
    #{pid => Pid, now => Now, realm_key => RealmKey, org_key => OrgKey, removed => Removed,
      handler => macula_group_keys:handler(Pid)}.

cleanup(#{pid := Pid, removed := Removed}) ->
    unlink(Pid),
    exit(Pid, shutdown),
    ets:delete(Removed).

%%------------------------------------------------------------------
%% Refusing: the realm capability alone
%%------------------------------------------------------------------

a_member_with_the_role_pulls(W) ->
    ?_assertMatch(#{epochs := [_]}, pull(W, ?GROUP, role(W, ?WHOLE))).

no_grant_is_refused(W) ->
    ?_assertMatch({error, <<"not_authorized", _/binary>>}, pull(W, ?GROUP, #{token => <<>>})).

a_grant_on_another_group_is_refused(W) ->
    ?_assertMatch({error, <<"not_authorized", _/binary>>},
                  pull(W, ?GROUP, role(W, [<<"mri:topic:io.macula/acme/chat">>, <<"mri:topic:io.macula/acme/chat/*">>]))).

a_grant_below_the_group_only_is_refused(W) ->
    ?_assertMatch({error, <<"not_authorized", _/binary>>},
                  pull(W, ?GROUP, role(W, [<<"mri:topic:io.macula/acme/orders/*">>]))).

a_grant_sharing_part_of_a_segment_is_refused(W) ->
    ?_assertMatch({error, <<"not_authorized", _/binary>>},
                  pull(W, ?GROUP, role(W, [<<"mri:topic:io.macula/acme/ord">>, <<"mri:topic:io.macula/acme/ord/*">>]))).

a_parent_prefix_admits(W) ->
    ?_assertMatch(#{epochs := [_]}, pull(W, ?GROUP, role(W, [<<"mri:topic:io.macula/acme/*">>]))).

%% T9: a station or a peer replaying a member's token.
a_token_for_another_node_is_refused(W) ->
    ?_test(begin
        #{token := Token} = role(W, ?WHOLE),
        ?assertMatch({error, <<"not_authorized", _/binary>>}, pull(W, ?GROUP, #{token => Token, caller => other_node()}))
    end).

%% T11: a withdrawn role keeps pulling until its token's exp, and no longer.
a_pull_after_exp_is_refused(W) ->
    ?_test(begin
        Role = role(W, ?WHOLE),
        ?assertMatch(#{epochs := [_]}, pull(W, ?GROUP, Role)),
        advance(W, 3600 * 1000),
        ?assertMatch({error, <<"not_authorized", _/binary>>}, pull(W, ?GROUP, Role))
    end).

a_past_epoch_pull_is_checked_like_a_current_one(W) ->
    ?_test(begin
        Role = role(W, ?WHOLE),
        #{epochs := [#{id := Id}]} = pull(W, ?GROUP, Role),
        advance(W, 20 * ?MIN),
        ?assertMatch(#{epochs := [#{id := Id}]}, pull(W, ?GROUP, Role#{epoch => Id})),
        ?assertMatch({error, <<"not_authorized", _/binary>>}, pull(W, ?GROUP, #{token => <<>>, epoch => Id})),
        advance(W, 3600 * 1000 - 20 * ?MIN),
        ?assertMatch({error, <<"not_authorized", _/binary>>}, pull(W, ?GROUP, Role#{epoch => Id}))
    end).

%% No union: refusing mode never falls back to the org grant.
the_org_grant_alone_is_refused(W) ->
    ?_assertMatch({error, <<"not_authorized", _/binary>>}, pull(W, ?GROUP, org_grant(W))).

the_refusal_names_the_capability_and_resource(W) ->
    ?_assertEqual({error, <<"not_authorized: pubsub/subscribe on mri:topic:io.macula/acme/orders">>},
                  pull(W, ?GROUP, #{token => <<>>})).

a_refused_caller_is_counted(#{pid := Pid} = W) ->
    ?_test(begin
        _ = pull(W, ?GROUP, role(W, ?WHOLE)),
        _ = pull(W, ?GROUP, #{token => <<>>}),
        _ = pull(W, ?GROUP, org_grant(W)),
        ?assertEqual(#{{?REALM, ?GROUP, admitted} => 1, {?REALM, ?GROUP, refused} => 2},
                     macula_group_keys:counts(Pid))
    end).

%% A group the distributor does not list keeps today's check: the org grant, refused without it.
an_unlisted_group_is_served_on_the_org_grant_as_today(#{pid := Pid} = W) ->
    ?_test(begin
        ?assertMatch(#{epochs := [_]}, pull(W, ?OPEN_GROUP, org_grant(W))),
        ?assertMatch({error, <<"not_authorized", _/binary>>}, pull(W, ?OPEN_GROUP, role(W, ?WHOLE))),
        ?assertMatch({error, <<"not_authorized", _/binary>>}, pull(W, ?OPEN_GROUP, #{token => <<>>})),
        ?assertEqual(#{}, macula_group_keys:counts(Pid))
    end).

removal_and_membership_still_apply(#{removed := Removed} = W) ->
    ?_test(begin
        Role = role(W, ?WHOLE),
        true = ets:insert(Removed, {maps:get(caller, Role)}),
        ?assertEqual({error, not_a_member}, pull(W, ?GROUP, Role))
    end).

%%------------------------------------------------------------------
%% Counting: the realm capability, the org grant let through and counted
%%------------------------------------------------------------------

counting_admits_the_role(#{pid := Pid} = W) ->
    ?_test(begin
        ?assertMatch(#{epochs := [_]}, pull(W, ?GROUP, role(W, ?WHOLE))),
        ?assertEqual(#{{?REALM, ?GROUP, admitted} => 1}, macula_group_keys:counts(Pid))
    end).

counting_admits_the_org_grant_and_counts_a_would_be_refusal(#{pid := Pid} = W) ->
    ?_test(begin
        ?assertMatch(#{epochs := [_]}, pull(W, ?GROUP, org_grant(W))),
        ?assertEqual(#{{?REALM, ?GROUP, would_refuse} => 1}, macula_group_keys:counts(Pid))
    end).

counting_refuses_a_caller_with_neither(#{pid := Pid} = W) ->
    ?_test(begin
        ?assertEqual({error, <<"not_authorized: pubsub/subscribe on mri:topic:io.macula/acme/orders">>},
                     pull(W, ?GROUP, #{token => <<>>})),
        ?assertEqual(#{{?REALM, ?GROUP, refused} => 1}, macula_group_keys:counts(Pid))
    end).

counting_refuses_a_token_for_another_node(W) ->
    ?_test(begin
        #{token := Role} = role(W, ?WHOLE),
        #{token := Org} = org_grant(W),
        ?assertMatch({error, <<"not_authorized", _/binary>>}, pull(W, ?GROUP, #{token => Role, caller => other_node()})),
        ?assertMatch({error, <<"not_authorized", _/binary>>}, pull(W, ?GROUP, #{token => Org, caller => other_node()}))
    end).

counting_refuses_a_pull_after_exp(W) ->
    ?_test(begin
        Role = role(W, ?WHOLE),
        advance(W, 3600 * 1000),
        ?assertMatch({error, <<"not_authorized", _/binary>>}, pull(W, ?GROUP, Role))
    end).

%%------------------------------------------------------------------
%% Start
%%------------------------------------------------------------------

%% T27: a restricted group's events must be sealed, so its distributor runs under `required' only.
a_restricted_group_under_a_weaker_policy_does_not_start_test() ->
    [?assertEqual({error, {restricted_group_not_required, ?GROUP}},
                  macula_group_keys:start_link(options(Policy, #{?GROUP => {realm_capability, <<7:256>>, refusing}})))
     || Policy <- [preferred, off]].

a_listed_group_the_org_does_not_own_does_not_start_test() ->
    ?assertEqual({error, {unknown_group, <<"io.macula/contoso/orders">>}},
                 macula_group_keys:start_link(options(required, #{<<"io.macula/contoso/orders">> => org_grant}))).

a_malformed_access_does_not_start_test() ->
    [?assertEqual({error, {invalid_group_access, ?GROUP}},
                  macula_group_keys:start_link(options(required, #{?GROUP => Access})))
     || Access <- [open, {realm_capability, <<7:256>>, loose}, {realm_capability, <<7:8>>, refusing}]].

%% Counting is the default mode, and an org grant group may sit beside it under any policy.
the_default_mode_is_counting_test() ->
    {ok, RealmKey} = macula_node_keys:generate(realm, ?PROFILE),
    {ok, OrgKey} = macula_node_keys:generate(org, ?PROFILE),
    {ok, Pid} = macula_group_keys:start_link(
                  (options(required, #{?GROUP => {realm_capability, macula_node_keys:key_id(RealmKey)}}))#{
                      org_key_id => macula_node_keys:key_id(OrgKey), now => fun() -> ?T0 end}),
    W = #{handler => macula_group_keys:handler(Pid), org_key => OrgKey, realm_key => RealmKey},
    ?assertMatch(#{epochs := [_]}, pull(W, ?GROUP, org_grant(W))),
    ?assertEqual(#{{?REALM, ?GROUP, would_refuse} => 1}, macula_group_keys:counts(Pid)),
    unlink(Pid),
    exit(Pid, shutdown).

options(Policy, Groups) ->
    #{org => ?ORG, policy => Policy, org_key_id => <<9:256>>, realm => ?REALM, profile => ?PROFILE,
      groups => Groups, membership => fun(_) -> ok end}.

%%------------------------------------------------------------------
%% The world under test
%%------------------------------------------------------------------

%% A pull as the provider link hands it to the handler: the verified caller, and the call's own token and proofs.
pull(#{handler := Handler}, Prefix, Opts) ->
    Handler(#{{text, <<"prefix">>} => {text, Prefix},
              {text, <<"epoch">>} => maps:get(epoch, Opts, {text, <<"current">>}),
              caller => maps:get(caller, Opts, crypto:strong_rand_bytes(32)),
              ucan_token => maps:get(token, Opts), proofs => maps:get(proofs, Opts, [])}).

%% A member whose role reads the group: the realm key's pubsub/subscribe on Withs, for a fresh caller.
role(#{realm_key := RealmKey}, Withs) ->
    granted(RealmKey, ?SUBSCRIBE, Withs).

%% The org's grant as the advertise gate checked it: `group_keys' on the org.
org_grant(#{org_key := OrgKey}) ->
    granted(OrgKey, <<"group_keys">>, [<<"mri:org:io.macula/acme">>]).

granted(Key, Can, Withs) ->
    Caller = crypto:strong_rand_bytes(32),
    Token = macula_ucan:create(Key, Caller, [#{with => With, can => Can} || With <- Withs],
                               #{exp => ?T0 div 1000 + 3600}),
    #{token => Token, caller => Caller}.

other_node() -> <<0:256>>.

advance(#{now := Now}, Ms) -> counters:add(Now, 1, Ms).
