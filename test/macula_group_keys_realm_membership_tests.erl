%%%-------------------------------------------------------------------
%%% @doc The distributor's DEFAULT membership path (docs/design/
%%% DESIGN_E2E_SEALED_PUBSUB.md §10): a member the realm endorsed is
%%% admitted, and one the realm tombstoned (withdrawn) is refused, read
%%% through the distributor's injectable `lookup' seam. The endorsement
%%% layer itself is tested in macula_hyparview_slot_endorsement_tests;
%%% this suite pins the distributor's wiring of it.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_group_keys_realm_membership_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EU_TIMEOUT, 120).
-define(ORG, <<"acme">>).
-define(PREFIX, <<"io.macula/acme/chat">>).
-define(R, (15 * 60000)).

realm_membership_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun fixture/0, fun cases/1}}.

%% Every case runs on its own, so each passes or fails by itself. Realm key
%% generation is the slow part, once per fixture (as the slot_endorsement
%% suite does).
cases(Fixture) ->
    [{case_name(Case), fun() -> Case(Fixture) end}
     || Case <- [fun a_realm_endorsed_member_is_admitted/1,
                 fun a_realm_tombstoned_member_is_refused/1]].

fixture() ->
    {ok, RealmKey} = macula_node_keys:generate(realm, pq_pure),
    Realm = crypto:strong_rand_bytes(32),
    Member = crypto:strong_rand_bytes(32),
    Endorsement = macula_record:sign(
                    macula_record:realm_member_endorsement(
                      Realm, #{realm => Realm, member_node => Member, roles => [<<"peer">>]}),
                    RealmKey),
    #{realm_key => RealmKey, realm => Realm, member => Member, endorsement => Endorsement}.

a_realm_endorsed_member_is_admitted(#{realm_key := Key, realm := Realm, member := Member, endorsement := Endorsement}) ->
    {ok, Handler, Pid} = distributor(Realm, Key, fun(_Key) -> {ok, [Endorsement]} end),
    ?assertMatch(#{epochs := [_]}, Handler(pull_payload(Member))),
    gen_server:stop(Pid).

a_realm_tombstoned_member_is_refused(#{realm_key := Key, realm := Realm, member := Member, endorsement := Endorsement}) ->
    Tombstone = macula_record:sign(macula_record:tombstone(Endorsement, revoked), Key),
    {ok, Handler, Pid} = distributor(Realm, Key, fun(_Key) -> {ok, [Endorsement, Tombstone]} end),
    ?assertEqual({error, not_a_member}, Handler(pull_payload(Member))),
    gen_server:stop(Pid).

%% A distributor whose default membership reads the realm slot through the
%% injected lookup: no real pool, no wire. The lookup answers what
%% `macula:find_records/2' would: DECODED records, `{ok, [Record]}'. `pool'
%% stays in the options (the default lookup needs it), unused while a lookup
%% is injected.
distributor(Realm, RealmKey, Lookup) ->
    Opts = #{org => ?ORG, policy => required, rotate_after_ms => ?R,
             pool => make_ref(), lookup => Lookup,
             realm => Realm, realm_key_id => macula_node_keys:key_id(RealmKey),
             profile => pq_pure},
    {ok, Pid} = macula_group_keys:start_link(Opts),
    {ok, macula_group_keys:handler(Pid), Pid}.

pull_payload(Member) ->
    #{{text, <<"prefix">>} => {text, ?PREFIX}, {text, <<"epoch">>} => {text, <<"current">>}, caller => Member}.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).
