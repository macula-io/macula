%% EUnit tests for macula_hyparview_endorsement:slot_endorsement/3,4: the endorsement a member's slot holds, read from
%% the entries a lookup of that slot returns. Only the entry of the realm key a node pins for the realm counts
%% (macula_record:signer_entry/4). The realm key's endorsement admits the member as verify_endorsement/3 does, and the
%% realm key's tombstone withdraws it. An endorsement or a tombstone under any other key changes nothing.
-module(macula_hyparview_slot_endorsement_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EU_TIMEOUT, 120).

slot_endorsement_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun fixture/0, fun cases/1}}.

%% Every case runs on its own, so each passes or fails by itself.
cases(Fixture) ->
    [{case_name(Case), fun() -> Case(Fixture) end}
     || Case <- [fun the_realm_keys_endorsement_counts_beside_another_keys_endorsement_and_tombstone/1,
                 fun the_realm_keys_tombstone_withdraws_the_endorsement/1,
                 fun another_keys_tombstone_alone_withdraws_nothing/1,
                 fun the_realm_keys_tombstone_for_another_member_is_refused/1,
                 fun a_tombstone_still_withdraws_at_the_endorsements_last_valid_moment/1,
                 fun a_slot_without_the_realm_keys_entry_is_not_found/1,
                 fun slot_standing_admits_the_member_its_slot_endorses/1,
                 fun slot_standing_refuses_a_withdrawn_member/1,
                 fun slot_standing_reads_the_members_own_slot/1,
                 fun slot_standing_refuses_when_the_lookup_fails/1,
                 fun slot_standing_refuses_when_the_lookup_exits/1]].

the_realm_keys_endorsement_counts_beside_another_keys_endorsement_and_tombstone(
  #{other_key := Other, endorsement := Endorsement, realm := Realm, member := Member} = Fixture) ->
    Foreign = macula_record:sign(endorsement(Realm, Member), Other),
    ForeignTombstone = macula_record:sign(macula_record:tombstone(Endorsement, revoked), Other),
    Entries = [wire(ForeignTombstone), wire(Foreign), wire(Endorsement)],
    ?assertMatch({{ok, [<<"peer">>]}, #{matching := 1, verified := 1}}, slot(Entries, Fixture)).

the_realm_keys_tombstone_withdraws_the_endorsement(#{realm_key := Key, endorsement := Endorsement} = Fixture) ->
    Tombstone = macula_record:sign(macula_record:tombstone(Endorsement, revoked), Key),
    ?assertMatch({{error, withdrawn}, #{matching := 2, verified := 1}},
                 slot([wire(Endorsement), wire(Tombstone)], Fixture)).

another_keys_tombstone_alone_withdraws_nothing(#{other_key := Other, endorsement := Endorsement} = Fixture) ->
    ForeignTombstone = macula_record:sign(macula_record:tombstone(Endorsement, revoked), Other),
    ?assertMatch({{error, not_found}, #{matching := 0, verified := 0}}, slot([wire(ForeignTombstone)], Fixture)).

the_realm_keys_tombstone_for_another_member_is_refused(#{realm_key := Key, realm := Realm} = Fixture) ->
    Another = macula_record:sign(endorsement(Realm, crypto:strong_rand_bytes(32)), Key),
    Tombstone = macula_record:sign(macula_record:tombstone(Another, revoked), Key),
    ?assertMatch({{error, wrong_member}, _Stats}, slot([wire(Tombstone)], Fixture)).

%% A tombstone lives until the record it withdraws has expired plus the clock tolerance, so at the endorsement's last
%% valid moment the realm key's tombstone still withdraws it.
a_tombstone_still_withdraws_at_the_endorsements_last_valid_moment(
  #{realm_key := Key, endorsement := Endorsement} = Fixture) ->
    Tombstone = macula_record:sign(macula_record:tombstone(Endorsement, revoked), Key),
    LastMoment = macula_record:expires_at(Endorsement),
    ?assertMatch({{error, withdrawn}, _Stats}, slot([wire(Endorsement), wire(Tombstone)], Fixture, LastMoment)).

a_slot_without_the_realm_keys_entry_is_not_found(Fixture) ->
    ?assertMatch({{error, not_found}, #{matching := 0, verified := 0}}, slot([], Fixture)).

%%------------------------------------------------------------------
%% slot_standing/2: the standing check an overlay's admission makes (macula#118)
%%------------------------------------------------------------------

slot_standing_admits_the_member_its_slot_endorses(#{endorsement := Endorsement, member := Member} = Fixture) ->
    ?assertEqual(ok, (standing(fun(_Key) -> {ok, [Endorsement]} end, Fixture))(Member)).

%% The endorsement still verifies on its own; the slot's tombstone is what refuses the member.
slot_standing_refuses_a_withdrawn_member(#{realm_key := Key, endorsement := Endorsement, member := Member} = Fixture) ->
    Tombstone = macula_record:sign(macula_record:tombstone(Endorsement, revoked), Key),
    ?assertEqual({error, withdrawn}, (standing(fun(_Key) -> {ok, [Endorsement, Tombstone]} end, Fixture))(Member)).

slot_standing_reads_the_members_own_slot(#{realm := Realm, endorsement := Endorsement, member := Member} = Fixture) ->
    Slot = macula_record:realm_member_endorsement_key(Realm, Member),
    Lookup = fun(Key) when Key =:= Slot -> {ok, [Endorsement]};
                (_OtherKey) -> {ok, []}
             end,
    ?assertEqual(ok, (standing(Lookup, Fixture))(Member)),
    ?assertEqual({error, not_found}, (standing(Lookup, Fixture))(crypto:strong_rand_bytes(32))).

%% A lookup that fails refuses, so an admission gated on it fails closed.
slot_standing_refuses_when_the_lookup_fails(#{member := Member} = Fixture) ->
    ?assertEqual({error, lookup_failed}, (standing(fun(_Key) -> {error, timeout} end, Fixture))(Member)).

%% A lookup through a pool that is down exits (gen_server:call to a dead pid); the standing check refuses instead of
%% taking the overlay process down with it.
slot_standing_refuses_when_the_lookup_exits(#{member := Member} = Fixture) ->
    Dead = spawn(fun() -> ok end),
    MRef = monitor(process, Dead),
    receive {'DOWN', MRef, process, Dead, _} -> ok end,
    Lookup = fun(Key) -> macula:find_records(Dead, Key) end,
    ?assertEqual({error, lookup_failed}, (standing(Lookup, Fixture))(Member)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% The realm key a node pins, another realm key, and the realm key's endorsement of a member.
fixture() ->
    {ok, RealmKey} = macula_node_keys:generate(realm, pq_pure),
    {ok, OtherKey} = macula_node_keys:generate(realm, pq_pure),
    Realm = crypto:strong_rand_bytes(32),
    Member = crypto:strong_rand_bytes(32),
    #{realm_key => RealmKey, other_key => OtherKey, realm => Realm, member => Member,
      endorsement => macula_record:sign(endorsement(Realm, Member), RealmKey)}.

slot(Entries, Fixture) ->
    slot(Entries, Fixture, erlang:system_time(millisecond)).

slot(Entries, #{realm_key := Key, realm := Realm, member := Member}, Now) ->
    Trust = #{profile => pq_pure, realm => Realm, realm_key_id => macula_node_keys:key_id(Key)},
    macula_hyparview_endorsement:slot_endorsement(Entries, Trust, Member, Now).

standing(Lookup, #{realm_key := Key, realm := Realm}) ->
    macula_hyparview_endorsement:slot_standing(
      Lookup, #{profile => pq_pure, realm => Realm, realm_key_id => macula_node_keys:key_id(Key)}).

wire(Record) ->
    macula_record:encode(Record).

endorsement(Realm, Member) ->
    macula_record:realm_member_endorsement(Realm, #{realm => Realm, member_node => Member, roles => [<<"peer">>]}).

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).
