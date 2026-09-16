%% EUnit tests for macula_hyparview_endorsement: a realm member endorsement signed with a realm key admits a member when
%% it verifies under the verifier's profile, its signer is the trusted realm key, and it names the realm, the member and
%% an active window. The JOIN frame carries the endorsement's wire form and no signature of its own.
-module(macula_hyparview_endorsement_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EU_TIMEOUT, 120).
-define(MINUTE, 60 * 1000).

endorsement_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun keys/0, fun cases/1}}.

%% Every case runs on its own, so each passes or fails by itself.
cases(Keys) ->
    [{case_name(Case), fun() -> Case(Keys) end}
     || Case <- [fun a_valid_endorsement_is_accepted/1,
                 fun every_endorsed_role_is_returned/1,
                 fun an_unsigned_endorsement_is_refused/1,
                 fun a_tampered_endorsement_is_refused/1,
                 fun an_endorsement_by_another_realm_key_is_refused/1,
                 fun a_record_of_another_type_is_refused/1,
                 fun an_endorsement_for_another_member_is_refused/1,
                 fun an_endorsement_for_another_realm_is_refused/1,
                 fun an_endorsement_not_yet_valid_is_refused/1,
                 fun an_endorsement_past_its_window_is_refused/1,
                 fun an_endorsement_window_of_30_days_is_accepted/1,
                 fun an_endorsement_window_over_30_days_is_refused/1,
                 fun an_endorsement_window_that_ends_before_it_starts_is_refused/1,
                 fun a_join_frame_carries_no_signature_of_its_own/1,
                 fun a_join_frame_carries_a_verifiable_endorsement/1]].

%%------------------------------------------------------------------
%% Verifying an endorsement
%%------------------------------------------------------------------

a_valid_endorsement_is_accepted(#{realm_key := Key} = Keys) ->
    Member = id(),
    ?assertEqual({ok, [<<"peer">>]}, verify(signed(unsigned(Keys, Member, [<<"peer">>], #{}), Key), Keys, Member)).

every_endorsed_role_is_returned(#{realm_key := Key} = Keys) ->
    Member = id(),
    Roles = [<<"peer">>, <<"directory">>, <<"replica">>],
    ?assertEqual({ok, Roles}, verify(signed(unsigned(Keys, Member, Roles, #{}), Key), Keys, Member)).

an_unsigned_endorsement_is_refused(Keys) ->
    Member = id(),
    ?assertEqual({error, malformed}, verify(unsigned(Keys, Member, [], #{}), Keys, Member)).

a_tampered_endorsement_is_refused(#{realm_key := Key} = Keys) ->
    Member = id(),
    #{tbs := <<Head:20/binary, Byte, Tail/binary>>} = Signed =
        macula_record:sign(unsigned(Keys, Member, [<<"peer">>], #{}), Key),
    Tampered = macula_record:encode(Signed#{tbs := <<Head/binary, (Byte bxor 1), Tail/binary>>}),
    ?assertEqual({error, signature_invalid}, verify(Tampered, Keys, Member)).

an_endorsement_by_another_realm_key_is_refused(#{other_key := Other} = Keys) ->
    Member = id(),
    Wire = signed(unsigned(Keys, Member, [<<"peer">>], #{}), Other),
    ?assertEqual({error, untrusted_signer}, verify(Wire, Keys, Member)).

a_record_of_another_type_is_refused(#{realm_key := Key} = Keys) ->
    ?assertEqual({error, wrong_type}, verify(signed(macula_record:envelope(16#20, #{}, #{}), Key), Keys, id())).

an_endorsement_for_another_member_is_refused(#{realm_key := Key} = Keys) ->
    Member = id(),
    ?assertEqual({error, wrong_member}, verify(signed(unsigned(Keys, Member, [], #{}), Key), Keys, id())).

an_endorsement_for_another_realm_is_refused(#{realm_key := Key} = Keys) ->
    Member = id(),
    Wire = signed(unsigned(Keys#{realm := id()}, Member, [], #{}), Key),
    ?assertEqual({error, wrong_realm}, verify(Wire, Keys, Member)).

an_endorsement_not_yet_valid_is_refused(#{realm_key := Key} = Keys) ->
    Member = id(),
    Future = erlang:system_time(millisecond) + 10 * ?MINUTE,
    Wire = signed(unsigned(Keys, Member, [], #{valid_from => Future, valid_until => Future + ?MINUTE}), Key),
    ?assertEqual({error, not_yet_valid}, verify(Wire, Keys, Member)).

an_endorsement_past_its_window_is_refused(#{realm_key := Key} = Keys) ->
    Member = id(),
    Now = erlang:system_time(millisecond),
    Window = #{valid_from => Now - 10 * ?MINUTE, valid_until => Now - 5 * ?MINUTE},
    Wire = signed(unsigned(Keys, Member, [], Window), Key),
    ?assertEqual({error, endorsement_expired}, verify(Wire, Keys, Member)).

%% An endorsement's window, valid_from to valid_until, is at most 30 days.
an_endorsement_window_of_30_days_is_accepted(#{realm_key := Key} = Keys) ->
    Member = id(),
    From = erlang:system_time(millisecond) - ?MINUTE,
    Wire = signed(unsigned(Keys, Member, [], #{valid_from => From, valid_until => From + 30 * 24 * 60 * ?MINUTE}), Key),
    ?assertEqual({ok, []}, verify(Wire, Keys, Member)).

%% The builder refuses a longer window, so the test lengthens the record's window before signing it.
an_endorsement_window_over_30_days_is_refused(#{realm_key := Key} = Keys) ->
    Member = id(),
    From = erlang:system_time(millisecond) - ?MINUTE,
    Until = From + 30 * 24 * 60 * ?MINUTE,
    Record = unsigned(Keys, Member, [], #{valid_from => From, valid_until => Until}),
    Longer = Record#{payload := (maps:get(payload, Record))#{{text, <<"valid_until">>} := Until + 1}},
    ?assertEqual({error, endorsement_window_too_long}, verify(signed(Longer, Key), Keys, Member)).

%% A window that ends before it starts is refused by its own name. The builder refuses one, so the test reverses the
%% record's window before signing it.
an_endorsement_window_that_ends_before_it_starts_is_refused(#{realm_key := Key} = Keys) ->
    Member = id(),
    From = erlang:system_time(millisecond) - ?MINUTE,
    Record = unsigned(Keys, Member, [], #{valid_from => From, valid_until => From + ?MINUTE}),
    Reversed = Record#{payload := (maps:get(payload, Record))#{{text, <<"valid_until">>} := From - 1}},
    ?assertEqual({error, endorsement_window_reversed}, verify(signed(Reversed, Key), Keys, Member)).

%%------------------------------------------------------------------
%% The JOIN frame
%%------------------------------------------------------------------

a_join_frame_carries_no_signature_of_its_own(#{realm_key := Key, realm := Realm} = Keys) ->
    Member = id(),
    Frame = macula_hyparview_endorsement:build_join(Realm, Member, signed(unsigned(Keys, Member, [], #{}), Key)),
    ?assertEqual(hyparview_join, macula_frame:frame_type(Frame)),
    ?assertMatch(#{realm := Realm, new_member := Member}, Frame),
    ?assertNot(maps:is_key(signature, Frame)).

%% The endorsement travels in the JOIN frame and verifies on the far side after the wire round trip.
a_join_frame_carries_a_verifiable_endorsement(#{realm_key := Key, realm := Realm} = Keys) ->
    Member = id(),
    Wire = signed(unsigned(Keys, Member, [<<"peer">>], #{}), Key),
    Frame = macula_hyparview_endorsement:build_join(Realm, Member, Wire),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    ?assertEqual(Wire, maps:get(record, Decoded)),
    ?assertEqual({ok, [<<"peer">>]}, verify(maps:get(record, Decoded), Keys, Member)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

keys() ->
    {ok, RealmKey} = macula_node_keys:generate(realm, pq_pure),
    {ok, OtherKey} = macula_node_keys:generate(realm, pq_pure),
    #{realm_key => RealmKey, other_key => OtherKey, realm => id()}.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

id() ->
    crypto:strong_rand_bytes(32).

unsigned(#{realm := Realm}, Member, Roles, Opts) ->
    macula_record:realm_member_endorsement(Realm, #{realm => Realm, member_node => Member, roles => Roles}, Opts).

signed(Record, Key) ->
    macula_record:encode(macula_record:sign(Record, Key)).

verify(Signed, #{realm_key := RealmKey, realm := Realm}, Member) ->
    Trust = #{profile => pq_pure, realm => Realm, realm_key_id => macula_node_keys:key_id(RealmKey)},
    macula_hyparview_endorsement:verify_endorsement(Signed, Trust, Member).
