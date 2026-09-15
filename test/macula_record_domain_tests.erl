%% EUnit tests for domain records (tags 0x20 to 0xFF) in macula_record. A subject is a non-empty binary: the builder
%% and every verifier refuse an empty one, since it would name a slot of its own apart from no subject.
%% domain_record_checked/1 names each refusal a pool makes before it signs a domain record, and a domain record's
%% tombstone signs within the domain maximum plus twice the clock tolerance, on the record's slot.
-module(macula_record_domain_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DAY, 86_400_000).
-define(MINUTE, 60_000).

an_empty_subject_is_refused_by_the_builder_test() ->
    ?assertError(function_clause, macula_record:envelope(16#20, #{}, #{subject_id => <<>>})).

an_empty_subject_is_refused_by_a_verifier_test() ->
    Id = key(),
    Signed = macula_record:sign((macula_record:envelope(16#20, #{}, #{}))#{subject => <<>>}, Id),
    ?assertEqual({error, malformed}, macula_record:verify(macula_record:encode(Signed), pq_pure)).

a_domain_record_is_checked_by_name_before_signing_test() ->
    #{created_at := Created} = Record = macula_record:envelope(16#20, #{}, #{}),
    Large = #{{text, <<"large">>} => {text, binary:copy(<<"b">>, 256 * 1024)}},
    Checked = [macula_record:domain_record_checked(Candidate)
               || Candidate <- [Record, Record#{subject => <<"s">>}, macula_record:node_record(<<1:256>>, [], 0),
                                Record#{subject => <<>>}, Record#{subject => 1},
                                Record#{expires_at := Created + 7 * ?DAY + 1}, Record#{expires_at := Created},
                                Record#{payload := Large}, not_a_record]],
    ?assertEqual([ok, ok, {error, not_a_domain_type}, {error, invalid_subject}, {error, invalid_subject},
                  {error, lifetime_too_long}, {error, lifetime_reversed}, {error, record_too_large},
                  {error, malformed}], Checked).

a_domain_record_tombstone_signs_within_the_maximum_and_twice_the_tolerance_on_its_slot_test() ->
    Id = key(),
    Withdrawn = macula_record:sign(macula_record:envelope(16#20, #{}, #{subject_id => <<"s1">>, ttl_ms => 7 * ?DAY}),
                                   Id),
    #{created_at := Created} = Tombstone = macula_record:tombstone(Withdrawn, shutdown),
    AtBound = macula_record:sign(Tombstone#{expires_at := Created + 7 * ?DAY + 10 * ?MINUTE}, Id),
    ?assertMatch({ok, _}, macula_record:verify(macula_record:encode(AtBound), pq_pure)),
    ?assertEqual(macula_record:storage_key(Withdrawn), macula_record:storage_key(AtBound)),
    ?assertError({lifetime_too_long, 16#0C},
                 macula_record:sign(Tombstone#{expires_at := Created + 7 * ?DAY + 10 * ?MINUTE + 1}, Id)).

%% A tombstone names a withdrawn type from 1 to 255, since every record type is a tag in that range, and a type beyond
%% it would be read by its low byte: one naming 16#100 is malformed however it was signed, and one naming 16#FF still
%% verifies.
a_tombstone_names_a_withdrawn_type_within_the_type_range_test() ->
    Id = key(),
    Withdrawn = macula_record:sign(macula_record:envelope(16#FF, #{}, #{}), Id),
    #{payload := Payload} = Tombstone = macula_record:tombstone(Withdrawn, shutdown),
    AtTheTop = macula_record:sign(Tombstone, Id),
    Beyond = macula_record:sign(Tombstone#{payload := Payload#{{text, <<"withdrawn_type">>} => 16#100}}, Id),
    ?assertMatch({ok, _}, macula_record:verify(macula_record:encode(AtTheTop), pq_pure)),
    ?assertEqual({error, malformed}, macula_record:verify(macula_record:encode(Beyond), pq_pure)).

key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.
