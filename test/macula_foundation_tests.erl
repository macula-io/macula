%% EUnit tests for macula_foundation: the trusted foundation key ids, and the check of a foundation record against them
%% (Part 5 §4, Part 6 §9.14 to §9.17). A foundation record carries one foundation signature in the signed-object
%% format, and a node trusts foundation keys by key id.
-module(macula_foundation_tests).

-include_lib("eunit/include/eunit.hrl").

%% A pq_hybrid key carries an RSA-4096 half, which takes up to about a second to generate.
-define(EU_TIMEOUT, 120).
-define(ENV_KEY, foundation_key_ids).
-define(HOUR, 60 * 60 * 1000).

%%------------------------------------------------------------------
%% Placeholder key ids
%%------------------------------------------------------------------

placeholder_count_test() ->
    ?assertEqual(5, length(macula_foundation:placeholder_key_ids())).

placeholder_key_ids_are_32_bytes_test() ->
    ?assertEqual([32, 32, 32, 32, 32], [byte_size(KeyId) || KeyId <- macula_foundation:placeholder_key_ids()]).

placeholder_key_ids_are_distinct_test() ->
    KeyIds = macula_foundation:placeholder_key_ids(),
    ?assertEqual(length(KeyIds), length(lists:usort(KeyIds))).

placeholder_key_ids_are_deterministic_test() ->
    ?assertEqual(macula_foundation:placeholder_key_ids(), macula_foundation:placeholder_key_ids()).

%%------------------------------------------------------------------
%% Configured or placeholder
%%------------------------------------------------------------------

live_key_ids_are_empty_without_configuration_test() ->
    with_key_ids(unset, fun() ->
        ?assertEqual([], macula_foundation:live_key_ids()),
        ?assert(macula_foundation:placeholder_mode())
    end).

live_key_ids_are_the_configured_ones_test() ->
    Live = [crypto:strong_rand_bytes(32) || _ <- lists:seq(1, 5)],
    with_key_ids(Live, fun() ->
        ?assertEqual(Live, macula_foundation:live_key_ids()),
        ?assertNot(macula_foundation:placeholder_mode())
    end).

key_ids_fall_back_to_the_placeholders_when_the_configuration_is_empty_test() ->
    with_key_ids([], fun() ->
        ?assertEqual(macula_foundation:placeholder_key_ids(), macula_foundation:key_ids())
    end).

key_ids_are_the_configured_ones_when_set_test() ->
    KeyId = crypto:strong_rand_bytes(32),
    with_key_ids([KeyId], fun() -> ?assertEqual([KeyId], macula_foundation:key_ids()) end).

%%------------------------------------------------------------------
%% is_foundation
%%------------------------------------------------------------------

is_foundation_recognises_a_trusted_key_id_test() ->
    KeyId = crypto:strong_rand_bytes(32),
    with_key_ids([KeyId], fun() -> ?assert(macula_foundation:is_foundation(KeyId)) end).

is_foundation_refuses_an_unknown_key_id_test() ->
    with_key_ids([crypto:strong_rand_bytes(32)], fun() ->
        ?assertNot(macula_foundation:is_foundation(crypto:strong_rand_bytes(32)))
    end).

is_foundation_refuses_a_value_that_is_not_a_key_id_test() ->
    ?assertNot(macula_foundation:is_foundation(<<1, 2, 3>>)).

%% A placeholder key id is trusted while no key id is configured; no key derives to it.
a_placeholder_key_id_is_trusted_in_placeholder_mode_test() ->
    with_key_ids(unset, fun() ->
        [Placeholder | _] = macula_foundation:placeholder_key_ids(),
        ?assert(macula_foundation:is_foundation(Placeholder)),
        ?assert(macula_foundation:placeholder_mode())
    end).

%%------------------------------------------------------------------
%% verify_record
%%------------------------------------------------------------------

verify_record_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun keys/0, fun verify_cases/1}}.

%% Every case runs on its own, so each passes or fails by itself.
verify_cases(Keys) ->
    [{case_name(Case), fun() -> Case(Keys) end}
     || Case <- [fun a_foundation_parameter_by_a_trusted_key_is_accepted/1,
                 fun every_foundation_type_is_accepted/1,
                 fun the_record_map_is_accepted_as_well_as_its_wire_form/1,
                 fun a_record_by_an_untrusted_foundation_key_is_refused/1,
                 fun a_record_of_another_type_is_refused/1,
                 fun a_tampered_record_is_refused/1,
                 fun an_expired_record_is_refused/1,
                 fun a_record_is_verified_under_the_verifiers_profile/1]].

a_foundation_parameter_by_a_trusted_key_is_accepted(#{foundation := Key}) ->
    KeyId = macula_node_keys:key_id(Key),
    Wire = signed(macula_record:foundation_parameter(<<"puzzle_difficulty">>, 8), Key),
    with_trusted(Key, fun() ->
        ?assertMatch({ok, #{type := 16#0E, key_id := KeyId}}, macula_foundation:verify_record(Wire, pq_pure))
    end).

every_foundation_type_is_accepted(#{foundation := Key}) ->
    Records = [macula_record:foundation_seed_list([]),
               macula_record:foundation_parameter(<<"x">>, 1),
               macula_record:foundation_realm_trust_list([]),
               macula_record:foundation_t3_attestation(crypto:strong_rand_bytes(32), 3)],
    with_trusted(Key, fun() ->
        ?assertEqual([{ok, 16#0D}, {ok, 16#0E}, {ok, 16#0F}, {ok, 16#10}],
                     [type_of(macula_foundation:verify_record(signed(Record, Key), pq_pure)) || Record <- Records])
    end).

the_record_map_is_accepted_as_well_as_its_wire_form(#{foundation := Key}) ->
    #{key := Carried, tbs := Tbs, signature := Signature} =
        macula_record:sign(macula_record:foundation_parameter(<<"x">>, 1), Key),
    Object = #{key => Carried, tbs => Tbs, signature => Signature},
    with_trusted(Key, fun() -> ?assertMatch({ok, _}, macula_foundation:verify_record(Object, pq_pure)) end).

a_record_by_an_untrusted_foundation_key_is_refused(#{foundation := Key, impostor := Impostor}) ->
    Wire = signed(macula_record:foundation_parameter(<<"x">>, 1), Impostor),
    with_trusted(Key, fun() ->
        ?assertEqual({error, not_foundation_signed}, macula_foundation:verify_record(Wire, pq_pure))
    end).

a_record_of_another_type_is_refused(#{foundation := Key}) ->
    Wire = signed(macula_record:envelope(16#20, #{}, #{}), Key),
    with_trusted(Key, fun() ->
        ?assertEqual({error, wrong_type}, macula_foundation:verify_record(Wire, pq_pure))
    end).

a_tampered_record_is_refused(#{foundation := Key}) ->
    #{tbs := <<Head:20/binary, Byte, Tail/binary>>} = Signed =
        macula_record:sign(macula_record:foundation_parameter(<<"x">>, 1), Key),
    Tampered = macula_record:encode(Signed#{tbs := <<Head/binary, (Byte bxor 1), Tail/binary>>}),
    with_trusted(Key, fun() ->
        ?assertEqual({error, signature_invalid}, macula_foundation:verify_record(Tampered, pq_pure))
    end).

an_expired_record_is_refused(#{foundation := Key}) ->
    #{expires_at := Expires} = Signed = macula_record:sign(macula_record:foundation_parameter(<<"x">>, 1), Key),
    with_trusted(Key, fun() ->
        ?assertEqual({error, expired},
                     macula_foundation:verify_record(macula_record:encode(Signed), pq_pure, Expires + ?HOUR))
    end).

a_record_is_verified_under_the_verifiers_profile(_Keys) ->
    {ok, Hybrid} = macula_node_keys:generate(foundation, pq_hybrid),
    Wire = signed(macula_record:foundation_parameter(<<"x">>, 1), Hybrid),
    with_trusted(Hybrid, fun() ->
        ?assertMatch({ok, #{type := 16#0E}}, macula_foundation:verify_record(Wire, pq_hybrid)),
        ?assertMatch({error, _}, macula_foundation:verify_record(Wire, pq_pure))
    end).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

keys() ->
    {ok, Foundation} = macula_node_keys:generate(foundation, pq_pure),
    {ok, Impostor} = macula_node_keys:generate(foundation, pq_pure),
    #{foundation => Foundation, impostor => Impostor}.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

signed(Record, Key) ->
    macula_record:encode(macula_record:sign(Record, Key)).

type_of({ok, #{type := Type}}) -> {ok, Type};
type_of(Refusal) -> Refusal.

with_trusted(Key, Fun) ->
    with_key_ids([macula_node_keys:key_id(Key)], Fun).

with_key_ids(KeyIds, Fun) ->
    Prev = application:get_env(macula, ?ENV_KEY),
    try
        set_key_ids(KeyIds),
        Fun()
    after
        restore(Prev)
    end.

set_key_ids(unset) -> application:unset_env(macula, ?ENV_KEY);
set_key_ids(KeyIds) -> application:set_env(macula, ?ENV_KEY, KeyIds).

restore(undefined) -> application:unset_env(macula, ?ENV_KEY);
restore({ok, KeyIds}) -> application:set_env(macula, ?ENV_KEY, KeyIds).
