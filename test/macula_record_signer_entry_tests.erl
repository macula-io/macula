%% EUnit tests for macula_record:signer_entry/3,4, the reader of one signer's entry among the entries a lookup of a slot
%% returns. Every entry of an answer is read as far as its carried key, whatever the answer's order or length, and only
%% entries under the expected key are verified: highest claimed version first, stopping at the first that verifies,
%% and at most 4. The stats say how many entries matched, how many signatures were verified, and how many entries the
%% answer held past the 80 a slot holds.
-module(macula_record_signer_entry_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EU_TIMEOUT, 120).

signer_entry_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun fixture/0, fun cases/1}}.

%% Every case runs on its own, so each passes or fails by itself.
cases(Fixture) ->
    [{case_name(Case), fun() -> Case(Fixture) end}
     || Case <- [fun the_holders_entry_is_found_whatever_the_order/1,
                 fun an_answer_of_1000_entries_with_the_holders_last_gives_it/1,
                 fun of_two_entries_under_the_holders_key_the_higher_version_is_taken/1,
                 fun an_answer_without_the_holders_key_is_not_found/1,
                 fun forged_entries_above_the_holders_record_cost_at_most_4_verifies/1,
                 fun a_matching_entry_that_does_not_verify_gives_its_refusal/1,
                 fun an_identity_key_is_selected_by_its_node_id/1]].

%% The holder's entry, among another key's entry with a tbs that is not CBOR and an entry that is no object at all, in
%% three orders. Selection never reads a tbs under another key, and verifies one signature.
the_holders_entry_is_found_whatever_the_order(#{holder := Holder, record := Record, foreign := Foreign,
                                                unreadable := Unreadable}) ->
    Entries = [macula_record:encode(Record), Unreadable, <<"not an object">> | Foreign],
    Id = macula_node_keys:key_id(Holder),
    Version = macula_record:version(Record),
    [?assertMatch({{ok, #{version := Version, key_id := Id}}, #{matching := 1, verified := 1, beyond_capacity := 0}},
                  macula_record:signer_entry(Order, {key_id, Id}, pq_pure))
     || Order <- [Entries, lists:reverse(Entries), tl(Entries) ++ [hd(Entries)]]].

%% A slot holds at most 80 entries, so an answer of 1,000 comes from a misbehaving station. Its order and length still
%% hide nothing: every entry is read, and the 920 past a slot's capacity are counted.
an_answer_of_1000_entries_with_the_holders_last_gives_it(#{holder := Holder, record := Record, foreign := Foreign}) ->
    Others = lists:sublist(lists:append(lists:duplicate(333, Foreign)), 999),
    Id = macula_node_keys:key_id(Holder),
    Version = macula_record:version(Record),
    ?assertMatch({{ok, #{version := Version, key_id := Id}}, #{matching := 1, verified := 1, beyond_capacity := 920}},
                 macula_record:signer_entry(Others ++ [macula_record:encode(Record)], {key_id, Id}, pq_pure)).

of_two_entries_under_the_holders_key_the_higher_version_is_taken(#{holder := Holder, record := Record}) ->
    Newer = macula_record:refresh(Record, Holder),
    Id = macula_node_keys:key_id(Holder),
    Version = macula_record:version(Newer),
    Entries = [macula_record:encode(Record), macula_record:encode(Newer)],
    ?assertMatch({{ok, #{version := Version}}, #{matching := 2, verified := 1, beyond_capacity := 0}},
                 macula_record:signer_entry(Entries, {key_id, Id}, pq_pure)).

an_answer_without_the_holders_key_is_not_found(#{holder := Holder, foreign := Foreign}) ->
    ?assertEqual({{error, not_found}, #{matching => 0, verified => 0, beyond_capacity => 0}},
                 macula_record:signer_entry(Foreign, {key_id, macula_node_keys:key_id(Holder)}, pq_pure)).

%% A station stores one entry per signer, so more than one matching entry means a misbehaving station. Entries that
%% carry the holder's key over a higher version with forged signatures cost at most 4 verifies, even with the holder's
%% record behind them.
forged_entries_above_the_holders_record_cost_at_most_4_verifies(#{holder := Holder, record := Record}) ->
    #{signature := <<Byte, Rest/binary>>} = Newer = macula_record:refresh(Record, Holder),
    Forged = macula_record:encode(Newer#{signature := <<(Byte bxor 1), Rest/binary>>}),
    Entries = lists:duplicate(50, Forged) ++ [macula_record:encode(Record)],
    ?assertEqual({{error, signature_invalid}, #{matching => 51, verified => 4, beyond_capacity => 0}},
                 macula_record:signer_entry(Entries, {key_id, macula_node_keys:key_id(Holder)}, pq_pure)).

a_matching_entry_that_does_not_verify_gives_its_refusal(#{holder := Holder, record := Record}) ->
    #{signature := <<Byte, Rest/binary>>} = Record,
    Forged = macula_record:encode(Record#{signature := <<(Byte bxor 1), Rest/binary>>}),
    ?assertEqual({{error, signature_invalid}, #{matching => 1, verified => 1, beyond_capacity => 0}},
                 macula_record:signer_entry([Forged], {key_id, macula_node_keys:key_id(Holder)}, pq_pure)).

%% A record a node signs names its signer by node_id, so an identity key's entry is selected by that.
an_identity_key_is_selected_by_its_node_id(#{foreign := Foreign}) ->
    Identity = key(identity),
    NodeId = macula_node_keys:key_id(Identity),
    Node = macula_record:sign(macula_record:node_record(NodeId, [], 0), Identity),
    Version = macula_record:version(Node),
    ?assertMatch({{ok, #{version := Version, key_id := NodeId}}, #{matching := 1, verified := 1}},
                 macula_record:signer_entry([macula_record:encode(Node) | Foreign], {node_id, NodeId}, pq_pure)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% A realm member endorsement signed by the holder, the same endorsement signed by three other realm keys, and an
%% object under another key whose tbs is not CBOR.
fixture() ->
    Holder = key(realm),
    Others = [key(realm) || _ <- lists:seq(1, 3)],
    Realm = crypto:strong_rand_bytes(32),
    Member = crypto:strong_rand_bytes(32),
    #{holder => Holder,
      record => macula_record:sign(endorsement(Realm, Member), Holder),
      foreign => [macula_record:encode(macula_record:sign(endorsement(Realm, Member), Key)) || Key <- Others],
      unreadable => macula_signed_object:encode(#{key => macula_node_keys:public_key(hd(Others)), tbs => <<16#FF>>,
                                                  signature => <<0>>})}.

key(Purpose) ->
    {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
    Key.

endorsement(Realm, Member) ->
    macula_record:realm_member_endorsement(Realm, #{realm => Realm, member_node => Member, roles => [<<"peer">>]}).

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).
