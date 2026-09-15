%% EUnit tests for refusal reports into a peering connection (identity migration step 3, item 5): the one
%% classification of which refusals are charged (DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md, 3.1), and a connection's count of
%% the refusals reported on it, by kind and charged.
-module(macula_peering_refusals_tests).

-include_lib("eunit/include/eunit.hrl").

refusals_every_verifier_reaches_from_the_same_bytes_are_charged_test_() ->
    [?_assert(macula_frame:charged_refusal(Kind)) || Kind <- [malformed_frame, signature_invalid, key_id_mismatch]].

refusals_that_depend_on_what_the_receiver_holds_are_not_charged_test_() ->
    [?_assertNot(macula_frame:charged_refusal(Kind))
     || Kind <- [seq_mismatch, stream_ended, request_mismatch, not_the_target, not_a_peer, no_subscriber]].

%% refusal_charge/1 is charged_refusal/1 over any term: it agrees with charged_refusal/1 on every kind that function
%% classifies, and a term it does not classify is unclassified, never an error.
refusal_charge_agrees_with_charged_refusal_and_classifies_any_term_test_() ->
    Charge = fun(true) -> charged; (false) -> uncharged end,
    Kinds = [malformed_frame, signature_invalid, key_id_mismatch, seq_mismatch, stream_ended, request_mismatch,
             not_the_target, placement_allowance, unsolicited_shuffle_reply, {expired, 1}, {expired, 600_001},
             {not_yet_valid, 1}, {not_yet_valid, 600_001}, ihave_allowance, graft_unanswered, wrong_realm, not_a_peer,
             no_subscriber],
    [?_assertEqual(Charge(macula_frame:charged_refusal(Kind)), macula_frame:refusal_charge(Kind)) || Kind <- Kinds]
    ++ [?_assertEqual(unclassified, macula_frame:refusal_charge(Other))
        || Other <- [not_a_kind, {expired, soon}, {not_yet_valid}, {signature_invalid, 1}, "text", 7]].

a_connection_counts_reports_by_kind_and_charged_test() ->
    Conn = conn(),
    ok = macula_peering:object_refused(Conn, signature_invalid),
    ok = macula_peering:object_refused(Conn, seq_mismatch),
    ok = macula_peering:object_refused(Conn, signature_invalid),
    Refusals = macula_peering:refusals(Conn),
    macula_peering:close(Conn),
    ?assertEqual(#{counts => #{signature_invalid => 2, seq_mismatch => 1}, charged => 2}, Refusals).

a_kind_outside_the_classification_is_refused_where_it_is_reported_test() ->
    Conn = conn(),
    ?assertError(function_clause, macula_peering:object_refused(Conn, not_a_refusal)),
    Refusals = macula_peering:refusals(Conn),
    macula_peering:close(Conn),
    ?assertEqual(#{counts => #{}, charged => 0}, Refusals).

%% A server-side connection waiting for its handshake, with no QUIC connection behind it.
conn() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Identity} = macula_node_keys:generate(identity, pq_pure),
    {ok, Conn} = macula_peering_conn_sup:start_conn(#{role => server, identity => Identity, issuer => self(),
                                                       puzzle => #{mode => off}, capabilities => 0,
                                                       controlling_pid => self(), quic_conn => make_ref()}),
    Conn.
