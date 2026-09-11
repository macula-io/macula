%% EUnit tests for macula_statement_issuer, on an injected clock: statements every 15 minutes valid for an hour, CONNECT
%% rotation every 5 days with the binding and statement first, statements for a rotated-out binding until its
%% not_after, TLS material by leaf hash, the TLS rotation notice and overdue warning, and restarts with and without a
%% key directory (DESIGN_PQ_HANDSHAKE_FRAMES.md, Binding; D22).
-module(macula_statement_issuer_tests).

-include_lib("eunit/include/eunit.hrl").

-export([log/2]).

-define(T0, 1789000000000).
-define(MINUTE, 60000).
-define(HOUR, 3600000).
-define(DAY, 86400000).
-define(LEAF, <<"a leaf certificate, as its listener presents it">>).
-define(OVERDUE, <<"_macula.statement_issuer.tls_rotation_overdue">>).

issuer_test_() ->
    {setup, fun identity/0, fun cases/1}.

cases(Identity) ->
    [{case_name(Case), fun() -> Case(Identity) end}
     || Case <- [fun connect_material_is_bound_and_stated_from_the_start/1,
                 fun a_statement_is_reissued_every_15_minutes_and_valid_for_an_hour/1,
                 fun the_connect_key_rotates_every_5_days_with_its_binding_and_statement_first/1,
                 fun a_rotated_out_binding_keeps_its_statements_until_its_not_after_and_none_after/1,
                 fun tls_material_is_found_by_the_hash_of_the_leaf/1,
                 fun the_owner_hears_a_tls_rotation_is_due_5_days_after_registering/1,
                 fun no_newer_tls_binding_a_day_before_not_after_warns_at_every_check/1,
                 fun a_restarted_issuer_with_a_key_directory_keeps_its_bindings/1,
                 fun a_restarted_issuer_without_a_key_directory_starts_a_new_binding/1,
                 fun the_issuer_never_writes_the_identity_key/1]].

%%------------------------------------------------------------------
%% Cases
%%------------------------------------------------------------------

connect_material_is_bound_and_stated_from_the_start(Identity) ->
    {_Tab, Clock} = clock(),
    Issuer = start(Identity, Clock, #{}),
    #{connect_key := Key, connect_binding := Binding, connect_status := Status} =
        macula_statement_issuer:connect_material(Issuer),
    ?assertMatch({ok, #{use := connect, not_after := ?T0 + 7 * ?DAY}},
                 macula_key_bindings:verify_connect_binding(Binding, public(Identity), pq_pure,
                                                            macula_node_keys:public_key(Key), ?T0)),
    ?assertMatch({ok, #{expires_at := ?T0 + ?HOUR}},
                 macula_key_bindings:verify_status(Status, Binding, public(Identity), pq_pure, ?T0)),
    gen_server:stop(Issuer).

a_statement_is_reissued_every_15_minutes_and_valid_for_an_hour(Identity) ->
    {Tab, Clock} = clock(),
    Issuer = start(Identity, Clock, #{}),
    #{connect_binding := Binding} = macula_statement_issuer:connect_material(Issuer),
    ok = macula_statement_issuer:subscribe(Issuer, hash(Binding)),
    ok = reissued_at(Issuer, Tab, Binding, Identity, ?T0 + 15 * ?MINUTE),
    ok = reissued_at(Issuer, Tab, Binding, Identity, ?T0 + 30 * ?MINUTE),
    gen_server:stop(Issuer).

the_connect_key_rotates_every_5_days_with_its_binding_and_statement_first(Identity) ->
    {Tab, Clock} = clock(),
    Issuer = start(Identity, Clock, #{}),
    #{connect_key := First} = macula_statement_issuer:connect_material(Issuer),
    tick_at(Issuer, Tab, ?T0 + 5 * ?DAY - 1),
    ?assertMatch(#{connect_key := First}, macula_statement_issuer:connect_material(Issuer)),
    Rotation = ?T0 + 5 * ?DAY,
    tick_at(Issuer, Tab, Rotation),
    #{connect_key := Second, connect_binding := Binding, connect_status := Status} =
        macula_statement_issuer:connect_material(Issuer),
    ?assertNotEqual(macula_node_keys:public_key(First), macula_node_keys:public_key(Second)),
    ?assertEqual({ok, #{use => connect, node_id => node_id(Identity), not_after => Rotation + 7 * ?DAY}},
                 macula_key_bindings:verify_connect_binding(Binding, public(Identity), pq_pure,
                                                            macula_node_keys:public_key(Second), Rotation)),
    ?assertMatch({ok, _}, macula_key_bindings:verify_status(Status, Binding, public(Identity), pq_pure, Rotation)),
    gen_server:stop(Issuer).

a_rotated_out_binding_keeps_its_statements_until_its_not_after_and_none_after(Identity) ->
    {Tab, Clock} = clock(),
    Issuer = start(Identity, Clock, #{}),
    #{connect_binding := Old} = macula_statement_issuer:connect_material(Issuer),
    ok = macula_statement_issuer:subscribe(Issuer, hash(Old)),
    tick_at(Issuer, Tab, ?T0 + 5 * ?DAY),
    ?assertMatch({statement, _}, statement_for(Issuer, hash(Old))),
    tick_at(Issuer, Tab, ?T0 + 7 * ?DAY),
    ?assertMatch({statement, _}, statement_for(Issuer, hash(Old))),
    tick_at(Issuer, Tab, ?T0 + 7 * ?DAY + 1),
    ?assertEqual(none, statement_for(Issuer, hash(Old))),
    ?assertEqual({error, unknown_binding}, macula_statement_issuer:subscribe(Issuer, hash(Old))),
    gen_server:stop(Issuer).

tls_material_is_found_by_the_hash_of_the_leaf(Identity) ->
    {_Tab, Clock} = clock(),
    Issuer = start(Identity, Clock, #{}),
    ok = macula_statement_issuer:register_tls_leaf(Issuer, ?LEAF, tls_key()),
    {ok, #{tls_binding := Binding, tls_status := Status}} =
        macula_statement_issuer:tls_material(Issuer, crypto:hash(sha384, ?LEAF)),
    ?assertMatch({ok, #{use := tls}},
                 macula_key_bindings:verify_tls_binding(Binding, public(Identity), pq_pure, ?LEAF, ?T0)),
    ?assertMatch({ok, _}, macula_key_bindings:verify_status(Status, Binding, public(Identity), pq_pure, ?T0)),
    ?assertEqual({error, unknown_leaf},
                 macula_statement_issuer:tls_material(Issuer, crypto:hash(sha384, <<"another leaf">>))),
    gen_server:stop(Issuer).

the_owner_hears_a_tls_rotation_is_due_5_days_after_registering(Identity) ->
    {Tab, Clock} = clock(),
    Issuer = start(Identity, Clock, #{}),
    ok = macula_statement_issuer:register_tls_leaf(Issuer, ?LEAF, tls_key()),
    tick_at(Issuer, Tab, ?T0 + 5 * ?DAY - 1),
    ?assertEqual(none, rotation_notice(Issuer)),
    tick_at(Issuer, Tab, ?T0 + 5 * ?DAY),
    ?assertEqual(due, rotation_notice(Issuer)),
    gen_server:stop(Issuer).

%% A station whose owner never rotates is visible a day before its binding ends every connection.
no_newer_tls_binding_a_day_before_not_after_warns_at_every_check(Identity) ->
    {Tab, Clock} = clock(),
    Handler = capture_diagnostics(),
    Issuer = start(Identity, Clock, #{}),
    ok = macula_statement_issuer:register_tls_leaf(Issuer, ?LEAF, tls_key()),
    tick_at(Issuer, Tab, ?T0 + 6 * ?DAY - 1),
    ?assertEqual(none, overdue_warning()),
    tick_at(Issuer, Tab, ?T0 + 6 * ?DAY),
    ?assertEqual(warned, overdue_warning()),
    tick_at(Issuer, Tab, ?T0 + 6 * ?DAY + 15 * ?MINUTE),
    ?assertEqual(warned, overdue_warning()),
    set_time(Tab, ?T0 + 6 * ?DAY + 20 * ?MINUTE),
    ok = macula_statement_issuer:register_tls_leaf(Issuer, <<"the next leaf">>, tls_key()),
    tick_at(Issuer, Tab, ?T0 + 6 * ?DAY + 30 * ?MINUTE),
    ?assertEqual(none, overdue_warning()),
    ok = logger:remove_handler(Handler),
    gen_server:stop(Issuer).

a_restarted_issuer_with_a_key_directory_keeps_its_bindings(Identity) ->
    {Tab, Clock} = clock(),
    Dir = key_dir(),
    First = start(Identity, Clock, #{key_dir => Dir}),
    #{connect_binding := Binding} = macula_statement_issuer:connect_material(First),
    ok = macula_statement_issuer:register_tls_leaf(First, ?LEAF, tls_key()),
    gen_server:stop(First),
    set_time(Tab, ?T0 + ?HOUR),
    Second = start(Identity, Clock, #{key_dir => Dir}),
    ?assertMatch(#{connect_binding := Binding}, macula_statement_issuer:connect_material(Second)),
    ?assertMatch({ok, _}, macula_statement_issuer:tls_material(Second, crypto:hash(sha384, ?LEAF))),
    ok = macula_statement_issuer:subscribe(Second, hash(Binding)),
    ok = reissued_at(Second, Tab, Binding, Identity, ?T0 + ?HOUR + 15 * ?MINUTE),
    gen_server:stop(Second),
    ok = file:del_dir_r(Dir).

a_restarted_issuer_without_a_key_directory_starts_a_new_binding(Identity) ->
    {_Tab, Clock} = clock(),
    First = start(Identity, Clock, #{}),
    #{connect_binding := Old} = macula_statement_issuer:connect_material(First),
    gen_server:stop(First),
    Second = start(Identity, Clock, #{}),
    #{connect_binding := New} = macula_statement_issuer:connect_material(Second),
    ?assertNotEqual(hash(Old), hash(New)),
    ?assertEqual({error, unknown_binding}, macula_statement_issuer:subscribe(Second, hash(Old))),
    gen_server:stop(Second).

the_issuer_never_writes_the_identity_key(Identity) ->
    {Tab, Clock} = clock(),
    Dir = key_dir(),
    Issuer = start(Identity, Clock, #{key_dir => Dir}),
    ok = macula_statement_issuer:register_tls_leaf(Issuer, ?LEAF, tls_key()),
    tick_at(Issuer, Tab, ?T0 + 5 * ?DAY),
    gen_server:stop(Issuer),
    Keys = filelib:wildcard(filename:join(Dir, "*.key")),
    ?assertEqual(3, length(Keys)),
    ?assertEqual([], [K || K <- Keys, element(1, macula_node_keys:load(K, identity, pq_pure)) =:= ok]),
    ?assertEqual([], [F || F <- filelib:wildcard(filename:join(Dir, "*")), holds_public_key(F, Identity)]),
    ok = file:del_dir_r(Dir).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

identity() ->
    {ok, Identity} = macula_node_keys:generate(identity, pq_pure),
    Identity.

tls_key() ->
    {ok, Key} = macula_node_keys:generate(tls, pq_pure),
    Key.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

public(Identity) ->
    macula_node_keys:public_key(Identity).

node_id(Identity) ->
    {ok, NodeId} = macula_node_keys:node_id(Identity),
    NodeId.

hash(#{tbs := Tbs}) ->
    crypto:hash(sha384, Tbs).

%% A clock the test moves: the issuer reads it from a public table.
clock() ->
    Tab = ets:new(issuer_clock, [public, set]),
    set_time(Tab, ?T0),
    {Tab, fun() -> ets:lookup_element(Tab, now, 2) end}.

set_time(Tab, Ms) ->
    true = ets:insert(Tab, {now, Ms}).

start(Identity, Clock, Extra) ->
    {ok, Issuer} = macula_statement_issuer:start_link(Extra#{identity_key => Identity, owner => self(),
                                                              clock => Clock}),
    Issuer.

tick_at(Issuer, Tab, Ms) ->
    set_time(Tab, Ms),
    ok = macula_statement_issuer:tick(Issuer).

%% At Ms a subscriber receives a statement for Binding that verifies there and expires an hour later.
reissued_at(Issuer, Tab, Binding, Identity, Ms) ->
    tick_at(Issuer, Tab, Ms),
    {statement, Statement} = statement_for(Issuer, hash(Binding)),
    Expected = Ms + ?HOUR,
    {ok, #{expires_at := Expected}} =
        macula_key_bindings:verify_status(Statement, Binding, public(Identity), pq_pure, Ms),
    ok.

statement_for(Issuer, Hash) ->
    receive
        {macula_statement, Issuer, Hash, Statement} -> {statement, Statement}
    after 200 ->
        none
    end.

rotation_notice(Issuer) ->
    receive
        {macula_tls_rotation_due, Issuer} -> due
    after 200 ->
        none
    end.

key_dir() ->
    filename:join(os:getenv("TMPDIR", "/tmp"),
                  "macula_statement_issuer_tests_" ++ integer_to_list(erlang:unique_integer([positive]))).

holds_public_key(File, Identity) ->
    {ok, Bytes} = file:read_file(File),
    binary:match(Bytes, public(Identity)) =/= nomatch.

capture_diagnostics() ->
    Handler = list_to_atom("statement_issuer_test_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = logger:add_handler(Handler, ?MODULE, #{config => #{test => self()}, level => all, filter_default => log}),
    Handler.

%% The logger handler that forwards the issuer's overdue warnings to the test process.
log(#{msg := {report, #{event := ?OVERDUE}}}, #{config := #{test := Test}}) ->
    Test ! overdue_warning;
log(_Event, _Config) ->
    ok.

overdue_warning() ->
    receive
        overdue_warning -> warned
    after 200 ->
        none
    end.
