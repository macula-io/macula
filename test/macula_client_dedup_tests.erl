%% EUnit tests for `macula_client_dedup'.
-module(macula_client_dedup_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<1:256>>).
-define(PUB,   <<2:256>>).

new_returns_table_test() ->
    Tab = macula_client_dedup:new(),
    ?assert(is_reference(Tab) orelse is_atom(Tab) orelse is_integer(Tab)),
    ets:delete(Tab),
    ok.

first_check_is_new_test() ->
    Tab = macula_client_dedup:new(),
    ?assertEqual(new, macula_client_dedup:check(Tab, ?REALM, ?PUB, 1)),
    ets:delete(Tab),
    ok.

repeat_check_is_duplicate_test() ->
    Tab = macula_client_dedup:new(),
    ?assertEqual(new, macula_client_dedup:check(Tab, ?REALM, ?PUB, 1)),
    ?assertEqual(duplicate, macula_client_dedup:check(Tab, ?REALM, ?PUB, 1)),
    ?assertEqual(duplicate, macula_client_dedup:check(Tab, ?REALM, ?PUB, 1)),
    ets:delete(Tab),
    ok.

different_seq_is_new_test() ->
    Tab = macula_client_dedup:new(),
    ?assertEqual(new, macula_client_dedup:check(Tab, ?REALM, ?PUB, 1)),
    ?assertEqual(new, macula_client_dedup:check(Tab, ?REALM, ?PUB, 2)),
    ?assertEqual(new, macula_client_dedup:check(Tab, ?REALM, ?PUB, 3)),
    ets:delete(Tab),
    ok.

different_publisher_is_new_test() ->
    Tab = macula_client_dedup:new(),
    Pub2 = <<3:256>>,
    ?assertEqual(new, macula_client_dedup:check(Tab, ?REALM, ?PUB, 1)),
    ?assertEqual(new, macula_client_dedup:check(Tab, ?REALM, Pub2, 1)),
    ets:delete(Tab),
    ok.

different_realm_is_new_test() ->
    Tab = macula_client_dedup:new(),
    Realm2 = <<99:256>>,
    ?assertEqual(new, macula_client_dedup:check(Tab, ?REALM, ?PUB, 1)),
    ?assertEqual(new, macula_client_dedup:check(Tab, Realm2, ?PUB, 1)),
    ets:delete(Tab),
    ok.

sweep_with_zero_window_drops_everything_test() ->
    Tab = macula_client_dedup:new(),
    new = macula_client_dedup:check(Tab, ?REALM, ?PUB, 1),
    new = macula_client_dedup:check(Tab, ?REALM, ?PUB, 2),
    new = macula_client_dedup:check(Tab, ?REALM, ?PUB, 3),
    %% Wait one ms so all entries' timestamps are strictly older than now.
    timer:sleep(2),
    Removed = macula_client_dedup:sweep(Tab, 0),
    ?assertEqual(3, Removed),
    %% After sweep, repeat checks count as new.
    ?assertEqual(new, macula_client_dedup:check(Tab, ?REALM, ?PUB, 1)),
    ets:delete(Tab),
    ok.

sweep_keeps_recent_entries_test() ->
    Tab = macula_client_dedup:new(),
    new = macula_client_dedup:check(Tab, ?REALM, ?PUB, 1),
    %% Sweep with a generous window — entry just inserted is far
    %% younger than 60s.
    Removed = macula_client_dedup:sweep(Tab, 60_000),
    ?assertEqual(0, Removed),
    ?assertEqual(duplicate, macula_client_dedup:check(Tab, ?REALM, ?PUB, 1)),
    ets:delete(Tab),
    ok.
