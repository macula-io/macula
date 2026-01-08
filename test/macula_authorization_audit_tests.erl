%% @doc Unit tests for macula_authorization_audit module.
-module(macula_authorization_audit_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup/Teardown
%%====================================================================

setup() ->
    %% Start telemetry application if not running
    _ = application:ensure_all_started(telemetry),
    %% Start the audit server
    {ok, Pid} = macula_authorization_audit:start_link(#{
        retention_seconds => 60,
        max_entries => 100,
        cleanup_interval => 60000
    }),
    Pid.

cleanup(Pid) ->
    %% Clear and stop server
    macula_authorization_audit:clear(Pid),
    macula_authorization_audit:stop(Pid),
    ok.

%%====================================================================
%% Test Generators
%%====================================================================

audit_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"start and stop server", fun test_start_stop/0},
         {"log authorized operation", fun test_log_authorized/0},
         {"log denied operation", fun test_log_denied/0},
         {"log error", fun test_log_error/0},
         {"get recent entries", fun test_get_recent/0},
         {"get by caller", fun test_get_by_caller/0},
         {"get by resource", fun test_get_by_resource/0},
         {"get stats returns counts", fun test_get_stats/0},
         {"clear removes all entries", fun test_clear/0},
         {"set retention", fun test_set_retention/0},
         {"set max entries", fun test_set_max_entries/0},
         {"enable and disable", fun test_enable_disable/0},
         {"max entries eviction", fun test_max_entries_eviction/0},
         {"entry has correct structure", fun test_entry_structure/0},
         {"multiple operations logged", fun test_multiple_operations/0},
         {"telemetry events emitted", fun test_telemetry_events/0}
     ]
    }.

%%====================================================================
%% Basic Functionality Tests
%%====================================================================

test_start_stop() ->
    %% Server is already started by setup, verify it's running
    Stats = macula_authorization_audit:get_stats(),
    ?assert(is_map(Stats)),
    ?assertEqual(0, maps:get(allowed_count, Stats)),
    ?assertEqual(0, maps:get(denied_count, Stats)).

test_log_authorized() ->
    CallerDID = <<"did:macula:io.macula.test">>,
    Resource = <<"io.macula.test.place_order">>,

    ok = macula_authorization_audit:log_authorized(call, CallerDID, Resource),

    %% Give async cast time to process
    timer:sleep(10),

    Entries = macula_authorization_audit:get_recent(10),
    ?assertEqual(1, length(Entries)),

    [Entry] = Entries,
    ?assertEqual(call, maps:get(operation, Entry)),
    ?assertEqual(CallerDID, maps:get(caller, Entry)),
    ?assertEqual(Resource, maps:get(resource, Entry)),
    ?assertEqual(allowed, maps:get(result, Entry)).

test_log_denied() ->
    CallerDID = <<"did:macula:io.macula.other">>,
    Resource = <<"io.macula.rgfaber.get_data">>,
    Reason = unauthorized,

    ok = macula_authorization_audit:log_denied(call, CallerDID, Resource, Reason),

    timer:sleep(10),

    Entries = macula_authorization_audit:get_recent(10),
    ?assertEqual(1, length(Entries)),

    [Entry] = Entries,
    ?assertEqual(call, maps:get(operation, Entry)),
    ?assertEqual(CallerDID, maps:get(caller, Entry)),
    ?assertEqual(Resource, maps:get(resource, Entry)),
    ?assertEqual(denied, maps:get(result, Entry)),
    ?assertEqual(unauthorized, maps:get(reason, Entry)).

test_log_error() ->
    CallerDID = <<"did:macula:io.macula.broken">>,
    Resource = <<"io.macula.broken.service">>,
    Error = {timeout, 5000},

    ok = macula_authorization_audit:log_error(call, CallerDID, Resource, Error),

    timer:sleep(10),

    Entries = macula_authorization_audit:get_recent(10),
    ?assertEqual(1, length(Entries)),

    [Entry] = Entries,
    ?assertEqual(error, maps:get(result, Entry)),
    ?assertEqual({timeout, 5000}, maps:get(reason, Entry)).

%%====================================================================
%% Query Tests
%%====================================================================

test_get_recent() ->
    %% Log 5 entries
    lists:foreach(fun(N) ->
        Caller = list_to_binary("did:macula:io.macula.user" ++ integer_to_list(N)),
        Resource = list_to_binary("io.macula.user" ++ integer_to_list(N) ++ ".service"),
        macula_authorization_audit:log_authorized(call, Caller, Resource)
    end, lists:seq(1, 5)),

    timer:sleep(20),

    %% Get 3 most recent
    Entries = macula_authorization_audit:get_recent(3),
    ?assertEqual(3, length(Entries)),

    %% Verify ordering (most recent first)
    [E1, E2, E3 | _] = Entries,
    ?assert(maps:get(timestamp, E1) >= maps:get(timestamp, E2)),
    ?assert(maps:get(timestamp, E2) >= maps:get(timestamp, E3)).

test_get_by_caller() ->
    TargetCaller = <<"did:macula:io.macula.target">>,
    OtherCaller = <<"did:macula:io.macula.other">>,

    %% Log entries for different callers
    macula_authorization_audit:log_authorized(call, TargetCaller, <<"res1">>),
    macula_authorization_audit:log_authorized(call, OtherCaller, <<"res2">>),
    macula_authorization_audit:log_authorized(call, TargetCaller, <<"res3">>),
    macula_authorization_audit:log_authorized(call, OtherCaller, <<"res4">>),
    macula_authorization_audit:log_authorized(call, TargetCaller, <<"res5">>),

    timer:sleep(20),

    %% Query for target caller
    Entries = macula_authorization_audit:get_by_caller(TargetCaller, 10),
    ?assertEqual(3, length(Entries)),

    %% All entries should be from target caller
    lists:foreach(fun(E) ->
        ?assertEqual(TargetCaller, maps:get(caller, E))
    end, Entries).

test_get_by_resource() ->
    TargetResource = <<"io.macula.target.service">>,
    OtherResource = <<"io.macula.other.service">>,

    %% Log entries for different resources
    macula_authorization_audit:log_authorized(call, <<"caller1">>, TargetResource),
    macula_authorization_audit:log_authorized(call, <<"caller2">>, OtherResource),
    macula_authorization_audit:log_authorized(call, <<"caller3">>, TargetResource),

    timer:sleep(20),

    %% Query for target resource
    Entries = macula_authorization_audit:get_by_resource(TargetResource, 10),
    ?assertEqual(2, length(Entries)),

    %% All entries should be for target resource
    lists:foreach(fun(E) ->
        ?assertEqual(TargetResource, maps:get(resource, E))
    end, Entries).

%%====================================================================
%% Stats and Configuration Tests
%%====================================================================

test_get_stats() ->
    %% Log some entries
    macula_authorization_audit:log_authorized(call, <<"c1">>, <<"r1">>),
    macula_authorization_audit:log_authorized(call, <<"c2">>, <<"r2">>),
    macula_authorization_audit:log_denied(call, <<"c3">>, <<"r3">>, unauthorized),
    macula_authorization_audit:log_error(call, <<"c4">>, <<"r4">>, timeout),

    timer:sleep(20),

    Stats = macula_authorization_audit:get_stats(),

    ?assertEqual(2, maps:get(allowed_count, Stats)),
    ?assertEqual(1, maps:get(denied_count, Stats)),
    ?assertEqual(1, maps:get(error_count, Stats)),
    ?assertEqual(4, maps:get(table_size, Stats)),
    ?assert(is_integer(maps:get(memory_bytes, Stats))).

test_clear() ->
    %% Log some entries
    macula_authorization_audit:log_authorized(call, <<"c1">>, <<"r1">>),
    macula_authorization_audit:log_authorized(call, <<"c2">>, <<"r2">>),

    timer:sleep(10),

    Entries1 = macula_authorization_audit:get_recent(10),
    ?assertEqual(2, length(Entries1)),

    %% Clear
    ok = macula_authorization_audit:clear(),

    Entries2 = macula_authorization_audit:get_recent(10),
    ?assertEqual(0, length(Entries2)),

    %% Stats should be reset
    Stats = macula_authorization_audit:get_stats(),
    ?assertEqual(0, maps:get(allowed_count, Stats)).

test_set_retention() ->
    ok = macula_authorization_audit:set_retention(120),

    Stats = macula_authorization_audit:get_stats(),
    ?assertEqual(120, maps:get(retention_seconds, Stats)).

test_set_max_entries() ->
    ok = macula_authorization_audit:set_max_entries(500),

    Stats = macula_authorization_audit:get_stats(),
    ?assertEqual(500, maps:get(max_entries, Stats)).

test_enable_disable() ->
    %% Initially enabled
    ?assert(macula_authorization_audit:is_enabled()),

    %% Disable
    ok = macula_authorization_audit:disable(),
    ?assertNot(macula_authorization_audit:is_enabled()),

    %% Log while disabled
    macula_authorization_audit:log_authorized(call, <<"c1">>, <<"r1">>),
    timer:sleep(10),

    %% Entry should NOT be in ETS (but stats still updated)
    Entries = macula_authorization_audit:get_recent(10),
    ?assertEqual(0, length(Entries)),

    %% Stats should still be updated
    Stats = macula_authorization_audit:get_stats(),
    ?assertEqual(1, maps:get(allowed_count, Stats)),

    %% Re-enable
    ok = macula_authorization_audit:enable(),
    ?assert(macula_authorization_audit:is_enabled()),

    %% Log while enabled
    macula_authorization_audit:log_authorized(call, <<"c2">>, <<"r2">>),
    timer:sleep(10),

    Entries2 = macula_authorization_audit:get_recent(10),
    ?assertEqual(1, length(Entries2)).

%%====================================================================
%% Capacity Tests
%%====================================================================

test_max_entries_eviction() ->
    %% Clear any existing entries first
    ok = macula_authorization_audit:clear(),

    %% Set max entries to 5
    ok = macula_authorization_audit:set_max_entries(5),

    %% Log 10 entries with slight delays to ensure different timestamps
    lists:foreach(fun(N) ->
        Caller = list_to_binary("did:macula:caller" ++ integer_to_list(N)),
        Resource = list_to_binary("evict_resource" ++ integer_to_list(N)),
        macula_authorization_audit:log_authorized(call, Caller, Resource),
        timer:sleep(10)
    end, lists:seq(1, 10)),

    timer:sleep(100),

    %% Should have at most 6 entries (eviction may lag slightly due to async)
    Entries = macula_authorization_audit:get_recent(100),
    ?assert(length(Entries) =< 6),  %% Allow slight race

    %% Just verify we don't have all 10 (eviction happened)
    ?assert(length(Entries) < 10).

%%====================================================================
%% Entry Structure Tests
%%====================================================================

test_entry_structure() ->
    Caller = <<"did:macula:io.macula.structure.test">>,
    Resource = <<"io.macula.structure.test.service">>,

    macula_authorization_audit:log_authorized(publish, Caller, Resource),

    timer:sleep(10),

    [Entry] = macula_authorization_audit:get_recent(1),

    %% Check required fields
    ?assert(maps:is_key(id, Entry)),
    ?assert(maps:is_key(timestamp, Entry)),
    ?assert(maps:is_key(operation, Entry)),
    ?assert(maps:is_key(caller, Entry)),
    ?assert(maps:is_key(resource, Entry)),
    ?assert(maps:is_key(result, Entry)),

    %% Check types
    ?assert(is_binary(maps:get(id, Entry))),
    ?assert(is_integer(maps:get(timestamp, Entry))),
    ?assertEqual(publish, maps:get(operation, Entry)),
    ?assertEqual(Caller, maps:get(caller, Entry)),
    ?assertEqual(Resource, maps:get(resource, Entry)),
    ?assertEqual(allowed, maps:get(result, Entry)).

%%====================================================================
%% Multiple Operations Tests
%%====================================================================

test_multiple_operations() ->
    %% Log different operation types
    macula_authorization_audit:log_authorized(call, <<"c1">>, <<"r1">>),
    macula_authorization_audit:log_authorized(publish, <<"c2">>, <<"r2">>),
    macula_authorization_audit:log_authorized(subscribe, <<"c3">>, <<"r3">>),
    macula_authorization_audit:log_authorized(announce, <<"c4">>, <<"r4">>),

    timer:sleep(20),

    Entries = macula_authorization_audit:get_recent(10),
    ?assertEqual(4, length(Entries)),

    Operations = [maps:get(operation, E) || E <- Entries],
    ?assert(lists:member(call, Operations)),
    ?assert(lists:member(publish, Operations)),
    ?assert(lists:member(subscribe, Operations)),
    ?assert(lists:member(announce, Operations)).

%%====================================================================
%% Telemetry Tests
%%====================================================================

test_telemetry_events() ->
    %% Set up telemetry handler
    Self = self(),
    HandlerId = make_ref(),

    telemetry:attach(
        HandlerId,
        [macula, authorization, allowed],
        fun(_EventName, Measurements, Metadata, _Config) ->
            Self ! {telemetry_event, allowed, Measurements, Metadata}
        end,
        #{}
    ),

    %% Log an authorized operation
    macula_authorization_audit:log_authorized(call, <<"caller">>, <<"resource">>),

    %% Should receive telemetry event
    receive
        {telemetry_event, allowed, Measurements, Metadata} ->
            ?assertEqual(1, maps:get(count, Measurements)),
            ?assertEqual(call, maps:get(operation, Metadata)),
            ?assertEqual(<<"caller">>, maps:get(caller, Metadata)),
            ?assertEqual(<<"resource">>, maps:get(resource, Metadata))
    after 100 ->
        ?assert(false, "Expected telemetry event not received")
    end,

    telemetry:detach(HandlerId).
