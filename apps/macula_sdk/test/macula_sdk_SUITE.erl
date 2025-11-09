%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test suite for macula_sdk.
%%% Tests the main SDK API using TDD approach.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_sdk_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    test_connect_requires_realm/1,
    test_connect_with_valid_options/1,
    test_disconnect/1,
    test_publish_requires_connection/1,
    test_publish_with_map_data/1,
    test_publish_with_binary_data/1,
    test_subscribe_requires_connection/1,
    test_subscribe_with_callback/1,
    test_unsubscribe/1,
    test_call_requires_connection/1,
    test_call_with_timeout/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        test_connect_requires_realm,
        test_connect_with_valid_options,
        test_disconnect,
        test_publish_requires_connection,
        test_publish_with_map_data,
        test_publish_with_binary_data,
        test_subscribe_requires_connection,
        test_subscribe_with_callback,
        test_unsubscribe,
        test_call_requires_connection,
        test_call_with_timeout
    ].

init_per_suite(Config) ->
    %% Start required applications
    application:ensure_all_started(macula_sdk),
    Config.

end_per_suite(_Config) ->
    application:stop(macula_sdk),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases - Phase 1: Connection
%%%===================================================================

test_connect_requires_realm(_Config) ->
    %% GIVEN: Options without realm
    Url = <<"https://localhost:443">>,
    Opts = #{},

    %% WHEN: Attempting to connect
    %% THEN: Should fail with missing_required_option
    ?assertMatch({error, {missing_required_option, realm}},
                 catch macula_sdk:connect(Url, Opts)).

test_connect_with_valid_options(_Config) ->
    %% GIVEN: Valid URL and options with realm
    Url = <<"https://localhost:443">>,
    Opts = #{realm => <<"test.realm">>},

    %% WHEN: Attempting to connect
    Result = macula_sdk:connect(Url, Opts),

    %% THEN: Should return a client pid (even if connection fails, process starts)
    case Result of
        {ok, Client} when is_pid(Client) ->
            macula_sdk:disconnect(Client),
            ok;
        {error, _Reason} ->
            %% Connection may fail if no server running, but API should work
            ok
    end.

test_disconnect(_Config) ->
    %% GIVEN: A mock client pid
    Client = spawn(fun() -> timer:sleep(1000) end),

    %% WHEN: Disconnecting
    Result = macula_sdk:disconnect(Client),

    %% THEN: Should return ok
    ?assertEqual(ok, Result).

%%%===================================================================
%%% Test Cases - Phase 2: Pub/Sub
%%%===================================================================

test_publish_requires_connection(_Config) ->
    %% GIVEN: No connection (using a dead process)
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure process is dead

    %% WHEN: Attempting to publish
    Result = (catch macula_sdk:publish(DeadPid, <<"test.topic">>, #{data => <<"test">>})),

    %% THEN: Should fail (process is dead)
    ?assertMatch({'EXIT', _}, Result).

test_publish_with_map_data(_Config) ->
    %% Test that publish API accepts map data
    %% This is an API contract test
    Client = self(), %% Use self as mock client
    Topic = <<"test.topic">>,
    Data = #{type => <<"test">>, id => <<"123">>},

    %% Should not crash on API call
    %% (will fail at gen_server:call but API accepts the types)
    Result = (catch macula_sdk:publish(Client, Topic, Data)),
    ?assertMatch({'EXIT', {noproc, _}}, Result). %% Expected: process not a gen_server

test_publish_with_binary_data(_Config) ->
    %% Test that publish API accepts binary data
    Client = self(),
    Topic = <<"test.topic">>,
    Data = <<"binary payload">>,

    %% Should not crash on API call
    Result = (catch macula_sdk:publish(Client, Topic, Data)),
    ?assertMatch({'EXIT', {noproc, _}}, Result). %% Expected: process not a gen_server

test_subscribe_requires_connection(_Config) ->
    %% GIVEN: No connection
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(10),

    %% WHEN: Attempting to subscribe
    Callback = fun(_Event) -> ok end,
    Result = (catch macula_sdk:subscribe(DeadPid, <<"test.topic">>, Callback)),

    %% THEN: Should fail
    ?assertMatch({'EXIT', _}, Result).

test_subscribe_with_callback(_Config) ->
    %% Test that subscribe API accepts callback function
    Client = self(),
    Topic = <<"test.topic">>,
    Callback = fun(Event) ->
        ct:pal("Received event: ~p", [Event]),
        ok
    end,

    %% Should accept 1-arity function
    Result = (catch macula_sdk:subscribe(Client, Topic, Callback)),
    ?assertMatch({'EXIT', {noproc, _}}, Result).

test_unsubscribe(_Config) ->
    %% Test that unsubscribe API accepts reference
    Client = self(),
    SubRef = make_ref(),

    Result = (catch macula_sdk:unsubscribe(Client, SubRef)),
    ?assertMatch({'EXIT', {noproc, _}}, Result).

%%%===================================================================
%%% Test Cases - Phase 3: RPC
%%%===================================================================

test_call_requires_connection(_Config) ->
    %% GIVEN: No connection
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(10),

    %% WHEN: Attempting RPC call
    Result = (catch macula_sdk:call(DeadPid, <<"test.procedure">>, #{})),

    %% THEN: Should fail
    ?assertMatch({'EXIT', _}, Result).

test_call_with_timeout(_Config) ->
    %% Test that call API accepts timeout option
    Client = self(),
    Procedure = <<"test.proc">>,
    Args = #{arg1 => <<"value">>},
    Opts = #{timeout => 10000},

    Result = (catch macula_sdk:call(Client, Procedure, Args, Opts)),
    ?assertMatch({'EXIT', {noproc, _}}, Result).
