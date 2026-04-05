%%%-------------------------------------------------------------------
%%% @doc Tests for macula_multi_relay — multi-homing relay client.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_multi_relay_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Dedup ring buffer tests
%%====================================================================

dedup_new_message_test() ->
    {ok, Pid} = start_test_multi_relay(),
    ?assertEqual(new, gen_server:call(Pid, {check_dedup, <<"msg-001">>})),
    stop_test(Pid).

dedup_duplicate_message_test() ->
    {ok, Pid} = start_test_multi_relay(),
    ?assertEqual(new, gen_server:call(Pid, {check_dedup, <<"msg-001">>})),
    ?assertEqual(duplicate, gen_server:call(Pid, {check_dedup, <<"msg-001">>})),
    stop_test(Pid).

dedup_different_messages_test() ->
    {ok, Pid} = start_test_multi_relay(),
    ?assertEqual(new, gen_server:call(Pid, {check_dedup, <<"msg-001">>})),
    ?assertEqual(new, gen_server:call(Pid, {check_dedup, <<"msg-002">>})),
    ?assertEqual(duplicate, gen_server:call(Pid, {check_dedup, <<"msg-001">>})),
    stop_test(Pid).

dedup_eviction_test() ->
    %% The ring buffer is 2048 entries. After 2048+1, the first entry should be evicted.
    {ok, Pid} = start_test_multi_relay(),
    %% Fill the buffer
    lists:foreach(fun(N) ->
        Id = integer_to_binary(N),
        ?assertEqual(new, gen_server:call(Pid, {check_dedup, Id}))
    end, lists:seq(1, 2048)),
    %% First entry should still be present (buffer not yet overflowed)
    ?assertEqual(duplicate, gen_server:call(Pid, {check_dedup, <<"1">>})),
    %% Add one more to overflow
    ?assertEqual(new, gen_server:call(Pid, {check_dedup, <<"2049">>})),
    %% Now "1" was evicted (it was the oldest), so it should be "new" again
    ?assertEqual(new, gen_server:call(Pid, {check_dedup, <<"1">>})),
    stop_test(Pid).

%%====================================================================
%% Message ID extraction tests
%%====================================================================

extract_binary_key_message_id_test() ->
    Msg = #{<<"message_id">> => <<"abc123">>},
    ?assertEqual(<<"abc123">>, macula_multi_relay:extract_message_id(Msg)).

extract_atom_key_message_id_test() ->
    Msg = #{message_id => <<"abc123">>},
    ?assertEqual(<<"abc123">>, macula_multi_relay:extract_message_id(Msg)).

extract_nested_payload_message_id_test() ->
    Msg = #{payload => #{<<"message_id">> => <<"abc123">>}},
    ?assertEqual(<<"abc123">>, macula_multi_relay:extract_message_id(Msg)).

extract_missing_message_id_test() ->
    ?assertEqual(undefined, macula_multi_relay:extract_message_id(#{topic => <<"t">>})),
    ?assertEqual(undefined, macula_multi_relay:extract_message_id(#{})).

%%====================================================================
%% Status test
%%====================================================================

status_test() ->
    {ok, Pid} = start_test_multi_relay(),
    %% Give spawn_connections time to run
    timer:sleep(200),
    {ok, Status} = macula_multi_relay:get_status(Pid),
    %% relay_client starts even if connection fails (deferred connect),
    %% so active_count = target_count
    ?assertEqual(2, maps:get(target_count, Status)),
    ?assertEqual(2, maps:get(active_count, Status)),
    ?assertEqual(0, maps:get(subscriptions, Status)),
    ?assertEqual(0, maps:get(procedures, Status)),
    stop_test(Pid).

%%====================================================================
%% Role assignment tests
%%====================================================================

assign_roles_empty_test() ->
    ?assertEqual([], macula_multi_relay:assign_roles([])).

%%====================================================================
%% Shuffle test
%%====================================================================

shuffle_preserves_elements_test() ->
    List = [1, 2, 3, 4, 5],
    Shuffled = macula_multi_relay:shuffle(List),
    ?assertEqual(lists:sort(List), lists:sort(Shuffled)).

shuffle_empty_test() ->
    ?assertEqual([], macula_multi_relay:shuffle([])).

%%====================================================================
%% Helpers
%%====================================================================

start_test_multi_relay() ->
    %% Start with no actual relay connections (localhost will fail to connect,
    %% but the gen_server still starts and dedup works)
    macula_multi_relay:start_link(#{
        relays => [<<"https://localhost:19999">>, <<"https://localhost:19998">>],
        connections => 2,
        realm => <<"test">>,
        identity => <<"test-node">>
    }).

stop_test(Pid) ->
    catch macula_multi_relay:stop(Pid),
    ok.
