%%%-------------------------------------------------------------------
%%% @doc Tests for macula_multi_relay — multi-homing relay client.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_multi_relay_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% ETS-based dedup tests
%%====================================================================

dedup_new_message_test() ->
    Tab = ets:new(test_dedup, [set, public]),
    %% First insert succeeds
    ?assertEqual(true, ets:insert_new(Tab, {<<"msg-001">>, true})),
    ets:delete(Tab).

dedup_duplicate_message_test() ->
    Tab = ets:new(test_dedup, [set, public]),
    ?assertEqual(true, ets:insert_new(Tab, {<<"msg-001">>, true})),
    %% Second insert fails (duplicate)
    ?assertEqual(false, ets:insert_new(Tab, {<<"msg-001">>, true})),
    ets:delete(Tab).

dedup_different_messages_test() ->
    Tab = ets:new(test_dedup, [set, public]),
    ?assertEqual(true, ets:insert_new(Tab, {<<"msg-001">>, true})),
    ?assertEqual(true, ets:insert_new(Tab, {<<"msg-002">>, true})),
    ?assertEqual(false, ets:insert_new(Tab, {<<"msg-001">>, true})),
    ets:delete(Tab).

dedup_concurrent_test() ->
    %% ETS insert_new is atomic — concurrent inserts for same key,
    %% only one returns true.
    Tab = ets:new(test_dedup, [set, public]),
    Self = self(),
    MsgId = <<"concurrent-msg">>,
    lists:foreach(fun(_) ->
        spawn(fun() -> Self ! ets:insert_new(Tab, {MsgId, true}) end)
    end, lists:seq(1, 10)),
    Results = [receive R -> R after 1000 -> timeout end || _ <- lists:seq(1, 10)],
    Trues = length([R || R <- Results, R =:= true]),
    ?assertEqual(1, Trues),
    ets:delete(Tab).

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
