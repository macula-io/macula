%%%-------------------------------------------------------------------
%%% @doc Tests for macula_relay_handler — core relay message routing.
%%%
%%% Tests pure functional parts: pg_members, safe_decode, try_rpc_on_peers,
%%% handle_rpc_call/reply state management, handle_message dispatch,
%%% state record initialization, and replay_pending.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_relay_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%% Mirror the state record from macula_relay_handler for test construction.
%% This must stay in sync with the source module.
-record(state, {
    conn :: reference(),
    stream :: reference() | undefined,
    node_id :: binary(),
    node_name :: binary(),
    is_peer :: boolean(),
    identified :: boolean(),
    pending_msgs :: [term()],
    recv_buffer :: binary(),
    pending_calls :: #{binary() => pid()}
}).

%%====================================================================
%% Setup / Teardown — ensure pg scope is running
%%====================================================================

setup() ->
    case whereis(pg) of
        undefined -> pg:start_link(pg);
        _ -> ok
    end.

%%====================================================================
%% Helpers
%%====================================================================

%% Build a minimal test state. Stream and conn are dummy references.
make_state() ->
    make_state(#{}).

make_state(Overrides) ->
    Conn = maps:get(conn, Overrides, make_ref()),
    Stream = maps:get(stream, Overrides, make_ref()),
    #state{
        conn = Conn,
        stream = Stream,
        node_id = maps:get(node_id, Overrides, <<"test-node">>),
        node_name = maps:get(node_name, Overrides, <<"test@host">>),
        is_peer = maps:get(is_peer, Overrides, false),
        identified = maps:get(identified, Overrides, true),
        pending_msgs = maps:get(pending_msgs, Overrides, []),
        recv_buffer = maps:get(recv_buffer, Overrides, <<>>),
        pending_calls = maps:get(pending_calls, Overrides, #{})
    }.

%%====================================================================
%% State Record Initialization
%%====================================================================

init_returns_empty_state_test() ->
    Conn = make_ref(),
    Stream = make_ref(),
    {ok, State} = macula_relay_handler:init({Conn, Stream}),
    ?assertEqual(Conn, State#state.conn),
    ?assertEqual(Stream, State#state.stream),
    ?assertEqual(<<>>, State#state.node_id),
    ?assertEqual(<<>>, State#state.node_name),
    ?assertEqual(false, State#state.is_peer),
    ?assertEqual(false, State#state.identified),
    ?assertEqual([], State#state.pending_msgs),
    ?assertEqual(<<>>, State#state.recv_buffer),
    ?assertEqual(#{}, State#state.pending_calls).

%%====================================================================
%% pg_members/1
%%====================================================================

pg_members_nonexistent_group_returns_empty_test() ->
    %% pg scope 'pg' should exist (started by kernel), but a random
    %% group should have no members.
    UniqueGroup = {test_relay_handler, make_ref()},
    ?assertEqual([], macula_relay_handler:pg_members(UniqueGroup)).

pg_members_returns_joined_pids_test() ->
    setup(),
    Group = {test_relay_handler_members, make_ref()},
    pg:join(pg, Group, self()),
    Members = macula_relay_handler:pg_members(Group),
    ?assert(lists:member(self(), Members)),
    pg:leave(pg, Group, self()).

pg_members_returns_empty_after_leave_test() ->
    setup(),
    Group = {test_relay_handler_leave, make_ref()},
    pg:join(pg, Group, self()),
    pg:leave(pg, Group, self()),
    ?assertEqual([], macula_relay_handler:pg_members(Group)).

%%====================================================================
%% safe_decode/1
%%====================================================================

safe_decode_valid_json_test() ->
    Json = iolist_to_binary(json:encode(#{<<"key">> => <<"value">>})),
    Result = macula_relay_handler:safe_decode(Json),
    ?assertEqual(<<"value">>, maps:get(<<"key">>, Result)).

safe_decode_invalid_json_returns_empty_map_test() ->
    ?assertEqual(#{}, macula_relay_handler:safe_decode(<<"not valid json">>)).

safe_decode_empty_binary_returns_empty_map_test() ->
    ?assertEqual(#{}, macula_relay_handler:safe_decode(<<>>)).

safe_decode_map_passthrough_test() ->
    Input = #{<<"already">> => <<"decoded">>},
    ?assertEqual(Input, macula_relay_handler:safe_decode(Input)).

safe_decode_non_binary_non_map_returns_empty_map_test() ->
    ?assertEqual(#{}, macula_relay_handler:safe_decode(42)),
    ?assertEqual(#{}, macula_relay_handler:safe_decode(undefined)),
    ?assertEqual(#{}, macula_relay_handler:safe_decode([1, 2, 3])).

%%====================================================================
%% try_rpc_on_peers/3
%%====================================================================

try_rpc_on_peers_empty_list_returns_not_found_test() ->
    ?assertEqual({error, not_found},
                 macula_relay_handler:try_rpc_on_peers(<<"proc">>, #{}, undefined, [])).

try_rpc_on_peers_skips_dead_pid_test() ->
    %% Spawn a process that exits immediately — its pid is dead.
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(10),
    %% macula_relay_client:call/4 on a dead pid will throw, which
    %% try_rpc_on_peers catches and continues to next peer.
    Result = macula_relay_handler:try_rpc_on_peers(
        <<"proc">>, #{}, undefined, [{<<"url1">>, DeadPid}]),
    ?assertEqual({error, not_found}, Result).

%%====================================================================
%% handle_rpc_call/5 — state management
%%====================================================================

handle_rpc_call_stores_caller_in_pending_test() ->
    State = make_state(),
    CallerPid = self(),
    CallId = <<"call-123">>,
    %% handle_rpc_call will try send_to_node which will fail (dummy stream),
    %% but the state update still happens.
    State2 = macula_relay_handler:handle_rpc_call(
        CallerPid, CallId, <<"proc">>, #{}, undefined, State),
    ?assertEqual(CallerPid, maps:get(CallId, State2#state.pending_calls)).

handle_rpc_call_preserves_existing_calls_test() ->
    ExistingCalls = #{<<"old-call">> => self()},
    State = make_state(#{pending_calls => ExistingCalls}),
    OtherPid = spawn(fun() -> receive stop -> ok end end),
    State2 = macula_relay_handler:handle_rpc_call(
        OtherPid, <<"new-call">>, <<"proc">>, #{}, undefined, State),
    ?assertEqual(self(), maps:get(<<"old-call">>, State2#state.pending_calls)),
    ?assertEqual(OtherPid, maps:get(<<"new-call">>, State2#state.pending_calls)),
    OtherPid ! stop.

%%====================================================================
%% handle_rpc_reply/3 — state management
%%====================================================================

handle_rpc_reply_returns_state_unchanged_pending_test() ->
    %% handle_rpc_reply does NOT remove from pending_calls — that
    %% happens in the REPLY message handler. It just sends to node.
    State = make_state(),
    State2 = macula_relay_handler:handle_rpc_reply(
        <<"call-456">>, #{<<"result">> => <<"ok">>}, undefined, State),
    %% State should be unchanged (no pending_calls modification).
    ?assertEqual(State#state.pending_calls, State2#state.pending_calls).

%%====================================================================
%% handle_message dispatch — PING → PONG
%%====================================================================

handle_message_ping_returns_state_test() ->
    %% PING should send PONG to the node and return state unchanged.
    %% send_to_node will fail on dummy stream but state is still returned.
    State = make_state(),
    Timestamp = erlang:system_time(millisecond),
    State2 = macula_relay_handler:handle_message(
        {ok, {ping, #{<<"timestamp">> => Timestamp}}}, State),
    %% State should be the same (ping doesn't modify state).
    ?assertEqual(State#state.node_id, State2#state.node_id),
    ?assertEqual(State#state.pending_calls, State2#state.pending_calls).

%%====================================================================
%% handle_message dispatch — unknown type
%%====================================================================

handle_message_unknown_type_returns_state_test() ->
    State = make_state(),
    State2 = macula_relay_handler:handle_message(
        {ok, {some_unknown_type, #{}}}, State),
    ?assertEqual(State#state.node_id, State2#state.node_id).

%%====================================================================
%% handle_message dispatch — decode error
%%====================================================================

handle_message_error_returns_state_test() ->
    State = make_state(),
    State2 = macula_relay_handler:handle_message(
        {error, bad_checksum}, State),
    ?assertEqual(State#state.node_id, State2#state.node_id).

%%====================================================================
%% handle_message — REPLY removes from pending_calls
%%====================================================================

handle_message_reply_removes_pending_call_test() ->
    CallerPid = spawn(fun() ->
        receive
            {relay_reply, _, _} -> ok;
            {relay_reply, _, _, _} -> ok
        after 1000 -> ok end
    end),
    CallId = <<"reply-test-call">>,
    State = make_state(#{pending_calls => #{CallId => CallerPid}}),
    State2 = macula_relay_handler:handle_message(
        {ok, {reply, #{<<"call_id">> => CallId,
                       <<"result">> => #{<<"data">> => <<"ok">>}}}},
        State),
    ?assertEqual(#{}, State2#state.pending_calls).

handle_message_reply_unknown_callid_no_crash_test() ->
    State = make_state(),
    State2 = macula_relay_handler:handle_message(
        {ok, {reply, #{<<"call_id">> => <<"nonexistent">>,
                       <<"result">> => #{}}}},
        State),
    ?assertEqual(#{}, State2#state.pending_calls).

%%====================================================================
%% handle_message — pre-CONNECT buffering
%%====================================================================

handle_message_buffers_before_connect_test() ->
    %% When identified=false, non-connect messages are buffered.
    State = make_state(#{identified => false, pending_msgs => []}),
    Msg = {ok, {subscribe, #{<<"topics">> => [<<"test">>]}}},
    State2 = macula_relay_handler:handle_message(Msg, State),
    ?assertEqual([Msg], State2#state.pending_msgs).

handle_message_buffers_multiple_before_connect_test() ->
    State = make_state(#{identified => false, pending_msgs => []}),
    Msg1 = {ok, {subscribe, #{<<"topics">> => [<<"t1">>]}}},
    Msg2 = {ok, {ping, #{<<"timestamp">> => 123}}},
    State2 = macula_relay_handler:handle_message(Msg1, State),
    State3 = macula_relay_handler:handle_message(Msg2, State2),
    %% Messages are prepended (reversed later in replay_pending).
    ?assertEqual(2, length(State3#state.pending_msgs)).

%%====================================================================
%% handle_message — PUBLISH broadcasts to pg members (excluding self)
%%====================================================================

handle_message_publish_does_not_send_to_self_test() ->
    setup(),
    %% Join a pg group, then publish — self should NOT receive relay_publish.
    Topic = <<"test.publish.self">>,
    Group = {relay_topic, Topic},
    pg:join(pg, Group, self()),
    State = make_state(#{identified => true}),
    _State2 = macula_relay_handler:handle_message(
        {ok, {publish, #{<<"topic">> => Topic,
                         <<"payload">> => <<"hello">>}}},
        State),
    %% Drain mailbox — should NOT have relay_publish.
    receive
        {relay_publish, Topic, _} -> ?assert(false);
        {relay_publish, Topic, _, _} -> ?assert(false)
    after 50 ->
        ok
    end,
    pg:leave(pg, Group, self()).

%%====================================================================
%% handle_message — internal protocol drops from non-peer
%%====================================================================

handle_message_drops_swim_from_non_peer_test() ->
    State = make_state(#{is_peer => false, identified => true}),
    State2 = macula_relay_handler:handle_message(
        {ok, {publish, #{<<"topic">> => <<"_swim.ping">>,
                         <<"payload">> => <<"data">>}}},
        State),
    %% Should silently return state (no crash, no processing).
    ?assertEqual(State#state.node_id, State2#state.node_id).

handle_message_drops_dht_from_non_peer_test() ->
    State = make_state(#{is_peer => false, identified => true}),
    State2 = macula_relay_handler:handle_message(
        {ok, {publish, #{<<"topic">> => <<"_dht.store">>,
                         <<"payload">> => <<"data">>}}},
        State),
    ?assertEqual(State#state.node_id, State2#state.node_id).

handle_message_drops_bloom_from_non_peer_test() ->
    State = make_state(#{is_peer => false, identified => true}),
    State2 = macula_relay_handler:handle_message(
        {ok, {publish, #{<<"topic">> => <<"_relay.bloom">>,
                         <<"payload">> => <<"data">>}}},
        State),
    ?assertEqual(State#state.node_id, State2#state.node_id).
