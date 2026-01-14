%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_content_transfer module.
%%% Tests the want/have/block exchange protocol.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_transfer_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    %% Create temp directory for content store
    TempDir = "/tmp/macula_content_transfer_test_" ++
              integer_to_list(erlang:system_time(microsecond)),
    ok = filelib:ensure_dir(TempDir ++ "/"),
    application:set_env(macula, content_store_dir, TempDir),
    {ok, StorePid} = macula_content_store:start_link(#{base_dir => TempDir}),
    {ok, TransferPid} = macula_content_transfer:start_link(#{
        node_id => test_node_id(),
        store => macula_content_store
    }),
    {StorePid, TransferPid, TempDir}.

cleanup({StorePid, TransferPid, TempDir}) ->
    gen_server:stop(TransferPid),
    gen_server:stop(StorePid),
    os:cmd("rm -rf " ++ TempDir),
    ok.

test_node_id() ->
    <<"test-node-", (integer_to_binary(erlang:system_time(microsecond)))/binary>>.

%%%===================================================================
%%% Basic Operations Tests
%%%===================================================================

basic_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun start_link_test/1,
      fun generate_request_id_test/1
     ]}.

start_link_test({_StorePid, TransferPid, _Dir}) ->
    fun() ->
        ?assert(is_pid(TransferPid)),
        ?assert(is_process_alive(TransferPid))
    end.

generate_request_id_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        ReqId1 = macula_content_transfer:generate_request_id(),
        ReqId2 = macula_content_transfer:generate_request_id(),
        ?assertEqual(16, byte_size(ReqId1)),
        ?assertNotEqual(ReqId1, ReqId2)
    end.

%%%===================================================================
%%% Want Message Tests
%%%===================================================================

want_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun create_want_msg_test/1,
      fun create_want_msg_with_priority_test/1,
      fun parse_want_msg_test/1
     ]}.

create_want_msg_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        MCIDs = [<<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>,
                 <<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>],
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_want_msg(MCIDs, NodeId),
        ?assertEqual(NodeId, maps:get(from_node, Msg)),
        Wants = maps:get(wants, Msg),
        ?assertEqual(2, length(Wants)),
        ?assert(maps:is_key(request_id, Msg))
    end.

create_want_msg_with_priority_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        MCID = <<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>,
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_want_msg([{MCID, 255}], NodeId),
        [Want] = maps:get(wants, Msg),
        ?assertEqual(MCID, maps:get(mcid, Want)),
        ?assertEqual(255, maps:get(priority, Want))
    end.

parse_want_msg_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        MCIDs = [<<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>],
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_want_msg(MCIDs, NodeId),
        {ok, Parsed} = macula_content_transfer:parse_want_msg(Msg),
        ?assertEqual(NodeId, maps:get(from_node, Parsed)),
        ?assertEqual(1, length(maps:get(wants, Parsed)))
    end.

%%%===================================================================
%%% Have Message Tests
%%%===================================================================

have_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun create_have_msg_test/1,
      fun create_have_msg_with_manifest_test/1,
      fun parse_have_msg_test/1
     ]}.

create_have_msg_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        MCIDs = [<<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>],
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_have_msg(MCIDs, NodeId),
        ?assertEqual(MCIDs, maps:get(haves, Msg)),
        ?assertEqual(NodeId, maps:get(from_node, Msg))
    end.

create_have_msg_with_manifest_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        MCIDs = [<<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>],
        ManifestMCID = <<1, 16#56, (crypto:strong_rand_bytes(32))/binary>>,
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_have_msg(MCIDs, NodeId, #{manifest_mcid => ManifestMCID}),
        ?assertEqual(ManifestMCID, maps:get(manifest_mcid, Msg))
    end.

parse_have_msg_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        MCIDs = [<<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>],
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_have_msg(MCIDs, NodeId),
        {ok, Parsed} = macula_content_transfer:parse_have_msg(Msg),
        ?assertEqual(MCIDs, maps:get(haves, Parsed))
    end.

%%%===================================================================
%%% Block Message Tests
%%%===================================================================

block_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun create_block_msg_test/1,
      fun parse_block_msg_test/1,
      fun block_msg_has_request_id_test/1
     ]}.

create_block_msg_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        Data = <<"test block data">>,
        MCID = <<1, 16#55, (macula_content_hasher:hash(blake3, Data))/binary>>,
        RequestId = macula_content_transfer:generate_request_id(),
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_block_msg(RequestId, MCID, Data, NodeId),
        ?assertEqual(RequestId, maps:get(request_id, Msg)),
        ?assertEqual(MCID, maps:get(mcid, Msg)),
        ?assertEqual(Data, maps:get(data, Msg)),
        ?assertEqual(NodeId, maps:get(from_node, Msg))
    end.

parse_block_msg_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        Data = <<"block parsing test">>,
        MCID = <<1, 16#55, (macula_content_hasher:hash(blake3, Data))/binary>>,
        RequestId = macula_content_transfer:generate_request_id(),
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_block_msg(RequestId, MCID, Data, NodeId),
        {ok, Parsed} = macula_content_transfer:parse_block_msg(Msg),
        ?assertEqual(Data, maps:get(data, Parsed)),
        ?assertEqual(MCID, maps:get(mcid, Parsed))
    end.

block_msg_has_request_id_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        Data = <<"block request id test">>,
        MCID = <<1, 16#55, (macula_content_hasher:hash(blake3, Data))/binary>>,
        RequestId = <<"unique-request-id-123">>,
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_block_msg(RequestId, MCID, Data, NodeId),
        ?assertEqual(RequestId, maps:get(request_id, Msg))
    end.

%%%===================================================================
%%% Manifest Message Tests
%%%===================================================================

manifest_msg_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun create_manifest_req_msg_test/1,
      fun create_manifest_res_msg_test/1,
      fun parse_manifest_req_msg_test/1,
      fun parse_manifest_res_msg_test/1
     ]}.

create_manifest_req_msg_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        MCID = <<1, 16#56, (crypto:strong_rand_bytes(32))/binary>>,
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_manifest_req_msg(MCID, NodeId),
        ?assertEqual(MCID, maps:get(mcid, Msg)),
        ?assertEqual(NodeId, maps:get(from_node, Msg)),
        ?assert(maps:is_key(request_id, Msg))
    end.

create_manifest_res_msg_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        Data = <<"manifest test content">>,
        {ok, Manifest} = macula_content_manifest:create(Data, #{name => <<"test.tar.gz">>}),
        MCID = maps:get(mcid, Manifest),
        {ok, ManifestBin} = macula_content_manifest:encode(Manifest),
        RequestId = macula_content_transfer:generate_request_id(),
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_manifest_res_msg(RequestId, MCID, ManifestBin, NodeId),
        ?assertEqual(RequestId, maps:get(request_id, Msg)),
        ?assertEqual(MCID, maps:get(mcid, Msg)),
        ?assertEqual(ManifestBin, maps:get(manifest, Msg))
    end.

parse_manifest_req_msg_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        MCID = <<1, 16#56, (crypto:strong_rand_bytes(32))/binary>>,
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_manifest_req_msg(MCID, NodeId),
        {ok, Parsed} = macula_content_transfer:parse_manifest_req_msg(Msg),
        ?assertEqual(MCID, maps:get(mcid, Parsed))
    end.

parse_manifest_res_msg_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        Data = <<"manifest parse test">>,
        {ok, Manifest} = macula_content_manifest:create(Data, #{}),
        MCID = maps:get(mcid, Manifest),
        {ok, ManifestBin} = macula_content_manifest:encode(Manifest),
        RequestId = macula_content_transfer:generate_request_id(),
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_manifest_res_msg(RequestId, MCID, ManifestBin, NodeId),
        {ok, Parsed} = macula_content_transfer:parse_manifest_res_msg(Msg),
        ?assertEqual(ManifestBin, maps:get(manifest, Parsed))
    end.

%%%===================================================================
%%% Cancel Message Tests
%%%===================================================================

cancel_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun create_cancel_msg_test/1,
      fun parse_cancel_msg_test/1
     ]}.

create_cancel_msg_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        RequestId = macula_content_transfer:generate_request_id(),
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_cancel_msg(RequestId, NodeId),
        ?assertEqual(RequestId, maps:get(request_id, Msg)),
        ?assertEqual(NodeId, maps:get(from_node, Msg))
    end.

parse_cancel_msg_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        RequestId = macula_content_transfer:generate_request_id(),
        NodeId = test_node_id(),
        Msg = macula_content_transfer:create_cancel_msg(RequestId, NodeId),
        {ok, Parsed} = macula_content_transfer:parse_cancel_msg(Msg),
        ?assertEqual(RequestId, maps:get(request_id, Parsed))
    end.

%%%===================================================================
%%% Request Tracking Tests
%%%===================================================================

tracking_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun track_want_request_test/1,
      fun pending_requests_test/1,
      fun complete_request_test/1,
      fun cancel_request_test/1,
      fun request_timeout_test/1
     ]}.

track_want_request_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        MCIDs = [<<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>],
        {ok, RequestId} = macula_content_transfer:request_blocks(MCIDs, <<"target-node">>),
        ?assertEqual(16, byte_size(RequestId)),
        Pending = macula_content_transfer:pending_requests(),
        ?assert(lists:member(RequestId, Pending))
    end.

pending_requests_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        ?assertEqual([], macula_content_transfer:pending_requests()),
        MCIDs1 = [<<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>],
        MCIDs2 = [<<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>],
        {ok, ReqId1} = macula_content_transfer:request_blocks(MCIDs1, <<"target1">>),
        {ok, ReqId2} = macula_content_transfer:request_blocks(MCIDs2, <<"target2">>),
        Pending = macula_content_transfer:pending_requests(),
        ?assertEqual(2, length(Pending)),
        ?assert(lists:member(ReqId1, Pending)),
        ?assert(lists:member(ReqId2, Pending))
    end.

complete_request_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        MCIDs = [<<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>],
        {ok, RequestId} = macula_content_transfer:request_blocks(MCIDs, <<"target">>),
        ?assert(lists:member(RequestId, macula_content_transfer:pending_requests())),
        ok = macula_content_transfer:complete_request(RequestId),
        ?assertNot(lists:member(RequestId, macula_content_transfer:pending_requests()))
    end.

cancel_request_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        MCIDs = [<<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>],
        {ok, RequestId} = macula_content_transfer:request_blocks(MCIDs, <<"target">>),
        ok = macula_content_transfer:cancel_request(RequestId),
        ?assertNot(lists:member(RequestId, macula_content_transfer:pending_requests()))
    end.

request_timeout_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        %% Test that requests can be queried for timeout status
        MCIDs = [<<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>],
        {ok, RequestId} = macula_content_transfer:request_blocks(MCIDs, <<"target">>),
        {ok, Info} = macula_content_transfer:request_info(RequestId),
        ?assert(maps:is_key(created_at, Info)),
        ?assert(maps:is_key(target_node, Info))
    end.

%%%===================================================================
%%% Handle Incoming Message Tests
%%%===================================================================

incoming_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun handle_want_stores_block_test/1,
      fun handle_block_stores_data_test/1,
      fun handle_manifest_req_returns_manifest_test/1
     ]}.

handle_want_stores_block_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        %% Store a block first
        Data = <<"block for want test">>,
        Hash = macula_content_hasher:hash(blake3, Data),
        MCID = <<1, 16#55, Hash/binary>>,
        ok = macula_content_store:put_block(MCID, Data),
        %% Handle a want message for that block
        NodeId = test_node_id(),
        WantMsg = macula_content_transfer:create_want_msg([MCID], NodeId),
        {ok, Response} = macula_content_transfer:handle_want(WantMsg),
        ?assertEqual(content_block, maps:get(type, Response)),
        ?assertEqual(Data, maps:get(data, Response))
    end.

handle_block_stores_data_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        %% Create a pending request
        Data = <<"incoming block data">>,
        Hash = macula_content_hasher:hash(blake3, Data),
        MCID = <<1, 16#55, Hash/binary>>,
        {ok, RequestId} = macula_content_transfer:request_blocks([MCID], <<"provider">>),
        %% Handle incoming block
        NodeId = <<"provider">>,
        BlockMsg = macula_content_transfer:create_block_msg(RequestId, MCID, Data, NodeId),
        ok = macula_content_transfer:handle_block(BlockMsg),
        %% Block should be stored
        ?assert(macula_content_store:has_block(MCID)),
        {ok, Retrieved} = macula_content_store:get_block(MCID),
        ?assertEqual(Data, Retrieved)
    end.

handle_manifest_req_returns_manifest_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        %% Store a manifest
        Data = <<"manifest content for request test">>,
        {ok, Manifest} = macula_content_manifest:create(Data, #{name => <<"test.pkg">>}),
        MCID = maps:get(mcid, Manifest),
        ok = macula_content_store:put_manifest(Manifest),
        %% Handle manifest request
        NodeId = test_node_id(),
        ReqMsg = macula_content_transfer:create_manifest_req_msg(MCID, NodeId),
        {ok, Response} = macula_content_transfer:handle_manifest_req(ReqMsg),
        ?assertEqual(content_manifest_res, maps:get(type, Response)),
        %% Decode and verify manifest
        ManifestBin = maps:get(manifest, Response),
        {ok, Decoded} = macula_content_manifest:decode(ManifestBin),
        ?assertEqual(MCID, maps:get(mcid, Decoded))
    end.

%%%===================================================================
%%% Block Verification Tests
%%%===================================================================

verification_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun reject_invalid_block_hash_test/1,
      fun accept_valid_block_hash_test/1
     ]}.

reject_invalid_block_hash_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        %% Create a pending request
        MCID = <<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>,
        {ok, RequestId} = macula_content_transfer:request_blocks([MCID], <<"provider">>),
        %% Send block with wrong data (hash won't match MCID)
        WrongData = <<"this data doesn't match the MCID">>,
        NodeId = <<"provider">>,
        BlockMsg = macula_content_transfer:create_block_msg(RequestId, MCID, WrongData, NodeId),
        {error, hash_mismatch} = macula_content_transfer:handle_block(BlockMsg)
    end.

accept_valid_block_hash_test({_StorePid, _TransferPid, _Dir}) ->
    fun() ->
        %% Create correct block
        Data = <<"valid block content">>,
        Hash = macula_content_hasher:hash(blake3, Data),
        MCID = <<1, 16#55, Hash/binary>>,
        {ok, RequestId} = macula_content_transfer:request_blocks([MCID], <<"provider">>),
        %% Send valid block
        NodeId = <<"provider">>,
        BlockMsg = macula_content_transfer:create_block_msg(RequestId, MCID, Data, NodeId),
        ok = macula_content_transfer:handle_block(BlockMsg)
    end.
