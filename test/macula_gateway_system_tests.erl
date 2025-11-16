%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_gateway_system module (ROOT SUPERVISOR).
%%% Tests root supervisor functionality - supervises quic_server, gateway, workers_sup.
%%%
%%% After Phase 2 QUIC Refactoring:
%%% - macula_gateway_system is the ROOT supervisor
%%% - It supervises: quic_server, gateway, workers_sup (as siblings)
%%% - Worker accessor functions moved to macula_gateway_workers_sup
%%%
%%% Responsibilities:
%%% - Start quic_server, gateway, and workers_sup as siblings
%%% - Use rest_for_one strategy for dependency-based restarts
%%% - Wire quic_server and gateway together via set_gateway/2
%%%
%%% Supervision Tests:
%%% - Verify rest_for_one behavior with 3 siblings
%%% - Verify quic_server crash restarts gateway and workers_sup
%%% - Verify gateway crash restarts workers_sup only
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_system_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    {ok, Pid} = macula_gateway_system:start_link(#{}),
    Pid.

cleanup(Pid) ->
    case erlang:is_process_alive(Pid) of
        true ->
            erlang:unlink(Pid),
            exit(Pid, shutdown),
            timer:sleep(50);
        false ->
            ok
    end.

%%%===================================================================
%%% Basic API Tests
%%%===================================================================

module_exports_test() ->
    Exports = macula_gateway_system:module_info(exports),

    %% Root supervisor only has start_link and init (no worker accessors)
    ?assert(lists:member({start_link, 1}, Exports)),
    ?assert(lists:member({init, 1}, Exports)).

supervisor_callbacks_test() ->
    Exports = macula_gateway_system:module_info(exports),
    ?assert(lists:member({init, 1}, Exports)).

%%%===================================================================
%%% Startup Tests
%%%===================================================================

start_link_test() ->
    {ok, Pid} = macula_gateway_system:start_link(#{}),
    ?assert(erlang:is_process_alive(Pid)),
    erlang:unlink(Pid),
    exit(Pid, shutdown),
    timer:sleep(50).

start_link_with_config_test() ->
    Config = #{realm => <<"test.realm">>, port => 5555},
    {ok, Pid} = macula_gateway_system:start_link(Config),
    ?assert(erlang:is_process_alive(Pid)),
    erlang:unlink(Pid),
    exit(Pid, shutdown),
    timer:sleep(50).

%%%===================================================================
%%% Child Supervision Tests
%%%===================================================================

supervisor_starts_all_children_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(SupPid) ->
        %% Verify all 4 children started
        Children = supervisor:which_children(SupPid),
        [
            ?_assertEqual(4, length(Children)),
            ?_assert(lists:keymember(macula_gateway_clients, 1, Children)),
            ?_assert(lists:keymember(macula_gateway_pubsub, 1, Children)),
            ?_assert(lists:keymember(macula_gateway_rpc, 1, Children)),
            ?_assert(lists:keymember(macula_gateway_mesh, 1, Children))
        ]
     end}.

get_client_manager_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(SupPid) ->
        {ok, ClientMgrPid} = macula_gateway_system:get_client_manager(SupPid),
        [
            ?_assert(is_pid(ClientMgrPid)),
            ?_assert(erlang:is_process_alive(ClientMgrPid))
        ]
     end}.

get_pubsub_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(SupPid) ->
        {ok, PubSubPid} = macula_gateway_system:get_pubsub(SupPid),
        [
            ?_assert(is_pid(PubSubPid)),
            ?_assert(erlang:is_process_alive(PubSubPid))
        ]
     end}.

get_rpc_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(SupPid) ->
        {ok, RpcPid} = macula_gateway_system:get_rpc(SupPid),
        [
            ?_assert(is_pid(RpcPid)),
            ?_assert(erlang:is_process_alive(RpcPid))
        ]
     end}.

%%%===================================================================
%%% Supervision Strategy Tests (rest_for_one)
%%%===================================================================

%% @doc Test rest_for_one: killing client_manager restarts all children
rest_for_one_client_manager_crash_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(SupPid) ->
        %% Get original child PIDs
        {ok, OrigClientMgr} = macula_gateway_sup:get_client_manager(SupPid),
        {ok, OrigPubSub} = macula_gateway_system:get_pubsub(SupPid),
        {ok, OrigRpc} = macula_gateway_system:get_rpc(SupPid),
        {ok, OrigMesh} = macula_gateway_system:get_mesh(SupPid),

        %% Kill client_manager (first child)
        exit(OrigClientMgr, kill),
        timer:sleep(100),

        %% All children should be restarted (rest_for_one: N and all after N)
        {ok, NewClientMgr} = macula_gateway_sup:get_client_manager(SupPid),
        {ok, NewPubSub} = macula_gateway_system:get_pubsub(SupPid),
        {ok, NewRpc} = macula_gateway_system:get_rpc(SupPid),
        {ok, NewMesh} = macula_gateway_system:get_mesh(SupPid),

        [
            ?_assertNot(OrigClientMgr =:= NewClientMgr),
            ?_assertNot(OrigPubSub =:= NewPubSub),
            ?_assertNot(OrigRpc =:= NewRpc),
            ?_assertNot(OrigMesh =:= NewMesh),
            ?_assert(erlang:is_process_alive(NewClientMgr)),
            ?_assert(erlang:is_process_alive(NewPubSub)),
            ?_assert(erlang:is_process_alive(NewRpc)),
            ?_assert(erlang:is_process_alive(NewMesh))
        ]
     end}.

%% @doc Test rest_for_one: killing pubsub restarts pubsub, rpc, mesh (not client_manager)
rest_for_one_pubsub_crash_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(SupPid) ->
        %% Get original child PIDs
        {ok, OrigClientMgr} = macula_gateway_sup:get_client_manager(SupPid),
        {ok, OrigPubSub} = macula_gateway_system:get_pubsub(SupPid),
        {ok, OrigRpc} = macula_gateway_system:get_rpc(SupPid),
        {ok, OrigMesh} = macula_gateway_system:get_mesh(SupPid),

        %% Kill pubsub (second child)
        exit(OrigPubSub, kill),
        timer:sleep(100),

        %% client_manager should NOT restart (before pubsub)
        %% pubsub, rpc, mesh should restart (N and after N)
        {ok, NewClientMgr} = macula_gateway_sup:get_client_manager(SupPid),
        {ok, NewPubSub} = macula_gateway_system:get_pubsub(SupPid),
        {ok, NewRpc} = macula_gateway_system:get_rpc(SupPid),
        {ok, NewMesh} = macula_gateway_system:get_mesh(SupPid),

        [
            ?_assertEqual(OrigClientMgr, NewClientMgr),  % Should be same PID
            ?_assertNot(OrigPubSub =:= NewPubSub),       % Should be new PID
            ?_assertNot(OrigRpc =:= NewRpc),             % Should be new PID
            ?_assertNot(OrigMesh =:= NewMesh),           % Should be new PID
            ?_assert(erlang:is_process_alive(NewClientMgr)),
            ?_assert(erlang:is_process_alive(NewPubSub)),
            ?_assert(erlang:is_process_alive(NewRpc)),
            ?_assert(erlang:is_process_alive(NewMesh))
        ]
     end}.

%% @doc Test rest_for_one: killing rpc restarts rpc, mesh (not client_manager, pubsub)
rest_for_one_rpc_crash_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(SupPid) ->
        %% Get original child PIDs
        {ok, OrigClientMgr} = macula_gateway_sup:get_client_manager(SupPid),
        {ok, OrigPubSub} = macula_gateway_system:get_pubsub(SupPid),
        {ok, OrigRpc} = macula_gateway_system:get_rpc(SupPid),
        {ok, OrigMesh} = macula_gateway_system:get_mesh(SupPid),

        %% Kill rpc (third child)
        exit(OrigRpc, kill),
        timer:sleep(100),

        %% client_manager, pubsub should NOT restart (before rpc)
        %% rpc, mesh should restart (N and after N)
        {ok, NewClientMgr} = macula_gateway_sup:get_client_manager(SupPid),
        {ok, NewPubSub} = macula_gateway_system:get_pubsub(SupPid),
        {ok, NewRpc} = macula_gateway_system:get_rpc(SupPid),
        {ok, NewMesh} = macula_gateway_system:get_mesh(SupPid),

        [
            ?_assertEqual(OrigClientMgr, NewClientMgr),  % Should be same PID
            ?_assertEqual(OrigPubSub, NewPubSub),        % Should be same PID
            ?_assertNot(OrigRpc =:= NewRpc),             % Should be new PID
            ?_assertNot(OrigMesh =:= NewMesh),           % Should be new PID
            ?_assert(erlang:is_process_alive(NewClientMgr)),
            ?_assert(erlang:is_process_alive(NewPubSub)),
            ?_assert(erlang:is_process_alive(NewRpc)),
            ?_assert(erlang:is_process_alive(NewMesh))
        ]
     end}.

%% @doc Test rest_for_one: killing mesh restarts only mesh (not client_manager, pubsub, rpc)
rest_for_one_mesh_crash_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(SupPid) ->
        %% Get original child PIDs
        {ok, OrigClientMgr} = macula_gateway_sup:get_client_manager(SupPid),
        {ok, OrigPubSub} = macula_gateway_system:get_pubsub(SupPid),
        {ok, OrigRpc} = macula_gateway_system:get_rpc(SupPid),
        {ok, OrigMesh} = macula_gateway_system:get_mesh(SupPid),

        %% Kill mesh (fourth/last child)
        exit(OrigMesh, kill),
        timer:sleep(100),

        %% client_manager, pubsub, rpc should NOT restart (before mesh)
        %% mesh should restart (N and after N, but N is last)
        {ok, NewClientMgr} = macula_gateway_sup:get_client_manager(SupPid),
        {ok, NewPubSub} = macula_gateway_system:get_pubsub(SupPid),
        {ok, NewRpc} = macula_gateway_system:get_rpc(SupPid),
        {ok, NewMesh} = macula_gateway_system:get_mesh(SupPid),

        [
            ?_assertEqual(OrigClientMgr, NewClientMgr),  % Should be same PID
            ?_assertEqual(OrigPubSub, NewPubSub),        % Should be same PID
            ?_assertEqual(OrigRpc, NewRpc),              % Should be same PID
            ?_assertNot(OrigMesh =:= NewMesh),           % Should be new PID
            ?_assert(erlang:is_process_alive(NewClientMgr)),
            ?_assert(erlang:is_process_alive(NewPubSub)),
            ?_assert(erlang:is_process_alive(NewRpc)),
            ?_assert(erlang:is_process_alive(NewMesh))
        ]
     end}.

children_stay_alive_after_supervisor_start_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(SupPid) ->
        {ok, ClientMgr} = macula_gateway_sup:get_client_manager(SupPid),
        {ok, PubSub} = macula_gateway_system:get_pubsub(SupPid),
        {ok, Rpc} = macula_gateway_system:get_rpc(SupPid),

        timer:sleep(100),

        [
            ?_assert(erlang:is_process_alive(ClientMgr)),
            ?_assert(erlang:is_process_alive(PubSub)),
            ?_assert(erlang:is_process_alive(Rpc))
        ]
     end}.

%%%===================================================================
%%% Integration Tests
%%%===================================================================

children_are_functional_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(SupPid) ->
        %% Get children
        {ok, ClientMgr} = macula_gateway_sup:get_client_manager(SupPid),
        {ok, PubSub} = macula_gateway_system:get_pubsub(SupPid),
        {ok, Rpc} = macula_gateway_system:get_rpc(SupPid),

        %% Test client manager functionality
        Client = spawn(fun() -> timer:sleep(1000) end),
        ok = macula_gateway_clients:client_connected(ClientMgr, Client,
            #{realm => <<"test">>, node_id => <<"node1">>}),
        {ok, _Info} = macula_gateway_clients:get_client_info(ClientMgr, Client),

        %% Test pubsub functionality
        Stream = spawn(fun() -> timer:sleep(1000) end),
        ok = macula_gateway_pubsub:subscribe(PubSub, Stream, <<"test.topic">>),
        {ok, Subs} = macula_gateway_pubsub:get_subscribers(PubSub, <<"test.topic">>),

        %% Test RPC functionality
        Handler = spawn(fun() -> timer:sleep(1000) end),
        ok = macula_gateway_rpc:register_handler(Rpc, <<"test.proc">>, Handler),
        {ok, _Handler} = macula_gateway_rpc:get_handler(Rpc, <<"test.proc">>),

        [
            ?_assert(lists:member(Stream, Subs))
        ]
     end}.
