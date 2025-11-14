%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_connection_sup module.
%%%
%%% Tests the supervision tree for connection subsystem:
%%% - Supervisor lifecycle (start/stop)
%%% - Child process creation
%%% - Supervision strategy (rest_for_one)
%%% - Fault isolation (crash and restart behavior)
%%% - Child specifications
%%%
%%% Supervision Tests:
%%% - Verify rest_for_one behavior: killing child N restarts N and all after N
%%% - Verify fault isolation: handler crashes don't restart connection_manager
%%% - Verify dependency consistency: connection_manager crash restarts all
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection_sup_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Supervisor Lifecycle Tests
%%%===================================================================

supervisor_can_be_stopped_test() ->
    %% Ensure gproc is started (first test alphabetically)
    application:ensure_all_started(gproc),

    Url = <<"http://localhost:4001">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, SupPid} = macula_connection_sup:start_link(Url, Opts),
    ?assert(is_process_alive(SupPid)),

    ok = macula_connection_sup:stop(SupPid),
    timer:sleep(100), % Allow shutdown to complete

    ?assertNot(is_process_alive(SupPid)).

supervisor_starts_successfully_test() ->
    Url = <<"http://localhost:4000">>,
    Opts = #{
        realm => <<"org.example.test">>,
        node_id => <<"test_node_123">>
    },

    Result = macula_connection_sup:start_link(Url, Opts),

    ?assertMatch({ok, _Pid}, Result),

    case Result of
        {ok, SupPid} ->
            ?assert(is_pid(SupPid)),
            ?assert(is_process_alive(SupPid)),
            macula_connection_sup:stop(SupPid),
            timer:sleep(100), % Allow cleanup
            ?assertNot(is_process_alive(SupPid));
        _ ->
            ok
    end.

%%%===================================================================
%%% Child Process Tests
%%%===================================================================

supervisor_creates_all_children_test() ->
    Url = <<"http://localhost:4002">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, SupPid} = macula_connection_sup:start_link(Url, Opts),

    %% Get list of children
    Children = supervisor:which_children(SupPid),

    %% Should have exactly 4 children
    ?assertEqual(4, length(Children)),

    %% Extract child IDs
    ChildIds = [Id || {Id, _Pid, _Type, _Modules} <- Children],

    %% Verify all expected children are present
    ?assert(lists:member(connection_manager, ChildIds)),
    ?assert(lists:member(pubsub_handler, ChildIds)),
    ?assert(lists:member(rpc_handler, ChildIds)),
    ?assert(lists:member(advertisement_manager, ChildIds)),

    macula_connection_sup:stop(SupPid),
    timer:sleep(100).

supervisor_children_have_correct_order_test() ->


    Url = <<"http://localhost:4003">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, SupPid} = macula_connection_sup:start_link(Url, Opts),

    Children = supervisor:which_children(SupPid),
    ChildIds = [Id || {Id, _Pid, _Type, _Modules} <- Children],

    %% connection_manager should be first (started first)
    ?assertEqual(connection_manager, lists:nth(1, ChildIds)),

    macula_connection_sup:stop(SupPid),
    timer:sleep(100).

supervisor_children_are_workers_test() ->


    Url = <<"http://localhost:4004">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, SupPid} = macula_connection_sup:start_link(Url, Opts),

    Children = supervisor:which_children(SupPid),

    %% All children should be workers
    lists:foreach(fun({_Id, _Pid, Type, _Modules}) ->
        ?assertEqual(worker, Type)
    end, Children),

    macula_connection_sup:stop(SupPid),
    timer:sleep(100).

supervisor_children_are_running_test() ->


    Url = <<"http://localhost:4005">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, SupPid} = macula_connection_sup:start_link(Url, Opts),

    Children = supervisor:which_children(SupPid),

    %% All children should have PIDs and be alive
    lists:foreach(fun({Id, Pid, _Type, _Modules}) ->
        case Pid of
            undefined ->
                ?assert(false, io_lib:format("Child ~p not started", [Id]));
            P when is_pid(P) ->
                ?assert(is_process_alive(P),
                       io_lib:format("Child ~p not alive", [Id]));
            _ ->
                ?assert(false, io_lib:format("Child ~p has invalid PID: ~p", [Id, Pid]))
        end
    end, Children),

    macula_connection_sup:stop(SupPid),
    timer:sleep(100).

%%%===================================================================
%%% Supervision Strategy Tests
%%%===================================================================

supervisor_count_children_test() ->


    Url = <<"http://localhost:4006">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, SupPid} = macula_connection_sup:start_link(Url, Opts),

    %% Get child counts
    Counts = supervisor:count_children(SupPid),

    %% Should have 4 specs, 4 active, 0 supervisors, 4 workers
    ?assertEqual(4, proplists:get_value(specs, Counts)),
    ?assertEqual(4, proplists:get_value(active, Counts)),
    ?assertEqual(0, proplists:get_value(supervisors, Counts)),
    ?assertEqual(4, proplists:get_value(workers, Counts)),

    macula_connection_sup:stop(SupPid),
    timer:sleep(100).

supervisor_provides_access_to_children_test() ->


    Url = <<"http://localhost:4007">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, SupPid} = macula_connection_sup:start_link(Url, Opts),

    Children = supervisor:which_children(SupPid),

    %% Can look up connection_manager
    ConnMgr = lists:keyfind(connection_manager, 1, Children),
    ?assertMatch({connection_manager, Pid, worker, [macula_connection_manager]}
                 when is_pid(Pid), ConnMgr),

    %% Can look up pubsub_handler
    PubSub = lists:keyfind(pubsub_handler, 1, Children),
    ?assertMatch({pubsub_handler, Pid, worker, [macula_pubsub_handler]}
                 when is_pid(Pid), PubSub),

    %% Can look up rpc_handler
    Rpc = lists:keyfind(rpc_handler, 1, Children),
    ?assertMatch({rpc_handler, Pid, worker, [macula_rpc_handler]}
                 when is_pid(Pid), Rpc),

    %% Can look up advertisement_manager
    AdvMgr = lists:keyfind(advertisement_manager, 1, Children),
    ?assertMatch({advertisement_manager, Pid, worker, [macula_advertisement_manager]}
                 when is_pid(Pid), AdvMgr),

    macula_connection_sup:stop(SupPid),
    timer:sleep(100).

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

supervisor_handles_invalid_url_test() ->


    %% Empty URL - children will start but connection_manager will fail to parse
    Url = <<"">>,
    Opts = #{realm => <<"test.realm">>},

    %% Supervisor should still start (children fail during init)
    Result = macula_connection_sup:start_link(Url, Opts),

    %% Will likely fail to start due to connection_manager init failure
    case Result of
        {ok, SupPid} ->
            %% If it started, stop it
            macula_connection_sup:stop(SupPid),
            timer:sleep(100);
        {error, _Reason} ->
            %% Expected - connection_manager failed to start
            ok
    end.

supervisor_restarts_with_different_urls_test() ->


    %% Start first supervisor
    Url1 = <<"http://localhost:5001">>,
    Opts1 = #{realm => <<"test.realm1">>},

    {ok, SupPid1} = macula_connection_sup:start_link(Url1, Opts1),
    macula_connection_sup:stop(SupPid1),
    timer:sleep(100),

    %% Start second supervisor with different URL
    Url2 = <<"http://localhost:5002">>,
    Opts2 = #{realm => <<"test.realm2">>},

    {ok, SupPid2} = macula_connection_sup:start_link(Url2, Opts2),
    ?assert(is_process_alive(SupPid2)),

    macula_connection_sup:stop(SupPid2),
    timer:sleep(100).

%%%===================================================================
%%% Options Passing Tests
%%%===================================================================

supervisor_passes_options_to_children_test() ->


    Url = <<"http://localhost:4008">>,
    CustomNodeId = <<"custom_node_987">>,
    Opts = #{
        realm => <<"test.realm">>,
        node_id => CustomNodeId
    },

    {ok, SupPid} = macula_connection_sup:start_link(Url, Opts),

    %% Supervisor should have started
    ?assert(is_process_alive(SupPid)),

    %% All children should have started (they receive Opts)
    Children = supervisor:which_children(SupPid),
    RunningChildren = [Pid || {_Id, Pid, _Type, _Modules} <- Children, is_pid(Pid)],

    %% Should have 4 running children
    ?assertEqual(4, length(RunningChildren)),

    macula_connection_sup:stop(SupPid),
    timer:sleep(100).

%%%===================================================================
%%% Supervision Strategy Tests (rest_for_one)
%%%===================================================================

%% Helper function to get child PID by ID
get_child_pid(SupPid, ChildId) ->
    Children = supervisor:which_children(SupPid),
    case lists:keyfind(ChildId, 1, Children) of
        {ChildId, Pid, _Type, _Modules} when is_pid(Pid) -> Pid;
        _ -> undefined
    end.

%% @doc Test rest_for_one: killing connection_manager restarts all children
rest_for_one_connection_manager_crash_test() ->
    Url = <<"http://localhost:6001">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, SupPid} = macula_connection_sup:start_link(Url, Opts),

    %% Get original child PIDs
    OrigConnMgr = get_child_pid(SupPid, connection_manager),
    OrigPubSub = get_child_pid(SupPid, pubsub_handler),
    OrigRpc = get_child_pid(SupPid, rpc_handler),
    OrigAdvMgr = get_child_pid(SupPid, advertisement_manager),

    ?assert(is_pid(OrigConnMgr)),
    ?assert(is_pid(OrigPubSub)),
    ?assert(is_pid(OrigRpc)),
    ?assert(is_pid(OrigAdvMgr)),

    %% Kill connection_manager (first child)
    exit(OrigConnMgr, kill),
    timer:sleep(200),

    %% All children should be restarted (rest_for_one: N and all after N)
    NewConnMgr = get_child_pid(SupPid, connection_manager),
    NewPubSub = get_child_pid(SupPid, pubsub_handler),
    NewRpc = get_child_pid(SupPid, rpc_handler),
    NewAdvMgr = get_child_pid(SupPid, advertisement_manager),

    ?assert(is_pid(NewConnMgr)),
    ?assert(is_pid(NewPubSub)),
    ?assert(is_pid(NewRpc)),
    ?assert(is_pid(NewAdvMgr)),

    ?assertNot(OrigConnMgr =:= NewConnMgr),
    ?assertNot(OrigPubSub =:= NewPubSub),
    ?assertNot(OrigRpc =:= NewRpc),
    ?assertNot(OrigAdvMgr =:= NewAdvMgr),

    macula_connection_sup:stop(SupPid),
    timer:sleep(100).

%% @doc Test rest_for_one: killing pubsub_handler restarts pubsub, rpc, advmgr (not connection_manager)
rest_for_one_pubsub_handler_crash_test() ->
    Url = <<"http://localhost:6002">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, SupPid} = macula_connection_sup:start_link(Url, Opts),

    %% Get original child PIDs
    OrigConnMgr = get_child_pid(SupPid, connection_manager),
    OrigPubSub = get_child_pid(SupPid, pubsub_handler),
    OrigRpc = get_child_pid(SupPid, rpc_handler),
    OrigAdvMgr = get_child_pid(SupPid, advertisement_manager),

    %% Kill pubsub_handler (second child)
    exit(OrigPubSub, kill),
    timer:sleep(200),

    %% connection_manager should NOT restart (before pubsub)
    %% pubsub, rpc, advmgr should restart (N and after N)
    NewConnMgr = get_child_pid(SupPid, connection_manager),
    NewPubSub = get_child_pid(SupPid, pubsub_handler),
    NewRpc = get_child_pid(SupPid, rpc_handler),
    NewAdvMgr = get_child_pid(SupPid, advertisement_manager),

    ?assertEqual(OrigConnMgr, NewConnMgr),  % Should be same PID
    ?assertNot(OrigPubSub =:= NewPubSub),   % Should be new PID
    ?assertNot(OrigRpc =:= NewRpc),         % Should be new PID
    ?assertNot(OrigAdvMgr =:= NewAdvMgr),   % Should be new PID

    ?assert(is_process_alive(NewConnMgr)),
    ?assert(is_process_alive(NewPubSub)),
    ?assert(is_process_alive(NewRpc)),
    ?assert(is_process_alive(NewAdvMgr)),

    macula_connection_sup:stop(SupPid),
    timer:sleep(100).

%% @doc Test rest_for_one: killing rpc_handler restarts rpc, advmgr (not connection_manager, pubsub)
rest_for_one_rpc_handler_crash_test() ->
    Url = <<"http://localhost:6003">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, SupPid} = macula_connection_sup:start_link(Url, Opts),

    %% Get original child PIDs
    OrigConnMgr = get_child_pid(SupPid, connection_manager),
    OrigPubSub = get_child_pid(SupPid, pubsub_handler),
    OrigRpc = get_child_pid(SupPid, rpc_handler),
    OrigAdvMgr = get_child_pid(SupPid, advertisement_manager),

    %% Kill rpc_handler (third child)
    exit(OrigRpc, kill),
    timer:sleep(200),

    %% connection_manager, pubsub should NOT restart (before rpc)
    %% rpc, advmgr should restart (N and after N)
    NewConnMgr = get_child_pid(SupPid, connection_manager),
    NewPubSub = get_child_pid(SupPid, pubsub_handler),
    NewRpc = get_child_pid(SupPid, rpc_handler),
    NewAdvMgr = get_child_pid(SupPid, advertisement_manager),

    ?assertEqual(OrigConnMgr, NewConnMgr),  % Should be same PID
    ?assertEqual(OrigPubSub, NewPubSub),    % Should be same PID
    ?assertNot(OrigRpc =:= NewRpc),         % Should be new PID
    ?assertNot(OrigAdvMgr =:= NewAdvMgr),   % Should be new PID

    ?assert(is_process_alive(NewConnMgr)),
    ?assert(is_process_alive(NewPubSub)),
    ?assert(is_process_alive(NewRpc)),
    ?assert(is_process_alive(NewAdvMgr)),

    macula_connection_sup:stop(SupPid),
    timer:sleep(100).

%% @doc Test rest_for_one: killing advertisement_manager restarts only advmgr (not others)
rest_for_one_advertisement_manager_crash_test() ->
    Url = <<"http://localhost:6004">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, SupPid} = macula_connection_sup:start_link(Url, Opts),

    %% Get original child PIDs
    OrigConnMgr = get_child_pid(SupPid, connection_manager),
    OrigPubSub = get_child_pid(SupPid, pubsub_handler),
    OrigRpc = get_child_pid(SupPid, rpc_handler),
    OrigAdvMgr = get_child_pid(SupPid, advertisement_manager),

    %% Kill advertisement_manager (fourth/last child)
    exit(OrigAdvMgr, kill),
    timer:sleep(200),

    %% connection_manager, pubsub, rpc should NOT restart (before advmgr)
    %% advmgr should restart (N and after N, but N is last)
    NewConnMgr = get_child_pid(SupPid, connection_manager),
    NewPubSub = get_child_pid(SupPid, pubsub_handler),
    NewRpc = get_child_pid(SupPid, rpc_handler),
    NewAdvMgr = get_child_pid(SupPid, advertisement_manager),

    ?assertEqual(OrigConnMgr, NewConnMgr),  % Should be same PID
    ?assertEqual(OrigPubSub, NewPubSub),    % Should be same PID
    ?assertEqual(OrigRpc, NewRpc),          % Should be same PID
    ?assertNot(OrigAdvMgr =:= NewAdvMgr),   % Should be new PID

    ?assert(is_process_alive(NewConnMgr)),
    ?assert(is_process_alive(NewPubSub)),
    ?assert(is_process_alive(NewRpc)),
    ?assert(is_process_alive(NewAdvMgr)),

    macula_connection_sup:stop(SupPid),
    timer:sleep(100).
