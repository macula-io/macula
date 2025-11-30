%%%-------------------------------------------------------------------
%% @doc Macula Leader Election using Raft Consensus (via ra library).
%%
%% This module provides distributed leader election for platform services.
%% It uses the ra library (Raft implementation) to achieve consensus across
%% the mesh network.
%%
%% Use Cases:
%% - Single coordinator election for matchmaking
%% - Primary node selection for stateful services
%% - Distributed locking primitives
%%
%% Architecture:
%% - Uses ra (Raft) for consensus
%% - Discovers cluster members via Macula DHT
%% - Provides callbacks for leadership changes
%%
%% @end
%%%-------------------------------------------------------------------

-module(macula_leader_election).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1]).
-export([get_leader/0, is_leader/0, get_members/0]).
-export([register_callback/2, unregister_callback/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    node_id :: binary(),
    realm :: binary(),
    cluster_name :: atom(),
    server_id :: atom(),
    leader :: atom() | undefined,
    members :: [atom()],
    callbacks :: #{atom() => fun()}
}).

-define(SERVER, ?MODULE).
-define(CLUSTER_NAME, macula_platform_cluster).

%%====================================================================
%% API functions
%%====================================================================

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @doc Get the current leader node
get_leader() ->
    gen_server:call(?SERVER, get_leader).

%% @doc Check if this node is the leader
is_leader() ->
    gen_server:call(?SERVER, is_leader).

%% @doc Get all cluster members
get_members() ->
    gen_server:call(?SERVER, get_members).

%% @doc Register a callback for leadership changes
%% Callback fun takes (IsLeader :: boolean())
register_callback(CallbackId, Fun) when is_function(Fun, 1) ->
    gen_server:call(?SERVER, {register_callback, CallbackId, Fun}).

%% @doc Unregister a leadership callback
unregister_callback(CallbackId) ->
    gen_server:call(?SERVER, {unregister_callback, CallbackId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Config) ->
    NodeId = maps:get(node_id, Config),
    Realm = maps:get(realm, Config),

    ?LOG_INFO("Initializing for node ~s in realm ~s",
              [NodeId, Realm]),

    %% Create unique server ID for this node
    ServerIdBin = <<Realm/binary, "_", NodeId/binary>>,
    ServerId = binary_to_atom(ServerIdBin, utf8),

    State = #state{
        node_id = NodeId,
        realm = Realm,
        cluster_name = ?CLUSTER_NAME,
        server_id = ServerId,
        leader = undefined,
        members = [],
        callbacks = #{}
    },

    %% Schedule cluster initialization after a delay
    %% (allows mesh to stabilize)
    erlang:send_after(5000, self(), init_cluster),

    ?LOG_INFO("Waiting for mesh to stabilize before initializing Raft cluster"),

    {ok, State}.

handle_call(get_leader, _From, State) ->
    {reply, {ok, State#state.leader}, State};

handle_call(is_leader, _From, State) ->
    IsLeader = (State#state.server_id =:= State#state.leader),
    {reply, IsLeader, State};

handle_call(get_members, _From, State) ->
    {reply, {ok, State#state.members}, State};

handle_call({register_callback, CallbackId, Fun}, _From, State) ->
    Callbacks = maps:put(CallbackId, Fun, State#state.callbacks),

    %% Immediately notify new callback of current leadership status (Khepri pattern)
    %% This ensures callbacks registered after election completes still get notified
    case State#state.leader of
        undefined ->
            ok;  % No leader yet, callback will fire when leader is elected
        Leader ->
            IsLeader = (State#state.server_id =:= Leader),
            try
                Fun(IsLeader)
            catch
                Class:Reason:Stacktrace ->
                    ?LOG_ERROR("Callback ~p failed on registration: ~p:~p~n~p",
                              [CallbackId, Class, Reason, Stacktrace])
            end
    end,

    {reply, ok, State#state{callbacks = Callbacks}};

handle_call({unregister_callback, CallbackId}, _From, State) ->
    Callbacks = maps:remove(CallbackId, State#state.callbacks),
    {reply, ok, State#state{callbacks = Callbacks}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init_cluster, State) ->
    ?LOG_INFO("Initializing Raft cluster"),

    %% For initial implementation, create a single-node cluster
    %% TODO: Discover peers via DHT and add them to cluster
    ClusterName = State#state.cluster_name,
    ServerId = State#state.server_id,

    %% Start ra system if not already started
    case ra:start() of
        ok ->
            ?LOG_INFO("Ra system started");
        {error, {already_started, _}} ->
            ?LOG_INFO("Ra system already running");
        {error, Reason} ->
            ?LOG_ERROR("Failed to start ra: ~p", [Reason])
    end,

    %% Define the ra server configuration
    %% Using our custom state machine for leader election
    Machine = {module, macula_leader_machine, #{}},

    %% Generate proper UID using ra:new_uid (Khepri pattern)
    UId = ra:new_uid(atom_to_binary(ClusterName, utf8)),

    ServerConfig = #{
        id => ServerId,
        uid => UId,
        cluster_name => ClusterName,
        log_init_args => #{uid => UId},
        initial_members => [ServerId],  % Single node for now
        machine => Machine
    },

    %% Start the ra server
    case ra:start_server(ServerConfig) of
        ok ->
            ?LOG_INFO("Ra server started: ~p", [ServerId]),

            %% Trigger election
            case ra:trigger_election(ServerId) of
                ok ->
                    ?LOG_INFO("Election triggered");
                {error, ElectReason} ->
                    ?LOG_WARNING("Election trigger failed: ~p", [ElectReason])
            end,

            %% Start checking for leader immediately (Khepri pattern: aggressive polling initially)
            erlang:send_after(500, self(), check_leader),

            {noreply, State#state{members = [ServerId]}};

        {error, {already_started, _}} ->
            ?LOG_INFO("Ra server already started"),
            %% Start checking immediately
            erlang:send_after(500, self(), check_leader),
            {noreply, State#state{members = [ServerId]}};

        {error, StartReason} ->
            ?LOG_ERROR("Failed to start ra server: ~p", [StartReason]),
            %% Retry after delay
            erlang:send_after(5000, self(), init_cluster),
            {noreply, State}
    end;

handle_info(check_leader, State) ->
    ServerId = State#state.server_id,

    %% Query ra for current leader with timeout (Khepri pattern)
    NewLeader = case ra:members(ServerId, 2000) of
        {ok, Members, Leader} ->
            ?LOG_DEBUG("Cluster state - Members: ~p, Leader: ~p",
                      [Members, Leader]),

            OldLeader = State#state.leader,

            %% Check if leadership changed
            case {OldLeader, Leader} of
                {Leader, Leader} ->
                    ok;  % No change
                {_, undefined} ->
                    ?LOG_DEBUG("No leader elected yet, retrying...");
                {undefined, Leader} when Leader =/= undefined ->
                    ?LOG_INFO("Leader elected: ~p", [Leader]),
                    IsLeader = (ServerId =:= Leader),
                    notify_callbacks(IsLeader, State#state.callbacks);
                {OldLeader, Leader} when OldLeader =/= Leader ->
                    ?LOG_INFO("Leadership changed: ~p -> ~p",
                             [OldLeader, Leader]),
                    IsLeader = (ServerId =:= Leader),
                    notify_callbacks(IsLeader, State#state.callbacks)
            end,

            Leader;

        {timeout, _} ->
            ?LOG_WARNING("Timeout querying ra members, will retry"),
            State#state.leader;

        {error, Reason} ->
            ?LOG_ERROR("Failed to query ra members: ~p, will retry", [Reason]),
            State#state.leader
    end,

    %% Schedule next check (shorter interval if no leader yet - Khepri pattern)
    NextCheckInterval = case NewLeader of
        undefined -> 1000;  % Check more frequently when waiting for leader
        _ -> 5000           % Normal interval when leader is established
    end,
    erlang:send_after(NextCheckInterval, self(), check_leader),

    {noreply, State#state{leader = NewLeader}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
notify_callbacks(IsLeader, Callbacks) ->
    maps:foreach(
        fun(CallbackId, Fun) ->
            try
                Fun(IsLeader)
            catch
                Class:Reason:Stacktrace ->
                    ?LOG_ERROR("Callback ~p failed: ~p:~p~n~p",
                              [CallbackId, Class, Reason, Stacktrace])
            end
        end,
        Callbacks
    ).
