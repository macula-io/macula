%%%-------------------------------------------------------------------
%%% @doc
%%% Peer System Supervisor - supervises the peer subsystem.
%%%
%%% Supervision Strategy:
%%% - rest_for_one: If child N crashes, restart N and all children after N
%%% - Rationale: connection_manager is foundational; handlers depend on it
%%%   but are independent of each other. This provides fault isolation
%%%   while maintaining consistency when connection_manager restarts.
%%%
%%% Architecture:
%%% <pre>
%%% macula_peer_system (this module)
%%% ├── macula_connection - QUIC connection lifecycle (transport layer)
%%% ├── macula_pubsub_handler - Pub/sub operations
%%% ├── macula_rpc_handler - RPC operations
%%% └── macula_advertisement_manager - DHT service advertisements
%%% </pre>
%%%
%%% Children (in dependency order):
%%% - macula_connection: QUIC connection lifecycle (foundational)
%%% - macula_pubsub_handler: Pub/sub operations (depends on connection)
%%% - macula_rpc_handler: RPC operations (depends on connection)
%%% - macula_advertisement_manager: DHT advertisements (depends on connection)
%%%
%%% Fault Isolation:
%%% - advertisement_manager crash → only advertisement restarts
%%% - rpc_handler crash → rpc + advertisement restart
%%% - pubsub_handler crash → pubsub + rpc + advertisement restart
%%% - connection crash → all restart (unavoidable - foundational)
%%%
%%% Renamed from macula_connection_sup (v0.7.10) to align with macula_peer
%%% nomenclature and macula_gateway_system naming convention.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peer_system).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the peer system supervisor with given URL and options.
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(Url, Opts) ->
    case supervisor:start_link(?MODULE, {Url, Opts}) of
        {ok, SupPid} ->
            %% Initialize handlers after supervisor has started
            %% This must be done asynchronously to avoid blocking the start_link call
            spawn(fun() -> init_handlers(SupPid) end),
            {ok, SupPid};
        Error ->
            Error
    end.

%% @doc Stop the peer system supervisor and all children.
-spec stop(pid()) -> ok.
stop(Sup) when is_pid(Sup) ->
    exit(Sup, shutdown),
    ok.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @doc Initialize the supervisor with child specifications.
-spec init({binary(), map()}) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({Url, Opts}) ->
    ?LOG_INFO("Starting peer system supervisor for ~s", [Url]),

    %% Supervision strategy: rest_for_one
    %% If child N crashes, restart N and all children after N
    %% - connection_manager crash → all restart (they depend on it)
    %% - handler crash → only that handler and later ones restart (fault isolation)
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 10,  % Max 10 restarts (increased tolerance)
        period => 60      % Within 60 seconds (longer period)
    },

    %% Children in dependency order (connection_manager is foundational)
    ChildSpecs = [
        %% 1. Connection Manager (must start first)
        #{
            id => connection_manager,
            start => {macula_connection, start_link, [Url, Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_connection]
        },

        %% 2. PubSub Handler
        #{
            id => pubsub_handler,
            start => {macula_pubsub_handler, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_pubsub_handler]
        },

        %% 3. RPC Handler
        #{
            id => rpc_handler,
            start => {macula_rpc_handler, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_rpc_handler]
        },

        %% 4. Advertisement Manager
        #{
            id => advertisement_manager,
            start => {macula_advertisement_manager, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_advertisement_manager]
        }
    ],

    %% Schedule initialization of handlers after supervisor starts
    %% This must be done asynchronously because supervisor init/1 must return
    %% before children can be queried
    %% Note: We return the supervisor PID in the return value, but we need to
    %% handle initialization in the start_link callback instead
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Initialize handlers by sending them the connection_manager_pid.
%% This must be called after the supervisor has started and children are running.
%% The function waits a brief moment for children to fully initialize, then
%% looks up their PIDs and sends the set_connection_manager_pid message.
-spec init_handlers(pid()) -> ok.
init_handlers(SupPid) ->
    %% Brief delay to ensure supervisor and children are fully initialized
    timer:sleep(50),

    %% Look up child PIDs from supervisor
    case supervisor:which_children(SupPid) of
        Children when is_list(Children) ->
            %% Extract PIDs for each child
            ConnMgrPid = find_child_pid(Children, connection_manager),
            PubSubPid = find_child_pid(Children, pubsub_handler),
            RpcPid = find_child_pid(Children, rpc_handler),
            AdvMgrPid = find_child_pid(Children, advertisement_manager),

            %% Send connection_manager_pid to children that need it
            case ConnMgrPid of
                undefined ->
                    ?LOG_ERROR("Connection manager not found in peer system");
                _ ->
                    send_if_pid(PubSubPid, {set_connection_manager_pid, ConnMgrPid}),
                    send_if_pid(RpcPid, {set_connection_manager_pid, ConnMgrPid}),
                    send_if_pid(AdvMgrPid, {set_connection_manager_pid, ConnMgrPid}),
                    ?LOG_INFO("Initialized handlers with connection_manager_pid: ~p", [ConnMgrPid])
            end;
        _ ->
            ?LOG_ERROR("Failed to query supervisor children")
    end,
    ok.

%% @private
%% @doc Find a child PID by child ID in supervisor children list.
-spec find_child_pid(list(), atom()) -> pid() | undefined.
find_child_pid(Children, ChildId) ->
    case lists:keyfind(ChildId, 1, Children) of
        {ChildId, Pid, _Type, _Modules} when is_pid(Pid) -> Pid;
        _ -> undefined
    end.

%% @private
%% @doc Send a cast message to a PID if it's valid.
-spec send_if_pid(pid() | undefined, term()) -> ok.
send_if_pid(undefined, _Msg) -> ok;
send_if_pid(Pid, Msg) when is_pid(Pid) ->
    gen_server:cast(Pid, Msg),
    ok.
