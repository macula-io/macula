%%%-------------------------------------------------------------------
%%% @doc
%%% Macula - Main API for distributed workloads on Macula platform.
%%%
%%% This is the ONLY module workload applications should import. It provides
%%% a stable, versioned API for all platform operations:
%%%
%%% - Mesh networking (connect, publish, subscribe, RPC)
%%% - Platform Layer (leader election, CRDTs, workload registration)
%%% - Service discovery (DHT queries, node identity)
%%%
%%% == Quick Start ==
%%%
%%% Connect to local platform and publish events:
%%% ```
%%% {ok, Client} = macula:connect_local(#{realm => &lt;&lt;"my.app"&gt;&gt;}),
%%% ok = macula:publish(Client, &lt;&lt;"my.events"&gt;&gt;, #{type => &lt;&lt;"test"&gt;&gt;}).
%%% '''
%%%
%%% == Architecture ==
%%%
%%% Workload applications run in the same BEAM VM as the Macula platform.
%%% Use `connect_local/1` to connect via process-to-process communication:
%%%
%%% ```
%%% Workload App → macula:connect_local/1 → macula_gateway → Mesh (QUIC/HTTP3)
%%% '''
%%%
%%% == Platform Layer (v0.10.0+) ==
%%%
%%% Register with platform for coordination features:
%%% ```
%%% {ok, #{leader_node := Leader}} = macula:register_workload(Client, #{
%%%     workload_name => &lt;&lt;"my_app"&gt;&gt;
%%% }).
%%% '''
%%%
%%% == DHT Network Bootstrap ==
%%%
%%% The platform handles DHT bootstrapping via `MACULA_BOOTSTRAP_PEERS`.
%%% Workloads don't need to manage peer discovery.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula).
-behaviour(macula_client_behaviour).

%% Mesh Networking API
-export([
    connect/2,
    connect_local/1,
    disconnect/1,
    publish/3,
    publish/4,
    subscribe/3,
    unsubscribe/2,
    discover_subscribers/2,
    call/3,
    call/4,
    advertise/3,
    advertise/4,
    unadvertise/2,
    get_node_id/1
]).

%% Platform Layer API (v0.10.0+)
-export([
    register_workload/2,
    get_leader/1,
    subscribe_leader_changes/2,
    propose_crdt_update/3,
    propose_crdt_update/4,
    read_crdt/2
]).

%% Type exports
-export_type([
    client/0,
    topic/0,
    event_data/0,
    procedure/0,
    args/0,
    options/0,
    subscription_ref/0
]).

%%%===================================================================
%%% Types
%%%===================================================================

-type client() :: pid().
%% Reference to a connected Macula mesh client.

-type topic() :: binary().
%% Topic name for pub/sub operations. Topics should describe event types,
%% not entity IDs. Example: `"my.app.user.registered"' (good),
%% not `"my.app.user.123.registered"' (bad - ID belongs in payload).

-type event_data() :: map() | binary().
%% Event payload data. Typically a map that will be JSON-encoded.

-type procedure() :: binary().
%% RPC procedure name. Example: `"my.app.get_user"'.

-type args() :: map() | list() | binary().
%% Arguments for RPC calls.

-type options() :: map().
%% Connection or operation options.

-type subscription_ref() :: reference().
%% Reference to an active subscription for unsubscribe operations.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Connect to a Macula mesh network.
%%
%% Creates a new HTTP/3 (QUIC) connection to the specified mesh endpoint.
%%
%% == Options ==
%%
%% <ul>
%% <li>`realm' - Required. Binary realm identifier (e.g., `&lt;&lt;"my.app.realm"&gt;&gt;')</li>
%% <li>`auth' - Optional. Authentication map with `api_key' or other auth methods</li>
%% <li>`timeout' - Optional. Connection timeout in milliseconds (default: 5000)</li>
%% <li>`node_id' - Optional. 32-byte node ID (generated if not provided)</li>
%% </ul>
%%
%% == Examples ==
%%
%% ```
%% %% Basic connection
%% {ok, Client} = macula:connect(&lt;&lt;"https://mesh.local:443"&gt;&gt;, #{
%%     realm => &lt;&lt;"my.realm"&gt;&gt;
%% }).
%%
%% %% With API key authentication
%% {ok, Client} = macula:connect(&lt;&lt;"https://mesh.local:443"&gt;&gt;, #{
%%     realm => &lt;&lt;"my.realm"&gt;&gt;,
%%     auth => #{api_key => &lt;&lt;"secret-key"&gt;&gt;}
%% }).
%% '''
-spec connect(Url :: binary(), Opts :: options()) ->
    {ok, client()} | {error, Reason :: term()}.
connect(Url, Opts) when is_binary(Url), is_map(Opts) ->
    macula_peer:start_link(Url, Opts);
connect(Url, Opts) when is_list(Url), is_map(Opts) ->
    connect(list_to_binary(Url), Opts).

%% @doc Connect to the local Macula gateway (for in-VM workloads).
%%
%% This function is used by applications running in the same BEAM VM as
%% the Macula platform. Instead of creating a QUIC connection to localhost,
%% it connects directly to the local `macula_gateway' process via
%% process-to-process communication.
%%
%% == Architecture ==
%%
%% ```
%% Phoenix/Elixir App → macula_local_client → macula_gateway
%%                                             ↓ (QUIC)
%%                                        Other Peers
%% '''
%%
%% == When to Use ==
%%
%% <ul>
%% <li>✅ Use `connect_local/1' when your application runs in the same VM as Macula</li>
%% <li>✅ Phoenix applications deployed with Macula in the same container</li>
%% <li>❌ Do NOT use `connect/2' with localhost URL - it creates unnecessary QUIC overhead</li>
%% </ul>
%%
%% == Options ==
%%
%% <ul>
%% <li>`realm' - Required. Binary realm identifier (e.g., `&lt;&lt;"my.app.realm"&gt;&gt;')</li>
%% <li>`event_handler' - Optional. PID to receive events (default: caller PID)</li>
%% </ul>
%%
%% == Examples ==
%%
%% ```
%% %% Elixir Phoenix application
%% {:ok, client} = :macula.connect_local(%{
%%     realm: "macula.arcade.dev"
%% })
%%
%% %% Erlang application
%% {ok, Client} = macula:connect_local(#{
%%     realm => &lt;&lt;"my.app.realm"&gt;&gt;
%% }).
%% '''
%%
%% @since v0.8.9
-spec connect_local(Opts :: options()) ->
    {ok, client()} | {error, Reason :: term()}.
connect_local(Opts) when is_map(Opts) ->
    macula_local_client:start_link(Opts).

%% @doc Disconnect from the Macula mesh.
%%
%% Cleanly closes the HTTP/3 connection and cleans up all subscriptions.
-spec disconnect(Client :: client()) -> ok | {error, Reason :: term()}.
disconnect(Client) when is_pid(Client) ->
    macula_peer:stop(Client).

%% @doc Publish an event to a topic.
%%
%% Publishes data to the specified topic. All subscribers to this topic
%% will receive the event.
%%
%% == Topic Design ==
%%
%% Topics should describe EVENT TYPES, not entity instances:
%% <ul>
%% <li>Good: `&lt;&lt;"my.app.user.registered"&gt;&gt;' (event type)</li>
%% <li>Bad: `&lt;&lt;"my.app.user.123.registered"&gt;&gt;' (entity ID in topic)</li>
%% </ul>
%%
%% Entity IDs belong in the event payload, not the topic name.
%%
%% == Examples ==
%%
%% ```
%% %% Publish with default options
%% ok = macula:publish(Client, &lt;&lt;"my.app.events"&gt;&gt;, #{
%%     type => &lt;&lt;"user.registered"&gt;&gt;,
%%     user_id => &lt;&lt;"user-123"&gt;&gt;,
%%     email => &lt;&lt;"user@example.com"&gt;&gt;
%% }).
%%
%% %% Publish with options
%% ok = macula:publish(Client, &lt;&lt;"my.app.events"&gt;&gt;, #{
%%     data => &lt;&lt;"important"&gt;&gt;
%% }, #{acknowledge => true}).
%% '''
-spec publish(Client :: client(), Topic :: topic(), Data :: event_data()) ->
    ok | {error, Reason :: term()}.
publish(Client, Topic, Data) when is_pid(Client), is_binary(Topic) ->
    macula_peer:publish(Client, Topic, Data).

%% @doc Publish an event with options.
-spec publish(Client :: client(), Topic :: topic(), Data :: event_data(),
              Opts :: options()) ->
    ok | {error, Reason :: term()}.
publish(Client, Topic, Data, Opts) when is_pid(Client), is_binary(Topic), is_map(Opts) ->
    macula_peer:publish(Client, Topic, Data, Opts).

%% @doc Subscribe to a topic.
%%
%% Subscribes to events on the specified topic. The callback function
%% will be invoked for each event received.
%%
%% == Callback Function ==
%%
%% The callback receives the event data and should return `ok'.
%%
%% == Examples ==
%%
%% ```
%% %% Simple subscription
%% {ok, SubRef} = macula:subscribe(Client, &lt;&lt;"my.app.events"&gt;&gt;,
%%     fun(EventData) ->
%%         io:format("Event: ~p~n", [EventData]),
%%         ok
%%     end).
%%
%% %% Unsubscribe later
%% ok = macula:unsubscribe(Client, SubRef).
%% '''
-spec subscribe(Client :: client(), Topic :: topic(),
                Callback :: fun((event_data()) -> ok)) ->
    {ok, subscription_ref()} | {error, Reason :: term()}.
subscribe(Client, Topic, Callback) when is_pid(Client), is_binary(Topic), is_function(Callback, 1) ->
    macula_peer:subscribe(Client, Topic, Callback).

%% @doc Unsubscribe from a topic.
%%
%% Removes the subscription identified by the subscription reference.
-spec unsubscribe(Client :: client(), SubRef :: subscription_ref()) ->
    ok | {error, Reason :: term()}.
unsubscribe(Client, SubRef) when is_pid(Client), is_reference(SubRef) ->
    macula_peer:unsubscribe(Client, SubRef).

%% @doc Discover subscribers to a topic via DHT query.
%%
%% Queries the DHT for all nodes subscribed to the given topic.
%% Returns a list of subscriber nodes with their node IDs and endpoints.
%%
%% This is used for P2P discovery before sending direct messages.
-spec discover_subscribers(Client :: client(), Topic :: topic()) ->
    {ok, [#{node_id := binary(), endpoint := binary()}]} | {error, Reason :: term()}.
discover_subscribers(Client, Topic) when is_pid(Client), is_binary(Topic) ->
    macula_peer:discover_subscribers(Client, Topic).

%% @doc Get the node ID of this client.
%%
%% Returns the 32-byte node ID assigned to this client.
-spec get_node_id(Client :: client()) -> {ok, binary()} | {error, Reason :: term()}.
get_node_id(Client) when is_pid(Client) ->
    macula_peer:get_node_id(Client).

%% @doc Make a synchronous RPC call.
%%
%% Calls a remote procedure and waits for the result.
%%
%% == Examples ==
%%
%% ```
%% %% Simple RPC call
%% {ok, User} = macula:call(Client, &lt;&lt;"my.app.get_user"&gt;&gt;, #{
%%     user_id => &lt;&lt;"user-123"&gt;&gt;
%% }).
%%
%% %% With timeout
%% {ok, Result} = macula:call(Client, &lt;&lt;"my.app.process"&gt;&gt;,
%%     #{data => &lt;&lt;"large"&gt;&gt;},
%%     #{timeout => 30000}).
%% '''
-spec call(Client :: client(), Procedure :: procedure(), Args :: args()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.
call(Client, Procedure, Args) when is_pid(Client), is_binary(Procedure) ->
    macula_peer:call(Client, Procedure, Args).

%% @doc Make an RPC call with options.
-spec call(Client :: client(), Procedure :: procedure(), Args :: args(),
           Opts :: options()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.
call(Client, Procedure, Args, Opts) when is_pid(Client), is_binary(Procedure), is_map(Opts) ->
    macula_peer:call(Client, Procedure, Args, Opts).

%% @doc Advertise a service that this client provides.
%%
%% Registers a handler function for the specified procedure and advertises
%% it to the DHT so other clients can discover and call it.
%%
%% The handler function receives a map of arguments and must return
%% `{ok, Result}' or `{error, Reason}'.
%%
%% == Options ==
%%
%% <ul>
%% <li>`ttl' - Advertisement TTL in seconds (default: 300)</li>
%% <li>`metadata' - Custom metadata map (default: #{})</li>
%% </ul>
%%
%% == Examples ==
%%
%% ```
%% %% Define a handler function
%% Handler = fun(#{user_id := UserId}) ->
%%     {ok, #{user_id => UserId, name => &lt;&lt;"Alice"&gt;&gt;}}
%% end.
%%
%% %% Advertise the service
%% {ok, Ref} = macula:advertise(
%%     Client,
%%     &lt;&lt;"my.app.get_user"&gt;&gt;,
%%     Handler
%% ).
%%
%% %% Other clients can now call:
%% %% {ok, User} = macula:call(OtherClient, &lt;&lt;"my.app.get_user"&gt;&gt;,
%% %%     #{user_id => &lt;&lt;"user-123"&gt;&gt;}).
%% '''
-spec advertise(Client :: client(), Procedure :: procedure(),
                Handler :: macula_service_registry:handler_fn()) ->
    {ok, reference()} | {error, Reason :: term()}.
advertise(Client, Procedure, Handler) when is_pid(Client), is_binary(Procedure), is_function(Handler) ->
    advertise(Client, Procedure, Handler, #{}).

%% @doc Advertise a service with options.
-spec advertise(Client :: client(), Procedure :: procedure(),
                Handler :: macula_service_registry:handler_fn(),
                Opts :: options()) ->
    {ok, reference()} | {error, Reason :: term()}.
advertise(Client, Procedure, Handler, Opts) when is_pid(Client), is_binary(Procedure),
                                                   is_function(Handler), is_map(Opts) ->
    macula_peer:advertise(Client, Procedure, Handler, Opts).

%% @doc Stop advertising a service.
%%
%% Removes the local handler and stops advertising to the DHT.
%%
%% == Examples ==
%%
%% ```
%% ok = macula:unadvertise(Client, &lt;&lt;"my.app.get_user"&gt;&gt;).
%% '''
-spec unadvertise(Client :: client(), Procedure :: procedure()) ->
    ok | {error, Reason :: term()}.
unadvertise(Client, Procedure) when is_pid(Client), is_binary(Procedure) ->
    macula_peer:unadvertise(Client, Procedure).

%%%===================================================================
%%% Platform Layer API (v0.10.0+)
%%%===================================================================

%% @doc Register this workload with the Platform Layer.
%%
%% Registers the workload application with Macula's Platform Layer and
%% returns information about the current platform cluster state, including
%% the current leader node.
%%
%% == Options ==
%%
%% <ul>
%% <li>`workload_name' - Required. Binary name identifying this workload type
%%     (e.g., `&lt;&lt;"macula_arcade"&gt;&gt;', `&lt;&lt;"my_app"&gt;&gt;')</li>
%% <li>`capabilities' - Optional. List of atoms describing workload capabilities
%%     (e.g., `[coordinator, game_server]')</li>
%% </ul>
%%
%% == Returns ==
%%
%% <ul>
%% <li>`leader_node' - Binary node ID of the current Platform Layer leader</li>
%% <li>`cluster_size' - Integer count of nodes in the platform cluster</li>
%% <li>`platform_version' - Binary version string (e.g., `&lt;&lt;"0.10.0"&gt;&gt;')</li>
%% </ul>
%%
%% == Examples ==
%%
%% ```
%% {ok, Client} = macula:connect_local(#{realm => &lt;&lt;"my.app"&gt;&gt;}),
%% {ok, Info} = macula:register_workload(Client, #{
%%     workload_name => &lt;&lt;"my_app_coordinator"&gt;&gt;,
%%     capabilities => [coordinator, matchmaking]
%% }),
%% #{leader_node := Leader, cluster_size := Size} = Info.
%% '''
%%
%% @since v0.10.0
-spec register_workload(Client :: client(), Opts :: options()) ->
    {ok, map()} | {error, Reason :: term()}.
register_workload(Client, Opts) when is_pid(Client), is_map(Opts) ->
    macula_local_client:register_workload(Client, Opts).

%% @doc Get the current Platform Layer leader node.
%%
%% Queries the Platform Layer for the current leader node ID. The leader
%% is elected via Raft consensus and handles coordination tasks.
%%
%% Returns `{error, no_leader}' if leader election is in progress.
%%
%% == Examples ==
%%
%% ```
%% case macula:get_leader(Client) of
%%     {ok, LeaderNodeId} ->
%%         %% Check if we're the leader
%%         {ok, OurNodeId} = macula:get_node_id(Client),
%%         case LeaderNodeId == OurNodeId of
%%             true -> coordinate_globally();
%%             false -> defer_to_leader()
%%         end;
%%     {error, no_leader} ->
%%         wait_for_leader_election()
%% end.
%% '''
%%
%% @since v0.10.0
-spec get_leader(Client :: client()) ->
    {ok, binary()} | {error, no_leader | term()}.
get_leader(Client) when is_pid(Client) ->
    macula_local_client:get_leader(Client).

%% @doc Subscribe to Platform Layer leader change notifications.
%%
%% Registers a callback function to be invoked whenever the Platform Layer
%% leader changes due to election or node failure.
%%
%% The callback receives a map with:
%% <ul>
%% <li>`old_leader' - Previous leader node ID (may be `undefined')</li>
%% <li>`new_leader' - New leader node ID</li>
%% <li>`term' - Raft term number (monotonically increasing)</li>
%% </ul>
%%
%% == Examples ==
%%
%% ```
%% {ok, SubRef} = macula:subscribe_leader_changes(Client,
%%     fun(#{old_leader := Old, new_leader := New}) ->
%%         io:format("Leader changed: ~p -> ~p~n", [Old, New]),
%%         handle_leadership_transition(New),
%%         ok
%%     end).
%% '''
%%
%% @since v0.10.0
-spec subscribe_leader_changes(Client :: client(), Callback :: fun((map()) -> ok)) ->
    {ok, subscription_ref()} | {error, Reason :: term()}.
subscribe_leader_changes(Client, Callback) when is_pid(Client), is_function(Callback, 1) ->
    macula_local_client:subscribe_leader_changes(Client, Callback).

%% @doc Propose a CRDT update to Platform Layer shared state.
%%
%% Updates platform-managed shared state using Conflict-Free Replicated
%% Data Types (CRDTs) for automatic conflict resolution across nodes.
%%
%% Default CRDT type is `lww_register' (Last-Write-Wins Register).
%% See `propose_crdt_update/4' for other CRDT types.
%%
%% == Examples ==
%%
%% ```
%% %% Store simple value (LWW-Register)
%% ok = macula:propose_crdt_update(
%%     Client,
%%     &lt;&lt;"my.app.config.max_users"&gt;&gt;,
%%     1000
%% ).
%%
%% %% Later read it back
%% {ok, 1000} = macula:read_crdt(Client, &lt;&lt;"my.app.config.max_users"&gt;&gt;).
%% '''
%%
%% @since v0.10.0
-spec propose_crdt_update(Client :: client(), Key :: binary(), Value :: term()) ->
    ok | {error, Reason :: term()}.
propose_crdt_update(Client, Key, Value) when is_pid(Client), is_binary(Key) ->
    propose_crdt_update(Client, Key, Value, #{crdt_type => lww_register}).

%% @doc Propose a CRDT update with specific CRDT type.
%%
%% Updates platform-managed shared state using the specified CRDT type
%% for automatic conflict resolution.
%%
%% == CRDT Types ==
%%
%% <ul>
%% <li>`lww_register' - Last-Write-Wins Register (default)
%%     <ul><li>Use for: Configuration values, latest status</li>
%%     <li>Conflict resolution: Latest timestamp wins</li></ul></li>
%% <li>`g_counter' - Grow-Only Counter
%%     <ul><li>Use for: Metrics, totals (never decrease)</li>
%%     <li>Operations: increment only</li></ul></li>
%% <li>`pn_counter' - Positive-Negative Counter
%%     <ul><li>Use for: Bidirectional counters (can increase/decrease)</li>
%%     <li>Operations: increment, decrement</li></ul></li>
%% <li>`g_set' - Grow-Only Set
%%     <ul><li>Use for: Accumulating collections (never remove)</li>
%%     <li>Operations: add elements only</li></ul></li>
%% <li>`or_set' - Observed-Remove Set
%%     <ul><li>Use for: Sets with add/remove operations</li>
%%     <li>Operations: add, remove elements</li></ul></li>
%% </ul>
%%
%% == Examples ==
%%
%% ```
%% %% Increment a counter
%% ok = macula:propose_crdt_update(
%%     Client,
%%     &lt;&lt;"my.app.active_games"&gt;&gt;,
%%     {increment, 1},
%%     #{crdt_type => pn_counter}
%% ).
%%
%% %% Add to a set
%% ok = macula:propose_crdt_update(
%%     Client,
%%     &lt;&lt;"my.app.player_ids"&gt;&gt;,
%%     {add, &lt;&lt;"player123"&gt;&gt;},
%%     #{crdt_type => or_set}
%% ).
%% '''
%%
%% @since v0.10.0
-spec propose_crdt_update(Client :: client(), Key :: binary(), Value :: term(), Opts :: options()) ->
    ok | {error, Reason :: term()}.
propose_crdt_update(Client, Key, Value, Opts) when is_pid(Client), is_binary(Key), is_map(Opts) ->
    macula_local_client:propose_crdt_update(Client, Key, Value, Opts).

%% @doc Read the current value of a CRDT-managed shared state entry.
%%
%% Reads from the local CRDT replica. The value reflects all converged
%% updates from across the platform cluster.
%%
%% Returns `{error, not_found}' if the key has never been written.
%%
%% == Examples ==
%%
%% ```
%% %% Read LWW-Register value
%% {ok, MaxUsers} = macula:read_crdt(Client, &lt;&lt;"my.app.config.max_users"&gt;&gt;).
%%
%% %% Read counter value
%% {ok, GameCount} = macula:read_crdt(Client, &lt;&lt;"my.app.active_games"&gt;&gt;).
%%
%% %% Read set value
%% {ok, PlayerSet} = macula:read_crdt(Client, &lt;&lt;"my.app.player_ids"&gt;&gt;).
%% '''
%%
%% @since v0.10.0
-spec read_crdt(Client :: client(), Key :: binary()) ->
    {ok, term()} | {error, not_found | term()}.
read_crdt(Client, Key) when is_pid(Client), is_binary(Key) ->
    macula_local_client:read_crdt(Client, Key).

%%%===================================================================
%%% Internal functions
%%%===================================================================
