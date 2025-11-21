%%%-------------------------------------------------------------------
%%% @doc
%%% Macula SDK - Main API module for HTTP/3 mesh client operations.
%%%
%%% This module provides the primary interface for applications to
%%% connect to Macula mesh networks and perform pub/sub and RPC
%%% operations over HTTP/3 (QUIC) transport.
%%%
%%% == Quick Start ==
%%%
%%% Connect to a mesh, publish events, subscribe to topics, and make RPC calls.
%%% See individual function documentation for detailed examples with code.
%%%
%%% == DHT Network Bootstrap (v0.8.7+) ==
%%%
%%% Macula v0.8.7+ implements platform-level DHT bootstrapping. Each macula node
%%% automatically joins the configured DHT network on startup via the
%%% `MACULA_BOOTSTRAP_PEERS` environment variable.
%%%
%%% <b>Platform Configuration (Recommended):</b>
%%% ```
%%% # Bootstrap node (no peers configured)
%%% MACULA_BOOTSTRAP_PEERS=  # empty - this IS a bootstrap peer
%%%
%%% # Other nodes (connect to bootstrap)
%%% MACULA_BOOTSTRAP_PEERS=https://bootstrap-node:4433
%%% '''
%%%
%%% <b>Client SDK Usage:</b>
%%% Applications connect to their LOCAL macula instance, which is already part
%%% of the DHT network:
%%% ```
%%% {ok, Client} = macula_client:connect(&lt;&lt;"https://localhost:4433"&gt;&gt;, #{
%%%     realm => &lt;&lt;"my.app"&gt;&gt;
%%% }).
%%% '''
%%%
%%% The platform handles DHT network formation - applications don't need to
%%% manage bootstrap peer URLs.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_client).

%% API exports
-export([
    connect/2,
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
%% {ok, Client} = macula_client:connect(&lt;&lt;"https://mesh.local:443"&gt;&gt;, #{
%%     realm => &lt;&lt;"my.realm"&gt;&gt;
%% }).
%%
%% %% With API key authentication
%% {ok, Client} = macula_client:connect(&lt;&lt;"https://mesh.local:443"&gt;&gt;, #{
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
%% ok = macula_client:publish(Client, &lt;&lt;"my.app.events"&gt;&gt;, #{
%%     type => &lt;&lt;"user.registered"&gt;&gt;,
%%     user_id => &lt;&lt;"user-123"&gt;&gt;,
%%     email => &lt;&lt;"user@example.com"&gt;&gt;
%% }).
%%
%% %% Publish with options
%% ok = macula_client:publish(Client, &lt;&lt;"my.app.events"&gt;&gt;, #{
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
%% {ok, SubRef} = macula_client:subscribe(Client, &lt;&lt;"my.app.events"&gt;&gt;,
%%     fun(EventData) ->
%%         io:format("Event: ~p~n", [EventData]),
%%         ok
%%     end).
%%
%% %% Unsubscribe later
%% ok = macula_client:unsubscribe(Client, SubRef).
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
%% {ok, User} = macula_client:call(Client, &lt;&lt;"my.app.get_user"&gt;&gt;, #{
%%     user_id => &lt;&lt;"user-123"&gt;&gt;
%% }).
%%
%% %% With timeout
%% {ok, Result} = macula_client:call(Client, &lt;&lt;"my.app.process"&gt;&gt;,
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
%% {ok, Ref} = macula_client:advertise(
%%     Client,
%%     &lt;&lt;"my.app.get_user"&gt;&gt;,
%%     Handler
%% ).
%%
%% %% Other clients can now call:
%% %% {ok, User} = macula_client:call(OtherClient, &lt;&lt;"my.app.get_user"&gt;&gt;,
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
%% ok = macula_client:unadvertise(Client, &lt;&lt;"my.app.get_user"&gt;&gt;).
%% '''
-spec unadvertise(Client :: client(), Procedure :: procedure()) ->
    ok | {error, Reason :: term()}.
unadvertise(Client, Procedure) when is_pid(Client), is_binary(Procedure) ->
    macula_peer:unadvertise(Client, Procedure).

%%%===================================================================
%%% Internal functions
%%%===================================================================
