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
%%% ```
%%% %% Connect to a Macula mesh
%%% {ok, Client} = macula_sdk:connect(<<"https://mesh.example.com:443">>, #{
%%%     realm => <<"my.app.realm">>,
%%%     auth => #{api_key => <<"your-api-key">>}
%%% }).
%%%
%%% %% Publish an event
%%% ok = macula_sdk:publish(Client, <<"my.app.events">>, #{
%%%     type => <<"user.registered">>,
%%%     user_id => <<"user-123">>
%%% }).
%%%
%%% %% Subscribe to events
%%% {ok, SubRef} = macula_sdk:subscribe(Client, <<"my.app.events">>,
%%%     fun(Event) ->
%%%         io:format("Received event: ~p~n", [Event])
%%%     end).
%%%
%%% %% Make an RPC call
%%% {ok, Result} = macula_sdk:call(Client, <<"my.app.get_user">>, #{
%%%     user_id => <<"user-123">>
%%% }).
%%%
%%% %% Disconnect
%%% ok = macula_sdk:disconnect(Client).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_sdk).

%% API exports
-export([
    connect/2,
    disconnect/1,
    publish/3,
    publish/4,
    subscribe/3,
    unsubscribe/2,
    call/3,
    call/4
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
%% not entity IDs. Example: <<"my.app.user.registered">> (good),
%% not <<"my.app.user.123.registered">> (bad - ID belongs in payload).

-type event_data() :: map() | binary().
%% Event payload data. Typically a map that will be JSON-encoded.

-type procedure() :: binary().
%% RPC procedure name. Example: <<"my.app.get_user">>.

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
%% <li>`realm' - Required. Binary realm identifier (e.g., <<"my.app.realm">>)</li>
%% <li>`auth' - Optional. Authentication map with `api_key' or other auth methods</li>
%% <li>`timeout' - Optional. Connection timeout in milliseconds (default: 5000)</li>
%% <li>`node_id' - Optional. 32-byte node ID (generated if not provided)</li>
%% </ul>
%%
%% == Examples ==
%%
%% ```
%% %% Basic connection
%% {ok, Client} = macula_sdk:connect(<<"https://mesh.local:443">>, #{
%%     realm => <<"my.realm">>
%% }).
%%
%% %% With API key authentication
%% {ok, Client} = macula_sdk:connect(<<"https://mesh.local:443">>, #{
%%     realm => <<"my.realm">>,
%%     auth => #{api_key => <<"secret-key">>}
%% }).
%% '''
-spec connect(Url :: binary(), Opts :: options()) ->
    {ok, client()} | {error, Reason :: term()}.
connect(Url, Opts) when is_binary(Url), is_map(Opts) ->
    macula_sdk_client:start_link(Url, Opts);
connect(Url, Opts) when is_list(Url), is_map(Opts) ->
    connect(list_to_binary(Url), Opts).

%% @doc Disconnect from the Macula mesh.
%%
%% Cleanly closes the HTTP/3 connection and cleans up all subscriptions.
-spec disconnect(Client :: client()) -> ok | {error, Reason :: term()}.
disconnect(Client) when is_pid(Client) ->
    macula_sdk_client:stop(Client).

%% @doc Publish an event to a topic.
%%
%% Publishes data to the specified topic. All subscribers to this topic
%% will receive the event.
%%
%% == Topic Design ==
%%
%% Topics should describe EVENT TYPES, not entity instances:
%% <ul>
%% <li>✅ Good: <<"my.app.user.registered">> (event type)</li>
%% <li>❌ Bad: <<"my.app.user.123.registered">> (entity ID in topic)</li>
%% </ul>
%%
%% Entity IDs belong in the event payload, not the topic name.
%%
%% == Examples ==
%%
%% ```
%% %% Publish with default options
%% ok = macula_sdk:publish(Client, <<"my.app.events">>, #{
%%     type => <<"user.registered">>,
%%     user_id => <<"user-123">>,
%%     email => <<"user@example.com">>
%% }).
%%
%% %% Publish with options
%% ok = macula_sdk:publish(Client, <<"my.app.events">>, #{
%%     data => <<"important">>
%% }, #{acknowledge => true}).
%% '''
-spec publish(Client :: client(), Topic :: topic(), Data :: event_data()) ->
    ok | {error, Reason :: term()}.
publish(Client, Topic, Data) when is_pid(Client), is_binary(Topic) ->
    macula_sdk_client:publish(Client, Topic, Data).

%% @doc Publish an event with options.
-spec publish(Client :: client(), Topic :: topic(), Data :: event_data(),
              Opts :: options()) ->
    ok | {error, Reason :: term()}.
publish(Client, Topic, Data, Opts) when is_pid(Client), is_binary(Topic), is_map(Opts) ->
    macula_sdk_client:publish(Client, Topic, Data, Opts).

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
%% {ok, SubRef} = macula_sdk:subscribe(Client, <<"my.app.events">>,
%%     fun(EventData) ->
%%         io:format("Event: ~p~n", [EventData]),
%%         ok
%%     end).
%%
%% %% Unsubscribe later
%% ok = macula_sdk:unsubscribe(Client, SubRef).
%% '''
-spec subscribe(Client :: client(), Topic :: topic(),
                Callback :: fun((event_data()) -> ok)) ->
    {ok, subscription_ref()} | {error, Reason :: term()}.
subscribe(Client, Topic, Callback) when is_pid(Client), is_binary(Topic), is_function(Callback, 1) ->
    macula_sdk_client:subscribe(Client, Topic, Callback).

%% @doc Unsubscribe from a topic.
%%
%% Removes the subscription identified by the subscription reference.
-spec unsubscribe(Client :: client(), SubRef :: subscription_ref()) ->
    ok | {error, Reason :: term()}.
unsubscribe(Client, SubRef) when is_pid(Client), is_reference(SubRef) ->
    macula_sdk_client:unsubscribe(Client, SubRef).

%% @doc Make a synchronous RPC call.
%%
%% Calls a remote procedure and waits for the result.
%%
%% == Examples ==
%%
%% ```
%% %% Simple RPC call
%% {ok, User} = macula_sdk:call(Client, <<"my.app.get_user">>, #{
%%     user_id => <<"user-123">>
%% }).
%%
%% %% With timeout
%% {ok, Result} = macula_sdk:call(Client, <<"my.app.process">>,
%%     #{data => <<"large">>},
%%     #{timeout => 30000}).
%% '''
-spec call(Client :: client(), Procedure :: procedure(), Args :: args()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.
call(Client, Procedure, Args) when is_pid(Client), is_binary(Procedure) ->
    macula_sdk_client:call(Client, Procedure, Args).

%% @doc Make an RPC call with options.
-spec call(Client :: client(), Procedure :: procedure(), Args :: args(),
           Opts :: options()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.
call(Client, Procedure, Args, Opts) when is_pid(Client), is_binary(Procedure), is_map(Opts) ->
    macula_sdk_client:call(Client, Procedure, Args, Opts).

%%%===================================================================
%%% Internal functions
%%%===================================================================
