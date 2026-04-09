%%%-------------------------------------------------------------------
%%% @doc Macula SDK — Public API for mesh applications.
%%%
%%% This is the main entry point for applications using the Macula SDK.
%%% All functions delegate to macula_mesh_client or macula_multi_relay
%%% for relay mesh communication.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula).

-include_lib("kernel/include/logger.hrl").

%% Connection
-export([connect/2, disconnect/1]).

%% Pub/Sub
-export([subscribe/3, unsubscribe/2, publish/3, publish/4]).

%% RPC
-export([call/3, call/4, advertise/3, advertise/4, unadvertise/2]).

%% Cluster (LAN)
-export([ensure_distributed/0, get_cookie/0, set_cookie/1,
         monitor_nodes/0, unmonitor_nodes/0]).

%% Mesh Distribution
-export([join_mesh/1]).

%% Types
-export_type([client/0, topic/0, procedure/0]).

-type client() :: pid().
-type topic() :: binary().
-type procedure() :: binary().

%%%===================================================================
%%% Connection
%%%===================================================================

%% @doc Connect to a Macula relay.
%%
%% Returns a client PID (macula_mesh_client gen_server) that you pass
%% to all other API functions. Accepts a single URL binary or a list.
-spec connect(binary() | [binary()], map()) -> {ok, client()} | {error, term()}.
connect(Url, Opts) when is_binary(Url) ->
    connect([Url], Opts);
connect(Relays, Opts) when is_list(Relays), is_map(Opts) ->
    Connections = maps:get(connections, Opts, 1),
    ClientOpts = Opts#{relays => Relays},
    case Connections of
        1 ->
            macula_mesh_client:start_link(ClientOpts);
        N when N > 1 ->
            macula_multi_relay:start_link(ClientOpts#{connections => N})
    end.

%% @doc Disconnect from the relay.
-spec disconnect(client()) -> ok.
disconnect(Client) when is_pid(Client) ->
    macula_mesh_client:stop(Client).

%%%===================================================================
%%% Pub/Sub
%%%===================================================================

%% @doc Subscribe to a topic.
%%
%% The callback receives the event payload (map or binary).
%% Returns a subscription reference for unsubscribing.
-spec subscribe(client(), topic(), fun((term()) -> ok) | pid()) ->
    {ok, reference()} | {error, term()}.
subscribe(Client, Topic, Callback) when is_pid(Client), is_binary(Topic) ->
    macula_mesh_client:subscribe(Client, Topic, Callback).

%% @doc Unsubscribe from a topic.
-spec unsubscribe(client(), reference()) -> ok | {error, term()}.
unsubscribe(Client, SubRef) when is_pid(Client) ->
    macula_mesh_client:unsubscribe(Client, SubRef).

%% @doc Publish an event to a topic (fire-and-forget).
-spec publish(client(), topic(), term()) -> ok.
publish(Client, Topic, Data) ->
    publish(Client, Topic, Data, #{}).

%% @doc Publish an event with options.
-spec publish(client(), topic(), term(), map()) -> ok.
publish(Client, Topic, Data, _Opts) when is_pid(Client), is_binary(Topic) ->
    macula_mesh_client:publish(Client, Topic, Data).

%%%===================================================================
%%% RPC
%%%===================================================================

%% @doc Call a remote procedure (default 5s timeout).
-spec call(client(), procedure(), term()) -> {ok, term()} | {error, term()}.
call(Client, Procedure, Args) ->
    call(Client, Procedure, Args, 5000).

%% @doc Call a remote procedure with timeout.
-spec call(client(), procedure(), term(), pos_integer()) ->
    {ok, term()} | {error, term()}.
call(Client, Procedure, Args, Timeout) when is_pid(Client), is_binary(Procedure) ->
    macula_mesh_client:call(Client, Procedure, Args, Timeout).

%% @doc Advertise an RPC procedure handler.
-spec advertise(client(), procedure(), fun()) -> {ok, reference()} | {error, term()}.
advertise(Client, Procedure, Handler) ->
    advertise(Client, Procedure, Handler, #{}).

%% @doc Advertise with options.
-spec advertise(client(), procedure(), fun(), map()) -> {ok, reference()} | {error, term()}.
advertise(Client, Procedure, Handler, _Opts) when is_pid(Client), is_binary(Procedure) ->
    macula_mesh_client:advertise(Client, Procedure, Handler).

%% @doc Stop advertising a procedure.
-spec unadvertise(client(), procedure()) -> ok | {error, term()}.
unadvertise(Client, Procedure) when is_pid(Client), is_binary(Procedure) ->
    macula_mesh_client:unadvertise(Client, Procedure).

%%%===================================================================
%%% Cluster (LAN)
%%%===================================================================

%% @doc Ensure this node is running in distributed mode.
-spec ensure_distributed() -> ok | {error, term()}.
ensure_distributed() -> macula_cluster:ensure_distributed().

%% @doc Get the Erlang cluster cookie.
-spec get_cookie() -> atom().
get_cookie() -> macula_cluster:get_cookie().

%% @doc Set the Erlang cluster cookie.
-spec set_cookie(atom() | binary()) -> ok.
set_cookie(Cookie) -> macula_cluster:set_cookie(Cookie).

%% @doc Subscribe to node up/down events.
-spec monitor_nodes() -> ok.
monitor_nodes() -> macula_cluster:monitor_nodes().

%% @doc Unsubscribe from node up/down events.
-spec unmonitor_nodes() -> ok.
unmonitor_nodes() -> macula_cluster:unmonitor_nodes().

%%%===================================================================
%%% Mesh Distribution
%%%===================================================================

%% @doc Join the Macula relay mesh with Erlang distribution.
%%
%% After calling this, standard OTP distribution works across firewalls.
%% Options: relays (required list), realm, identity, site, tls_verify.
-spec join_mesh(map()) -> ok | {error, term()}.
join_mesh(Opts) ->
    Relays = maps:get(relays, Opts),
    ClientOpts = #{
        relays => Relays,
        realm => maps:get(realm, Opts, <<"io.macula">>),
        identity => maps:get(identity, Opts, atom_to_binary(node())),
        tls_verify => maps:get(tls_verify, Opts, none),
        site => maps:get(site, Opts, undefined)
    },
    case macula_mesh_client:start_link(ClientOpts) of
        {ok, Client} ->
            %% Wait for relay connection before advertising
            wait_for_relay(Client, 30),
            os:putenv("MACULA_DIST_MODE", "relay"),
            macula_dist_relay:register_mesh_client(Client),
            macula_dist_relay:advertise_dist_accept(),
            ?LOG_INFO("[macula] Joined mesh — distribution enabled"),
            ok;
        {error, Reason} ->
            ?LOG_ERROR("[macula] Failed to join mesh: ~p", [Reason]),
            {error, Reason}
    end.

%% @private Wait until the mesh client has an active QUIC stream.
wait_for_relay(_Client, 0) ->
    ?LOG_WARNING("[macula] Relay connection not ready after timeout");
wait_for_relay(Client, Retries) ->
    case catch macula_mesh_client:is_connected(Client) of
        true ->
            ?LOG_INFO("[macula] Relay connected"),
            ok;
        _ ->
            timer:sleep(1000),
            wait_for_relay(Client, Retries - 1)
    end.
