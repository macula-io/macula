%%%-------------------------------------------------------------------
%%% @doc
%%% Connection pool manager for endpoint connections.
%%%
%%% Manages a pool of QUIC connections to remote endpoints, providing
%%% connection caching and reuse to avoid connection overhead for
%%% multi-endpoint RPC operations.
%%%
%%% Connection pool structure:
%%% #{Endpoint => #{connection => Conn, stream => Stream, last_used => Timestamp}}
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection_pool).

-include_lib("kernel/include/logger.hrl").
-include("macula_config.hrl").

%% API
-export([
    get_or_create_connection/4,
    create_connection/4,
    close_all_connections/1
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Get or create a connection to an endpoint.
%% Returns {ok, Conn, Stream, UpdatedPool} or {error, Reason, Pool}.
-spec get_or_create_connection(binary(), binary(), binary(), map()) ->
    {ok, pid(), pid(), map()} | {error, term(), map()}.
get_or_create_connection(Endpoint, NodeId, RealmId, Pool) when is_map(Pool) ->
    case maps:get(Endpoint, Pool, undefined) of
        undefined ->
            %% No existing connection, create new one
            case create_connection(Endpoint, NodeId, RealmId, Pool) of
                {ok, Conn, Stream, _Pool2} ->
                    %% Cache the connection
                    ConnectionInfo = #{
                        connection => Conn,
                        stream => Stream,
                        last_used => erlang:system_time(second)
                    },
                    UpdatedPool = Pool#{Endpoint => ConnectionInfo},
                    {ok, Conn, Stream, UpdatedPool};
                {error, Reason, Pool2} ->
                    {error, Reason, Pool2}
            end;

        #{connection := Conn, stream := Stream} = ConnectionInfo ->
            %% Existing connection found, reuse it
            %% Note: Stream is still active (quicer creates with active=true by default)
            UpdatedConnectionInfo = ConnectionInfo#{last_used => erlang:system_time(second)},
            UpdatedPool = Pool#{Endpoint => UpdatedConnectionInfo},
            {ok, Conn, Stream, UpdatedPool}
    end.

%% @doc Create a new connection to an endpoint.
-spec create_connection(binary(), binary(), binary(), map()) ->
    {ok, pid(), pid(), map()} | {error, term(), map()}.
create_connection(Endpoint, NodeId, RealmId, Pool) when is_map(Pool) ->
    %% Parse endpoint URL
    case macula_utils:parse_url(Endpoint) of
        {Host, Port} ->
            ?LOG_INFO("Creating connection to endpoint: ~s:~p", [Host, Port]),

            %% Connect via QUIC with TLS configuration from macula_tls (v0.11.0+)
            %% Use hostname-aware TLS options for certificate verification
            TlsOpts = macula_tls:quic_client_opts_with_hostname(Host),
            QuicOpts = merge_quic_opts([
                {alpn, ["macula"]},
                {idle_timeout_ms, 60000},
                {keep_alive_interval_ms, 20000},
                {handshake_idle_timeout_ms, 30000}
            ], TlsOpts),

            ConnectResult = try
                macula_quic:connect(Host, Port, QuicOpts, ?DEFAULT_TIMEOUT)
            catch
                _:Error ->
                    {error, Error}
            end,

            case ConnectResult of
                {ok, Conn} ->
                    %% Open bidirectional stream
                    case macula_quic:open_stream(Conn) of
                        {ok, Stream} ->
                            %% Note: quicer creates streams with active=true by default
                            %% Calling process (this gen_server) is already the owner
                            %% No need for controlling_process or explicit setopt

                            %% Send CONNECT message
                            %% Include advertise endpoint for peer-to-peer connections
                            LocalEndpoint = case application:get_env(macula, advertise_endpoint) of
                                {ok, Ep} when is_binary(Ep) -> Ep;
                                _ ->
                                    %% Construct from NODE_HOST env var
                                    NodeHost = list_to_binary(os:getenv("NODE_HOST", "localhost")),
                                    <<"https://", NodeHost/binary, ":9443">>
                            end,

                            ConnectMsg = #{
                                version => <<"1.0">>,
                                node_id => NodeId,
                                realm_id => RealmId,
                                capabilities => [rpc],  % Only RPC for endpoint connections
                                endpoint => LocalEndpoint
                            },

                            case send_connect_message(Stream, ConnectMsg) of
                                ok ->
                                    ?LOG_INFO("Connected to endpoint: ~s:~p", [Host, Port]),
                                    {ok, Conn, Stream, Pool};
                                {error, Reason} ->
                                    macula_quic:close(Stream),
                                    macula_quic:close(Conn),
                                    ?LOG_ERROR("Handshake failed with endpoint ~s:~p: ~p",
                                              [Host, Port, Reason]),
                                    {error, {handshake_failed, Reason}, Pool}
                            end;
                        {error, Reason} ->
                            macula_quic:close(Conn),
                            ?LOG_ERROR("Stream open failed with endpoint ~s:~p: ~p",
                                      [Host, Port, Reason]),
                            {error, {stream_open_failed, Reason}, Pool}
                    end;
                {error, Reason} ->
                    ?LOG_ERROR("Connection failed to endpoint ~s:~p: ~p",
                              [Host, Port, Reason]),
                    {error, {connection_failed, Reason}, Pool};
                {error, Type, Details} ->
                    ?LOG_ERROR("Connection failed to endpoint ~s:~p: ~p ~p",
                              [Host, Port, Type, Details]),
                    {error, {connection_failed, {Type, Details}}, Pool};
                Other ->
                    ?LOG_ERROR("Connection failed to endpoint ~s:~p: ~p",
                              [Host, Port, Other]),
                    {error, {connection_failed, Other}, Pool}
            end
    end.

%% @doc Close all connections in the pool.
-spec close_all_connections(map()) -> ok.
close_all_connections(Pool) when is_map(Pool) ->
    maps:foreach(
        fun(_Endpoint, #{connection := Conn, stream := Stream}) ->
            catch macula_quic:close(Stream),
            catch macula_quic:close(Conn)
        end,
        Pool
    ),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Merge QUIC options, with second list taking precedence.
%% @private
-spec merge_quic_opts(list(), list()) -> list().
merge_quic_opts(BaseOpts, OverrideOpts) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            lists:keystore(Key, 1, Acc, {Key, Value})
        end,
        BaseOpts,
        OverrideOpts
    ).

%% @doc Send CONNECT message through a stream.
-spec send_connect_message(pid(), map()) -> ok | {error, term()}.
send_connect_message(Stream, ConnectMsg) ->
    try
        %% Encode message using protocol encoder
        Binary = macula_protocol_encoder:encode(connect, ConnectMsg),

        %% Send via QUIC stream
        case macula_quic:send(Stream, Binary) of
            ok -> ok;
            {error, Reason} -> {error, Reason}
        end
    catch
        _:Error ->
            {error, Error}
    end.
