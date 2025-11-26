%%%-------------------------------------------------------------------
%%% @doc
%%% Peer Connection Pool - Manages pooled QUIC connections to remote peers.
%%%
%%% Problem: Creating a new QUIC connection per message adds ~50-200ms latency.
%%% Solution: Pool connections and reuse them for subsequent messages.
%%%
%%% Design:
%%%   - ETS-based connection pool for O(1) lookups
%%%   - LRU eviction when pool is full
%%%   - Automatic connection health monitoring
%%%   - Configurable pool size and idle timeout
%%%
%%% Expected improvement: 1.5-2x latency reduction for repeated messaging.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peer_connection_pool).

-behaviour(gen_server).

-include_lib("quicer/include/quicer.hrl").

%% API
-export([
    start_link/0,
    start_link/1,
    get_connection/1,
    return_connection/2,
    invalidate/1,
    stats/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(SERVER, ?MODULE).
-define(TABLE, macula_peer_connection_pool_table).
-define(DEFAULT_MAX_CONNECTIONS, 100).
-define(DEFAULT_IDLE_TIMEOUT_MS, 60000).  % 60 seconds
-define(CLEANUP_INTERVAL_MS, 10000).  % Cleanup every 10 seconds

-record(state, {
    table :: ets:tid(),
    max_connections :: pos_integer(),
    idle_timeout_ms :: pos_integer(),
    hits :: non_neg_integer(),
    misses :: non_neg_integer(),
    evictions :: non_neg_integer()
}).

-record(pooled_conn, {
    endpoint :: binary(),
    connection :: term(),
    stream :: term(),
    last_used :: integer(),
    created :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the connection pool with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the connection pool with options.
%% Options:
%%   - max_connections: Maximum pooled connections (default: 100)
%%   - idle_timeout_ms: Idle connection timeout (default: 60000)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Get a connection to an endpoint (from pool or create new).
%% Returns {ok, Connection, Stream} or {error, Reason}.
-spec get_connection(binary()) -> {ok, term(), term()} | {error, term()}.
get_connection(Endpoint) ->
    gen_server:call(?SERVER, {get_connection, Endpoint}, 10000).

%% @doc Return a connection to the pool for reuse.
-spec return_connection(binary(), {term(), term()}) -> ok.
return_connection(Endpoint, {Connection, Stream}) ->
    gen_server:cast(?SERVER, {return_connection, Endpoint, Connection, Stream}).

%% @doc Invalidate (remove) a connection from the pool.
%% Called when a connection fails or is no longer valid.
-spec invalidate(binary()) -> ok.
invalidate(Endpoint) ->
    gen_server:cast(?SERVER, {invalidate, Endpoint}).

%% @doc Get pool statistics.
-spec stats() -> map().
stats() ->
    gen_server:call(?SERVER, stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    MaxConnections = maps:get(max_connections, Opts, ?DEFAULT_MAX_CONNECTIONS),
    IdleTimeoutMs = maps:get(idle_timeout_ms, Opts, ?DEFAULT_IDLE_TIMEOUT_MS),

    Table = ets:new(?TABLE, [
        named_table,
        set,
        public,
        {keypos, #pooled_conn.endpoint},
        {read_concurrency, true}
    ]),

    %% Schedule periodic cleanup
    erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_idle_connections),

    {ok, #state{
        table = Table,
        max_connections = MaxConnections,
        idle_timeout_ms = IdleTimeoutMs,
        hits = 0,
        misses = 0,
        evictions = 0
    }}.

handle_call({get_connection, Endpoint}, _From, State) ->
    case lookup_connection(Endpoint) of
        {ok, Conn, Stream} ->
            %% Pool hit - update last_used timestamp
            update_last_used(Endpoint),
            {reply, {ok, Conn, Stream}, State#state{hits = State#state.hits + 1}};
        not_found ->
            %% Pool miss - create new connection
            case create_connection(Endpoint) of
                {ok, Conn, Stream} ->
                    %% Don't store in pool yet - caller will return it after use
                    {reply, {ok, Conn, Stream}, State#state{misses = State#state.misses + 1}};
                {error, Reason} ->
                    {reply, {error, Reason}, State#state{misses = State#state.misses + 1}}
            end
    end;

handle_call(stats, _From, #state{hits = Hits, misses = Misses, evictions = Evictions} = State) ->
    Total = Hits + Misses,
    HitRate = case Total of
        0 -> 0.0;
        _ -> Hits / Total * 100
    end,
    PoolSize = ets:info(?TABLE, size),
    Stats = #{
        hits => Hits,
        misses => Misses,
        evictions => Evictions,
        total => Total,
        hit_rate => HitRate,
        pool_size => PoolSize
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({return_connection, Endpoint, Connection, Stream}, State) ->
    %% Check pool size limit
    CurrentSize = ets:info(?TABLE, size),
    NewState = case CurrentSize >= State#state.max_connections of
        true ->
            %% Pool full - evict oldest connection
            evict_oldest_connection(),
            State#state{evictions = State#state.evictions + 1};
        false ->
            State
    end,

    %% Store connection in pool
    Now = erlang:system_time(millisecond),
    PooledConn = #pooled_conn{
        endpoint = Endpoint,
        connection = Connection,
        stream = Stream,
        last_used = Now,
        created = Now
    },
    ets:insert(?TABLE, PooledConn),

    {noreply, NewState};

handle_cast({invalidate, Endpoint}, State) ->
    case ets:lookup(?TABLE, Endpoint) of
        [#pooled_conn{connection = Conn, stream = Stream}] ->
            %% Close connection and stream
            close_connection(Conn, Stream),
            ets:delete(?TABLE, Endpoint);
        [] ->
            ok
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_idle_connections, State) ->
    %% Remove idle connections that have timed out
    Now = erlang:system_time(millisecond),
    Cutoff = Now - State#state.idle_timeout_ms,

    %% Find and remove expired connections
    Expired = ets:select(?TABLE, [{
        #pooled_conn{endpoint = '$1', last_used = '$2', connection = '$3', stream = '$4', _ = '_'},
        [{'<', '$2', Cutoff}],
        [{{'$1', '$3', '$4'}}]
    }]),

    lists:foreach(fun({Endpoint, Conn, Stream}) ->
        close_connection(Conn, Stream),
        ets:delete(?TABLE, Endpoint)
    end, Expired),

    %% Schedule next cleanup
    erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_idle_connections),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Close all pooled connections
    AllConns = ets:tab2list(?TABLE),
    lists:foreach(fun(#pooled_conn{connection = Conn, stream = Stream}) ->
        close_connection(Conn, Stream)
    end, AllConns),
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
%% @doc Look up a connection in the pool.
lookup_connection(Endpoint) ->
    case ets:lookup(?TABLE, Endpoint) of
        [#pooled_conn{connection = Conn, stream = Stream}] ->
            %% Check if connection is still alive
            case is_connection_alive(Conn) of
                true ->
                    {ok, Conn, Stream};
                false ->
                    %% Connection died - remove from pool
                    ets:delete(?TABLE, Endpoint),
                    not_found
            end;
        [] ->
            not_found
    end.

%% @private
%% @doc Update last_used timestamp for a connection.
update_last_used(Endpoint) ->
    Now = erlang:system_time(millisecond),
    ets:update_element(?TABLE, Endpoint, {#pooled_conn.last_used, Now}).

%% @private
%% @doc Create a new QUIC connection to an endpoint.
create_connection(Endpoint) ->
    case parse_endpoint(Endpoint) of
        {ok, Host, Port} ->
            ConnectOpts = [
                {alpn, ["macula"]},
                {verify, none},
                {idle_timeout_ms, 60000},
                {keep_alive_interval_ms, 20000},
                {handshake_idle_timeout_ms, 30000}
            ],

            case macula_quic:connect(Host, Port, ConnectOpts, 5000) of
                {ok, Conn} ->
                    case macula_quic:open_stream(Conn) of
                        {ok, Stream} ->
                            {ok, Conn, Stream};
                        {error, Reason} ->
                            quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
                            {error, {stream_failed, Reason}}
                    end;
                {error, transport_down, _Details} ->
                    {error, {connect_failed, transport_down}};
                {error, Reason} ->
                    {error, {connect_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_endpoint, Reason}}
    end.

%% @private
%% @doc Parse endpoint string into host and port.
parse_endpoint(Endpoint) when is_binary(Endpoint) ->
    parse_endpoint(binary_to_list(Endpoint));
parse_endpoint(Endpoint) when is_list(Endpoint) ->
    case string:split(Endpoint, ":") of
        [Host, PortStr] ->
            try
                Port = list_to_integer(PortStr),
                {ok, Host, Port}
            catch
                _:_ -> {error, invalid_port}
            end;
        _ ->
            {error, invalid_format}
    end.

%% @private
%% @doc Check if a QUIC connection is still alive.
is_connection_alive(Conn) ->
    %% Try to get connection stats - if it fails, connection is dead
    case quicer:getstat(Conn, [recv_cnt]) of
        {ok, _} -> true;
        _ -> false
    end.

%% @private
%% @doc Evict the oldest (LRU) connection from the pool.
evict_oldest_connection() ->
    %% Find oldest connection by last_used timestamp
    case ets:first(?TABLE) of
        '$end_of_table' ->
            ok;
        _ ->
            %% Find connection with minimum last_used
            Oldest = ets:foldl(fun(#pooled_conn{endpoint = E, last_used = LU} = PC, Acc) ->
                case Acc of
                    none -> {E, LU, PC};
                    {_E, MinLU, _PC} when LU < MinLU -> {E, LU, PC};
                    _ -> Acc
                end
            end, none, ?TABLE),

            case Oldest of
                none ->
                    ok;
                {Endpoint, _, #pooled_conn{connection = Conn, stream = Stream}} ->
                    close_connection(Conn, Stream),
                    ets:delete(?TABLE, Endpoint)
            end
    end.

%% @private
%% @doc Close a QUIC connection and stream gracefully.
close_connection(Conn, Stream) ->
    catch quicer:async_shutdown_stream(Stream, ?QUIC_STREAM_SHUTDOWN_FLAG_GRACEFUL, 0),
    catch quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
    ok.
