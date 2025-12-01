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
    stats/0,
    %% NAT system API
    put/2,
    get_connected_peers/0
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

%% @doc Put a connection directly into the pool (NAT system API).
%% Used by connection upgrade to store successfully upgraded direct connections.
-spec put(binary(), term()) -> ok.
put(PeerId, Connection) ->
    gen_server:cast(?SERVER, {put_direct, PeerId, Connection}).

%% @doc Get list of peer IDs with active connections.
%% Returns list of peer IDs that have pooled connections.
-spec get_connected_peers() -> [binary()].
get_connected_peers() ->
    try
        Pattern = #pooled_conn{endpoint = '$1', _ = '_'},
        ets:match(?TABLE, Pattern)
    catch
        _:_ -> []
    end.

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
    LookupResult = lookup_connection(Endpoint),
    do_get_connection(LookupResult, Endpoint, State);

handle_call(stats, _From, #state{hits = Hits, misses = Misses, evictions = Evictions} = State) ->
    Total = Hits + Misses,
    HitRate = calculate_hit_rate(Hits, Total),
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
    CurrentSize = ets:info(?TABLE, size),
    NewState = maybe_evict_for_space(CurrentSize, State),
    store_connection(Endpoint, Connection, Stream),
    {noreply, NewState};

handle_cast({invalidate, Endpoint}, State) ->
    LookupResult = ets:lookup(?TABLE, Endpoint),
    do_invalidate(LookupResult, Endpoint),
    {noreply, State};

handle_cast({put_direct, PeerId, Connection}, State) ->
    %% Store a direct connection (from NAT traversal upgrade)
    %% Connection here is typically just the connection handle, no stream yet
    CurrentSize = ets:info(?TABLE, size),
    NewState = maybe_evict_for_space(CurrentSize, State),
    Now = erlang:system_time(millisecond),
    PooledConn = #pooled_conn{
        endpoint = PeerId,
        connection = Connection,
        stream = undefined,  % No stream yet for direct connections
        last_used = Now,
        created = Now
    },
    ets:insert(?TABLE, PooledConn),
    {noreply, NewState};

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
    LookupResult = ets:lookup(?TABLE, Endpoint),
    do_lookup(LookupResult, Endpoint).

%% @private Connection found - check if alive
do_lookup([#pooled_conn{connection = Conn, stream = Stream}], Endpoint) ->
    check_connection_alive(Conn, Stream, Endpoint);
%% @private No connection in pool
do_lookup([], _Endpoint) ->
    not_found.

%% @private Check if connection is alive and return appropriately
check_connection_alive(Conn, Stream, Endpoint) ->
    check_alive_result(is_connection_alive(Conn), Conn, Stream, Endpoint).

%% @private Connection is alive - return it
check_alive_result(true, Conn, Stream, _Endpoint) ->
    {ok, Conn, Stream};
%% @private Connection died - remove from pool
check_alive_result(false, _Conn, _Stream, Endpoint) ->
    ets:delete(?TABLE, Endpoint),
    not_found.

%% @private
%% @doc Update last_used timestamp for a connection.
update_last_used(Endpoint) ->
    Now = erlang:system_time(millisecond),
    ets:update_element(?TABLE, Endpoint, {#pooled_conn.last_used, Now}).

%% @private
%% @doc Create a new QUIC connection to an endpoint.
create_connection(Endpoint) ->
    ParseResult = parse_endpoint(Endpoint),
    do_create_connection(ParseResult).

%% @private Endpoint parsed - attempt connection
do_create_connection({ok, Host, Port}) ->
    ConnectOpts = [
        {alpn, ["macula"]},
        {verify, none},
        {idle_timeout_ms, 60000},
        {keep_alive_interval_ms, 20000},
        {handshake_idle_timeout_ms, 30000}
    ],
    ConnResult = macula_quic:connect(Host, Port, ConnectOpts, 5000),
    do_create_stream(ConnResult);
%% @private Invalid endpoint
do_create_connection({error, Reason}) ->
    {error, {invalid_endpoint, Reason}}.

%% @private Connection established - open stream
do_create_stream({ok, Conn}) ->
    StreamResult = macula_quic:open_stream(Conn),
    handle_stream_result(StreamResult, Conn);
%% @private Transport down
do_create_stream({error, transport_down, _Details}) ->
    {error, {connect_failed, transport_down}};
%% @private Connection failed
do_create_stream({error, Reason}) ->
    {error, {connect_failed, Reason}}.

%% @private Stream opened successfully
handle_stream_result({ok, Stream}, Conn) ->
    {ok, Conn, Stream};
%% @private Stream open failed
handle_stream_result({error, Reason}, Conn) ->
    quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
    {error, {stream_failed, Reason}}.

%% @private
%% @doc Parse endpoint string into host and port.
%% Supports formats: "host:port", "https://host:port", "http://host:port"
parse_endpoint(Endpoint) when is_binary(Endpoint) ->
    parse_endpoint(binary_to_list(Endpoint));
parse_endpoint(Endpoint) when is_list(Endpoint) ->
    %% Strip protocol prefix if present
    Stripped = strip_protocol(Endpoint),
    %% Split from the end to handle IPv6 addresses correctly
    case string:split(Stripped, ":", trailing) of
        [Host, PortStr] -> parse_port(Host, PortStr);
        _ -> {error, invalid_format}
    end.

%% @private Strip protocol prefix (https://, http://) from endpoint
strip_protocol("https://" ++ Rest) -> Rest;
strip_protocol("http://" ++ Rest) -> Rest;
strip_protocol(Endpoint) -> Endpoint.

%% @private Parse port string to integer
parse_port(Host, PortStr) ->
    parse_port_result(Host, catch list_to_integer(PortStr)).

%% @private Port parsed successfully
parse_port_result(Host, Port) when is_integer(Port), Port > 0, Port < 65536 ->
    {ok, Host, Port};
%% @private Invalid port value or parse error
parse_port_result(_Host, _) ->
    {error, invalid_port}.

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
    FirstKey = ets:first(?TABLE),
    do_evict(FirstKey).

%% @private Empty table - nothing to evict
do_evict('$end_of_table') ->
    ok;
%% @private Table has entries - find oldest
do_evict(_) ->
    Oldest = find_oldest_connection(),
    evict_connection(Oldest).

%% @private Find connection with minimum last_used timestamp
find_oldest_connection() ->
    ets:foldl(fun compare_connection_age/2, none, ?TABLE).

%% @private Compare and keep older connection
compare_connection_age(#pooled_conn{endpoint = E, last_used = LU} = PC, none) ->
    {E, LU, PC};
compare_connection_age(#pooled_conn{endpoint = E, last_used = LU} = PC, {_E, MinLU, _PC}) when LU < MinLU ->
    {E, LU, PC};
compare_connection_age(_PC, Acc) ->
    Acc.

%% @private No oldest found (shouldn't happen)
evict_connection(none) ->
    ok;
%% @private Evict the oldest connection
evict_connection({Endpoint, _, #pooled_conn{connection = Conn, stream = Stream}}) ->
    close_connection(Conn, Stream),
    ets:delete(?TABLE, Endpoint).

%% @private
%% @doc Close a QUIC connection and stream gracefully.
close_connection(Conn, Stream) ->
    catch quicer:async_shutdown_stream(Stream, ?QUIC_STREAM_SHUTDOWN_FLAG_GRACEFUL, 0),
    catch quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
    ok.

%%%===================================================================
%%% Helper Functions for Refactored Code
%%%===================================================================

%% @private Pool hit - return connection
do_get_connection({ok, Conn, Stream}, Endpoint, State) ->
    update_last_used(Endpoint),
    {reply, {ok, Conn, Stream}, State#state{hits = State#state.hits + 1}};
%% @private Pool miss - create new connection
do_get_connection(not_found, Endpoint, State) ->
    CreateResult = create_connection(Endpoint),
    do_get_create_connection(CreateResult, State).

%% @private Connection created successfully
do_get_create_connection({ok, Conn, Stream}, State) ->
    {reply, {ok, Conn, Stream}, State#state{misses = State#state.misses + 1}};
%% @private Connection creation failed
do_get_create_connection({error, Reason}, State) ->
    {reply, {error, Reason}, State#state{misses = State#state.misses + 1}}.

%% @private Calculate hit rate percentage
calculate_hit_rate(_Hits, 0) ->
    0.0;
calculate_hit_rate(Hits, Total) ->
    Hits / Total * 100.

%% @private Pool full - evict oldest
maybe_evict_for_space(CurrentSize, #state{max_connections = Max, evictions = Evictions} = State)
  when CurrentSize >= Max ->
    evict_oldest_connection(),
    State#state{evictions = Evictions + 1};
%% @private Pool has space
maybe_evict_for_space(_CurrentSize, State) ->
    State.

%% @private Store connection in pool
store_connection(Endpoint, Connection, Stream) ->
    Now = erlang:system_time(millisecond),
    PooledConn = #pooled_conn{
        endpoint = Endpoint,
        connection = Connection,
        stream = Stream,
        last_used = Now,
        created = Now
    },
    ets:insert(?TABLE, PooledConn).

%% @private Connection found - close and delete
do_invalidate([#pooled_conn{connection = Conn, stream = Stream}], Endpoint) ->
    close_connection(Conn, Stream),
    ets:delete(?TABLE, Endpoint);
%% @private No connection found
do_invalidate([], _Endpoint) ->
    ok.
