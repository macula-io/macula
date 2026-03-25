%%%-------------------------------------------------------------------
%%% @doc
%%% Peer Connection Pool - Reuses QUIC connections to remote peers.
%%%
%%% Stores one QUIC connection per endpoint. Callers open fresh streams
%%% on the reused connection (QUIC multiplexing). Connections are
%%% health-checked, LRU-evicted, and cleaned up on idle timeout.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peer_connection_pool).

-behaviour(gen_server).

-include_lib("quicer/include/quicer.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/0,
    start_link/1,
    get_connection/1,
    return_connection/2,
    invalidate/1,
    stats/0,
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
-define(DEFAULT_IDLE_TIMEOUT_MS, 60000).
-define(CLEANUP_INTERVAL_MS, 10000).

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
    last_used :: integer(),
    created :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link(#{}).

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Get a connection to an endpoint. Returns the QUIC connection handle.
%% Caller must open their own stream via macula_quic:open_stream(Conn).
-spec get_connection(binary()) -> {ok, term()} | {error, term()}.
get_connection(Endpoint) ->
    %% Direct ETS read for fast path (no gen_server call)
    case ets:info(?TABLE) of
        undefined -> create_and_pool(Endpoint);
        _ ->
            case ets:lookup(?TABLE, Endpoint) of
                [#pooled_conn{connection = Conn}] ->
                    case is_connection_alive(Conn) of
                        true ->
                            ets:update_element(?TABLE, Endpoint,
                                {#pooled_conn.last_used, erlang:system_time(millisecond)}),
                            gen_server:cast(?SERVER, hit),
                            {ok, Conn};
                        false ->
                            ets:delete(?TABLE, Endpoint),
                            create_and_pool(Endpoint)
                    end;
                [] ->
                    create_and_pool(Endpoint)
            end
    end.

%% @doc Return a connection to the pool (store/update).
-spec return_connection(binary(), term()) -> ok.
return_connection(Endpoint, Connection) ->
    gen_server:cast(?SERVER, {return_connection, Endpoint, Connection}).

%% @doc Invalidate and close a pooled connection.
-spec invalidate(binary()) -> ok.
invalidate(Endpoint) ->
    gen_server:cast(?SERVER, {invalidate, Endpoint}).

%% @doc Get pool statistics.
-spec stats() -> map().
stats() ->
    gen_server:call(?SERVER, stats).

%% @doc Put a connection directly (NAT system API).
-spec put(binary(), term()) -> ok.
put(PeerId, Connection) ->
    gen_server:cast(?SERVER, {return_connection, PeerId, Connection}).

%% @doc Get list of peer endpoints with active connections.
-spec get_connected_peers() -> [binary()].
get_connected_peers() ->
    case ets:info(?TABLE) of
        undefined -> [];
        _ -> ets:match(?TABLE, #pooled_conn{endpoint = '$1', _ = '_'})
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    MaxConnections = maps:get(max_connections, Opts, ?DEFAULT_MAX_CONNECTIONS),
    IdleTimeoutMs = maps:get(idle_timeout_ms, Opts, ?DEFAULT_IDLE_TIMEOUT_MS),

    Table = ets:new(?TABLE, [
        named_table, set, public,
        {keypos, #pooled_conn.endpoint},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),

    erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_idle_connections),

    {ok, #state{
        table = Table,
        max_connections = MaxConnections,
        idle_timeout_ms = IdleTimeoutMs,
        hits = 0,
        misses = 0,
        evictions = 0
    }}.

handle_call(stats, _From, #state{hits = Hits, misses = Misses, evictions = Evictions} = State) ->
    Total = Hits + Misses,
    HitRate = case Total of 0 -> 0.0; _ -> Hits / Total * 100 end,
    Stats = #{
        hits => Hits, misses => Misses, evictions => Evictions,
        total => Total, hit_rate => HitRate,
        pool_size => ets:info(?TABLE, size)
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(hit, State) ->
    {noreply, State#state{hits = State#state.hits + 1}};

handle_cast(miss, State) ->
    {noreply, State#state{misses = State#state.misses + 1}};

handle_cast({return_connection, Endpoint, Connection}, State) ->
    CurrentSize = ets:info(?TABLE, size),
    NewState = maybe_evict(CurrentSize, State),
    Now = erlang:system_time(millisecond),
    ets:insert(?TABLE, #pooled_conn{
        endpoint = Endpoint,
        connection = Connection,
        last_used = Now,
        created = Now
    }),
    {noreply, NewState};

handle_cast({invalidate, Endpoint}, State) ->
    case ets:lookup(?TABLE, Endpoint) of
        [#pooled_conn{connection = Conn}] ->
            catch quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
            ets:delete(?TABLE, Endpoint);
        [] -> ok
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_idle_connections, State) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now - State#state.idle_timeout_ms,
    Expired = ets:select(?TABLE, [{
        #pooled_conn{endpoint = '$1', last_used = '$2', connection = '$3', _ = '_'},
        [{'<', '$2', Cutoff}],
        [{{'$1', '$3'}}]
    }]),
    lists:foreach(fun({Endpoint, Conn}) ->
        catch quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
        ets:delete(?TABLE, Endpoint)
    end, Expired),
    erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_idle_connections),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    lists:foreach(fun(#pooled_conn{connection = Conn}) ->
        catch quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0)
    end, ets:tab2list(?TABLE)),
    ok.

%%%===================================================================
%%% Internal
%%%===================================================================

create_and_pool(Endpoint) ->
    case create_connection(Endpoint) of
        {ok, Conn} ->
            gen_server:cast(?SERVER, miss),
            gen_server:cast(?SERVER, {return_connection, Endpoint, Conn}),
            {ok, Conn};
        {error, _} = Err ->
            gen_server:cast(?SERVER, miss),
            Err
    end.

create_connection(Endpoint) when is_binary(Endpoint) ->
    create_connection(binary_to_list(Endpoint));
create_connection(Endpoint) when is_list(Endpoint) ->
    Stripped = strip_protocol(Endpoint),
    case string:split(Stripped, ":", trailing) of
        [Host, PortStr] ->
            case catch list_to_integer(PortStr) of
                Port when is_integer(Port), Port > 0, Port < 65536 ->
                    do_connect(Host, Port);
                _ -> {error, invalid_port}
            end;
        _ -> {error, invalid_format}
    end.

do_connect(Host, Port) ->
    ConnOpts = [
        {alpn, ["macula"]},
        {verify, none},
        {idle_timeout_ms, 60000},
        {keep_alive_interval_ms, 20000},
        {handshake_idle_timeout_ms, 30000}
    ],
    case macula_quic:connect(Host, Port, ConnOpts, 5000) of
        {ok, Conn} -> {ok, Conn};
        {error, transport_down, _} -> {error, {connect_failed, transport_down}};
        {error, Reason} -> {error, {connect_failed, Reason}}
    end.

strip_protocol("https://" ++ Rest) -> Rest;
strip_protocol("http://" ++ Rest) -> Rest;
strip_protocol(Endpoint) -> Endpoint.

is_connection_alive(Conn) ->
    case quicer:getstat(Conn, [recv_cnt]) of
        {ok, _} -> true;
        _ -> false
    end.

maybe_evict(CurrentSize, #state{max_connections = Max, evictions = Evictions} = State)
  when CurrentSize >= Max ->
    evict_oldest(),
    State#state{evictions = Evictions + 1};
maybe_evict(_CurrentSize, State) ->
    State.

evict_oldest() ->
    case ets:first(?TABLE) of
        '$end_of_table' -> ok;
        _ ->
            Oldest = ets:foldl(fun
                (#pooled_conn{endpoint = E, last_used = LU}, none) -> {E, LU};
                (#pooled_conn{endpoint = E, last_used = LU}, {_OE, OLU}) when LU < OLU -> {E, LU};
                (_, Acc) -> Acc
            end, none, ?TABLE),
            case Oldest of
                none -> ok;
                {Endpoint, _} ->
                    case ets:lookup(?TABLE, Endpoint) of
                        [#pooled_conn{connection = Conn}] ->
                            catch quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0);
                        _ -> ok
                    end,
                    ets:delete(?TABLE, Endpoint)
            end
    end.
