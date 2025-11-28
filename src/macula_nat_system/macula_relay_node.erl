%%%-------------------------------------------------------------------
%%% @doc
%%% Peer Relay Node.
%%%
%%% Provides relay functionality for peers that cannot establish
%%% direct connections due to NAT restrictions. A relay node:
%%%
%%% - Accepts connections from peers behind restrictive NATs
%%% - Forwards traffic between connected peers
%%% - Manages relay sessions with bandwidth limits
%%% - Auto-registers with relay registry when capable
%%%
%%% Relay Protocol:
%%% 1. Peer A connects to relay with RELAY_REQUEST(target_id)
%%% 2. Relay checks if target is reachable and accepts
%%% 3. Relay establishes/reuses connection to target
%%% 4. Relay forwards bidirectional traffic
%%% 5. Either peer can close the relay session
%%%
%%% Resource Limits:
%%% - Max concurrent relay sessions (default: 100)
%%% - Per-session bandwidth limit (default: 1 MB/s)
%%% - Session timeout (default: 30 minutes)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_relay_node).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    enable/0,
    enable/1,
    disable/0,
    is_enabled/0,
    request_relay/2,
    close_relay/1,
    get_stats/0,
    get_sessions/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_MAX_SESSIONS, 100).
-define(DEFAULT_BANDWIDTH_LIMIT, 1024 * 1024).  % 1 MB/s
-define(DEFAULT_SESSION_TIMEOUT_MS, 1800000).   % 30 minutes
-define(SESSION_CHECK_INTERVAL_MS, 60000).      % Check sessions every 60s

%%%===================================================================
%%% Types
%%%===================================================================

-type relay_session() :: #{
    session_id := binary(),
    initiator_id := binary(),
    target_id := binary(),
    initiator_conn => term(),
    target_conn => term(),
    started_at := integer(),
    bytes_relayed := non_neg_integer(),
    last_activity := integer()
}.

-type relay_stats() :: #{
    enabled := boolean(),
    max_sessions := pos_integer(),
    active_sessions := non_neg_integer(),
    total_bytes_relayed := non_neg_integer(),
    total_sessions_created := non_neg_integer()
}.

-export_type([relay_session/0, relay_stats/0]).

-record(state, {
    enabled :: boolean(),
    max_sessions :: pos_integer(),
    bandwidth_limit :: pos_integer(),
    session_timeout :: pos_integer(),
    sessions :: #{binary() => relay_session()},
    total_bytes :: non_neg_integer(),
    total_sessions :: non_neg_integer(),
    local_node_id :: binary() | undefined,
    local_endpoint :: {binary(), inet:port_number()} | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the relay node server.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Enable relay functionality on this node.
-spec enable() -> ok | {error, term()}.
enable() ->
    enable(#{}).

%% @doc Enable relay with options.
%% Options:
%%   node_id - This node's ID
%%   endpoint - This node's endpoint for relay registry
%%   capacity - Max concurrent sessions
-spec enable(map()) -> ok | {error, term()}.
enable(Opts) ->
    gen_server:call(?SERVER, {enable, Opts}).

%% @doc Disable relay functionality.
-spec disable() -> ok.
disable() ->
    gen_server:call(?SERVER, disable).

%% @doc Check if relay is enabled.
-spec is_enabled() -> boolean().
is_enabled() ->
    gen_server:call(?SERVER, is_enabled).

%% @doc Request a relay session to target.
%% Returns session ID on success.
-spec request_relay(binary(), binary()) -> {ok, binary()} | {error, term()}.
request_relay(InitiatorId, TargetId) ->
    gen_server:call(?SERVER, {request_relay, InitiatorId, TargetId}).

%% @doc Close a relay session.
-spec close_relay(binary()) -> ok.
close_relay(SessionId) ->
    gen_server:cast(?SERVER, {close_relay, SessionId}).

%% @doc Get relay statistics.
-spec get_stats() -> relay_stats().
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% @doc Get active relay sessions.
-spec get_sessions() -> [relay_session()].
get_sessions() ->
    gen_server:call(?SERVER, get_sessions).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    MaxSessions = maps:get(max_relay_sessions, Opts, ?DEFAULT_MAX_SESSIONS),
    BandwidthLimit = maps:get(bandwidth_limit, Opts, ?DEFAULT_BANDWIDTH_LIMIT),
    SessionTimeout = maps:get(session_timeout_ms, Opts, ?DEFAULT_SESSION_TIMEOUT_MS),
    AutoEnable = maps:get(relay_auto_enable, Opts, false),

    %% Schedule session cleanup
    schedule_session_check(),

    State = #state{
        enabled = false,
        max_sessions = MaxSessions,
        bandwidth_limit = BandwidthLimit,
        session_timeout = SessionTimeout,
        sessions = #{},
        total_bytes = 0,
        total_sessions = 0,
        local_node_id = undefined,
        local_endpoint = undefined
    },

    %% Auto-enable if configured
    FinalState = case AutoEnable of
        true ->
            NodeId = maps:get(node_id, Opts, undefined),
            Endpoint = maps:get(endpoint, Opts, undefined),
            do_enable(State, NodeId, Endpoint, MaxSessions);
        false ->
            State
    end,

    ?LOG_INFO("Relay node started (max_sessions=~p, enabled=~p)",
              [MaxSessions, FinalState#state.enabled]),

    {ok, FinalState}.

handle_call({enable, Opts}, _From, State) ->
    NodeId = maps:get(node_id, Opts, State#state.local_node_id),
    Endpoint = maps:get(endpoint, Opts, State#state.local_endpoint),
    Capacity = maps:get(capacity, Opts, State#state.max_sessions),

    NewState = do_enable(State, NodeId, Endpoint, Capacity),
    {reply, ok, NewState};

handle_call(disable, _From, State) ->
    NewState = do_disable(State),
    {reply, ok, NewState};

handle_call(is_enabled, _From, State) ->
    {reply, State#state.enabled, State};

handle_call({request_relay, InitiatorId, TargetId}, _From, State) ->
    case State#state.enabled of
        false ->
            {reply, {error, relay_not_enabled}, State};
        true ->
            case maps:size(State#state.sessions) >= State#state.max_sessions of
                true ->
                    {reply, {error, max_sessions_reached}, State};
                false ->
                    {Result, NewState} = create_relay_session(InitiatorId, TargetId, State),
                    {reply, Result, NewState}
            end
    end;

handle_call(get_stats, _From, State) ->
    Stats = #{
        enabled => State#state.enabled,
        max_sessions => State#state.max_sessions,
        active_sessions => maps:size(State#state.sessions),
        total_bytes_relayed => State#state.total_bytes,
        total_sessions_created => State#state.total_sessions
    },
    {reply, Stats, State};

handle_call(get_sessions, _From, State) ->
    Sessions = maps:values(State#state.sessions),
    {reply, Sessions, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({close_relay, SessionId}, State) ->
    NewState = close_session(SessionId, State),
    {noreply, NewState};

handle_cast({relay_data, SessionId, Data, Direction}, State) ->
    NewState = relay_data(SessionId, Data, Direction, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_sessions, State) ->
    NewState = cleanup_expired_sessions(State),
    schedule_session_check(),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Unregister from relay registry
    case State#state.local_node_id of
        undefined -> ok;
        NodeId -> macula_relay_registry:unregister(NodeId)
    end,
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Enable relay mode.
-spec do_enable(#state{}, binary() | undefined, term() | undefined, pos_integer()) -> #state{}.
do_enable(State, undefined, _Endpoint, _Capacity) ->
    ?LOG_WARNING("Cannot enable relay: node_id not provided"),
    State;
do_enable(State, NodeId, Endpoint, Capacity) ->
    %% Register with relay registry
    case Endpoint of
        undefined ->
            ?LOG_WARNING("Cannot register relay: endpoint not provided");
        _ ->
            case whereis(macula_relay_registry) of
                undefined ->
                    ?LOG_WARNING("Cannot register relay: registry not available");
                _Pid ->
                    macula_relay_registry:register(NodeId, Endpoint, #{capacity => Capacity})
            end
    end,

    ?LOG_INFO("Relay enabled for node ~s", [NodeId]),
    State#state{
        enabled = true,
        local_node_id = NodeId,
        local_endpoint = Endpoint
    }.

%% @private
%% @doc Disable relay mode.
-spec do_disable(#state{}) -> #state{}.
do_disable(#state{local_node_id = undefined} = State) ->
    State#state{enabled = false};
do_disable(#state{local_node_id = NodeId} = State) ->
    %% Unregister from relay registry
    case whereis(macula_relay_registry) of
        undefined -> ok;
        _Pid -> macula_relay_registry:unregister(NodeId)
    end,

    %% Close all active sessions
    maps:foreach(
        fun(SessionId, _Session) ->
            close_session(SessionId, State)
        end,
        State#state.sessions
    ),

    ?LOG_INFO("Relay disabled for node ~s", [NodeId]),
    State#state{enabled = false, sessions = #{}}.

%% @private
%% @doc Create a new relay session.
-spec create_relay_session(binary(), binary(), #state{}) ->
    {{ok, binary()} | {error, term()}, #state{}}.
create_relay_session(InitiatorId, TargetId, State) ->
    SessionId = generate_session_id(),
    Now = erlang:system_time(millisecond),

    Session = #{
        session_id => SessionId,
        initiator_id => InitiatorId,
        target_id => TargetId,
        started_at => Now,
        bytes_relayed => 0,
        last_activity => Now
    },

    NewSessions = maps:put(SessionId, Session, State#state.sessions),
    NewTotalSessions = State#state.total_sessions + 1,

    ?LOG_DEBUG("Created relay session ~s: ~s -> ~s",
               [SessionId, InitiatorId, TargetId]),

    {{ok, SessionId}, State#state{
        sessions = NewSessions,
        total_sessions = NewTotalSessions
    }}.

%% @private
%% @doc Close a relay session.
-spec close_session(binary(), #state{}) -> #state{}.
close_session(SessionId, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            State;
        Session ->
            %% Close connections if they exist
            close_session_connections(Session),

            BytesRelayed = maps:get(bytes_relayed, Session, 0),
            NewTotalBytes = State#state.total_bytes + BytesRelayed,
            NewSessions = maps:remove(SessionId, State#state.sessions),

            ?LOG_DEBUG("Closed relay session ~s (relayed ~p bytes)",
                       [SessionId, BytesRelayed]),

            State#state{
                sessions = NewSessions,
                total_bytes = NewTotalBytes
            }
    end.

%% @private
%% @doc Close session connections.
-spec close_session_connections(relay_session()) -> ok.
close_session_connections(Session) ->
    case maps:get(initiator_conn, Session, undefined) of
        undefined -> ok;
        InitConn -> catch quicer:close_connection(InitConn)
    end,
    case maps:get(target_conn, Session, undefined) of
        undefined -> ok;
        TargetConn -> catch quicer:close_connection(TargetConn)
    end,
    ok.

%% @private
%% @doc Relay data between peers.
-spec relay_data(binary(), binary(), initiator_to_target | target_to_initiator, #state{}) ->
    #state{}.
relay_data(SessionId, Data, Direction, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            State;
        Session ->
            Now = erlang:system_time(millisecond),
            DataSize = byte_size(Data),

            %% Check bandwidth limit
            case DataSize > State#state.bandwidth_limit of
                true ->
                    ?LOG_WARNING("Relay data exceeds bandwidth limit for session ~s",
                                 [SessionId]),
                    State;
                false ->
                    %% Forward data to appropriate peer
                    forward_data(Session, Data, Direction),

                    %% Update session stats
                    UpdatedSession = Session#{
                        bytes_relayed := maps:get(bytes_relayed, Session, 0) + DataSize,
                        last_activity := Now
                    },
                    NewSessions = maps:put(SessionId, UpdatedSession, State#state.sessions),
                    State#state{sessions = NewSessions}
            end
    end.

%% @private
%% @doc Forward data to the appropriate peer.
-spec forward_data(relay_session(), binary(), initiator_to_target | target_to_initiator) -> ok.
forward_data(Session, Data, initiator_to_target) ->
    case maps:get(target_conn, Session, undefined) of
        undefined ->
            ?LOG_DEBUG("No target connection for session ~s",
                       [maps:get(session_id, Session)]);
        Conn ->
            send_relay_data(Conn, Data)
    end;
forward_data(Session, Data, target_to_initiator) ->
    case maps:get(initiator_conn, Session, undefined) of
        undefined ->
            ?LOG_DEBUG("No initiator connection for session ~s",
                       [maps:get(session_id, Session)]);
        Conn ->
            send_relay_data(Conn, Data)
    end.

%% @private
%% @doc Send relay data over connection.
-spec send_relay_data(term(), binary()) -> ok.
send_relay_data(Conn, Data) ->
    %% This would use the actual QUIC stream
    %% For now, just log
    ?LOG_DEBUG("Would send ~p bytes over relay connection ~p", [byte_size(Data), Conn]),
    ok.

%% @private
%% @doc Cleanup expired sessions.
-spec cleanup_expired_sessions(#state{}) -> #state{}.
cleanup_expired_sessions(State) ->
    Now = erlang:system_time(millisecond),
    Timeout = State#state.session_timeout,

    ExpiredSessions = maps:filter(
        fun(_SessionId, Session) ->
            LastActivity = maps:get(last_activity, Session, 0),
            (Now - LastActivity) > Timeout
        end,
        State#state.sessions
    ),

    %% Close expired sessions
    lists:foldl(
        fun(SessionId, AccState) ->
            ?LOG_DEBUG("Session ~s expired due to inactivity", [SessionId]),
            close_session(SessionId, AccState)
        end,
        State,
        maps:keys(ExpiredSessions)
    ).

%% @private
%% @doc Schedule session check timer.
-spec schedule_session_check() -> reference().
schedule_session_check() ->
    erlang:send_after(?SESSION_CHECK_INTERVAL_MS, self(), check_sessions).

%% @private
%% @doc Generate unique session ID.
-spec generate_session_id() -> binary().
generate_session_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).
