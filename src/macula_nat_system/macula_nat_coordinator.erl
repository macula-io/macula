%%%-------------------------------------------------------------------
%%% @doc
%%% NAT Hole Punch Coordinator.
%%%
%%% Coordinates hole punching between two NATted peers using
%%% NATCracker-informed strategies. Determines the optimal connection
%%% approach based on both peers' NAT profiles:
%%%
%%% Connection Strategy Decision Tree:
%%% 1. Direct Connection - Possible when:
%%%    - Either peer has public IP (no NAT)
%%%    - Target has EI mapping + EI filtering (Full Cone)
%%%
%%% 2. Hole Punching - Possible when:
%%%    - Both peers have EI or HD mapping (predictable external address)
%%%    - At least one has PP or PC allocation (predictable ports)
%%%    - Neither has PD mapping + PD filtering + RD allocation
%%%
%%% 3. Relay Required - When:
%%%    - Either peer has symmetric NAT (PD+PD+RD)
%%%    - Hole punching attempts fail
%%%
%%% Hole Punch Coordination Protocol:
%%% 1. Initiator sends PUNCH_REQUEST to coordinator (any public-IP peer)
%%% 2. Coordinator fetches both peers' NAT profiles from DHT
%%% 3. Coordinator sends PUNCH_COORDINATE to both peers with timing info
%%% 4. Both peers send PUNCH_EXECUTE at coordinated time
%%% 5. Coordinator receives PUNCH_RESULT from both peers
%%% 6. If failed, falls back to relay
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_nat_coordinator).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    request_connection/2,
    request_connection/3,
    coordinate_punch/3,
    report_result/3,
    get_pending/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(PUNCH_TIMEOUT_MS, 5000).      % 5 seconds for hole punch attempt
-define(COORDINATION_DELAY_MS, 100).  % 100ms delay for clock sync
-define(MAX_PUNCH_ATTEMPTS, 3).
-define(CLEANUP_INTERVAL_MS, 30000).  % Cleanup stale sessions every 30s

%%%===================================================================
%%% Types
%%%===================================================================

-type connection_strategy() :: direct | hole_punch | relay.

-type punch_session() :: #{
    session_id := binary(),
    initiator_id := binary(),
    target_id := binary(),
    initiator_profile := macula_nat_cache:nat_profile() | undefined,
    target_profile := macula_nat_cache:nat_profile() | undefined,
    strategy := connection_strategy(),
    state := pending | coordinating | executing | completed | failed,
    attempts := non_neg_integer(),
    created_at := integer(),
    result => success | timeout | unreachable
}.

-export_type([connection_strategy/0, punch_session/0]).

-record(state, {
    sessions :: #{binary() => punch_session()},
    pending_by_peer :: #{binary() => [binary()]},  % peer_id -> [session_ids]
    punch_timeout_ms :: pos_integer(),
    max_attempts :: pos_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the NAT coordinator server.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Request a connection to a target peer.
%% Returns the recommended strategy and session ID for tracking.
-spec request_connection(binary(), binary()) ->
    {ok, connection_strategy(), binary()} | {error, term()}.
request_connection(InitiatorId, TargetId) ->
    request_connection(InitiatorId, TargetId, #{}).

%% @doc Request connection with options.
-spec request_connection(binary(), binary(), map()) ->
    {ok, connection_strategy(), binary()} | {error, term()}.
request_connection(InitiatorId, TargetId, Opts) ->
    gen_server:call(?SERVER, {request_connection, InitiatorId, TargetId, Opts}).

%% @doc Coordinate a hole punch between two peers.
%% Called by a public-IP peer acting as coordinator.
-spec coordinate_punch(binary(), binary(), map()) -> ok | {error, term()}.
coordinate_punch(InitiatorId, TargetId, Opts) ->
    gen_server:cast(?SERVER, {coordinate_punch, InitiatorId, TargetId, Opts}).

%% @doc Report result of a hole punch attempt.
-spec report_result(binary(), binary(), success | failure) -> ok.
report_result(SessionId, ReporterId, Result) ->
    gen_server:cast(?SERVER, {report_result, SessionId, ReporterId, Result}).

%% @doc Get all pending punch sessions (for debugging).
-spec get_pending() -> [punch_session()].
get_pending() ->
    gen_server:call(?SERVER, get_pending).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    PunchTimeout = maps:get(punch_timeout_ms, Opts, ?PUNCH_TIMEOUT_MS),
    MaxAttempts = maps:get(max_punch_attempts, Opts, ?MAX_PUNCH_ATTEMPTS),

    %% Schedule periodic cleanup
    schedule_cleanup(),

    ?LOG_INFO("NAT coordinator started (timeout=~p ms, max_attempts=~p)",
              [PunchTimeout, MaxAttempts]),

    {ok, #state{
        sessions = #{},
        pending_by_peer = #{},
        punch_timeout_ms = PunchTimeout,
        max_attempts = MaxAttempts
    }}.

handle_call({request_connection, InitiatorId, TargetId, _Opts}, _From, State) ->
    %% Fetch NAT profiles for both peers
    InitiatorProfile = fetch_nat_profile(InitiatorId),
    TargetProfile = fetch_nat_profile(TargetId),

    %% Determine optimal strategy
    Strategy = determine_strategy(InitiatorProfile, TargetProfile),

    %% Create session
    SessionId = generate_session_id(),
    Session = #{
        session_id => SessionId,
        initiator_id => InitiatorId,
        target_id => TargetId,
        initiator_profile => InitiatorProfile,
        target_profile => TargetProfile,
        strategy => Strategy,
        state => pending,
        attempts => 0,
        created_at => erlang:system_time(second)
    },

    NewState = add_session(Session, State),

    ?LOG_INFO("Connection request ~s: ~s -> ~s, strategy=~p",
              [SessionId, InitiatorId, TargetId, Strategy]),

    %% For hole_punch strategy, initiate coordination
    case Strategy of
        hole_punch ->
            self() ! {start_punch, SessionId};
        _ ->
            ok
    end,

    {reply, {ok, Strategy, SessionId}, NewState};

handle_call(get_pending, _From, #state{sessions = Sessions} = State) ->
    Pending = [S || S <- maps:values(Sessions),
                    maps:get(state, S) =/= completed,
                    maps:get(state, S) =/= failed],
    {reply, Pending, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({coordinate_punch, InitiatorId, TargetId, Opts}, State) ->
    %% External coordinator request (from another peer)
    SessionId = maps:get(session_id, Opts, generate_session_id()),

    case maps:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            NewSession = Session#{state => coordinating},
            NewState = update_session(NewSession, State),
            do_coordinate_punch(NewSession),
            {noreply, NewState};
        error ->
            %% New coordination request
            InitiatorProfile = fetch_nat_profile(InitiatorId),
            TargetProfile = fetch_nat_profile(TargetId),

            Session = #{
                session_id => SessionId,
                initiator_id => InitiatorId,
                target_id => TargetId,
                initiator_profile => InitiatorProfile,
                target_profile => TargetProfile,
                strategy => hole_punch,
                state => coordinating,
                attempts => 0,
                created_at => erlang:system_time(second)
            },

            NewState = add_session(Session, State),
            do_coordinate_punch(Session),
            {noreply, NewState}
    end;

handle_cast({report_result, SessionId, ReporterId, Result}, State) ->
    case maps:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            NewState = process_punch_result(Session, ReporterId, Result, State),
            {noreply, NewState};
        error ->
            ?LOG_WARNING("Punch result for unknown session: ~s", [SessionId]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({start_punch, SessionId}, State) ->
    case maps:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            NewSession = Session#{state => coordinating},
            NewState = update_session(NewSession, State),
            do_coordinate_punch(NewSession),
            {noreply, NewState};
        error ->
            {noreply, State}
    end;

handle_info({punch_timeout, SessionId}, State) ->
    case maps:find(SessionId, State#state.sessions) of
        {ok, #{state := executing} = Session} ->
            NewState = handle_punch_timeout(Session, State),
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

handle_info(cleanup, State) ->
    NewState = cleanup_stale_sessions(State),
    schedule_cleanup(),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions - Strategy Determination
%%%===================================================================

%% @private
%% @doc Fetch NAT profile for a peer (local cache first, then DHT).
-spec fetch_nat_profile(binary()) -> macula_nat_cache:nat_profile() | undefined.
fetch_nat_profile(PeerId) ->
    do_fetch_nat_profile(whereis(macula_nat_cache), PeerId).

do_fetch_nat_profile(undefined, _PeerId) ->
    undefined;
do_fetch_nat_profile(_Pid, PeerId) ->
    nat_cache_result_to_profile(macula_nat_cache:get_from_dht(PeerId)).

nat_cache_result_to_profile({ok, Profile}) -> Profile;
nat_cache_result_to_profile(_) -> undefined.

%% @private
%% @doc Determine connection strategy based on NAT profiles.
%% Following NATCracker methodology for optimal path selection.
-spec determine_strategy(macula_nat_cache:nat_profile() | undefined,
                         macula_nat_cache:nat_profile() | undefined) ->
    connection_strategy().
determine_strategy(undefined, undefined) ->
    %% No profile info - try hole punch, fall back to relay
    hole_punch;
determine_strategy(undefined, TargetProfile) ->
    determine_strategy_with_target(TargetProfile);
determine_strategy(InitiatorProfile, undefined) ->
    determine_strategy_with_initiator(InitiatorProfile);
determine_strategy(InitiatorProfile, TargetProfile) ->
    %% Both profiles available - full analysis
    determine_strategy_full(InitiatorProfile, TargetProfile).

%% @private
%% @doc Strategy when only target profile is known.
-spec determine_strategy_with_target(macula_nat_cache:nat_profile()) -> connection_strategy().
determine_strategy_with_target(#{can_receive_unsolicited := true}) ->
    direct;  % Target can receive direct connections
determine_strategy_with_target(#{requires_relay := true}) ->
    relay;   % Target has symmetric NAT
determine_strategy_with_target(_) ->
    hole_punch.  % Try hole punch

%% @private
%% @doc Strategy when only initiator profile is known.
-spec determine_strategy_with_initiator(macula_nat_cache:nat_profile()) -> connection_strategy().
determine_strategy_with_initiator(#{requires_relay := true}) ->
    relay;   % Initiator has symmetric NAT
determine_strategy_with_initiator(_) ->
    hole_punch.  % Try hole punch (optimistic)

%% @private
%% @doc Full strategy determination with both profiles.
-spec determine_strategy_full(macula_nat_cache:nat_profile(),
                               macula_nat_cache:nat_profile()) ->
    connection_strategy().
determine_strategy_full(InitiatorProfile, TargetProfile) ->
    CanDirect = can_connect_direct(InitiatorProfile, TargetProfile),
    CanPunch = can_hole_punch(InitiatorProfile, TargetProfile),
    select_strategy(CanDirect, CanPunch).

select_strategy(true, _) -> direct;
select_strategy(false, true) -> hole_punch;
select_strategy(false, false) -> relay.

%% @private
%% @doc Check if direct connection is possible.
%% Direct works when target can receive unsolicited packets.
-spec can_connect_direct(macula_nat_cache:nat_profile(),
                          macula_nat_cache:nat_profile()) -> boolean().
can_connect_direct(_InitiatorProfile, #{can_receive_unsolicited := true}) ->
    true;  % Target has Full Cone NAT or public IP
can_connect_direct(#{can_receive_unsolicited := true}, _TargetProfile) ->
    true;  % Initiator has Full Cone - target can call back
can_connect_direct(_, _) ->
    false.

%% @private
%% @doc Check if hole punching is feasible based on NAT types.
%% Hole punching works with predictable mapping + predictable allocation.
-spec can_hole_punch(macula_nat_cache:nat_profile(),
                      macula_nat_cache:nat_profile()) -> boolean().
can_hole_punch(InitiatorProfile, TargetProfile) ->
    InitiatorFeasible = is_punch_feasible(InitiatorProfile),
    TargetFeasible = is_punch_feasible(TargetProfile),
    InitiatorFeasible andalso TargetFeasible.

%% @private
%% @doc Check if a single peer's NAT allows hole punching.
-spec is_punch_feasible(macula_nat_cache:nat_profile()) -> boolean().
is_punch_feasible(#{requires_relay := true}) ->
    false;  % Symmetric NAT - hole punch won't work
is_punch_feasible(#{mapping_policy := pd, allocation_policy := rd}) ->
    false;  % Port-dependent mapping + random allocation = unpredictable
is_punch_feasible(#{mapping_policy := MappingPolicy, allocation_policy := AllocationPolicy}) ->
    %% Hole punch feasible with:
    %% - EI or HD mapping (predictable external address per destination)
    %% - PP or PC allocation (predictable port assignment)
    is_mapping_predictable(MappingPolicy) andalso is_allocation_predictable(AllocationPolicy);
is_punch_feasible(_) ->
    true.  % Unknown profile - try optimistically

is_mapping_predictable(ei) -> true;
is_mapping_predictable(hd) -> true;
is_mapping_predictable(_) -> false.

is_allocation_predictable(pp) -> true;
is_allocation_predictable(pc) -> true;
is_allocation_predictable(_) -> false.

%%%===================================================================
%%% Internal functions - Hole Punch Coordination
%%%===================================================================

%% @private
%% @doc Perform hole punch coordination.
%% Sends PUNCH_COORDINATE to both peers with timing information.
-spec do_coordinate_punch(punch_session()) -> ok.
do_coordinate_punch(#{session_id := SessionId,
                      initiator_id := InitiatorId,
                      target_id := TargetId,
                      initiator_profile := InitiatorProfile,
                      target_profile := TargetProfile}) ->

    ?LOG_DEBUG("Coordinating hole punch ~s between ~s and ~s",
               [SessionId, InitiatorId, TargetId]),

    %% Calculate punch timing (synchronized moment for both peers)
    PunchTime = erlang:system_time(millisecond) + ?COORDINATION_DELAY_MS,

    %% Predict ports based on NAT profiles
    InitiatorPorts = predict_punch_ports(InitiatorProfile),
    TargetPorts = predict_punch_ports(TargetProfile),

    %% Send coordination message to initiator
    send_punch_coordinate(InitiatorId, #{
        session_id => SessionId,
        peer_id => TargetId,
        peer_ports => TargetPorts,
        punch_time => PunchTime,
        role => initiator
    }),

    %% Send coordination message to target
    send_punch_coordinate(TargetId, #{
        session_id => SessionId,
        peer_id => InitiatorId,
        peer_ports => InitiatorPorts,
        punch_time => PunchTime,
        role => target
    }),

    %% Schedule timeout
    erlang:send_after(?PUNCH_TIMEOUT_MS, self(), {punch_timeout, SessionId}),

    ok.

%% @private
%% @doc Predict ports for hole punching based on NAT profile.
%% Uses macula_port_predictor for intelligent prediction when available.
-spec predict_punch_ports(macula_nat_cache:nat_profile() | undefined) -> [inet:port_number()].
predict_punch_ports(undefined) ->
    %% No profile - try common port range
    [4433, 4434, 4435];
predict_punch_ports(Profile) ->
    NodeId = maps:get(node_id, Profile, <<>>),
    AllocationPolicy = maps:get(allocation_policy, Profile, unknown),
    BasePort = extract_base_port(maps:get(reflexive_address, Profile, undefined)),
    do_predict_punch_ports(whereis(macula_port_predictor), Profile, NodeId, AllocationPolicy, BasePort).

extract_base_port({_, Port}) -> Port;
extract_base_port(undefined) -> undefined.

do_predict_punch_ports(undefined, Profile, _NodeId, _AllocationPolicy, BasePort) ->
    %% Fallback to simple prediction
    simple_predict_punch_ports(Profile, BasePort);
do_predict_punch_ports(_Pid, _Profile, NodeId, AllocationPolicy, BasePort) ->
    Prediction = macula_port_predictor:predict(NodeId, AllocationPolicy,
                                                #{base_port => BasePort, count => 5}),
    maps:get(ports, Prediction, [4433, 4434, 4435]).

%% @private
%% @doc Simple port prediction fallback for hole punching.
-spec simple_predict_punch_ports(map(), inet:port_number() | undefined) -> [inet:port_number()].
simple_predict_punch_ports(#{allocation_policy := pp}, Port) when is_integer(Port) ->
    [Port];
simple_predict_punch_ports(#{allocation_policy := pc, port_delta := Delta}, Port)
  when is_integer(Port), is_integer(Delta) ->
    [Port, Port + Delta, Port + (2 * Delta)];
simple_predict_punch_ports(_, Port) when is_integer(Port) ->
    [Port, Port + 1, Port + 2];
simple_predict_punch_ports(_, _) ->
    [4433, 4434, 4435].

%% @private
%% @doc Send PUNCH_COORDINATE message to a peer.
%% First tries direct delivery via QUIC, falls back to DHT storage if unavailable.
-spec send_punch_coordinate(binary(), map()) -> ok.
send_punch_coordinate(PeerId, CoordinateInfo) ->
    %% Try to lookup peer endpoint for direct delivery
    case lookup_peer_endpoint_for_punch(PeerId) of
        {ok, Endpoint} ->
            send_punch_coordinate_direct(PeerId, Endpoint, CoordinateInfo);
        {error, _Reason} ->
            %% Fall back to DHT storage (peer must poll)
            send_punch_coordinate_via_dht(PeerId, CoordinateInfo)
    end.

%% @private
%% @doc Send PUNCH_COORDINATE directly via QUIC connection.
-spec send_punch_coordinate_direct(binary(), binary(), map()) -> ok.
send_punch_coordinate_direct(PeerId, Endpoint, CoordinateInfo) ->
    ?LOG_DEBUG("Sending PUNCH_COORDINATE directly to ~s via ~s", [binary:encode_hex(PeerId), Endpoint]),

    %% Send punch_coordinate message via peer connector
    Result = macula_peer_connector:send_message(Endpoint, punch_coordinate, CoordinateInfo),
    log_direct_send_result(Result, PeerId, CoordinateInfo).

%% @private Direct send succeeded
log_direct_send_result(ok, PeerId, _CoordinateInfo) ->
    ?LOG_DEBUG("PUNCH_COORDINATE sent directly to ~s", [binary:encode_hex(PeerId)]),
    ok;
%% @private Direct send failed - fall back to DHT
log_direct_send_result({error, Reason}, PeerId, CoordinateInfo) ->
    ?LOG_WARNING("Direct PUNCH_COORDINATE failed (~p), falling back to DHT", [Reason]),
    send_punch_coordinate_via_dht(PeerId, CoordinateInfo).

%% @private
%% @doc Store PUNCH_COORDINATE in DHT for peer to poll.
-spec send_punch_coordinate_via_dht(binary(), map()) -> ok.
send_punch_coordinate_via_dht(PeerId, CoordinateInfo) ->
    case whereis(macula_routing_server) of
        undefined ->
            ?LOG_WARNING("Cannot store PUNCH_COORDINATE: routing server unavailable"),
            ok;
        Pid ->
            Message = #{type => punch_coordinate, payload => CoordinateInfo},
            SessionId = maps:get(session_id, CoordinateInfo),
            Key = punch_coordination_key(PeerId, SessionId),
            safe_store_coordinate(Pid, PeerId, Key, Message)
    end.

%% @private
%% @doc Lookup peer endpoint for direct punch coordination delivery.
-spec lookup_peer_endpoint_for_punch(binary()) -> {ok, binary()} | {error, not_found}.
lookup_peer_endpoint_for_punch(NodeId) ->
    %% Try NAT cache first (has reflexive address and endpoint)
    case whereis(macula_nat_cache) of
        undefined ->
            lookup_peer_from_dht(NodeId);
        _Pid ->
            case macula_nat_cache:get_from_dht(NodeId) of
                {ok, Profile} ->
                    %% Extract endpoint from profile if available
                    case maps:get(endpoint, Profile, undefined) of
                        undefined ->
                            build_endpoint_from_profile(Profile);
                        Endpoint when is_binary(Endpoint) ->
                            {ok, Endpoint}
                    end;
                not_found ->
                    lookup_peer_from_dht(NodeId);
                {error, _} ->
                    lookup_peer_from_dht(NodeId)
            end
    end.

%% @private Build endpoint from NAT profile reflexive address.
-spec build_endpoint_from_profile(map()) -> {ok, binary()} | {error, not_found}.
build_endpoint_from_profile(Profile) ->
    case maps:get(reflexive_address, Profile, undefined) of
        {IP, Port} when is_tuple(IP), is_integer(Port) ->
            IPStr = inet:ntoa(IP),
            Endpoint = iolist_to_binary([IPStr, ":", integer_to_list(Port)]),
            {ok, Endpoint};
        _ ->
            {error, not_found}
    end.

%% @private Look up peer endpoint from DHT.
-spec lookup_peer_from_dht(binary()) -> {ok, binary()} | {error, not_found}.
lookup_peer_from_dht(NodeId) ->
    case whereis(macula_routing_server) of
        undefined ->
            {error, not_found};
        Pid ->
            Key = crypto:hash(sha256, <<"peer.endpoint.", NodeId/binary>>),
            case macula_routing_server:find_value(Pid, Key, 20) of
                {ok, #{<<"host">> := Host, <<"port">> := Port}} ->
                    Endpoint = iolist_to_binary([Host, ":", integer_to_list(Port)]),
                    {ok, Endpoint};
                _ ->
                    {error, not_found}
            end
    end.

safe_store_coordinate(Pid, PeerId, Key, Message) ->
    Result = (catch macula_routing_server:store(Pid, Key, Message)),
    log_coordinate_result(Result, PeerId).

log_coordinate_result({'EXIT', Reason}, PeerId) ->
    ?LOG_WARNING("Failed to store PUNCH_COORDINATE for ~s: ~p", [binary:encode_hex(PeerId), Reason]),
    ok;
log_coordinate_result(_, PeerId) ->
    ?LOG_DEBUG("Stored PUNCH_COORDINATE for ~s in DHT", [binary:encode_hex(PeerId)]),
    ok.

%% @private
%% @doc Generate DHT key for punch coordination.
-spec punch_coordination_key(binary(), binary()) -> binary().
punch_coordination_key(PeerId, SessionId) ->
    crypto:hash(sha256, <<"punch.coord.", PeerId/binary, ".", SessionId/binary>>).

%%%===================================================================
%%% Internal functions - Result Processing
%%%===================================================================

%% @private
%% @doc Process punch result from a peer.
-spec process_punch_result(punch_session(), binary(), success | failure, #state{}) -> #state{}.
process_punch_result(#{session_id := SessionId} = Session, ReporterId, success, State) ->
    ?LOG_INFO("Hole punch SUCCESS for session ~s (reporter: ~s)", [SessionId, ReporterId]),

    NewSession = Session#{
        state => completed,
        result => success
    },
    update_session(NewSession, State);

process_punch_result(#{session_id := SessionId,
                       attempts := Attempts} = Session, ReporterId, failure, State)
  when Attempts < State#state.max_attempts ->
    ?LOG_DEBUG("Hole punch attempt ~p failed for ~s (reporter: ~s), retrying",
               [Attempts + 1, SessionId, ReporterId]),

    %% Retry with incremented attempt count
    NewSession = Session#{
        state => coordinating,
        attempts => Attempts + 1
    },
    NewState = update_session(NewSession, State),
    do_coordinate_punch(NewSession),
    NewState;

process_punch_result(#{session_id := SessionId} = Session, ReporterId, failure, State) ->
    ?LOG_INFO("Hole punch FAILED for session ~s after max attempts (reporter: ~s)",
              [SessionId, ReporterId]),

    NewSession = Session#{
        state => failed,
        result => unreachable
    },
    update_session(NewSession, State).

%% @private
%% @doc Handle punch timeout.
-spec handle_punch_timeout(punch_session(), #state{}) -> #state{}.
handle_punch_timeout(#{session_id := SessionId, attempts := Attempts} = Session, State)
  when Attempts < State#state.max_attempts ->
    ?LOG_DEBUG("Hole punch timeout for ~s, attempt ~p, retrying", [SessionId, Attempts + 1]),

    NewSession = Session#{
        state => coordinating,
        attempts => Attempts + 1
    },
    NewState = update_session(NewSession, State),
    do_coordinate_punch(NewSession),
    NewState;

handle_punch_timeout(#{session_id := SessionId} = Session, State) ->
    ?LOG_INFO("Hole punch TIMEOUT for session ~s after max attempts", [SessionId]),

    NewSession = Session#{
        state => failed,
        result => timeout
    },
    update_session(NewSession, State).

%%%===================================================================
%%% Internal functions - Session Management
%%%===================================================================

%% @private
%% @doc Generate unique session ID.
-spec generate_session_id() -> binary().
generate_session_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

%% @private
%% @doc Add a new session to state.
-spec add_session(punch_session(), #state{}) -> #state{}.
add_session(#{session_id := SessionId,
              initiator_id := InitiatorId,
              target_id := TargetId} = Session, State) ->
    #state{sessions = Sessions, pending_by_peer = PendingByPeer} = State,

    NewSessions = Sessions#{SessionId => Session},
    NewPendingByPeer = add_pending_for_peer(InitiatorId, SessionId,
                           add_pending_for_peer(TargetId, SessionId, PendingByPeer)),

    State#state{sessions = NewSessions, pending_by_peer = NewPendingByPeer}.

%% @private
%% @doc Update existing session in state.
-spec update_session(punch_session(), #state{}) -> #state{}.
update_session(#{session_id := SessionId} = Session, State) ->
    #state{sessions = Sessions} = State,
    State#state{sessions = Sessions#{SessionId => Session}}.

%% @private
%% @doc Add session ID to peer's pending list.
-spec add_pending_for_peer(binary(), binary(), #{binary() => [binary()]}) ->
    #{binary() => [binary()]}.
add_pending_for_peer(PeerId, SessionId, PendingByPeer) ->
    Existing = maps:get(PeerId, PendingByPeer, []),
    PendingByPeer#{PeerId => [SessionId | Existing]}.

%% @private
%% @doc Cleanup stale sessions (older than 5 minutes).
-spec cleanup_stale_sessions(#state{}) -> #state{}.
cleanup_stale_sessions(#state{sessions = Sessions} = State) ->
    Now = erlang:system_time(second),
    MaxAge = 300,  % 5 minutes
    {Kept, Removed} = partition_sessions(Sessions, Now, MaxAge),
    log_cleanup_result(Removed),
    State#state{sessions = Kept}.

partition_sessions(Sessions, Now, MaxAge) ->
    maps:fold(
        fun(SessionId, Session, Acc) ->
            classify_session(SessionId, Session, Now, MaxAge, Acc)
        end,
        {#{}, []},
        Sessions
    ).

classify_session(SessionId, #{created_at := CreatedAt, state := SessionState} = Session, Now, MaxAge, {Kept, Removed}) ->
    ShouldRemove = should_remove_session(Now - CreatedAt, MaxAge, SessionState),
    accumulate_session(ShouldRemove, SessionId, Session, Kept, Removed).

should_remove_session(Age, MaxAge, _SessionState) when Age > MaxAge ->
    true;
should_remove_session(_Age, _MaxAge, completed) ->
    true;
should_remove_session(_Age, _MaxAge, failed) ->
    true;
should_remove_session(_Age, _MaxAge, _SessionState) ->
    false.

accumulate_session(true, SessionId, _Session, Kept, Removed) ->
    {Kept, [SessionId | Removed]};
accumulate_session(false, SessionId, Session, Kept, Removed) ->
    {Kept#{SessionId => Session}, Removed}.

log_cleanup_result([]) ->
    ok;
log_cleanup_result(Removed) ->
    ?LOG_DEBUG("Cleaned up ~p stale punch sessions", [length(Removed)]).

%% @private
%% @doc Schedule periodic cleanup.
-spec schedule_cleanup() -> reference().
schedule_cleanup() ->
    erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup).
