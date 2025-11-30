%%%-------------------------------------------------------------------
%%% @doc Macula Ping/Pong - Direct P2P Async RPC Demo
%%%
%%% A peer-to-peer communication module that demonstrates bidirectional
%%% messaging across NAT boundaries using Macula's async RPC with
%%% direct P2P delivery (NATS-style request/reply).
%%%
%%% Each node:
%%% - Registers local "ping.handler" RPC handler
%%% - Periodically sends PING RPCs to random peers (direct P2P)
%%% - Receives PONGs via callback
%%% - Measures Round-Trip Time (RTT) for each exchange
%%% - Tracks statistics per peer and per NAT type
%%%
%%% Architecture:
%%% - Bootstrap: DHT only (no pub/sub routing)
%%% - Communication: Direct P2P via QUIC
%%% - Discovery: DHT for service registration
%%%
%%% Flow:
%%% 1. fc01 registers local "ping.handler" (handles incoming PINGs)
%%% 2. fc01 calls macula_rpc_handler:request_to/5 to ping rc05 directly
%%% 3. Request goes directly to rc05 via QUIC (NAT-aware, no DHT lookup)
%%% 4. rc05's "ping.handler" executes, returns PONG
%%% 5. fc01 receives PONG via callback, measures RTT
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_ping_pong).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([ping/1, get_stats/0, get_peer_stats/1, reset_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERVAL, 5000).      % 5 seconds between PINGs
-define(DEFAULT_TIMEOUT, 3000).       % 3 seconds PING timeout
-define(PING_PROCEDURE, <<"ping.handler">>).

-record(peer_stats, {
    pings_sent = 0 :: non_neg_integer(),
    pongs_received = 0 :: non_neg_integer(),
    timeouts = 0 :: non_neg_integer(),
    min_rtt_ms :: non_neg_integer() | undefined,
    max_rtt_ms :: non_neg_integer() | undefined,
    total_rtt_ms = 0 :: non_neg_integer(),
    last_seen :: integer() | undefined,
    nat_type :: binary() | undefined
}).

-record(pending_ping, {
    target_peer :: binary(),
    sent_at :: integer(),
    from :: term() | undefined  % For manual ping calls
}).

-record(state, {
    node_id :: binary(),
    nat_type :: binary(),
    known_peers = [] :: [binary()],
    pending_pings = #{} :: #{binary() => #pending_ping{}},
    peer_stats = #{} :: #{binary() => #peer_stats{}},
    global_stats :: #peer_stats{},
    interval :: pos_integer(),
    timeout :: pos_integer(),
    timer_ref :: reference() | undefined,
    setup_complete = false :: boolean()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start with default settings
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start with options
%% Options:
%%   - interval: milliseconds between PINGs (default: 5000)
%%   - timeout: milliseconds to wait for PONG (default: 3000)
%%   - node_id: custom node identifier (default: NODE_ID env var)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Send a PING to a specific peer
-spec ping(binary()) -> {ok, integer()} | {error, term()}.
ping(PeerId) ->
    gen_server:call(?SERVER, {ping, PeerId}, 10000).

%% @doc Get all statistics
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% @doc Get statistics for a specific peer
-spec get_peer_stats(binary()) -> map() | not_found.
get_peer_stats(PeerId) ->
    gen_server:call(?SERVER, {get_peer_stats, PeerId}).

%% @doc Reset all statistics
-spec reset_stats() -> ok.
reset_stats() ->
    gen_server:cast(?SERVER, reset_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    NodeId = get_node_id(Opts),
    NatType = get_nat_type(NodeId),
    Interval = maps:get(interval, Opts, ?DEFAULT_INTERVAL),
    Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),

    macula_console:info(NodeId, iolist_to_binary([<<"Starting up (NAT: ">>, NatType, <<")">>])),

    %% Schedule setup after gen_server is fully started
    self() ! setup,

    {ok, #state{
        node_id = NodeId,
        nat_type = NatType,
        interval = Interval,
        timeout = Timeout,
        global_stats = #peer_stats{}
    }}.

handle_call({ping, PeerId}, From, State) ->
    %% Manual ping request - async response
    NewState = do_send_ping(PeerId, From, State),
    {noreply, NewState};

handle_call(get_stats, _From, State) ->
    Stats = format_all_stats(State),
    {reply, Stats, State};

handle_call({get_peer_stats, PeerId}, _From, State) ->
    case maps:get(PeerId, State#state.peer_stats, not_found) of
        not_found ->
            {reply, not_found, State};
        PeerStats ->
            {reply, format_peer_stats(PeerId, PeerStats), State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(reset_stats, State) ->
    macula_console:info(State#state.node_id, <<"Resetting statistics">>),
    {noreply, State#state{
        peer_stats = #{},
        global_stats = #peer_stats{}
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(setup, State) ->
    NewState = setup_ping_pong(State),
    {noreply, NewState};

handle_info(ping_tick, State) ->
    NewState = do_ping_tick(State),
    %% Schedule next tick
    TimerRef = erlang:send_after(State#state.interval, self(), ping_tick),
    {noreply, NewState#state{timer_ref = TimerRef}};

%% Handle async RPC reply via process message
handle_info({rpc_reply, RequestId, Result}, State) ->
    NewState = handle_rpc_reply(RequestId, Result, State),
    {noreply, NewState};

handle_info({peer_discovered, PeerId}, State) when PeerId =/= State#state.node_id ->
    %% Add to known peers if not already present
    KnownPeers = State#state.known_peers,
    NewKnownPeers = case lists:member(PeerId, KnownPeers) of
        true -> KnownPeers;
        false ->
            PeerNat = get_nat_type(PeerId),
            macula_console:node_connected(PeerId, PeerNat),
            [PeerId | KnownPeers]
    end,
    {noreply, State#state{known_peers = NewKnownPeers}};

handle_info({peer_discovered, _SelfId}, State) ->
    %% Ignore self-discovery
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    macula_console:warning(State#state.node_id, <<"Shutting down">>),
    print_final_stats(State),
    ok.

%%%===================================================================
%%% Internal functions - Setup
%%%===================================================================

%% @private Get node identifier
get_node_id(Opts) ->
    case maps:get(node_id, Opts, undefined) of
        undefined ->
            case os:getenv("NODE_ID") of
                false ->
                    {ok, Hostname} = inet:gethostname(),
                    list_to_binary(Hostname);
                NodeId ->
                    list_to_binary(NodeId)
            end;
        NodeId when is_binary(NodeId) ->
            NodeId;
        NodeId when is_list(NodeId) ->
            list_to_binary(NodeId)
    end.

%% @private Determine NAT type from node ID prefix
get_nat_type(NodeId) ->
    case NodeId of
        <<"fc", _/binary>> -> <<"full_cone">>;
        <<"rc", _/binary>> -> <<"restricted">>;
        <<"sy", _/binary>> -> <<"symmetric">>;
        <<"bootstrap", _/binary>> -> <<"public">>;
        _ -> <<"unknown">>
    end.

%% @private Setup RPC handler and start ping loop
setup_ping_pong(State) ->
    NodeId = State#state.node_id,
    NatType = State#state.nat_type,
    macula_console:info(NodeId, <<"Setting up async RPC handler...">>),

    %% Wait for mesh to stabilize
    timer:sleep(3000),

    %% Register local PING handler
    case register_ping_handler(NodeId, NatType) of
        ok ->
            macula_console:success(NodeId, iolist_to_binary([<<"Registered ">>, ?PING_PROCEDURE, <<" handler">>]));
        {error, Reason} ->
            macula_console:error(NodeId, iolist_to_binary([<<"Failed to register handler: ">>, io_lib:format("~p", [Reason])]))
    end,

    %% Discover initial peers
    KnownPeers = discover_peers(State),
    macula_console:info(NodeId, iolist_to_binary([<<"Discovered ">>, integer_to_binary(length(KnownPeers)), <<" initial peers">>])),

    %% Start ping loop
    macula_console:info(NodeId, iolist_to_binary([<<"Starting PING loop (interval: ">>, integer_to_binary(State#state.interval), <<"ms, timeout: ">>, integer_to_binary(State#state.timeout), <<"ms)">>])),
    TimerRef = erlang:send_after(State#state.interval, self(), ping_tick),

    State#state{
        timer_ref = TimerRef,
        known_peers = KnownPeers,
        setup_complete = true
    }.

%% @private Register local handler for incoming PINGs
register_ping_handler(NodeId, NatType) ->
    %% Handler receives PING args and returns PONG
    Handler = fun(Args) ->
        FromNode = maps:get(<<"from">>, Args, <<"unknown">>),
        SentAt = maps:get(<<"sent_at">>, Args, 0),

        ?LOG_DEBUG("PING handler invoked from ~s", [FromNode]),

        %% Return PONG response
        {ok, #{
            <<"type">> => <<"pong">>,
            <<"from">> => NodeId,
            <<"nat_type">> => NatType,
            <<"sent_at">> => SentAt,
            <<"received_at">> => erlang:system_time(millisecond),
            <<"responded_at">> => erlang:system_time(millisecond)
        }}
    end,

    %% Register with local RPC handler
    case get_rpc_handler() of
        {ok, RpcPid} ->
            macula_rpc_handler:register_local_procedure(RpcPid, ?PING_PROCEDURE, Handler),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Discover available peers.
%% Uses PEER_COUNT env var to determine the peer setup size:
%%   - PEER_COUNT=50 (default): fc01-17, rc01-17, sy01-16 (50 total)
%%   - PEER_COUNT=3: fc01, rc01, sy01 (simple test setup)
%%   - PEER_COUNT=N: proportionally scales the peer list
discover_peers(State) ->
    NodeId = State#state.node_id,
    PeerCount = get_peer_count(),
    AllPeers = generate_peer_list(PeerCount),
    lists:filter(fun(P) -> P =/= NodeId end, AllPeers).

%% @private Get peer count from environment variable
get_peer_count() ->
    case os:getenv("PEER_COUNT") of
        false -> 50;  % Default to 50-peer setup
        CountStr ->
            case catch list_to_integer(CountStr) of
                N when is_integer(N), N > 0 -> N;
                _ -> 50
            end
    end.

%% @private Generate peer list based on count.
%% For standard counts, maintains 17:17:16 ratio (fc:rc:sy).
generate_peer_list(Count) when Count =< 3 ->
    %% Simple setup: one of each type
    [<<"fc01">>, <<"rc01">>, <<"sy01">>];
generate_peer_list(50) ->
    %% Standard 50-peer setup
    FullCone = [iolist_to_binary([<<"fc">>, pad_num(N)]) || N <- lists:seq(1, 17)],
    Restricted = [iolist_to_binary([<<"rc">>, pad_num(N)]) || N <- lists:seq(1, 17)],
    Symmetric = [iolist_to_binary([<<"sy">>, pad_num(N)]) || N <- lists:seq(1, 16)],
    FullCone ++ Restricted ++ Symmetric;
generate_peer_list(Count) ->
    %% Scale proportionally: 1/3 each type
    PerType = max(1, Count div 3),
    FullCone = [iolist_to_binary([<<"fc">>, pad_num(N)]) || N <- lists:seq(1, PerType)],
    Restricted = [iolist_to_binary([<<"rc">>, pad_num(N)]) || N <- lists:seq(1, PerType)],
    Symmetric = [iolist_to_binary([<<"sy">>, pad_num(N)]) || N <- lists:seq(1, PerType)],
    FullCone ++ Restricted ++ Symmetric.

%% @private Pad number to 2 digits
pad_num(N) when N < 10 ->
    iolist_to_binary([<<"0">>, integer_to_binary(N)]);
pad_num(N) ->
    integer_to_binary(N).

%% @private Compute endpoint from peer name.
%% In Docker, containers use hostname:4433 format.
-spec compute_peer_endpoint(binary()) -> binary().
compute_peer_endpoint(PeerName) when is_binary(PeerName) ->
    iolist_to_binary([PeerName, <<":4433">>]).

%%%===================================================================
%%% Internal functions - PING/PONG
%%%===================================================================

%% @private Handle periodic ping tick
do_ping_tick(State) ->
    case select_random_peer(State) of
        {ok, TargetPeer} ->
            do_send_ping(TargetPeer, undefined, State);
        {error, no_peers} ->
            macula_console:warning(State#state.node_id, <<"No peers available to ping">>),
            State
    end.

%% @private Select a random peer to ping
select_random_peer(#state{known_peers = []}) ->
    {error, no_peers};
select_random_peer(#state{known_peers = Peers}) ->
    Index = rand:uniform(length(Peers)),
    {ok, lists:nth(Index, Peers)}.

%% @private Send a PING to target peer using async RPC
do_send_ping(TargetPeer, From, State) ->
    NodeId = State#state.node_id,
    NatType = State#state.nat_type,
    SentAt = erlang:system_time(millisecond),

    TargetNat = get_nat_type(TargetPeer),
    macula_console:ping(NodeId, TargetPeer, NatType, TargetNat),

    %% Build PING args
    PingArgs = #{
        <<"type">> => <<"ping">>,
        <<"from">> => NodeId,
        <<"nat_type">> => NatType,
        <<"sent_at">> => SentAt
    },

    %% Send async RPC request directly to target peer (bypasses DHT discovery)
    %% Compute endpoint from peer name for direct P2P (Docker containers use hostname:4433)
    Endpoint = compute_peer_endpoint(TargetPeer),
    case get_rpc_handler() of
        {ok, RpcPid} ->
            Opts = #{timeout => State#state.timeout, endpoint => Endpoint},
            case macula_rpc_handler:request_to(RpcPid, TargetPeer, ?PING_PROCEDURE, PingArgs, Opts) of
                {ok, RequestId} ->
                    %% Track pending PING
                    PendingInfo = #pending_ping{
                        target_peer = TargetPeer,
                        sent_at = SentAt,
                        from = From
                    },
                    NewPending = maps:put(RequestId, PendingInfo, State#state.pending_pings),

                    %% Update peer stats (ping sent)
                    NewPeerStats = update_ping_sent(TargetPeer, State#state.peer_stats),
                    NewGlobal = increment_sent(State#state.global_stats),

                    State#state{
                        pending_pings = NewPending,
                        peer_stats = NewPeerStats,
                        global_stats = NewGlobal
                    };
                {error, Reason} ->
                    macula_console:error(NodeId, iolist_to_binary([<<"RPC request failed: ">>, io_lib:format("~p", [Reason])])),
                    maybe_reply(From, {error, Reason}),
                    State
            end;
        {error, Reason} ->
            macula_console:error(NodeId, iolist_to_binary([<<"No RPC handler: ">>, io_lib:format("~p", [Reason])])),
            maybe_reply(From, {error, Reason}),
            State
    end.

%% @private Handle async RPC reply
handle_rpc_reply(RequestId, Result, State) ->
    case maps:take(RequestId, State#state.pending_pings) of
        {#pending_ping{target_peer = TargetPeer, sent_at = SentAt, from = From}, NewPending} ->
            case Result of
                {ok, PongMsg} ->
                    %% Calculate RTT
                    ReceivedAt = erlang:system_time(millisecond),
                    RTT = ReceivedAt - SentAt,

                    %% Extract NAT type from response
                    PeerNat = maps:get(<<"nat_type">>, PongMsg, <<"unknown">>),

                    macula_console:pong(State#state.node_id, TargetPeer, RTT, PeerNat),

                    %% Update statistics
                    NewPeerStats = update_pong_received(TargetPeer, RTT, PeerNat, State#state.peer_stats),
                    NewGlobal = update_rtt(RTT, State#state.global_stats),

                    %% Reply to caller if manual ping
                    maybe_reply(From, {ok, RTT}),

                    State#state{
                        pending_pings = NewPending,
                        peer_stats = NewPeerStats,
                        global_stats = NewGlobal
                    };

                {error, timeout} ->
                    TargetNat = get_nat_type(TargetPeer),
                    macula_console:timeout(State#state.node_id, TargetPeer, TargetNat),

                    %% Update statistics
                    NewPeerStats = update_timeout(TargetPeer, State#state.peer_stats),
                    NewGlobal = increment_timeout(State#state.global_stats),

                    maybe_reply(From, {error, timeout}),

                    State#state{
                        pending_pings = NewPending,
                        peer_stats = NewPeerStats,
                        global_stats = NewGlobal
                    };

                {error, Reason} ->
                    macula_console:error(State#state.node_id, iolist_to_binary([<<"PING error -> ">>, TargetPeer, <<": ">>, io_lib:format("~p", [Reason])])),
                    maybe_reply(From, {error, Reason}),
                    State#state{pending_pings = NewPending}
            end;
        error ->
            %% Unknown request ID (already handled or never sent)
            State
    end.

%% @private Maybe reply to caller
maybe_reply(undefined, _Result) ->
    ok;
maybe_reply(From, Result) ->
    gen_server:reply(From, Result).

%%%===================================================================
%%% Statistics functions
%%%===================================================================

%% @private Update stats when PING is sent
update_ping_sent(PeerId, PeerStats) ->
    Stats = maps:get(PeerId, PeerStats, #peer_stats{}),
    NewStats = Stats#peer_stats{pings_sent = Stats#peer_stats.pings_sent + 1},
    maps:put(PeerId, NewStats, PeerStats).

%% @private Update stats when PONG is received
update_pong_received(PeerId, RTT, NatType, PeerStats) ->
    Stats = maps:get(PeerId, PeerStats, #peer_stats{}),
    NewStats = Stats#peer_stats{
        pongs_received = Stats#peer_stats.pongs_received + 1,
        min_rtt_ms = min_val(Stats#peer_stats.min_rtt_ms, RTT),
        max_rtt_ms = max_val(Stats#peer_stats.max_rtt_ms, RTT),
        total_rtt_ms = Stats#peer_stats.total_rtt_ms + RTT,
        last_seen = erlang:system_time(millisecond),
        nat_type = NatType
    },
    maps:put(PeerId, NewStats, PeerStats).

%% @private Update stats when timeout occurs
update_timeout(PeerId, PeerStats) ->
    Stats = maps:get(PeerId, PeerStats, #peer_stats{}),
    NewStats = Stats#peer_stats{timeouts = Stats#peer_stats.timeouts + 1},
    maps:put(PeerId, NewStats, PeerStats).

%% @private Update global RTT stats
update_rtt(RTT, Stats) ->
    Stats#peer_stats{
        pongs_received = Stats#peer_stats.pongs_received + 1,
        min_rtt_ms = min_val(Stats#peer_stats.min_rtt_ms, RTT),
        max_rtt_ms = max_val(Stats#peer_stats.max_rtt_ms, RTT),
        total_rtt_ms = Stats#peer_stats.total_rtt_ms + RTT
    }.

%% @private Increment sent counter
increment_sent(Stats) ->
    Stats#peer_stats{pings_sent = Stats#peer_stats.pings_sent + 1}.

%% @private Increment timeout counter
increment_timeout(Stats) ->
    Stats#peer_stats{timeouts = Stats#peer_stats.timeouts + 1}.

%% @private Min value handling undefined
min_val(undefined, V) -> V;
min_val(V1, V2) -> min(V1, V2).

%% @private Max value handling undefined
max_val(undefined, V) -> V;
max_val(V1, V2) -> max(V1, V2).

%%%===================================================================
%%% Formatting functions
%%%===================================================================

%% @private Format all statistics
format_all_stats(State) ->
    GlobalStats = State#state.global_stats,
    PeerStatsMap = State#state.peer_stats,

    %% Group by NAT type
    ByNat = group_by_nat(maps:to_list(PeerStatsMap)),

    #{
        node_id => State#state.node_id,
        nat_type => State#state.nat_type,
        global => #{
            pings_sent => GlobalStats#peer_stats.pings_sent,
            pongs_received => GlobalStats#peer_stats.pongs_received,
            timeouts => GlobalStats#peer_stats.timeouts,
            success_rate => calc_success_rate(GlobalStats),
            min_rtt_ms => GlobalStats#peer_stats.min_rtt_ms,
            max_rtt_ms => GlobalStats#peer_stats.max_rtt_ms,
            avg_rtt_ms => calc_avg_rtt(GlobalStats)
        },
        by_nat_type => ByNat,
        peers_contacted => maps:size(PeerStatsMap),
        known_peers => length(State#state.known_peers),
        pending_pings => maps:size(State#state.pending_pings)
    }.

%% @private Format peer statistics
format_peer_stats(PeerId, Stats) ->
    #{
        peer_id => PeerId,
        nat_type => Stats#peer_stats.nat_type,
        pings_sent => Stats#peer_stats.pings_sent,
        pongs_received => Stats#peer_stats.pongs_received,
        timeouts => Stats#peer_stats.timeouts,
        success_rate => calc_success_rate(Stats),
        min_rtt_ms => Stats#peer_stats.min_rtt_ms,
        max_rtt_ms => Stats#peer_stats.max_rtt_ms,
        avg_rtt_ms => calc_avg_rtt(Stats),
        last_seen => Stats#peer_stats.last_seen
    }.

%% @private Group peer stats by NAT type
group_by_nat(PeerStatsList) ->
    %% First, group and merge all stats as records
    Grouped = lists:foldl(fun({_PeerId, Stats}, Acc) ->
        NatType = case Stats#peer_stats.nat_type of
            undefined -> <<"unknown">>;
            Type -> Type
        end,
        Existing = maps:get(NatType, Acc, #peer_stats{}),
        Merged = merge_stats(Existing, Stats),
        maps:put(NatType, Merged, Acc)
    end, #{}, PeerStatsList),
    %% Then format all the merged records to maps
    maps:map(fun(_NatType, MergedStats) -> format_nat_group(MergedStats) end, Grouped).

%% @private Merge two stats records
merge_stats(A, B) ->
    #peer_stats{
        pings_sent = A#peer_stats.pings_sent + B#peer_stats.pings_sent,
        pongs_received = A#peer_stats.pongs_received + B#peer_stats.pongs_received,
        timeouts = A#peer_stats.timeouts + B#peer_stats.timeouts,
        min_rtt_ms = min_val(A#peer_stats.min_rtt_ms, B#peer_stats.min_rtt_ms),
        max_rtt_ms = max_val(A#peer_stats.max_rtt_ms, B#peer_stats.max_rtt_ms),
        total_rtt_ms = A#peer_stats.total_rtt_ms + B#peer_stats.total_rtt_ms
    }.

%% @private Format NAT group stats
format_nat_group(Stats) ->
    #{
        pings_sent => Stats#peer_stats.pings_sent,
        pongs_received => Stats#peer_stats.pongs_received,
        timeouts => Stats#peer_stats.timeouts,
        success_rate => calc_success_rate(Stats),
        min_rtt_ms => Stats#peer_stats.min_rtt_ms,
        max_rtt_ms => Stats#peer_stats.max_rtt_ms,
        avg_rtt_ms => calc_avg_rtt(Stats)
    }.

%% @private Calculate success rate
calc_success_rate(#peer_stats{pings_sent = 0}) ->
    0.0;
calc_success_rate(#peer_stats{pings_sent = Sent, pongs_received = Received}) ->
    (Received / Sent) * 100.

%% @private Calculate average RTT
calc_avg_rtt(#peer_stats{pongs_received = 0}) ->
    undefined;
calc_avg_rtt(#peer_stats{pongs_received = Count, total_rtt_ms = Total}) ->
    Total / Count.

%% @private Print final statistics on shutdown
print_final_stats(State) ->
    Stats = format_all_stats(State),
    Global = maps:get(global, Stats),
    NodeId = maps:get(node_id, Stats),

    %% Calculate avg_rtt_ms for stats/2 call
    AvgRtt = case maps:get(avg_rtt_ms, Global) of
        undefined -> 0;
        Avg when is_float(Avg) -> round(Avg);
        Avg -> Avg
    end,

    %% Use macula_console:stats/2 for beautiful output
    StatsMap = #{
        pings_sent => maps:get(pings_sent, Global),
        pongs_received => maps:get(pongs_received, Global),
        timeouts => maps:get(timeouts, Global),
        avg_rtt_ms => AvgRtt
    },
    macula_console:stats(NodeId, StatsMap),

    %% Also show NAT type breakdown via info
    maps:foreach(fun(NatType, NatStats) ->
        SuccessRate = maps:get(success_rate, NatStats),
        macula_console:info(NodeId, iolist_to_binary([
            <<"  ">>, NatType, <<": ">>,
            <<"sent=">>, integer_to_binary(maps:get(pings_sent, NatStats)),
            <<", received=">>, integer_to_binary(maps:get(pongs_received, NatStats)),
            <<", rate=">>, io_lib:format("~.1f%", [SuccessRate])
        ]))
    end, maps:get(by_nat_type, Stats)).

%%%===================================================================
%%% Peer handler helpers
%%%===================================================================

%% @private Get the RPC handler from the first available peer system
get_rpc_handler() ->
    case whereis(macula_peers_sup) of
        undefined ->
            {error, no_peers_sup};
        _Pid ->
            case macula_peers_sup:list_peers() of
                [] ->
                    {error, no_peers};
                [PeerSystemPid | _] ->
                    get_rpc_from_supervisor(PeerSystemPid)
            end
    end.

%% @private Extract RPC handler PID from peer system supervisor
get_rpc_from_supervisor(SupPid) ->
    case supervisor:which_children(SupPid) of
        Children when is_list(Children) ->
            case find_child_pid(Children, rpc_handler) of
                undefined -> {error, no_rpc_handler};
                RpcPid -> {ok, RpcPid}
            end;
        _ ->
            {error, no_children}
    end.

%% @private Find a child PID by child ID
find_child_pid(Children, ChildId) ->
    case lists:keyfind(ChildId, 1, Children) of
        {ChildId, Pid, _Type, _Modules} when is_pid(Pid) -> Pid;
        _ -> undefined
    end.
