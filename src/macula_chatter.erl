%%%-------------------------------------------------------------------
%%% @doc Macula Chatter - P2P PubSub Demo for NAT Traversal Testing
%%%
%%% A pub/sub chat application that demonstrates broadcast messaging
%%% across NAT boundaries using Macula's pub/sub capabilities.
%%%
%%% Each chatter node:
%%% - Subscribes to "chat.room.global" topic
%%% - Periodically broadcasts numbered messages to all peers
%%% - Tracks delivery metrics per peer (by NAT type)
%%% - Reports delivery rates at shutdown
%%%
%%% PubSub Delivery Metrics:
%%% - Each broadcast includes a sequence number
%%% - Receivers track which sequence numbers they've seen per sender
%%% - Gaps in sequence numbers indicate missed messages
%%% - Delivery rate = received / expected (based on max seq seen)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_chatter).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([send_message/1, send_direct/2, get_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(CHAT_TOPIC, <<"chat.room.global">>).
-define(CHAT_RPC, <<"chat.receive">>).
-define(DEFAULT_INTERVAL, 5000).  % 5 seconds between messages

%% Per-peer tracking for delivery metrics
-record(peer_tracker, {
    nat_type :: binary() | undefined,
    max_seq = 0 :: non_neg_integer(),        % Highest sequence number seen
    received_count = 0 :: non_neg_integer(), % Total messages received
    first_seen :: non_neg_integer() | undefined,
    last_seen :: non_neg_integer() | undefined
}).

-record(state, {
    node_id :: binary(),
    nat_type :: binary() | undefined,
    messages_sent = 0 :: non_neg_integer(),
    messages_received = 0 :: non_neg_integer(),
    peer_trackers = #{} :: #{binary() => #peer_tracker{}},  % peer_id => tracker
    interval :: pos_integer(),
    timer_ref :: reference() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the chatter with default settings
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the chatter with options
%% Options:
%%   - interval: milliseconds between broadcasts (default: 5000)
%%   - node_id: custom node identifier (default: hostname)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Send a message to all peers via pubsub
-spec send_message(binary()) -> ok.
send_message(Message) ->
    gen_server:cast(?SERVER, {send_message, Message}).

%% @doc Send a direct message to a specific peer via RPC
-spec send_direct(binary(), binary()) -> ok | {error, term()}.
send_direct(PeerId, Message) ->
    gen_server:call(?SERVER, {send_direct, PeerId, Message}).

%% @doc Get statistics about messages sent/received
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    NodeId = get_node_id(Opts),
    Interval = maps:get(interval, Opts, ?DEFAULT_INTERVAL),
    NatType = get_nat_type(),

    macula_console:info(NodeId, <<"PubSub chatter starting...">>),

    %% Schedule setup after gen_server is fully started
    self() ! setup,

    {ok, #state{
        node_id = NodeId,
        nat_type = NatType,
        interval = Interval
    }}.

handle_call(get_stats, _From, State) ->
    Stats = #{
        node_id => State#state.node_id,
        nat_type => State#state.nat_type,
        messages_sent => State#state.messages_sent,
        messages_received => State#state.messages_received,
        peer_stats => format_peer_stats(State#state.peer_trackers),
        delivery_by_nat => calc_delivery_by_nat(State#state.peer_trackers),
        uptime_ms => erlang:system_time(millisecond)
    },
    {reply, Stats, State};

handle_call({send_direct, PeerId, Message}, _From, State) ->
    Result = do_send_direct(PeerId, Message, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({send_message, Message}, State) ->
    NewState = do_broadcast(Message, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(setup, State) ->
    NewState = setup_chatter(State),
    {noreply, NewState};

handle_info(broadcast_tick, State) ->
    %% Sequence number for delivery tracking
    SeqNum = State#state.messages_sent + 1,

    NewState = do_broadcast(SeqNum, State),

    %% Schedule next broadcast
    TimerRef = erlang:send_after(State#state.interval, self(), broadcast_tick),
    {noreply, NewState#state{timer_ref = TimerRef}};

handle_info({pubsub_message, FromNode, SeqNum, SenderNat}, State) ->
    %% Update peer tracker with sequence number for delivery metrics
    Now = erlang:system_time(millisecond),
    Trackers = State#state.peer_trackers,
    Tracker = maps:get(FromNode, Trackers, #peer_tracker{}),

    UpdatedTracker = Tracker#peer_tracker{
        nat_type = SenderNat,
        max_seq = max(SeqNum, Tracker#peer_tracker.max_seq),
        received_count = Tracker#peer_tracker.received_count + 1,
        first_seen = case Tracker#peer_tracker.first_seen of
            undefined -> Now;
            T -> T
        end,
        last_seen = Now
    },

    %% Log with colored output
    DeliveryRate = calc_peer_delivery_rate(UpdatedTracker),
    macula_console:pubsub_recv(State#state.node_id, FromNode, SeqNum, SenderNat, DeliveryRate),

    NewState = State#state{
        messages_received = State#state.messages_received + 1,
        peer_trackers = maps:put(FromNode, UpdatedTracker, Trackers)
    },
    {noreply, NewState};

%% Legacy handler for RPC-based chat messages (backwards compatibility)
handle_info({chat_message, FromNode, _Message}, State) ->
    Trackers = State#state.peer_trackers,
    Tracker = maps:get(FromNode, Trackers, #peer_tracker{}),
    Now = erlang:system_time(millisecond),

    UpdatedTracker = Tracker#peer_tracker{
        received_count = Tracker#peer_tracker.received_count + 1,
        first_seen = case Tracker#peer_tracker.first_seen of
            undefined -> Now;
            T -> T
        end,
        last_seen = Now
    },

    NewState = State#state{
        messages_received = State#state.messages_received + 1,
        peer_trackers = maps:put(FromNode, UpdatedTracker, Trackers)
    },
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    NodeId = State#state.node_id,
    macula_console:info(NodeId, <<"Shutting down, printing delivery stats...">>),
    print_delivery_stats(State),
    ok.

%%%===================================================================
%%% Internal functions
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

%% @private Setup subscriptions and RPC handlers
setup_chatter(State) ->
    NodeId = State#state.node_id,
    io:format("[Chatter ~s] Setting up pub/sub and RPC handlers...~n", [NodeId]),

    %% Wait a bit for the mesh to stabilize
    timer:sleep(2000),

    %% Try to subscribe to chat topic
    case setup_pubsub(NodeId) of
        ok ->
            io:format("[Chatter ~s] Subscribed to ~s~n", [NodeId, ?CHAT_TOPIC]);
        {error, SubReason} ->
            io:format("[Chatter ~s] Failed to subscribe: ~p~n", [NodeId, SubReason])
    end,

    %% Try to register RPC handler
    case setup_rpc(NodeId) of
        ok ->
            io:format("[Chatter ~s] Registered RPC handler ~s~n", [NodeId, ?CHAT_RPC]);
        {error, RpcReason} ->
            io:format("[Chatter ~s] Failed to register RPC: ~p~n", [NodeId, RpcReason])
    end,

    %% Start broadcasting
    io:format("[Chatter ~s] Starting broadcasts every ~pms~n", [NodeId, State#state.interval]),
    TimerRef = erlang:send_after(State#state.interval, self(), broadcast_tick),

    State#state{timer_ref = TimerRef}.

%% @private Setup pub/sub subscription
setup_pubsub(NodeId) ->
    Self = self(),
    %% Callback receives a single map: #{topic, matched_pattern, payload}
    Handler = fun(#{payload := Payload}) ->
        case decode_chat_message(Payload) of
            {ok, FromNode, SeqNum, SenderNat} when FromNode =/= NodeId ->
                Self ! {pubsub_message, FromNode, SeqNum, SenderNat};
            {ok, _FromNode, _SeqNum, _SenderNat} ->
                %% Ignore our own messages
                ok;
            {error, _Reason} ->
                ok
        end
    end,

    %% Find the bootstrap connection handlers
    case get_peer_handlers() of
        {ok, #{pubsub := PubSubPid}} ->
            try
                macula_pubsub_handler:subscribe(PubSubPid, ?CHAT_TOPIC, Handler),
                ok
            catch
                _:Reason ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Setup RPC handler for direct messages
setup_rpc(NodeId) ->
    Self = self(),
    Handler = fun(Args) ->
        FromNode = maps:get(<<"from">>, Args, <<"unknown">>),
        Message = maps:get(<<"message">>, Args, <<"">>),

        %% Only process if not from ourselves
        case FromNode of
            NodeId -> ok;
            _ -> Self ! {chat_message, FromNode, Message}
        end,

        {ok, #{<<"status">> => <<"delivered">>, <<"to">> => NodeId}}
    end,

    %% Find the bootstrap connection to register with
    case get_peer_handlers() of
        {ok, #{rpc := RpcPid}} ->
            try
                macula_rpc_handler:register_local_procedure(RpcPid, ?CHAT_RPC, Handler),
                ok
            catch
                _:Reason ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Broadcast a message to all peers
do_broadcast(SeqNum, State) ->
    NodeId = State#state.node_id,
    NatType = State#state.nat_type,
    Payload = encode_chat_message(NodeId, SeqNum, NatType),

    case get_peer_handlers() of
        {ok, #{pubsub := PubSubPid}} ->
            try
                macula_pubsub_handler:publish(PubSubPid, ?CHAT_TOPIC, Payload, #{}),
                macula_console:pubsub_send(NodeId, SeqNum, NatType),
                State#state{messages_sent = SeqNum}
            catch
                _:Reason ->
                    macula_console:error(NodeId, iolist_to_binary([
                        <<"Broadcast FAILED: ">>, io_lib:format("~p", [Reason])
                    ])),
                    State
            end;
        {error, Reason} ->
            macula_console:warning(NodeId, iolist_to_binary([
                <<"No peer connection: ">>, io_lib:format("~p", [Reason])
            ])),
            State
    end.

%% @private Send a direct message to a specific peer
do_send_direct(_PeerId, Message, State) ->
    NodeId = State#state.node_id,
    Args = #{
        <<"from">> => NodeId,
        <<"message">> => Message
    },

    case get_peer_handlers() of
        {ok, #{rpc := RpcPid}} ->
            try
                macula_rpc_handler:call(RpcPid, ?CHAT_RPC, Args)
            catch
                _:Reason ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Get the first available peer system and extract handler PIDs
%% Returns {ok, #{pubsub => Pid, rpc => Pid, connection => Pid}} or {error, Reason}
get_peer_handlers() ->
    case whereis(macula_peers_sup) of
        undefined ->
            {error, no_peers_sup};
        _Pid ->
            case macula_peers_sup:list_peers() of
                [] ->
                    {error, no_peers};
                [PeerSystemPid | _] ->
                    %% Get child PIDs from the peer system supervisor
                    get_handlers_from_supervisor(PeerSystemPid)
            end
    end.

%% @private Extract handler PIDs from peer system supervisor
get_handlers_from_supervisor(SupPid) ->
    case supervisor:which_children(SupPid) of
        Children when is_list(Children) ->
            PubSubPid = find_child_pid(Children, pubsub_handler),
            RpcPid = find_child_pid(Children, rpc_handler),
            ConnPid = find_child_pid(Children, connection_manager),
            case {PubSubPid, RpcPid, ConnPid} of
                {undefined, _, _} -> {error, no_pubsub_handler};
                {_, undefined, _} -> {error, no_rpc_handler};
                {_, _, undefined} -> {error, no_connection_manager};
                _ -> {ok, #{pubsub => PubSubPid, rpc => RpcPid, connection => ConnPid}}
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

%% @private Encode a chat message for transmission
encode_chat_message(FromNode, SeqNum, NatType) ->
    msgpack:pack(#{
        <<"type">> => <<"pubsub_chat">>,
        <<"from">> => FromNode,
        <<"seq">> => SeqNum,
        <<"nat_type">> => NatType,
        <<"timestamp">> => erlang:system_time(millisecond)
    }).

%% @private Decode a received chat message
decode_chat_message(Payload) ->
    case msgpack:unpack(Payload) of
        {ok, #{<<"type">> := <<"pubsub_chat">>, <<"from">> := From,
               <<"seq">> := Seq, <<"nat_type">> := Nat}} ->
            {ok, From, Seq, Nat};
        %% Backwards compatibility with old format
        {ok, #{<<"type">> := <<"chat">>, <<"from">> := From}} ->
            {ok, From, 0, undefined};
        {ok, _Other} ->
            {error, invalid_format};
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% Delivery Metrics
%%%===================================================================

%% @private Get NAT type from environment or cache
get_nat_type() ->
    case os:getenv("NAT_TYPE") of
        false ->
            case whereis(macula_nat_cache) of
                undefined -> undefined;
                _Pid ->
                    case macula_nat_cache:get_local() of
                        {ok, #{mapping_policy := M, filtering_policy := F}} ->
                            classify_nat_type(M, F);
                        _ -> undefined
                    end
            end;
        Type ->
            list_to_binary(Type)
    end.

%% @private Classify NAT type from policies
classify_nat_type(endpoint_independent, endpoint_independent) -> <<"full_cone">>;
classify_nat_type(endpoint_independent, address_dependent) -> <<"restricted">>;
classify_nat_type(endpoint_independent, address_and_port_dependent) -> <<"port_restricted">>;
classify_nat_type(_, _) -> <<"symmetric">>.

%% @private Calculate delivery rate for a single peer
calc_peer_delivery_rate(#peer_tracker{max_seq = 0}) -> 0.0;
calc_peer_delivery_rate(#peer_tracker{max_seq = MaxSeq, received_count = Received}) ->
    (Received / MaxSeq) * 100.0.

%% @private Format peer stats for get_stats/0
format_peer_stats(Trackers) ->
    maps:map(fun(_PeerId, Tracker) ->
        #{
            nat_type => Tracker#peer_tracker.nat_type,
            received => Tracker#peer_tracker.received_count,
            max_seq => Tracker#peer_tracker.max_seq,
            delivery_rate => calc_peer_delivery_rate(Tracker)
        }
    end, Trackers).

%% @private Calculate delivery rate grouped by NAT type
calc_delivery_by_nat(Trackers) ->
    %% Group trackers by NAT type
    Grouped = maps:fold(fun(_PeerId, Tracker, Acc) ->
        NatType = case Tracker#peer_tracker.nat_type of
            undefined -> <<"unknown">>;
            T -> T
        end,
        Existing = maps:get(NatType, Acc, {0, 0}),
        {TotalReceived, TotalExpected} = Existing,
        NewReceived = TotalReceived + Tracker#peer_tracker.received_count,
        NewExpected = TotalExpected + Tracker#peer_tracker.max_seq,
        maps:put(NatType, {NewReceived, NewExpected}, Acc)
    end, #{}, Trackers),

    %% Calculate rates
    maps:map(fun(_NatType, {Received, Expected}) ->
        case Expected of
            0 -> #{received => 0, expected => 0, rate => 0.0};
            _ -> #{received => Received, expected => Expected,
                   rate => (Received / Expected) * 100.0}
        end
    end, Grouped).

%% @private Print delivery stats on shutdown
print_delivery_stats(State) ->
    NodeId = State#state.node_id,
    NatType = State#state.nat_type,
    Trackers = State#state.peer_trackers,

    io:format("~n"),
    io:format("╔══════════════════════════════════════════════════════════════╗~n"),
    io:format("║              PUBSUB DELIVERY STATISTICS                      ║~n"),
    io:format("╠══════════════════════════════════════════════════════════════╣~n"),
    io:format("║  Node: ~-20s  NAT Type: ~-15s        ║~n",
              [NodeId, format_nat(NatType)]),
    io:format("║  Messages Sent: ~-10B  Messages Received: ~-10B     ║~n",
              [State#state.messages_sent, State#state.messages_received]),
    io:format("║  Unique Peers: ~-10B                                     ║~n",
              [maps:size(Trackers)]),
    io:format("╠══════════════════════════════════════════════════════════════╣~n"),
    io:format("║  DELIVERY BY NAT TYPE:                                       ║~n"),

    ByNat = calc_delivery_by_nat(Trackers),
    maps:foreach(fun(Nat, #{received := R, expected := E, rate := Rate}) ->
        io:format("║    ~-15s: ~5B/~-5B (~6.1f%%)                      ║~n",
                  [format_nat(Nat), R, E, Rate])
    end, ByNat),

    io:format("╚══════════════════════════════════════════════════════════════╝~n"),
    io:format("~n").

%% @private Format NAT type for display
format_nat(undefined) -> "unknown";
format_nat(Nat) when is_binary(Nat) -> binary_to_list(Nat);
format_nat(Nat) -> Nat.
