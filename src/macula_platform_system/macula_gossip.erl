%%%-------------------------------------------------------------------
%%% @doc Gossip protocol for CRDT state replication.
%%%
%%% Implements a gossip-based protocol for eventually-consistent state
%%% synchronization across nodes. Uses push-pull-push anti-entropy:
%%%
%%% - Push: Periodically sends local state changes to random peers
%%% - Pull: Requests state from peers when needed
%%% - Anti-entropy: Full state synchronization to repair divergence
%%%
%%% Configuration Parameters:
%%% - push_interval: 1000ms (how often to push to peers)
%%% - anti_entropy_interval: 30000ms (how often to run anti-entropy)
%%% - fanout: 3 (number of peers to contact per round)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gossip).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    stop/1,
    %% State management
    put/4,
    get/2,
    delete/2,
    get_all/1,
    %% Gossip control
    push_state/2,
    pull_state/2,
    anti_entropy/1,
    force_sync/1,
    %% Peer management
    add_peer/2,
    remove_peer/2,
    get_peers/1,
    %% Metrics
    get_stats/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% Gossip message handlers (called by gateway/connection)
-export([
    handle_gossip_push/2,
    handle_gossip_pull/2,
    handle_gossip_pull_reply/2,
    handle_gossip_sync/2,
    handle_gossip_sync_reply/2
]).

-define(DEFAULT_PUSH_INTERVAL, 1000).      % 1 second
-define(DEFAULT_ANTI_ENTROPY_INTERVAL, 30000).  % 30 seconds
-define(DEFAULT_FANOUT, 3).
-define(MAX_STATES_PER_MESSAGE, 100).

-record(state, {
    node_id :: binary(),
    realm :: binary(),
    %% Local CRDT states: #{Key => {Type, CrdtState, VectorClock}}
    states :: #{binary() => {atom(), term(), map()}},
    %% Pending pulls: #{RequestId => {From, Timeout}}
    pending_pulls :: #{binary() => {pid(), reference()}},
    %% Known peers for gossip
    peers :: [binary()],
    %% Send function for peer communication
    send_fn :: fun((binary(), map()) -> ok | {error, term()}),
    %% Configuration
    push_interval :: pos_integer(),
    anti_entropy_interval :: pos_integer(),
    fanout :: pos_integer(),
    %% Timers
    push_timer :: reference() | undefined,
    anti_entropy_timer :: reference() | undefined,
    %% Stats
    push_count :: non_neg_integer(),
    pull_count :: non_neg_integer(),
    merge_count :: non_neg_integer(),
    conflict_count :: non_neg_integer()
}).

-type gossip_state() :: #state{}.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the gossip server.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc Stop the gossip server.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Store a CRDT value.
%% Type is the CRDT type: lww_register, or_set, gcounter, pncounter
-spec put(pid(), binary(), atom(), term()) -> ok.
put(Pid, Key, Type, Value) ->
    gen_server:call(Pid, {put, Key, Type, Value}).

%% @doc Get a CRDT value.
-spec get(pid(), binary()) -> {ok, {atom(), term()}} | {error, not_found}.
get(Pid, Key) ->
    gen_server:call(Pid, {get, Key}).

%% @doc Delete a CRDT value.
-spec delete(pid(), binary()) -> ok.
delete(Pid, Key) ->
    gen_server:call(Pid, {delete, Key}).

%% @doc Get all stored CRDT states.
-spec get_all(pid()) -> #{binary() => {atom(), term()}}.
get_all(Pid) ->
    gen_server:call(Pid, get_all).

%% @doc Push local state to a specific peer.
-spec push_state(pid(), binary()) -> ok | {error, term()}.
push_state(Pid, PeerNodeId) ->
    gen_server:call(Pid, {push_state, PeerNodeId}).

%% @doc Pull state from a specific peer.
-spec pull_state(pid(), binary()) -> ok | {error, term()}.
pull_state(Pid, PeerNodeId) ->
    gen_server:call(Pid, {pull_state, PeerNodeId}).

%% @doc Trigger anti-entropy synchronization.
-spec anti_entropy(pid()) -> ok.
anti_entropy(Pid) ->
    gen_server:cast(Pid, anti_entropy).

%% @doc Force synchronization with all peers.
-spec force_sync(pid()) -> ok.
force_sync(Pid) ->
    gen_server:cast(Pid, force_sync).

%% @doc Add a peer to the gossip list.
-spec add_peer(pid(), binary()) -> ok.
add_peer(Pid, PeerNodeId) ->
    gen_server:call(Pid, {add_peer, PeerNodeId}).

%% @doc Remove a peer from the gossip list.
-spec remove_peer(pid(), binary()) -> ok.
remove_peer(Pid, PeerNodeId) ->
    gen_server:call(Pid, {remove_peer, PeerNodeId}).

%% @doc Get the list of known peers.
-spec get_peers(pid()) -> [binary()].
get_peers(Pid) ->
    gen_server:call(Pid, get_peers).

%% @doc Get gossip statistics.
-spec get_stats(pid()) -> map().
get_stats(Pid) ->
    gen_server:call(Pid, get_stats).

%%%===================================================================
%%% Gossip Message Handlers
%%%===================================================================

%% @doc Handle incoming gossip_push message.
-spec handle_gossip_push(pid(), map()) -> ok.
handle_gossip_push(Pid, Msg) ->
    gen_server:cast(Pid, {gossip_push, Msg}).

%% @doc Handle incoming gossip_pull message.
-spec handle_gossip_pull(pid(), map()) -> ok.
handle_gossip_pull(Pid, Msg) ->
    gen_server:cast(Pid, {gossip_pull, Msg}).

%% @doc Handle incoming gossip_pull_reply message.
-spec handle_gossip_pull_reply(pid(), map()) -> ok.
handle_gossip_pull_reply(Pid, Msg) ->
    gen_server:cast(Pid, {gossip_pull_reply, Msg}).

%% @doc Handle incoming gossip_sync message.
-spec handle_gossip_sync(pid(), map()) -> ok.
handle_gossip_sync(Pid, Msg) ->
    gen_server:cast(Pid, {gossip_sync, Msg}).

%% @doc Handle incoming gossip_sync_reply message.
-spec handle_gossip_sync_reply(pid(), map()) -> ok.
handle_gossip_sync_reply(Pid, Msg) ->
    gen_server:cast(Pid, {gossip_sync_reply, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Config) ->
    NodeId = maps:get(node_id, Config, macula_id:node_id()),
    Realm = maps:get(realm, Config, <<"default">>),

    %% Default send function (can be overridden for testing)
    DefaultSendFn = fun(_PeerNodeId, _Msg) -> ok end,
    SendFn = maps:get(send_fn, Config, DefaultSendFn),

    %% Configuration
    PushInterval = maps:get(push_interval, Config, ?DEFAULT_PUSH_INTERVAL),
    AntiEntropyInterval = maps:get(anti_entropy_interval, Config, ?DEFAULT_ANTI_ENTROPY_INTERVAL),
    Fanout = maps:get(fanout, Config, ?DEFAULT_FANOUT),

    %% Initial peers
    Peers = maps:get(peers, Config, []),

    State = #state{
        node_id = NodeId,
        realm = Realm,
        states = #{},
        pending_pulls = #{},
        peers = Peers,
        send_fn = SendFn,
        push_interval = PushInterval,
        anti_entropy_interval = AntiEntropyInterval,
        fanout = Fanout,
        push_timer = undefined,
        anti_entropy_timer = undefined,
        push_count = 0,
        pull_count = 0,
        merge_count = 0,
        conflict_count = 0
    },

    %% Start timers
    State1 = start_push_timer(State),
    State2 = start_anti_entropy_timer(State1),

    ?LOG_INFO("Gossip server started for node ~s in realm ~s", [NodeId, Realm]),
    {ok, State2}.

handle_call({put, Key, Type, Value}, _From, State) ->
    {Reply, NewState} = do_put(Key, Type, Value, State),
    {reply, Reply, NewState};

handle_call({get, Key}, _From, State) ->
    Reply = do_get(Key, State),
    {reply, Reply, State};

handle_call({delete, Key}, _From, State) ->
    NewState = do_delete(Key, State),
    {reply, ok, NewState};

handle_call(get_all, _From, #state{states = States} = State) ->
    Result = maps:map(fun(_K, {Type, CrdtState, _VClock}) -> {Type, CrdtState} end, States),
    {reply, Result, State};

handle_call({push_state, PeerNodeId}, _From, State) ->
    Result = do_push_state(PeerNodeId, State),
    {reply, Result, State};

handle_call({pull_state, PeerNodeId}, From, State) ->
    NewState = do_pull_state(PeerNodeId, From, State),
    {noreply, NewState};

handle_call({add_peer, PeerNodeId}, _From, #state{peers = Peers} = State) ->
    NewPeers = case lists:member(PeerNodeId, Peers) of
        true -> Peers;
        false -> [PeerNodeId | Peers]
    end,
    {reply, ok, State#state{peers = NewPeers}};

handle_call({remove_peer, PeerNodeId}, _From, #state{peers = Peers} = State) ->
    NewPeers = lists:delete(PeerNodeId, Peers),
    {reply, ok, State#state{peers = NewPeers}};

handle_call(get_peers, _From, #state{peers = Peers} = State) ->
    {reply, Peers, State};

handle_call(get_stats, _From, State) ->
    Stats = #{
        node_id => State#state.node_id,
        realm => State#state.realm,
        state_count => maps:size(State#state.states),
        peer_count => length(State#state.peers),
        push_count => State#state.push_count,
        pull_count => State#state.pull_count,
        merge_count => State#state.merge_count,
        conflict_count => State#state.conflict_count
    },
    {reply, Stats, State}.

handle_cast(anti_entropy, State) ->
    NewState = do_anti_entropy(State),
    {noreply, NewState};

handle_cast(force_sync, State) ->
    NewState = do_force_sync(State),
    {noreply, NewState};

handle_cast({gossip_push, Msg}, State) ->
    NewState = handle_incoming_push(Msg, State),
    {noreply, NewState};

handle_cast({gossip_pull, Msg}, State) ->
    NewState = handle_incoming_pull(Msg, State),
    {noreply, NewState};

handle_cast({gossip_pull_reply, Msg}, State) ->
    NewState = handle_incoming_pull_reply(Msg, State),
    {noreply, NewState};

handle_cast({gossip_sync, Msg}, State) ->
    NewState = handle_incoming_sync(Msg, State),
    {noreply, NewState};

handle_cast({gossip_sync_reply, Msg}, State) ->
    NewState = handle_incoming_sync_reply(Msg, State),
    {noreply, NewState}.

handle_info(push_timer, State) ->
    NewState = do_periodic_push(State),
    NewState1 = start_push_timer(NewState),
    {noreply, NewState1};

handle_info(anti_entropy_timer, State) ->
    NewState = do_anti_entropy(State),
    NewState1 = start_anti_entropy_timer(NewState),
    {noreply, NewState1};

handle_info({pull_timeout, RequestId}, #state{pending_pulls = Pending} = State) ->
    case maps:get(RequestId, Pending, undefined) of
        undefined ->
            {noreply, State};
        {From, _TimerRef} ->
            gen_server:reply(From, {error, timeout}),
            NewPending = maps:remove(RequestId, Pending),
            {noreply, State#state{pending_pulls = NewPending}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{push_timer = PushTimer, anti_entropy_timer = AETimer}) ->
    cancel_timer(PushTimer),
    cancel_timer(AETimer),
    ok.

%%%===================================================================
%%% Internal Functions - State Management
%%%===================================================================

-spec do_put(binary(), atom(), term(), gossip_state()) -> {ok, gossip_state()}.
do_put(Key, Type, Value, #state{node_id = NodeId, states = States} = State) ->
    VClock = increment_vclock(NodeId, get_vclock(Key, States)),
    NewStates = maps:put(Key, {Type, Value, VClock}, States),
    {ok, State#state{states = NewStates}}.

-spec do_get(binary(), gossip_state()) -> {ok, {atom(), term()}} | {error, not_found}.
do_get(Key, #state{states = States}) ->
    case maps:find(Key, States) of
        {ok, {Type, CrdtState, _VClock}} ->
            {ok, {Type, CrdtState}};
        error ->
            {error, not_found}
    end.

-spec do_delete(binary(), gossip_state()) -> gossip_state().
do_delete(Key, #state{states = States} = State) ->
    NewStates = maps:remove(Key, States),
    State#state{states = NewStates}.

%%%===================================================================
%%% Internal Functions - Gossip Operations
%%%===================================================================

-spec do_push_state(binary(), gossip_state()) -> ok | {error, term()}.
do_push_state(PeerNodeId, #state{node_id = NodeId, states = States, send_fn = SendFn}) ->
    %% Build push messages for all states
    StateList = maps:fold(
        fun(Key, {Type, CrdtState, VClock}, Acc) ->
            Msg = #{
                <<"node_id">> => NodeId,
                <<"state_type">> => Type,
                <<"state_key">> => Key,
                <<"state">> => CrdtState,
                <<"vector_clock">> => VClock
            },
            [Msg | Acc]
        end,
        [],
        States
    ),
    %% Send each state (could batch in future)
    lists:foreach(
        fun(Msg) -> SendFn(PeerNodeId, {gossip_push, Msg}) end,
        StateList
    ),
    ok.

-spec do_pull_state(binary(), {pid(), term()}, gossip_state()) -> gossip_state().
do_pull_state(PeerNodeId, From, #state{node_id = NodeId, send_fn = SendFn, pending_pulls = Pending, pull_count = Count} = State) ->
    RequestId = macula_id:message_id(),
    Msg = #{
        <<"node_id">> => NodeId,
        <<"request_id">> => RequestId,
        <<"state_keys">> => []  % Empty = all states
    },
    SendFn(PeerNodeId, {gossip_pull, Msg}),
    %% Set timeout for response
    TimerRef = erlang:send_after(5000, self(), {pull_timeout, RequestId}),
    NewPending = maps:put(RequestId, {From, TimerRef}, Pending),
    State#state{pending_pulls = NewPending, pull_count = Count + 1}.

-spec do_periodic_push(gossip_state()) -> gossip_state().
do_periodic_push(#state{peers = [], push_count = Count} = State) ->
    State#state{push_count = Count};
do_periodic_push(#state{peers = Peers, fanout = Fanout, push_count = Count} = State) ->
    %% Select random peers up to fanout
    SelectedPeers = select_random_peers(Peers, Fanout),
    lists:foreach(
        fun(Peer) -> do_push_state(Peer, State) end,
        SelectedPeers
    ),
    State#state{push_count = Count + length(SelectedPeers)}.

-spec do_anti_entropy(gossip_state()) -> gossip_state().
do_anti_entropy(#state{peers = []} = State) ->
    State;
do_anti_entropy(#state{peers = Peers, fanout = Fanout, node_id = NodeId, states = States, send_fn = SendFn} = State) ->
    %% Build digest of local state (key -> vector clock)
    Digest = maps:map(fun(_Key, {Type, _CrdtState, VClock}) -> {Type, VClock} end, States),

    %% Send sync request to random peers
    SelectedPeers = select_random_peers(Peers, Fanout),
    Msg = #{
        <<"node_id">> => NodeId,
        <<"digest">> => Digest
    },
    lists:foreach(
        fun(Peer) -> SendFn(Peer, {gossip_sync, Msg}) end,
        SelectedPeers
    ),
    State.

-spec do_force_sync(gossip_state()) -> gossip_state().
do_force_sync(#state{peers = []} = State) ->
    State;
do_force_sync(#state{peers = Peers} = State) ->
    %% Push to all peers
    lists:foreach(
        fun(Peer) -> do_push_state(Peer, State) end,
        Peers
    ),
    State.

%%%===================================================================
%%% Internal Functions - Incoming Message Handlers
%%%===================================================================

-spec handle_incoming_push(map(), gossip_state()) -> gossip_state().
handle_incoming_push(Msg, #state{node_id = LocalNodeId, states = States, merge_count = MergeCount, conflict_count = ConflictCount} = State) ->
    SenderNodeId = maps:get(<<"node_id">>, Msg, undefined),
    Key = maps:get(<<"state_key">>, Msg, undefined),
    Type = maps:get(<<"state_type">>, Msg, undefined),
    RemoteState = maps:get(<<"state">>, Msg, undefined),
    RemoteVClock = maps:get(<<"vector_clock">>, Msg, #{}),

    %% Skip if from self or invalid
    case SenderNodeId of
        LocalNodeId ->
            State;
        undefined ->
            State;
        _ when Key =:= undefined; Type =:= undefined; RemoteState =:= undefined ->
            State;
        _ ->
            {NewStates, NewMergeCount, NewConflictCount} =
                merge_state(Key, Type, RemoteState, RemoteVClock, States, MergeCount, ConflictCount),
            State#state{
                states = NewStates,
                merge_count = NewMergeCount,
                conflict_count = NewConflictCount
            }
    end.

-spec handle_incoming_pull(map(), gossip_state()) -> gossip_state().
handle_incoming_pull(Msg, #state{node_id = NodeId, states = States, send_fn = SendFn} = State) ->
    SenderNodeId = maps:get(<<"node_id">>, Msg, undefined),
    RequestedKeys = maps:get(<<"state_keys">>, Msg, []),

    %% Build response with requested states (or all if empty)
    StateList = case RequestedKeys of
        [] ->
            %% All states
            maps:fold(
                fun(Key, {Type, CrdtState, VClock}, Acc) ->
                    [#{key => Key, type => Type, state => CrdtState, vector_clock => VClock} | Acc]
                end,
                [],
                States
            );
        Keys ->
            %% Only requested keys
            lists:filtermap(
                fun(Key) ->
                    case maps:find(Key, States) of
                        {ok, {Type, CrdtState, VClock}} ->
                            {true, #{key => Key, type => Type, state => CrdtState, vector_clock => VClock}};
                        error ->
                            false
                    end
                end,
                Keys
            )
    end,

    %% Send reply
    Reply = #{
        <<"node_id">> => NodeId,
        <<"request_id">> => maps:get(<<"request_id">>, Msg, <<>>),
        <<"states">> => StateList
    },
    SendFn(SenderNodeId, {gossip_pull_reply, Reply}),
    State.

-spec handle_incoming_pull_reply(map(), gossip_state()) -> gossip_state().
handle_incoming_pull_reply(Msg, #state{pending_pulls = Pending, states = States, merge_count = MergeCount, conflict_count = ConflictCount} = State) ->
    RequestId = maps:get(<<"request_id">>, Msg, <<>>),
    ReceivedStates = maps:get(<<"states">>, Msg, []),

    %% Cancel pending timer and reply
    NewPending = case maps:get(RequestId, Pending, undefined) of
        undefined ->
            Pending;
        {From, TimerRef} ->
            erlang:cancel_timer(TimerRef),
            gen_server:reply(From, ok),
            maps:remove(RequestId, Pending)
    end,

    %% Merge received states
    {NewStates, NewMergeCount, NewConflictCount} = lists:foldl(
        fun(#{key := Key, type := Type, state := RemoteState, vector_clock := VClock}, {StatesAcc, MC, CC}) ->
            merge_state(Key, Type, RemoteState, VClock, StatesAcc, MC, CC)
        end,
        {States, MergeCount, ConflictCount},
        ReceivedStates
    ),

    State#state{
        pending_pulls = NewPending,
        states = NewStates,
        merge_count = NewMergeCount,
        conflict_count = NewConflictCount
    }.

-spec handle_incoming_sync(map(), gossip_state()) -> gossip_state().
handle_incoming_sync(Msg, #state{node_id = NodeId, states = States, send_fn = SendFn} = State) ->
    SenderNodeId = maps:get(<<"node_id">>, Msg, undefined),
    RemoteDigest = maps:get(<<"digest">>, Msg, #{}),

    %% Find states we have that are newer than remote, or that remote doesn't have
    StatesToSend = maps:fold(
        fun(Key, {Type, CrdtState, LocalVClock}, Acc) ->
            case maps:find(Key, RemoteDigest) of
                {ok, {_RemoteType, RemoteVClock}} ->
                    %% Check if our clock is newer
                    case vclock_dominates(LocalVClock, RemoteVClock) of
                        true ->
                            [#{key => Key, type => Type, state => CrdtState, vector_clock => LocalVClock} | Acc];
                        false ->
                            Acc
                    end;
                error ->
                    %% Remote doesn't have this key
                    [#{key => Key, type => Type, state => CrdtState, vector_clock => LocalVClock} | Acc]
            end
        end,
        [],
        States
    ),

    %% Find keys remote has that we don't
    MissingKeys = maps:fold(
        fun(Key, _Value, Acc) ->
            case maps:is_key(Key, States) of
                true -> Acc;
                false -> [Key | Acc]
            end
        end,
        [],
        RemoteDigest
    ),

    %% Send sync reply
    Reply = #{
        <<"node_id">> => NodeId,
        <<"states">> => StatesToSend,
        <<"missing">> => MissingKeys
    },
    SendFn(SenderNodeId, {gossip_sync_reply, Reply}),
    State.

-spec handle_incoming_sync_reply(map(), gossip_state()) -> gossip_state().
handle_incoming_sync_reply(Msg, #state{states = States, merge_count = MergeCount, conflict_count = ConflictCount} = State) ->
    ReceivedStates = maps:get(<<"states">>, Msg, []),
    MissingKeys = maps:get(<<"missing">>, Msg, []),
    SenderNodeId = maps:get(<<"node_id">>, Msg, undefined),

    %% Merge received states
    {NewStates, NewMergeCount, NewConflictCount} = lists:foldl(
        fun(#{key := Key, type := Type, state := RemoteState, vector_clock := VClock}, {StatesAcc, MC, CC}) ->
            merge_state(Key, Type, RemoteState, VClock, StatesAcc, MC, CC)
        end,
        {States, MergeCount, ConflictCount},
        ReceivedStates
    ),

    %% Push missing keys to sender
    case MissingKeys of
        [] ->
            ok;
        _ ->
            lists:foreach(
                fun(Key) ->
                    case maps:find(Key, NewStates) of
                        {ok, {Type, CrdtState, VClock}} ->
                            PushMsg = #{
                                <<"node_id">> => State#state.node_id,
                                <<"state_type">> => Type,
                                <<"state_key">> => Key,
                                <<"state">> => CrdtState,
                                <<"vector_clock">> => VClock
                            },
                            (State#state.send_fn)(SenderNodeId, {gossip_push, PushMsg});
                        error ->
                            ok
                    end
                end,
                MissingKeys
            )
    end,

    State#state{
        states = NewStates,
        merge_count = NewMergeCount,
        conflict_count = NewConflictCount
    }.

%%%===================================================================
%%% Internal Functions - CRDT Merging
%%%===================================================================

-spec merge_state(binary(), atom(), term(), map(), map(), non_neg_integer(), non_neg_integer()) ->
    {map(), non_neg_integer(), non_neg_integer()}.
merge_state(Key, Type, RemoteState, RemoteVClock, States, MergeCount, ConflictCount) ->
    case maps:find(Key, States) of
        {ok, {LocalType, LocalState, LocalVClock}} when LocalType =:= Type ->
            %% Both have the state - merge
            case vclock_compare(LocalVClock, RemoteVClock) of
                equal ->
                    %% Same version, no change needed
                    {States, MergeCount, ConflictCount};
                dominates ->
                    %% Local is newer, keep local
                    {States, MergeCount, ConflictCount};
                dominated ->
                    %% Remote is newer, take remote
                    NewStates = maps:put(Key, {Type, RemoteState, RemoteVClock}, States),
                    {NewStates, MergeCount + 1, ConflictCount};
                concurrent ->
                    %% Concurrent updates - merge CRDTs
                    MergedState = merge_crdt(Type, LocalState, RemoteState),
                    MergedVClock = vclock_merge(LocalVClock, RemoteVClock),
                    NewStates = maps:put(Key, {Type, MergedState, MergedVClock}, States),
                    {NewStates, MergeCount + 1, ConflictCount + 1}
            end;
        {ok, {_DifferentType, _LocalState, _LocalVClock}} ->
            %% Type mismatch - log and keep remote (last write wins at type level)
            ?LOG_WARNING("Type mismatch for key ~s: local has different type, taking remote ~p", [Key, Type]),
            NewStates = maps:put(Key, {Type, RemoteState, RemoteVClock}, States),
            {NewStates, MergeCount + 1, ConflictCount + 1};
        error ->
            %% Don't have this key locally, add it
            NewStates = maps:put(Key, {Type, RemoteState, RemoteVClock}, States),
            {NewStates, MergeCount + 1, ConflictCount}
    end.

-spec merge_crdt(atom(), term(), term()) -> term().
merge_crdt(lww_register, Local, Remote) ->
    macula_crdt:lww_merge(Local, Remote);
merge_crdt(or_set, Local, Remote) ->
    macula_crdt:or_merge(Local, Remote);
merge_crdt(gcounter, Local, Remote) ->
    macula_crdt:gcounter_merge(Local, Remote);
merge_crdt(pncounter, Local, Remote) ->
    macula_crdt:pncounter_merge(Local, Remote);
merge_crdt(_UnknownType, _Local, Remote) ->
    %% Unknown type - take remote
    Remote.

%%%===================================================================
%%% Internal Functions - Vector Clocks
%%%===================================================================

-spec get_vclock(binary(), map()) -> map().
get_vclock(Key, States) ->
    case maps:find(Key, States) of
        {ok, {_Type, _State, VClock}} -> VClock;
        error -> #{}
    end.

-spec increment_vclock(binary(), map()) -> map().
increment_vclock(NodeId, VClock) ->
    Current = maps:get(NodeId, VClock, 0),
    maps:put(NodeId, Current + 1, VClock).

-spec vclock_compare(map(), map()) -> equal | dominates | dominated | concurrent.
vclock_compare(VClock1, VClock2) when VClock1 =:= VClock2 ->
    equal;
vclock_compare(VClock1, VClock2) ->
    Dom1 = vclock_dominates(VClock1, VClock2),
    Dom2 = vclock_dominates(VClock2, VClock1),
    case {Dom1, Dom2} of
        {true, false} -> dominates;
        {false, true} -> dominated;
        {false, false} -> concurrent;
        {true, true} -> equal  % Shouldn't happen, but handle it
    end.

-spec vclock_dominates(map(), map()) -> boolean().
vclock_dominates(VClock1, VClock2) ->
    %% VClock1 dominates VClock2 if all entries in VClock2 are <= corresponding entries in VClock1
    %% and at least one entry is strictly greater
    AllKeys = lists:usort(maps:keys(VClock1) ++ maps:keys(VClock2)),
    {AllGE, SomeGT} = lists:foldl(
        fun(Key, {AllGEAcc, SomeGTAcc}) ->
            V1 = maps:get(Key, VClock1, 0),
            V2 = maps:get(Key, VClock2, 0),
            {AllGEAcc andalso V1 >= V2, SomeGTAcc orelse V1 > V2}
        end,
        {true, false},
        AllKeys
    ),
    AllGE andalso SomeGT.

-spec vclock_merge(map(), map()) -> map().
vclock_merge(VClock1, VClock2) ->
    maps:fold(
        fun(Key, V2, Acc) ->
            V1 = maps:get(Key, Acc, 0),
            maps:put(Key, max(V1, V2), Acc)
        end,
        VClock1,
        VClock2
    ).

%%%===================================================================
%%% Internal Functions - Timer Management
%%%===================================================================

-spec start_push_timer(gossip_state()) -> gossip_state().
start_push_timer(#state{push_interval = Interval} = State) ->
    TimerRef = erlang:send_after(Interval, self(), push_timer),
    State#state{push_timer = TimerRef}.

-spec start_anti_entropy_timer(gossip_state()) -> gossip_state().
start_anti_entropy_timer(#state{anti_entropy_interval = Interval} = State) ->
    TimerRef = erlang:send_after(Interval, self(), anti_entropy_timer),
    State#state{anti_entropy_timer = TimerRef}.

-spec cancel_timer(reference() | undefined) -> ok.
cancel_timer(undefined) ->
    ok;
cancel_timer(TimerRef) ->
    erlang:cancel_timer(TimerRef),
    ok.

%%%===================================================================
%%% Internal Functions - Peer Selection
%%%===================================================================

-spec select_random_peers([binary()], pos_integer()) -> [binary()].
select_random_peers(Peers, MaxCount) when length(Peers) =< MaxCount ->
    Peers;
select_random_peers(Peers, MaxCount) ->
    %% Shuffle and take first MaxCount
    Shuffled = [X || {_, X} <- lists:sort([{rand:uniform(), P} || P <- Peers])],
    lists:sublist(Shuffled, MaxCount).
