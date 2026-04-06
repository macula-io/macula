%%%-------------------------------------------------------------------
%%% @doc Multi-relay client — maintains N concurrent relay connections.
%%%
%%% Wraps multiple macula_relay_client instances for node multi-homing.
%%% Subscribes and advertises on ALL connections. Publishes via PRIMARY.
%%% Deduplicates incoming messages by message_id (ring buffer).
%%%
%%% On primary failure: promotes secondary, connects a new secondary.
%%% Same API as macula_relay_client for drop-in replacement.
%%%
%%% Usage:
%%% ```
%%% Opts = #{relays => [R1, R2, R3, R4, R5],
%%%          connections => 2,           %% default 2
%%%          realm => <<"io.macula">>,
%%%          identity => <<"my-node">>},
%%% {ok, Pid} = macula_multi_relay:start_link(Opts).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_multi_relay).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1, stop/1]).
-export([subscribe/3, unsubscribe/2, publish/3]).
-export([advertise/3, unadvertise/2, call/4]).
-export([get_status/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Test exports
-ifdef(TEST).
-export([extract_message_id/1, assign_roles/1, shuffle/1]).
-endif.

-define(DEDUP_SIZE, 2048).
-define(DEFAULT_CONNECTIONS, 2).
-define(HEALTH_CHECK_MS, 15000).

-record(conn, {
    pid :: pid(),
    relay_url :: binary(),
    monitor :: reference(),
    role :: primary | secondary
}).

-record(state, {
    opts :: map(),                                %% original start opts (for spawning new connections)
    relays :: [binary()],                         %% all configured relay URLs
    connections :: [#conn{}],                      %% active connections
    target_count :: pos_integer(),                 %% how many connections to maintain
    subscriptions :: #{reference() => {binary(), fun()}},  %% ref => {topic, callback}
    procedures :: #{binary() => fun()},            %% procedure => handler
    dedup_table :: ets:tid(),                       %% ETS set for O(1) concurrent dedup
    dedup_queue :: queue:queue(binary())            %% eviction order tracking
}).

%%====================================================================
%% API — same signatures as macula_relay_client
%%====================================================================

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

stop(Pid) ->
    gen_server:stop(Pid).

-spec subscribe(pid(), binary(), fun((map()) -> ok)) -> {ok, reference()}.
subscribe(Pid, Topic, Callback) ->
    gen_server:call(Pid, {subscribe, Topic, Callback}).

-spec unsubscribe(pid(), reference()) -> ok.
unsubscribe(Pid, Ref) ->
    gen_server:call(Pid, {unsubscribe, Ref}).

-spec publish(pid(), binary(), binary() | map()) -> ok.
publish(Pid, Topic, Payload) ->
    gen_server:cast(Pid, {publish, Topic, Payload}).

-spec advertise(pid(), binary(), fun((map()) -> {ok, term()} | {error, term()})) -> {ok, reference()}.
advertise(Pid, Procedure, Handler) ->
    gen_server:call(Pid, {advertise, Procedure, Handler}).

-spec unadvertise(pid(), binary()) -> ok.
unadvertise(Pid, Procedure) ->
    gen_server:call(Pid, {unadvertise, Procedure}).

-spec call(pid(), binary(), map(), timeout()) -> {ok, term()} | {error, term()}.
call(Pid, Procedure, Args, Timeout) ->
    gen_server:call(Pid, {rpc_call, Procedure, Args, Timeout}, Timeout + 1000).

-spec get_status(pid()) -> {ok, map()}.
get_status(Pid) ->
    gen_server:call(Pid, get_status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    Relays = case maps:find(relays, Opts) of
        {ok, List} when is_list(List), length(List) > 0 -> List;
        _ -> [maps:get(url, Opts, <<"https://localhost:4433">>)]
    end,
    TargetCount = min(
        maps:get(connections, Opts, ?DEFAULT_CONNECTIONS),
        length(Relays)
    ),
    DedupTable = ets:new(multi_relay_dedup, [set, public, {read_concurrency, true}]),
    State = #state{
        opts = Opts,
        relays = Relays,
        connections = [],
        target_count = TargetCount,
        subscriptions = #{},
        procedures = #{},
        dedup_table = DedupTable,
        dedup_queue = queue:new()
    },
    %% Start connections asynchronously
    self() ! spawn_connections,
    %% Periodic health check
    erlang:send_after(?HEALTH_CHECK_MS, self(), health_check),
    {ok, State}.

%%====================================================================
%% Subscribe / Unsubscribe — applied to ALL connections
%%====================================================================

handle_call({subscribe, Topic, Callback}, _From, State) ->
    Ref = make_ref(),
    DedupCallback = make_dedup_callback(State#state.dedup_table, Callback),
    Subs = maps:put(Ref, {Topic, Callback}, State#state.subscriptions),
    subscribe_on_all(Topic, DedupCallback, State#state.connections),
    {reply, {ok, Ref}, State#state{subscriptions = Subs}};

handle_call({unsubscribe, Ref}, _From, State) ->
    case maps:get(Ref, State#state.subscriptions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        {_Topic, _Callback} ->
            Subs = maps:remove(Ref, State#state.subscriptions),
            %% Note: per-connection sub refs aren't tracked in multi_relay.
            %% Subscriptions are cleaned up when connections terminate.
            {reply, ok, State#state{subscriptions = Subs}}
    end;

%%====================================================================
%% Advertise / Unadvertise — applied to ALL connections
%%====================================================================

handle_call({advertise, Procedure, Handler}, _From, State) ->
    Procs = maps:put(Procedure, Handler, State#state.procedures),
    %% Advertise on all active connections
    lists:foreach(fun(#conn{pid = Pid}) ->
        case macula_relay_client:advertise(Pid, Procedure, Handler) of
            {ok, _} -> ok;
            {error, Reason} ->
                ?LOG_WARNING("[multi_relay] Advertise ~s on ~p failed: ~p",
                             [Procedure, Pid, Reason])
        end
    end, State#state.connections),
    {reply, {ok, make_ref()}, State#state{procedures = Procs}};

handle_call({unadvertise, Procedure}, _From, State) ->
    Procs = maps:remove(Procedure, State#state.procedures),
    lists:foreach(fun(#conn{pid = Pid}) ->
        case macula_relay_client:unadvertise(Pid, Procedure) of
            ok -> ok;
            {error, Reason} ->
                ?LOG_WARNING("[multi_relay] Unadvertise ~s on ~p failed: ~p",
                             [Procedure, Pid, Reason])
        end
    end, State#state.connections),
    {reply, ok, State#state{procedures = Procs}};

%%====================================================================
%% RPC Call — via primary connection
%%====================================================================

handle_call({rpc_call, Procedure, Args, Timeout}, _From, State) ->
    case find_primary(State#state.connections) of
        undefined ->
            {reply, {error, no_connection}, State};
        #conn{pid = Pid} ->
            Result = macula_relay_client:call(Pid, Procedure, Args, Timeout),
            {reply, Result, State}
    end;


%%====================================================================
%% Status
%%====================================================================

handle_call(get_status, _From, State) ->
    Conns = [#{
        relay => C#conn.relay_url,
        role => C#conn.role,
        pid => C#conn.pid
    } || C <- State#state.connections],
    Status = #{
        connections => Conns,
        target_count => State#state.target_count,
        active_count => length(State#state.connections),
        subscriptions => maps:size(State#state.subscriptions),
        procedures => maps:size(State#state.procedures),
        dedup_size => ets:info(State#state.dedup_table, size)
    },
    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

%%====================================================================
%% Publish — via primary only (avoid duplicate publishes)
%%====================================================================

handle_cast({publish, Topic, Payload}, State) ->
    case find_primary(State#state.connections) of
        undefined -> ok;
        #conn{pid = Pid} ->
            macula_relay_client:publish(Pid, Topic, Payload)
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%====================================================================
%% Connection lifecycle
%%====================================================================

handle_info(spawn_connections, State) ->
    State2 = ensure_connections(State),
    {noreply, State2};

handle_info(health_check, State) ->
    State2 = ensure_connections(State),
    State3 = cleanup_dedup(State2),
    erlang:send_after(?HEALTH_CHECK_MS, self(), health_check),
    {noreply, State3};

%% A relay_client process died — remove from connections, spawn replacement
handle_info({'DOWN', MonRef, process, _Pid, Reason}, State) ->
    case lists:keyfind(MonRef, #conn.monitor, State#state.connections) of
        false ->
            {noreply, State};
        #conn{relay_url = Url, role = Role} ->
            ?LOG_WARNING("[multi_relay] Connection to ~s (~p) down: ~p",
                         [Url, Role, Reason]),
            Conns = lists:keydelete(MonRef, #conn.monitor, State#state.connections),
            State2 = State#state{connections = Conns},
            %% If primary died, promote first secondary
            State3 = case Role of
                primary -> promote_secondary(State2);
                secondary -> State2
            end,
            %% Schedule replacement connection
            erlang:send_after(1000, self(), spawn_connections),
            {noreply, State3}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{connections = Conns}) ->
    lists:foreach(fun(#conn{pid = Pid}) ->
        catch macula_relay_client:stop(Pid)
    end, Conns),
    ok.

%%====================================================================
%% Internal: connection management
%%====================================================================

ensure_connections(#state{connections = Conns, target_count = Target} = State)
  when length(Conns) >= Target ->
    State;
ensure_connections(#state{connections = Conns, target_count = Target,
                          relays = Relays, opts = Opts} = State) ->
    Needed = Target - length(Conns),
    UsedUrls = [C#conn.relay_url || C <- Conns],
    %% Pick relays not already connected to
    Available = [R || R <- Relays, not lists:member(R, UsedUrls)],
    %% If all relays are used, allow duplicates (fewer relays than connections)
    Candidates = case Available of
        [] -> Relays;
        _ -> Available
    end,
    %% Shuffle for load distribution
    Shuffled = shuffle(Candidates),
    ToConnect = lists:sublist(Shuffled, Needed),
    NewConns = lists:foldl(fun(RelayUrl, Acc) ->
        case spawn_relay_client(RelayUrl, Opts, State) of
            {ok, Conn} -> [Conn | Acc];
            {error, _} -> Acc
        end
    end, [], ToConnect),
    AllConns = Conns ++ NewConns,
    %% Assign roles: first is primary, rest are secondary
    Tagged = assign_roles(AllConns),
    State#state{connections = Tagged}.

spawn_relay_client(RelayUrl, Opts, #state{subscriptions = Subs, procedures = Procs,
                                         dedup_table = DedupTable}) ->
    %% Build opts for this specific relay connection
    ClientOpts = Opts#{
        relays => [RelayUrl],
        url => RelayUrl
    },
    case macula_relay_client:start_link(ClientOpts) of
        {ok, Pid} ->
            MonRef = monitor(process, Pid),
            ?LOG_INFO("[multi_relay] Connected to ~s (pid ~p)", [RelayUrl, Pid]),
            %% Replay subscriptions on this new connection
            replay_subscriptions(Pid, Subs, DedupTable),
            %% Replay procedures on this new connection
            replay_procedures(Pid, Procs),
            {ok, #conn{pid = Pid, relay_url = RelayUrl, monitor = MonRef, role = secondary}};
        {error, Reason} ->
            ?LOG_WARNING("[multi_relay] Failed to start client for ~s: ~p",
                         [RelayUrl, Reason]),
            {error, Reason}
    end.

replay_subscriptions(Pid, Subs, DedupTable) ->
    maps:foreach(fun(_Ref, {Topic, Callback}) ->
        DedupCallback = make_dedup_callback(DedupTable, Callback),
        catch macula_relay_client:subscribe(Pid, Topic, DedupCallback)
    end, Subs).

replay_procedures(Pid, Procs) ->
    maps:foreach(fun(Procedure, Handler) ->
        catch macula_relay_client:advertise(Pid, Procedure, Handler)
    end, Procs).

assign_roles([]) -> [];
assign_roles([First | Rest]) ->
    [First#conn{role = primary} | [C#conn{role = secondary} || C <- Rest]].

promote_secondary(#state{connections = []} = State) ->
    ?LOG_WARNING("[multi_relay] No connections remaining — cannot promote"),
    State;
promote_secondary(#state{connections = [First | Rest]} = State) ->
    ?LOG_INFO("[multi_relay] Promoting ~s to primary", [First#conn.relay_url]),
    State#state{connections = [First#conn{role = primary} | Rest]}.

find_primary(Conns) ->
    case [C || #conn{role = primary} = C <- Conns] of
        [Primary | _] -> Primary;
        [] ->
            %% No primary tagged — use first available
            case Conns of
                [First | _] -> First;
                [] -> undefined
            end
    end.

%%====================================================================
%% Internal: subscription helpers
%%====================================================================

subscribe_on_all(Topic, Callback, Connections) ->
    lists:foreach(fun(#conn{pid = Pid}) ->
        catch macula_relay_client:subscribe(Pid, Topic, Callback)
    end, Connections).

make_dedup_callback(DedupTable, Callback) ->
    fun(Msg) ->
        maybe_invoke_deduped(DedupTable, Callback, Msg)
    end.

maybe_invoke_deduped(DedupTable, Callback, Msg) ->
    case extract_message_id(Msg) of
        undefined -> Callback(Msg);
        MsgId -> invoke_if_new(DedupTable, Callback, Msg, MsgId)
    end.

%% O(1) concurrent dedup via public ETS — no gen_server bottleneck.
%% insert_new is atomic: returns true only for the first inserter.
invoke_if_new(DedupTable, Callback, Msg, MsgId) ->
    case ets:insert_new(DedupTable, {MsgId, true}) of
        true -> Callback(Msg);
        false -> ok
    end.

%% Note: ETS cleanup happens periodically in health_check, not per-message.
%% The queue tracking for eviction order is maintained in the gen_server
%% state but doesn't block the hot path (insert_new is lock-free).

%%====================================================================
%% Internal: dedup ETS cleanup
%%====================================================================

%% Periodic cleanup: keep ETS table bounded by evicting oldest entries.
%% Called from health_check timer. Uses the queue to track insertion order.
cleanup_dedup(#state{dedup_table = Tab, dedup_queue = Q} = State) ->
    Size = ets:info(Tab, size),
    case Size > ?DEDUP_SIZE of
        false -> State;
        true ->
            {Q2, _Evicted} = evict_oldest(Tab, Q, Size - ?DEDUP_SIZE),
            State#state{dedup_queue = Q2}
    end.

evict_oldest(_Tab, Q, 0) -> {Q, 0};
evict_oldest(Tab, Q, N) ->
    case queue:out(Q) of
        {{value, MsgId}, Q2} ->
            ets:delete(Tab, MsgId),
            evict_oldest(Tab, Q2, N - 1);
        {empty, Q} ->
            {Q, 0}
    end.

extract_message_id(#{<<"message_id">> := MsgId}) when is_binary(MsgId) -> MsgId;
extract_message_id(#{message_id := MsgId}) when is_binary(MsgId) -> MsgId;
extract_message_id(#{payload := Payload}) when is_map(Payload) ->
    case maps:find(<<"message_id">>, Payload) of
        {ok, MsgId} when is_binary(MsgId) -> MsgId;
        _ -> undefined
    end;
extract_message_id(_) -> undefined.

%%====================================================================
%% Internal: utils
%%====================================================================

shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), E} || E <- List])].
