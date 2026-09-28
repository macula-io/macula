%% @doc How many handshake v5 session proofs this station signs: by default at most 30 a minute for one client node,
%% and 30 a second for all clients together (plans/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md section 3, "Signing cost as an attack
%% surface"). A composite session proof costs 6 to 10 ms of signing, so the total bounds this station's signing to
%% about a quarter of one core, and a reconnect storm of 1000 clients is admitted in about 30 seconds. The station asks
%% only after the client's CONNECT proof has verified, so a refusal here costs the client a composite signature of its
%% own. Past either limit the station refuses CONNECT with session_proof_rate, and the refusal names the limit.
%%
%% The fleet's CPUs differ several-fold, so both limits are macula application environment options,
%% `session_proofs_per_node_per_minute' and `session_proofs_per_second', read once when this process starts and never
%% looked up per handshake. A value that is not an integer of at least 1 refuses the start, naming itself.
%%
%% Fixed windows: the minute and the second a request falls in. This process only owns the table and purges the
%% windows that ended, once a minute; every count goes to the table directly.
-module(macula_session_proof_rate).
-behaviour(gen_server).

-export([start_link/0, allow/2, limits/0, purge/1, windows/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(TABLE, ?MODULE).
-define(LIMITS, {?MODULE, limits}).
-define(DEFAULT_PER_NODE_PER_MINUTE, 30).
-define(DEFAULT_PER_SECOND, 30).
-define(PURGE_INTERVAL_MS, 60000).

-type limits() :: #{per_node_per_minute := pos_integer(), per_second := pos_integer()}.
-export_type([limits/0]).

-spec start_link() -> {ok, pid()} | {error, {invalid_limit, atom(), term()}}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Whether the station may sign one more session proof for the client NodeId at Now (milliseconds).
-spec allow(<<_:256>>, integer()) -> ok | {error, {session_proof_rate, per_node_per_minute | per_second}}.
allow(NodeId, Now) ->
    #{per_node_per_minute := PerNode, per_second := PerSecond} = limits(),
    within(counted({node, NodeId, Now div 60000}) =< PerNode, per_node_per_minute,
           fun() -> within(counted({total, Now div 1000}) =< PerSecond, per_second, fun() -> ok end) end).

counted(Key) ->
    ets:update_counter(?TABLE, Key, 1, {Key, 0}).

within(true, _Limit, Next) -> Next();
within(false, Limit, _Next) -> {error, {session_proof_rate, Limit}}.

%% @doc The limits in force, as read at start.
-spec limits() -> limits().
limits() ->
    persistent_term:get(?LIMITS).

%% @doc Delete the windows that ended before Now.
-spec purge(integer()) -> ok.
purge(Now) ->
    Minute = Now div 60000,
    Second = Now div 1000,
    _ = ets:select_delete(?TABLE, [{{{node, '_', '$1'}, '_'}, [{'<', '$1', Minute}], [true]},
                                   {{{total, '$1'}, '_'}, [{'<', '$1', Second}], [true]}]),
    ok.

%% @doc How many windows the table holds.
-spec windows() -> non_neg_integer().
windows() ->
    ets:info(?TABLE, size).

%%------------------------------------------------------------------
%% The table's owner
%%------------------------------------------------------------------

init([]) ->
    configured(limit(session_proofs_per_node_per_minute, ?DEFAULT_PER_NODE_PER_MINUTE),
               limit(session_proofs_per_second, ?DEFAULT_PER_SECOND)).

limit(Name, Default) ->
    valid_limit(Name, application:get_env(macula, Name, Default)).

valid_limit(_Name, Value) when is_integer(Value), Value >= 1 -> {ok, Value};
valid_limit(Name, Value) -> {error, {invalid_limit, Name, Value}}.

configured({ok, PerNode}, {ok, PerSecond}) ->
    ok = persistent_term:put(?LIMITS, #{per_node_per_minute => PerNode, per_second => PerSecond}),
    ?TABLE = ets:new(?TABLE, [named_table, public, set, {write_concurrency, true}]),
    {ok, schedule_purge(#{})};
configured({error, Invalid}, _PerSecond) ->
    refused(Invalid);
configured(_PerNode, {error, Invalid}) ->
    refused(Invalid).

refused({invalid_limit, Name, Value} = Invalid) ->
    logger:error("[macula_session_proof_rate] refusing to start: macula ~p must be an integer of at least 1, got ~0p",
                 [Name, Value]),
    {stop, Invalid}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(purge, State) ->
    ok = purge(erlang:system_time(millisecond)),
    {noreply, schedule_purge(State)};
handle_info(_Other, State) ->
    {noreply, State}.

schedule_purge(State) ->
    _ = erlang:send_after(?PURGE_INTERVAL_MS, self(), purge),
    State.
