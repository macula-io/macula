%% @doc Which handshake version a client dials each node with, and the node-wide handshake counters
%% (plans/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md sections 3, 4 and 6).
%%
%% A node never seen is dialled with version 5. A node that refused a v5 CONNECT with unsupported_version is dialled
%% with version 4 for the next 10 minutes, so a slow roll does not pay a failed v5 handshake on every new connection,
%% then with version 5 again. A node that completed one v5 handshake in this run is never dialled with version 4
%% again: a later unsupported_version from it is refused as a downgrade, which also refuses a station rolled back
%% below v5 until forget_v5_peer/1 is called for it or this node restarts. There is no timer on that memory, because
%% a timer is also an attacker's wait.
%%
%% Fallbacks are counted per node. The first is expected while the fleet rolls; from the second, the caller logs a
%% warning at most once a minute per node.
%%
%% This process only owns the table. Every read and write goes to the table directly, so no connection queues on it.
-module(macula_peer_versions).
-behaviour(gen_server).

-export([start_link/0, dial_version/2, unsupported_version/2, completed_v5/1, seen_v5/1, forget_v5_peer/1,
         fallback_warning/2, count/1, counters/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(TABLE, ?MODULE).
-define(V4_CACHE_MS, 10 * 60000).
-define(WARNING_INTERVAL_MS, 60000).
-define(COUNTERS, [v4_connections, v5_connections, v4_control_frames, v4_fallbacks, v5_downgrade_refused,
                   v4_hello_to_v5_connect, session_proof_invalid, session_proof_missing, session_proof_rate,
                   exporter_unavailable]).

-type counter() :: v4_connections | v5_connections | v4_control_frames | v4_fallbacks | v5_downgrade_refused
                 | v4_hello_to_v5_connect | session_proof_invalid | session_proof_missing | session_proof_rate
                 | exporter_unavailable.
-export_type([counter/0]).

%% Rows: {{seen_v5, NodeId}}, {{v4_until, NodeId}, Ms}, {{fallbacks, NodeId}, Count}, {{warned, NodeId}, Ms},
%% {{counter, Name}, Count}.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc The version to put in CONNECT to NodeId at Now.
-spec dial_version(<<_:256>>, integer()) -> 4 | 5.
dial_version(NodeId, Now) ->
    version_for(ets:lookup(?TABLE, {v4_until, NodeId}), Now).

version_for([{_, Until}], Now) when Now < Until -> 4;
version_for(_NoneOrExpired, _Now) -> 5.

%% @doc NodeId refused a v5 CONNECT with unsupported_version. A node seen on v5 in this run is refused as a downgrade;
%% any other falls back to version 4 for 10 minutes, with its fallback count.
-spec unsupported_version(<<_:256>>, integer()) -> downgrade_refused | {fall_back, pos_integer()}.
unsupported_version(NodeId, Now) ->
    refused_or_fallen_back(seen_v5(NodeId), NodeId, Now).

refused_or_fallen_back(true, _NodeId, _Now) ->
    ok = count(v5_downgrade_refused),
    downgrade_refused;
refused_or_fallen_back(false, NodeId, Now) ->
    true = ets:insert(?TABLE, {{v4_until, NodeId}, Now + ?V4_CACHE_MS}),
    ok = count(v4_fallbacks),
    {fall_back, ets:update_counter(?TABLE, {fallbacks, NodeId}, 1, {{fallbacks, NodeId}, 0})}.

%% @doc A v5 handshake with NodeId completed: it is never dialled with version 4 again in this run.
-spec completed_v5(<<_:256>>) -> ok.
completed_v5(NodeId) ->
    true = ets:insert(?TABLE, {{seen_v5, NodeId}}),
    true = ets:delete(?TABLE, {v4_until, NodeId}),
    ok.

-spec seen_v5(<<_:256>>) -> boolean().
seen_v5(NodeId) ->
    ets:member(?TABLE, {seen_v5, NodeId}).

%% @doc Forget that NodeId completed a v5 handshake, so a station deliberately rolled back below v5 is dialled again.
%% An operator action, never automatic.
-spec forget_v5_peer(<<_:256>>) -> ok.
forget_v5_peer(NodeId) ->
    true = ets:delete(?TABLE, {seen_v5, NodeId}),
    ok.

%% @doc Whether to log a warning for NodeId's fallbacks at Now: from its second fallback, at most once a minute.
-spec fallback_warning(<<_:256>>, integer()) -> no_warning | {warn, pos_integer()}.
fallback_warning(NodeId, Now) ->
    warning_due(fallbacks(NodeId), ets:lookup(?TABLE, {warned, NodeId}), NodeId, Now).

fallbacks(NodeId) ->
    fallback_count(ets:lookup(?TABLE, {fallbacks, NodeId})).

fallback_count([{_, Count}]) -> Count;
fallback_count([]) -> 0.

warning_due(Count, _Warned, _NodeId, _Now) when Count < 2 ->
    no_warning;
warning_due(_Count, [{_, At}], _NodeId, Now) when Now - At < ?WARNING_INTERVAL_MS ->
    no_warning;
warning_due(Count, _NeverOrLongAgo, NodeId, Now) ->
    true = ets:insert(?TABLE, {{warned, NodeId}, Now}),
    {warn, Count}.

%% @doc Count one handshake event, node-wide.
-spec count(counter()) -> ok.
count(Name) when Name =:= v4_connections; Name =:= v5_connections; Name =:= v4_control_frames;
                 Name =:= v4_fallbacks; Name =:= v5_downgrade_refused; Name =:= v4_hello_to_v5_connect;
                 Name =:= session_proof_invalid; Name =:= session_proof_missing; Name =:= session_proof_rate;
                 Name =:= exporter_unavailable ->
    _ = ets:update_counter(?TABLE, {counter, Name}, 1, {{counter, Name}, 0}),
    ok.

%% @doc Every handshake counter, zero included.
-spec counters() -> #{counter() => non_neg_integer()}.
counters() ->
    maps:from_list([{Name, counted(ets:lookup(?TABLE, {counter, Name}))} || Name <- ?COUNTERS]).

counted([{_, Count}]) -> Count;
counted([]) -> 0.

%%------------------------------------------------------------------
%% The table's owner
%%------------------------------------------------------------------

init([]) ->
    ?TABLE = ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}, {write_concurrency, true}]),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.
