%%%-------------------------------------------------------------------
%%% @doc The node's count of served stream sessions.
%%%
%%% A station link asks `admit/2' before it serves a session it has
%%% started, naming the verified caller whose STREAM_OPEN the session
%%% serves. The session is admitted while that caller holds fewer than
%%% `max_served_sessions_per_caller' sessions on the node and the node
%%% fewer than `max_served_sessions', both read from the macula application
%%% env when it asks. An admitted session holds its place until its stream
%%% process ends, however it ends: this process monitors it.
%%%
%%% The caps are 16 per caller and 1000 on the node by default. A cap is
%%% kept per caller and not per link, because a link to a station carries
%%% every caller that station sends here: a cap per link would let one busy
%%% caller take the places of all the others. 16 leaves a caller room for
%%% many sessions at once, and it takes at least 63 callers to fill the
%%% node. The node cap bounds the processes all served sessions hold
%%% together, two for each session: its handler and its stream. A node that
%%% serves more sets its own value.
%%%
%%% The sessions are kept in a table that `macula_stream_sessions_keeper'
%%% holds, so they outlive this process. When it starts again it counts the
%%% kept sessions anew and monitors their streams, and a stream that ended
%%% in between frees its place at once.
%%%
%%% A refusal is counted by its reason, `caller_limit' or `node_limit'. A
%%% refusal is logged at once when no refusal was logged within the last
%%% `served_session_refusal_log_interval_ms'; the ones after it are logged
%%% together when that interval has passed.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_sessions).

-behaviour(gen_server).

-export([start_link/0, new_table/0, admit/2, sessions/0, refusals/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export_type([refusal/0]).

-define(SERVER, ?MODULE).

%% The table's rows: `{{stream, Pid}, Caller}' for each admitted session,
%% `{{caller, Caller}, Count}' for each caller that holds a session,
%% `{sessions, Count}', and `{{refused, Reason}, Count}'.
-define(TABLE, macula_stream_sessions).

-define(MAX_SESSIONS_PER_CALLER, 16).
-define(MAX_SESSIONS, 1000).
-define(REFUSAL_LOG_INTERVAL_MS, 60000).

-type refusal() :: caller_limit | node_limit.

-record(state, {
    %% Refusals not logged yet, when refusals were last logged, and the
    %% timer that logs the ones held back.
    unlogged  = #{}       :: #{refusal() => pos_integer()},
    logged_at = undefined :: undefined | integer(),
    flush     = undefined :: undefined | reference()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Create the table the sessions are kept in, owned by the calling
%% process. `macula_stream_sessions_keeper' creates it, so it outlives this
%% process.
-spec new_table() -> ets:table().
new_table() ->
    ets:new(?TABLE, [set, public, named_table]).

%% @doc Admit the session that `Stream' serves for the verified `Caller', or
%% refuse it when that caller or the node already holds as many sessions as
%% its cap allows. A stream already admitted keeps its one place.
-spec admit(binary(), pid()) -> ok | {error, refusal()}.
admit(Caller, Stream) when is_binary(Caller), is_pid(Stream) ->
    gen_server:call(?SERVER, {admit, Caller, Stream}).

%% @doc The number of served sessions the node holds now.
-spec sessions() -> non_neg_integer().
sessions() ->
    gen_server:call(?SERVER, sessions).

%% @doc The refusals since the macula application started, by reason.
-spec refusals() -> #{refusal() => pos_integer()}.
refusals() ->
    gen_server:call(?SERVER, refusals).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = count_kept_sessions(ets:match_object(?TABLE, {{stream, '_'}, '_'})),
    {ok, #state{}}.

handle_call({admit, Caller, Stream}, _From, S) ->
    on_admission(admission(ets:member(?TABLE, {stream, Stream}), Caller), Caller, Stream, S);
handle_call(sessions, _From, S) ->
    {reply, count(sessions), S};
handle_call(refusals, _From, S) ->
    {reply, maps:from_list(refusal_counts(ets:match_object(?TABLE, {{refused, '_'}, '_'}))), S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({'DOWN', _Mon, process, Stream, _Reason}, S) ->
    ok = release(ets:lookup(?TABLE, {stream, Stream})),
    {noreply, S};
handle_info(log_refusals, #state{unlogged = Unlogged} = S) when map_size(Unlogged) =:= 0 ->
    {noreply, S#state{flush = undefined}};
handle_info(log_refusals, S) ->
    {noreply, log_refusals(S#state{flush = undefined}, erlang:monotonic_time(millisecond))};
handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

%%%===================================================================
%%% Internal
%%%===================================================================

%% Counts the kept sessions anew from their stream rows and monitors each
%% stream again. A stream that ended while no counter ran frees its place
%% when its monitor fires, at once.
count_kept_sessions(Streams) ->
    true = ets:match_delete(?TABLE, {{caller, '_'}, '_'}),
    true = ets:insert(?TABLE, [{sessions, length(Streams)} | caller_rows(Streams)]),
    _ = [erlang:monitor(process, Stream) || {{stream, Stream}, _Caller} <- Streams],
    ok.

caller_rows(Streams) ->
    Counts = lists:foldl(fun({_Key, Caller}, Acc) -> bump(Caller, Acc) end, #{}, Streams),
    [{{caller, Caller}, N} || {Caller, N} <- maps:to_list(Counts)].

refusal_counts(Rows) ->
    [{Reason, N} || {{refused, Reason}, N} <- Rows].

admission(true, _Caller) ->
    admitted;
admission(false, Caller) ->
    admission_for(count(sessions) < limit(max_served_sessions, ?MAX_SESSIONS),
                  count({caller, Caller}) < limit(max_served_sessions_per_caller,
                                                  ?MAX_SESSIONS_PER_CALLER)).

admission_for(false, _CallerHasRoom) -> {error, node_limit};
admission_for(true, false)           -> {error, caller_limit};
admission_for(true, true)            -> ok.

limit(Key, Default) ->
    application:get_env(macula, Key, Default).

count(Key) ->
    ets:lookup_element(?TABLE, Key, 2, 0).

on_admission(ok, Caller, Stream, S) ->
    _ = erlang:monitor(process, Stream),
    true = ets:insert(?TABLE, [{{stream, Stream}, Caller},
                               {{caller, Caller}, count({caller, Caller}) + 1},
                               {sessions, count(sessions) + 1}]),
    {reply, ok, S};
on_admission(admitted, _Caller, _Stream, S) ->
    {reply, ok, S};
on_admission({error, Reason} = Refused, _Caller, _Stream, S) ->
    {reply, Refused, count_refusal(Reason, S)}.

%% A session's place frees when its stream process ends.
release([{{stream, _Stream} = Key, Caller}]) ->
    true = ets:delete(?TABLE, Key),
    true = ets:insert(?TABLE, {sessions, count(sessions) - 1}),
    one_less(count({caller, Caller}), Caller);
release([]) ->
    ok.

one_less(1, Caller) ->
    true = ets:delete(?TABLE, {caller, Caller}),
    ok;
one_less(N, Caller) ->
    true = ets:insert(?TABLE, {{caller, Caller}, N - 1}),
    ok.

bump(Key, Counts) ->
    maps:update_with(Key, fun(N) -> N + 1 end, 1, Counts).

count_refusal(Reason, #state{unlogged = Unlogged} = S) ->
    _ = ets:update_counter(?TABLE, {refused, Reason}, 1, {{refused, Reason}, 0}),
    report(S#state{unlogged = bump(Reason, Unlogged)}, erlang:monotonic_time(millisecond)).

%% Logs now when nothing was logged within the interval; otherwise holds the
%% refusals back for one timer that logs them when the interval has passed.
report(#state{logged_at = undefined} = S, Now) ->
    log_refusals(S, Now);
report(#state{logged_at = At} = S, Now) ->
    report_after(Now - At >= log_interval(), S, Now).

report_after(true, S, Now) ->
    log_refusals(S, Now);
report_after(false, #state{flush = undefined, logged_at = At} = S, Now) ->
    S#state{flush = erlang:send_after(max(0, At + log_interval() - Now), self(), log_refusals)};
report_after(false, S, _Now) ->
    S.

log_refusals(#state{unlogged = Unlogged, flush = Flush} = S, Now) ->
    ok = cancel(Flush),
    logger:warning("[macula_stream_sessions] served stream sessions refused since the last report: ~p",
                   [Unlogged]),
    S#state{unlogged = #{}, logged_at = Now, flush = undefined}.

cancel(undefined) -> ok;
cancel(Timer) -> _ = erlang:cancel_timer(Timer), ok.

log_interval() ->
    limit(served_session_refusal_log_interval_ms, ?REFUSAL_LOG_INTERVAL_MS).
