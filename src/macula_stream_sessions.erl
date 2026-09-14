%%%-------------------------------------------------------------------
%%% @doc The node's count of served stream sessions, and of the bytes their
%%% streams keep unread.
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
%%% A served stream charges the bytes of each chunk it queues unread with
%%% `charge/2', and gives them back with `release/2' when a reader takes the
%%% chunk. Both run in the stream's own process against the table, so no
%%% chunk waits on another process. A charge is refused when it would take
%%% the bytes of the stream's caller past `max_served_inbox_bytes_per_caller'
%%% (16 MiB by default) or the node's past `max_served_inbox_bytes' (256 MiB
%%% by default). The caller budget is one stream's own bound, so a caller
%%% with many sessions keeps no more unread than one session may; the node
%%% budget is sixteen callers' worth. What a stream still has charged when
%%% its process ends comes back then, through this process's monitor.
%%%
%%% A charge adds to the caller's count, then the node's, then the stream's
%%% own, and a release takes off in the opposite order; a refused add is
%%% taken back at once. So a stream that ends in the middle of one leaves at
%%% most that chunk counted too much, never too little, and this process
%%% clears what is left when the caller holds no session any more, and on
%%% the node when it holds none. Those clears run where a session ends, and
%%% a new session is admitted by this same process before its stream can
%%% charge anything.
%%%
%%% The sessions and bytes are kept in a table that
%%% `macula_stream_sessions_keeper' holds, so they outlive this process.
%%% When it starts again it counts the kept sessions anew and monitors their
%%% streams, and a stream that ended in between frees its place and its
%%% bytes at once.
%%%
%%% A refusal is counted by its reason: `caller_limit' or `node_limit' for a
%%% session, `caller_budget' or `node_budget' for a charge. A refusal is
%%% logged at once when no refusal was logged within the last
%%% `served_session_refusal_log_interval_ms'; the ones after it are logged
%%% together when that interval has passed.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_sessions).

-behaviour(gen_server).

-export([start_link/0, new_table/0, admit/2, sessions/0,
         charge/2, release/2, inbox_bytes/0, refusals/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export_type([refusal/0]).

-define(SERVER, ?MODULE).

%% The table's rows: `{{stream, Pid}, Caller}' for each admitted session,
%% `{{caller, Caller}, Count}' for each caller that holds a session,
%% `{sessions, Count}', `{{stream_bytes, Pid}, Bytes}',
%% `{{caller_bytes, Caller}, Bytes}', `{inbox_bytes, Bytes}', and
%% `{{refused, Reason}, Count}'.
-define(TABLE, macula_stream_sessions).

-define(MAX_SESSIONS_PER_CALLER, 16).
-define(MAX_SESSIONS, 1000).
-define(MAX_INBOX_BYTES_PER_CALLER, 16#1000000).
-define(MAX_INBOX_BYTES, 16#10000000).
-define(REFUSAL_LOG_INTERVAL_MS, 60000).
%% The counter only does table operations, so a link waits at most this long
%% for an admission before refusing it as `unavailable'.
-define(ADMIT_TIMEOUT_MS, 1000).

-type refusal() :: caller_limit | node_limit | caller_budget | node_budget.

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
%%
%% Admission fails closed and never holds up the link that asks: when this
%% process does not answer within a second, because it is restarting, gone
%% or held up, the session is refused as `unavailable'.
-spec admit(binary(), pid()) -> ok | {error, caller_limit | node_limit | unavailable}.
admit(Caller, Stream) when is_binary(Caller), is_pid(Stream) ->
    try gen_server:call(?SERVER, {admit, Caller, Stream}, ?ADMIT_TIMEOUT_MS)
    catch exit:{_Why, {gen_server, call, _Args}} -> {error, unavailable}
    end.

%% @doc The number of served sessions the node holds now.
-spec sessions() -> non_neg_integer().
sessions() ->
    gen_server:call(?SERVER, sessions).

%% @doc Charge `Bytes' that `Stream' keeps unread to its caller and the node,
%% or refuse when that would take either past its budget, or when `Stream' is
%% not an admitted session. Called in the stream's own process.
-spec charge(pid(), non_neg_integer()) -> ok | {error, caller_budget | node_budget | not_admitted}.
charge(Stream, Bytes) when is_pid(Stream), is_integer(Bytes), Bytes >= 0 ->
    charge_caller(ets:lookup(?TABLE, {stream, Stream}), Stream, Bytes).

%% @doc Give back `Bytes' that `Stream' charged and no longer keeps. Called in
%% the stream's own process.
-spec release(pid(), non_neg_integer()) -> ok.
release(Stream, Bytes) when is_pid(Stream), is_integer(Bytes), Bytes >= 0 ->
    release_caller(ets:lookup(?TABLE, {stream, Stream}), Stream, Bytes).

%% @doc The bytes the node's served streams keep unread now.
-spec inbox_bytes() -> integer().
inbox_bytes() ->
    ets:lookup_element(?TABLE, inbox_bytes, 2, 0).

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

handle_cast({charge_refused, Reason}, S) ->
    {noreply, count_refusal(Reason, S)};
handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({'DOWN', _Mon, process, Stream, _Reason}, S) ->
    ok = release_session(ets:lookup(?TABLE, {stream, Stream})),
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
%%% Internal: sessions
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

%% A session's bytes and place free when its stream process ends. The
%% stream's own count is taken first, so a restart in between never gives
%% the same bytes back twice.
release_session([{{stream, Stream} = Key, Caller}]) ->
    ok = give_back(Caller, taken_bytes(ets:take(?TABLE, {stream_bytes, Stream}))),
    true = ets:delete(?TABLE, Key),
    ok = one_less_session(count(sessions)),
    one_less(count({caller, Caller}), Caller);
release_session([]) ->
    ok.

taken_bytes([{_Key, Bytes}]) -> Bytes;
taken_bytes([])              -> 0.

give_back(Caller, Bytes) ->
    ok = add(inbox_bytes, -Bytes),
    add({caller_bytes, Caller}, -Bytes).

%% A node that holds no session keeps no unread bytes: what a stream that
%% ended in the middle of an update left counted is cleared with the last one.
one_less_session(1) ->
    true = ets:insert(?TABLE, {sessions, 0}),
    true = ets:delete(?TABLE, inbox_bytes),
    ok;
one_less_session(N) ->
    true = ets:insert(?TABLE, {sessions, N - 1}),
    ok.

%% A caller that holds no session keeps no unread bytes either.
one_less(1, Caller) ->
    true = ets:delete(?TABLE, {caller, Caller}),
    true = ets:delete(?TABLE, {caller_bytes, Caller}),
    ok;
one_less(N, Caller) ->
    true = ets:insert(?TABLE, {{caller, Caller}, N - 1}),
    ok.

bump(Key, Counts) ->
    maps:update_with(Key, fun(N) -> N + 1 end, 1, Counts).

%%%===================================================================
%%% Internal: the budget on unread bytes
%%%===================================================================

charge_caller([], _Stream, _Bytes) ->
    {error, not_admitted};
charge_caller([{_Key, Caller}], Stream, Bytes) ->
    charged_caller(add_within({caller_bytes, Caller}, Bytes,
                              limit(max_served_inbox_bytes_per_caller, ?MAX_INBOX_BYTES_PER_CALLER)),
                   Caller, Stream, Bytes).

charged_caller(refused, _Caller, _Stream, _Bytes) ->
    refuse_charge(caller_budget);
charged_caller(added, Caller, Stream, Bytes) ->
    charged_node(add_within(inbox_bytes, Bytes, limit(max_served_inbox_bytes, ?MAX_INBOX_BYTES)),
                 Caller, Stream, Bytes).

charged_node(refused, Caller, _Stream, Bytes) ->
    ok = add({caller_bytes, Caller}, -Bytes),
    refuse_charge(node_budget);
charged_node(added, _Caller, Stream, Bytes) ->
    add({stream_bytes, Stream}, Bytes).

%% Adds Bytes to the count at Key, and takes them back at once when that took
%% it past Max: for that moment the count is over by only the refused bytes.
add_within(Key, Bytes, Max) ->
    within(ets:update_counter(?TABLE, Key, {2, Bytes}, {Key, 0}) =< Max, Key, Bytes).

within(true, _Key, _Bytes) ->
    added;
within(false, Key, Bytes) ->
    ok = add(Key, -Bytes),
    refused.

add(Key, Bytes) ->
    _ = ets:update_counter(?TABLE, Key, {2, Bytes}, {Key, 0}),
    ok.

%% This process counts a refused charge and logs it with the other refusals.
refuse_charge(Reason) ->
    ok = gen_server:cast(?SERVER, {charge_refused, Reason}),
    {error, Reason}.

release_caller([], _Stream, _Bytes) ->
    ok;
release_caller([{_Key, Caller}], Stream, Bytes) ->
    ok = add({stream_bytes, Stream}, -Bytes),
    ok = add(inbox_bytes, -Bytes),
    add({caller_bytes, Caller}, -Bytes).

%%%===================================================================
%%% Internal: refusals
%%%===================================================================

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
