%%%-------------------------------------------------------------------
%%% @doc The node's count of served stream sessions.
%%%
%%% A station link asks `admit/2' before it serves a session it has
%%% started. The session is admitted while its link serves fewer than
%%% `max_served_sessions_per_link' sessions and the node fewer than
%%% `max_served_sessions', both read from the macula application env when
%%% it asks. An admitted session holds its place until its stream process
%%% ends, however it ends: this process monitors it.
%%%
%%% A refusal is counted by its reason, `link_limit' or `node_limit'. A
%%% refusal is logged at once when no refusal was logged within the last
%%% `served_session_refusal_log_interval_ms'; the ones after it are logged
%%% together when that interval has passed.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_sessions).

-behaviour(gen_server).

-export([start_link/0, admit/2, sessions/0, refusals/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export_type([refusal/0]).

-define(SERVER, ?MODULE).

%% A dedicated stream carries one session, so a link's default cap is the
%% number of concurrent streams QUIC lets a peer open by default.
-define(MAX_SESSIONS_PER_LINK, 100).
-define(MAX_SESSIONS, 1000).
-define(REFUSAL_LOG_INTERVAL_MS, 60000).

-type refusal() :: link_limit | node_limit.

-record(state, {
    %% The admitted sessions: stream pid => {link pid, monitor}.
    streams   = #{}       :: #{pid() => {pid(), reference()}},
    per_link  = #{}       :: #{pid() => pos_integer()},
    total     = 0         :: non_neg_integer(),
    refused   = #{}       :: #{refusal() => pos_integer()},
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

%% @doc Admit the session that `Stream' serves on `Link', or refuse it when
%% the link or the node already serves as many sessions as its cap allows.
-spec admit(pid(), pid()) -> ok | {error, refusal()}.
admit(Link, Stream) when is_pid(Link), is_pid(Stream) ->
    gen_server:call(?SERVER, {admit, Link, Stream}).

%% @doc The number of served sessions the node holds now.
-spec sessions() -> non_neg_integer().
sessions() ->
    gen_server:call(?SERVER, sessions).

%% @doc The refusals since this process started, by reason.
-spec refusals() -> #{refusal() => pos_integer()}.
refusals() ->
    gen_server:call(?SERVER, refusals).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({admit, Link, Stream}, _From, S) ->
    on_admission(admission(Link, S), Link, Stream, S);
handle_call(sessions, _From, #state{total = Total} = S) ->
    {reply, Total, S};
handle_call(refusals, _From, #state{refused = Refused} = S) ->
    {reply, Refused, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({'DOWN', Mon, process, Stream, _Reason}, #state{streams = Streams} = S) ->
    {noreply, release(maps:find(Stream, Streams), Mon, Stream, S)};
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

admission(Link, #state{total = Total, per_link = PerLink}) ->
    admission_for(Total < limit(max_served_sessions, ?MAX_SESSIONS),
                  maps:get(Link, PerLink, 0) < limit(max_served_sessions_per_link,
                                                     ?MAX_SESSIONS_PER_LINK)).

admission_for(false, _LinkHasRoom) -> {error, node_limit};
admission_for(true, false)         -> {error, link_limit};
admission_for(true, true)          -> ok.

limit(Key, Default) ->
    application:get_env(macula, Key, Default).

on_admission(ok, Link, Stream, #state{streams = Streams, per_link = PerLink, total = Total} = S) ->
    Mon = erlang:monitor(process, Stream),
    {reply, ok, S#state{streams  = Streams#{Stream => {Link, Mon}},
                        per_link = bump(Link, PerLink),
                        total    = Total + 1}};
on_admission({error, Reason} = Refused, _Link, _Stream, S) ->
    {reply, Refused, count_refusal(Reason, S)}.

%% A session's place frees when its stream process ends.
release({ok, {Link, Mon}}, Mon, Stream,
        #state{streams = Streams, per_link = PerLink, total = Total} = S) ->
    S#state{streams  = maps:remove(Stream, Streams),
            per_link = one_less(maps:get(Link, PerLink), Link, PerLink),
            total    = Total - 1};
release(_NotAdmitted, _Mon, _Stream, S) ->
    S.

one_less(1, Link, PerLink) -> maps:remove(Link, PerLink);
one_less(N, Link, PerLink) -> PerLink#{Link => N - 1}.

bump(Key, Counts) ->
    maps:update_with(Key, fun(N) -> N + 1 end, 1, Counts).

count_refusal(Reason, #state{refused = Refused, unlogged = Unlogged} = S) ->
    report(S#state{refused = bump(Reason, Refused), unlogged = bump(Reason, Unlogged)},
           erlang:monotonic_time(millisecond)).

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
