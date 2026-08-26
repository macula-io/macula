%%%-------------------------------------------------------------------
%%% @doc Addressable content-store put/get, with a real, peer-visible
%%% abort, real pause/resume, and (for chunked content) parallel
%%% multi-stream chunk transfer — the foundation `macula_feeder'/
%%% `macula_download' (and, later, `macula_pusher'/`macula_upload')
%%% build on. See PLAN_PUSH_UPLOAD.md, Phases 1-3.
%%%
%%% `macula:put_content/2'/`get_content/2' are ONE opaque blocking
%%% call each: pick a link, open a dedicated content stream, run the
%%% transfer, close the stream — no handle exists mid-transfer, so
%%% cancelling meant killing whatever process happened to be blocked
%%% in the call. That killed the caller's wait, but never touched the
%%% underlying stream: `macula_station_link' — not the killed process
%%% — owns the stream's `content_stream_bufs'/`content_pending' state,
%%% so a `terminate/2' that never runs (because the blocking call's
%%% own process was `exit(_, kill)'d mid-`gen_server:call') leaked
%%% that state on the link forever, cleaned up only by the eventual
%%% `content_call_timeout' firing against an already-dead caller.
%%%
%%% This module owns the picked link and the open stream itself, in
%%% its own gen_server state, updated as soon as a worker process
%%% resolves them — so `cancel/1,3' can always reach in and tear the
%%% stream down explicitly, from any point in the transfer's
%%% lifecycle, with nothing left to time out.
%%%
%%% == Cancel is a real abort, not a dropped connection ==
%%%
%%% `cancel/3' resets every currently-open content stream's send side
%%% via `macula_quic:reset_stream/2' (through `macula_station_link:
%%% abort_content_stream/4') — a QUIC RESET_STREAM frame the PEER's
%%% own read genuinely observes as `{quic, stream_closed, PeerStream,
%%% {reset, Code}}', not merely a connection that went away. This is
%%% NOT `macula_stream:abort/3' (streaming RPC's abort) — that targets
%%% a `macula_stream' gen_server's own STREAM_ERROR application
%%% framing, and a content-transfer stream is not one of those; it is
%%% a raw QUIC dedicated stream owned directly by `macula_station_link'
%%% (see that module's `open_content_stream/1'). The two "stream"
%%% concepts share a name and nothing else — do not reuse
%%% `macula_stream:abort/3' here.
%%%
%%% == Lifecycle ==
%%%
%%% `start_put/2,3', `start_put_station/4,5', `start_get/2,3',
%%% `start_get_station/4,5' return `{ok, Pid}' immediately; the
%%% resolve/dial sequence runs in a linked worker. `await/1,2' blocks
%%% for the outcome (`{ok, Mcid}' / `{ok, Bytes}' / `{error, Reason}')
%%% — repeatable and from any process; the result is cached once
%%% known. The process does NOT self-terminate on completion (a second
%%% `await/1' after success must still answer) — call `cancel/1' when
%%% done with the handle, whether the transfer succeeded, failed, or
%%% is still in flight; on an already-resolved transfer this is a pure
%%% reap (nothing left to abort).
%%%
%%% == Pause/resume (chunked content only) ==
%%%
%%% Single-block content is one wire round trip — there is no "between
%%% chunks" for it to pause at, so `pause/1' on a single-block transfer
%%% is a harmless no-op (the transfer just runs to completion). For
%%% chunked content, each chunk's own put/get is still ONE uninterrupted
%%% blocking call underneath (pausing mid-chunk would leave a half-sent
%%% block the station can't verify) — what `pause/1' actually controls
%%% is whether the NEXT chunk starts, on EVERY stream, once whichever
%%% chunk is currently in flight on it finishes. `resume/1' re-arms
%%% every stream from exactly its own next un-sent/un-fetched chunk,
%%% never from the start. `cancel/1,3' still works at any point, paused
%%% or not — every stream with a chunk step in flight has it killed and
%%% reset exactly as described above; a stream idle between chunks
%%% (nothing in flight) just gets its stream reset directly.
%%%
%%% == Multi-stream parallel chunk transfer (chunked content only) ==
%%%
%%% Chunks are distributed round-robin (`Index rem StreamCount') across
%%% up to `stream_count' dedicated content streams on the SAME link
%%% (`Opts''s `stream_count' key, default 4, capped at the actual chunk
%%% count so a 2-chunk transfer never opens more than 2 streams) —
%%% each stream runs its own independent chunk-by-chunk loop
%%% concurrently, all driven by this ONE gen_server via
%%% `handle_continue/2' (never by the streams' own worker processes,
%%% which each do exactly one network call and report back). The
%%% manifest is put (or, for a get, its chunks reassembled and
%%% verified) only once every stream has drained its own share. A get
%%% doesn't know the chunk count — and therefore how many streams are
%%% worth opening — until its manifest is fetched, so it starts on the
%%% ONE stream the connect step already opened and expands to more once
%%% the count is known; a put knows upfront and opens every extra
%%% stream immediately. If opening an extra stream fails, this degrades
%%% gracefully to fewer streams rather than failing the transfer — a
%%% single-stream transfer is still a correct, if slower, one. A single
%%% stream's own chunk failing (a genuine `{error, _}', not a crash)
%%% fails the WHOLE transfer, same as a sequential transfer would —
%%% every other stream's in-flight work is killed and every stream
%%% reset before the caller's `await/1,2' sees the error.
%%%
%%% == Correlation-id registry ==
%%%
%%% Each transfer mints a `share_id' (`crypto:strong_rand_bytes(16)',
%%% overridable via `Opts''s `share_id' key so a wrapper that already
%%% publishes it in a `sharing.*_started_v1' mesh fact — see
%%% `macula_feeder'/`macula_download' — can keep the same id) and
%%% registers it in `macula_content_transfer_registry', so a caller
%%% that only saw the id in a published fact, not the pid, can still
%%% resolve it to `cancel/1,3'.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_transfer).

-behaviour(gen_server).

-export([start_put/2, start_put/3,
         start_put_station/4, start_put_station/5,
         start_get/2, start_get/3,
         start_get_station/4, start_get_station/5]).
-export([await/1, await/2, cancel/1, cancel/3, pause/1, resume/1, share_id/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).

-ifdef(TEST).
-export([verify_block_hash/2]).
-endif.

-define(CONTENT_REALM,                 <<0:256>>).
-define(CONTENT_PUT_BLOCK_PROC,        <<"_content.put_block">>).
-define(CONTENT_GET_BLOCK_PROC,        <<"_content.get_block">>).
-define(CONTENT_PUT_MANIFEST_PROC,     <<"_content.put_manifest">>).
-define(CONTENT_GET_MANIFEST_PROC,     <<"_content.get_manifest">>).
%% Bigger timeout than DHT records — chunks are 256 KiB and a put
%% writes through the file-backed store on the relay.
-define(CONTENT_BLOCK_TIMEOUT_MS,      15_000).
-define(CONTENT_MANIFEST_TIMEOUT_MS,   5_000).
-define(CONTENT_RETRY_BACKOFF_MS,      200).
%% Default parallelism for a chunked transfer's streams — see the
%% moduledoc's "Multi-stream" section. Overridable per transfer via
%% `Opts''s `stream_count' key; always clamped to at least 1 and at
%% most the actual chunk count regardless of what's requested.
-define(DEFAULT_STREAM_COUNT, 4).

-type kind() :: put | get.
-type dial() :: {pooled, macula:pool()}
              | {station, macula:pool(), macula_client:seed(), pos_integer(), map()}.

%% One dedicated content stream's own independent chunk-by-chunk queue.
%% `remaining' holds items not yet dispatched (PUT: `{Index, Bytes}';
%% GET: plain `Index'); `in_flight' is the ONE item currently out for
%% its network round trip (or `undefined'), tracked separately rather
%% than left at the head of `remaining' so a step's outcome doesn't
%% need to echo its own item back — the gen_server already knows what
%% it dispatched. A lane is done for good once `remaining = []',
%% `in_flight = undefined', `worker = undefined'.
-record(lane, {
    stream    :: reference(),
    remaining :: [term()],
    in_flight :: term() | undefined,
    worker    :: pid() | undefined
}).

%% Chunked-transfer driving state — `undefined' in `#state.chunk' for a
%% single-block transfer (nothing to drive between chunks) and for a
%% chunked one until its content stream is open.
%%
%% put: `manifest' is known upfront (pure, `macula_manifest:create/1',
%% no network) and so are all `lanes' — every chunk is assigned to one
%% immediately (see `content_link_chunked/4'). `lanes' shrinking to "all
%% empty and idle" (`lane_done/1') triggers the final put_manifest step.
%%
%% get: `manifest' and `lanes' both start `undefined' — fetching the
%% manifest (on the one stream the connect step already opened) IS the
%% first step; only once it's back, and `chunk_count' with it, can
%% `lanes' be set up and chunks distributed. `acc' accumulates fetched
%% chunks keyed by index (not arrival order — different lanes finish in
%% whatever order their own network calls happen to complete in), so
%% reassembly reads it back out in the correct 0..chunk_count-1 order
%% regardless of which lane delivered which chunk first.
-record(chunk, {
    manifest    :: map() | undefined,
    lanes       :: [#lane{}] | undefined,
    chunk_count :: non_neg_integer() | undefined,
    acc         :: #{non_neg_integer() => binary()}
}).

-record(state, {
    kind         :: kind(),
    payload      :: binary(),         % put: Bytes; get: Mcid
    share_id     :: binary(),
    link_pid     :: pid() | undefined,
    stream       :: reference() | undefined,
    worker       :: pid() | undefined,
    result       :: {ok, term()} | {error, term()} | undefined,
    waiters      :: [gen_server:from()],
    paused       :: boolean(),
    chunk        :: #chunk{} | undefined,
    stream_count :: pos_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start an addressable put through the pool's own connected
%% link (whichever `macula_client:pick_connected_link/1' picks).
-spec start_put(macula:pool(), binary()) -> {ok, pid()}.
start_put(Pool, Bytes) -> start_put(Pool, Bytes, #{}).

%% @doc As `start_put/2'. `Opts' may carry `share_id' (binary,
%% overrides the minted default) and `stream_count' (positive integer,
%% default 4 — see the moduledoc's "Multi-stream" section; irrelevant
%% for a single-block transfer).
-spec start_put(macula:pool(), binary(), map()) -> {ok, pid()}.
start_put(Pool, Bytes, Opts) when is_pid(Pool), is_binary(Bytes), is_map(Opts) ->
    gen_server:start_link(?MODULE, {put, {pooled, Pool}, Bytes, Opts}, []).

%% @doc As `start_put/2', dialing `Station' directly (reusing a live
%% link or dialing + waiting up to `TimeoutMs' for one) instead of
%% picking from the pool's existing links — the addressable
%% counterpart to `macula:put_content_station/4'.
-spec start_put_station(macula:pool(), macula_client:seed(), binary(),
                        pos_integer()) -> {ok, pid()}.
start_put_station(Pool, Station, Bytes, TimeoutMs) ->
    start_put_station(Pool, Station, Bytes, TimeoutMs, #{}).

%% @doc As `start_put_station/4'. `Opts' may carry `share_id',
%% `stream_count' (see `start_put/3') plus a per-call TLS trust
%% override for this dial — `verify', `expected_node_id',
%% `pin_tls_cert' (see `macula:put_content_station/5').
-spec start_put_station(macula:pool(), macula_client:seed(), binary(),
                        pos_integer(), map()) -> {ok, pid()}.
start_put_station(Pool, Station, Bytes, TimeoutMs, Opts)
  when is_pid(Pool), is_binary(Bytes), is_integer(TimeoutMs), TimeoutMs > 0,
       is_map(Opts) ->
    LinkOpts = maps:with([verify, expected_node_id, pin_tls_cert], Opts),
    gen_server:start_link(?MODULE,
        {put, {station, Pool, Station, TimeoutMs, LinkOpts}, Bytes, Opts}, []).

%% @doc Start an addressable get through the pool's own connected
%% link. See `macula:get_content/2'.
-spec start_get(macula:pool(), macula:mcid()) -> {ok, pid()}.
start_get(Pool, Mcid) -> start_get(Pool, Mcid, #{}).

%% @doc As `start_get/2'. `Opts' may carry `share_id' and
%% `stream_count' (see `start_put/3').
-spec start_get(macula:pool(), macula:mcid(), map()) -> {ok, pid()}.
start_get(Pool, Mcid, Opts) when is_pid(Pool), is_binary(Mcid), is_map(Opts) ->
    gen_server:start_link(?MODULE, {get, {pooled, Pool}, Mcid, Opts}, []).

%% @doc As `start_get/2', dialing `Station' directly — the addressable
%% counterpart to `macula:get_content_station/4'.
-spec start_get_station(macula:pool(), macula_client:seed(), macula:mcid(),
                        pos_integer()) -> {ok, pid()}.
start_get_station(Pool, Station, Mcid, TimeoutMs) ->
    start_get_station(Pool, Station, Mcid, TimeoutMs, #{}).

%% @doc As `start_get_station/4'. `Opts' as `start_put_station/5'.
-spec start_get_station(macula:pool(), macula_client:seed(), macula:mcid(),
                        pos_integer(), map()) -> {ok, pid()}.
start_get_station(Pool, Station, Mcid, TimeoutMs, Opts)
  when is_pid(Pool), is_binary(Mcid), is_integer(TimeoutMs), TimeoutMs > 0,
       is_map(Opts) ->
    LinkOpts = maps:with([verify, expected_node_id, pin_tls_cert], Opts),
    gen_server:start_link(?MODULE,
        {get, {station, Pool, Station, TimeoutMs, LinkOpts}, Mcid, Opts}, []).

%% @doc Block for the transfer's outcome: `{ok, Mcid}' (put),
%% `{ok, Bytes}' (get), or `{error, Reason}'. Safe to call more than
%% once, from more than one process, before or after the result is
%% known.
-spec await(pid()) -> {ok, term()} | {error, term()}.
await(Pid) -> await(Pid, infinity).

%% @doc As `await/1' with an explicit timeout on THIS call only — a
%% timeout here does not cancel the transfer itself.
-spec await(pid(), timeout()) -> {ok, term()} | {error, term()}.
await(Pid, Timeout) -> gen_server:call(Pid, await, Timeout).

%% @doc As `cancel/3' with a default code/message.
-spec cancel(pid()) -> ok.
cancel(Pid) -> cancel(Pid, 0, <<"cancelled">>).

%% @doc Cancel `Pid''s transfer and reap the process. Resets every
%% currently-open content stream with `Code' — genuinely peer-visible,
%% see the moduledoc. `Message' is local-only (logged at the link; QUIC
%% RESET_STREAM carries only the numeric code on the wire). If the
%% transfer already resolved (success or failure), this is a pure reap
%% — nothing left to abort. Either way `await/1,2' answers
%% `{error, cancelled}' to anyone still waiting.
-spec cancel(pid(), non_neg_integer(), binary()) -> ok.
cancel(Pid, Code, Message)
  when is_integer(Code), Code >= 0, is_binary(Message) ->
    gen_server:call(Pid, {cancel, Code, Message}).

%% @doc Pause a chunked transfer between chunks, on every stream — each
%% stream's in-flight chunk (if any) still completes, no stream closes,
%% but none of them starts its next chunk until `resume/1'. A no-op on
%% a single-block transfer or one that has already resolved (nothing to
%% pause either way).
-spec pause(pid()) -> ok.
pause(Pid) -> gen_server:call(Pid, pause).

%% @doc Resume a transfer paused via `pause/1' — every stream continues
%% from exactly its own next un-sent/un-fetched chunk, never from the
%% start. A no-op if not actually paused, not chunked, or already
%% resolved.
-spec resume(pid()) -> ok.
resume(Pid) -> gen_server:call(Pid, resume).

%% @doc This transfer's `share_id', for publishing in a mesh fact or
%% looking itself up later via `macula_content_transfer_registry'.
-spec share_id(pid()) -> binary().
share_id(Pid) -> gen_server:call(Pid, share_id).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Kind, Dial, Payload, Opts}) ->
    process_flag(trap_exit, true),
    ShareId = maps:get(share_id, Opts, crypto:strong_rand_bytes(16)),
    StreamCount = maps:get(stream_count, Opts, ?DEFAULT_STREAM_COUNT),
    ok = macula_content_transfer_registry:register_share(ShareId, self()),
    Self = self(),
    Worker = spawn_link(fun() -> connect_and_run(Self, Kind, Dial, Payload) end),
    {ok, #state{kind = Kind, payload = Payload, share_id = ShareId,
               worker = Worker, waiters = [], paused = false, chunk = undefined,
               stream_count = StreamCount}}.

%% @private
handle_call(await, From, #state{result = undefined, waiters = Waiters} = State) ->
    {noreply, State#state{waiters = [From | Waiters]}};
handle_call(await, _From, #state{result = Result} = State) ->
    {reply, Result, State};
handle_call(share_id, _From, #state{share_id = Id} = State) ->
    {reply, Id, State};
handle_call(pause, _From, State) ->
    {reply, ok, State#state{paused = true}};
handle_call(resume, _From, #state{paused = true, chunk = Chunk, result = undefined} = State)
        when Chunk =/= undefined ->
    {reply, ok, State#state{paused = false}, {continue, next_step}};
handle_call(resume, _From, State) ->
    {reply, ok, State#state{paused = false}};
handle_call({cancel, _Code, _Message}, _From, #state{result = Result} = State)
        when Result =/= undefined ->
    {stop, normal, ok, State};
handle_call({cancel, Code, Message}, _From,
            #state{worker = Worker, link_pid = LinkPid, stream = Stream,
                  chunk = Chunk, waiters = Waiters} = State) ->
    kill_worker(Worker),
    kill_lane_workers(Chunk),
    abort_all_streams(LinkPid, Stream, Chunk, Code, Message),
    [gen_server:reply(From, {error, cancelled}) || From <- Waiters],
    {stop, normal, ok, State#state{result = {error, cancelled}, waiters = []}};
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
%% The connect worker's link/stream — for a single-block transfer it
%% keeps running (unchanged since Phase 1: transfers, closes, reports
%% `content_result' itself). For chunked content it stops here and
%% hands off: this process starts driving the chunk-by-chunk loop(s).
handle_info({content_link, LinkPid, Stream}, #state{kind = Kind, payload = Payload} = State) ->
    NewState = State#state{link_pid = LinkPid, stream = Stream},
    case is_chunked(Kind, Payload) of
        true  -> content_link_chunked(Kind, LinkPid, Stream, Payload, NewState);
        false -> {noreply, NewState}
    end;
%% Single-block path only (chunked finalizes via `finalize/2' instead).
handle_info({content_result, Result}, #state{waiters = Waiters} = State) ->
    [gen_server:reply(From, Result) || From <- Waiters],
    {noreply, State#state{result = Result, waiters = []}};
%% One lane's step outcome — identified by which stream reported it,
%% since (unlike Phase 1-2) more than one step can be in flight at
%% once, one per open stream.
handle_info({lane_step_result, Stream, Outcome}, #state{chunk = #chunk{lanes = Lanes}} = State)
        when Lanes =/= undefined ->
    lane_result(lists:keyfind(Stream, #lane.stream, Lanes), Outcome, State);
%% A single (non-lane) step's outcome — the get-manifest-fetch
%% bootstrap, or the put-manifest finalize. There is only ever one of
%% these in flight at a time.
handle_info({step_result, Outcome}, #state{worker = Worker} = State)
        when is_pid(Worker) ->
    step_result(Outcome, State#state{worker = undefined});
handle_info({'EXIT', Worker, Reason}, #state{result = undefined} = State)
        when Reason =/= normal ->
    exit_from_tracked_worker(is_tracked_worker(Worker, State), Reason, State);
handle_info(_Msg, State) ->
    {noreply, State}.

exit_from_tracked_worker(true, Reason, State) ->
    {stop, {worker_crashed, Reason}, State};
exit_from_tracked_worker(false, _Reason, State) ->
    {noreply, State}.

is_tracked_worker(Worker, #state{worker = Worker}) ->
    true;
is_tracked_worker(Worker, #state{chunk = #chunk{lanes = Lanes}}) when Lanes =/= undefined ->
    lists:keymember(Worker, #lane.worker, Lanes);
is_tracked_worker(_Worker, _State) ->
    false.

%% @private
handle_continue(next_step, #state{paused = true} = State) ->
    {noreply, State};
handle_continue(next_step, #state{chunk = #chunk{lanes = undefined}} = State) ->
    %% Only a get reaches this: its manifest isn't fetched yet, so
    %% there's nothing to distribute across streams yet either.
    dispatch_get_manifest_step(State);
handle_continue(next_step, State) ->
    dispatch_lanes_or_finish(State).

kill_worker(Worker) when is_pid(Worker) ->
    unlink(Worker),
    exit(Worker, kill);
kill_worker(undefined) ->
    ok.

kill_lane_workers(undefined) -> ok;
kill_lane_workers(#chunk{lanes = undefined}) -> ok;
kill_lane_workers(#chunk{lanes = Lanes}) ->
    lists:foreach(fun(#lane{worker = W}) -> kill_worker(W) end, Lanes).

abort_stream_if_open(LinkPid, Stream, Code, Message)
  when is_pid(LinkPid), is_reference(Stream) ->
    macula_station_link:abort_content_stream(LinkPid, Stream, Code, Message);
abort_stream_if_open(_LinkPid, _Stream, _Code, _Message) ->
    ok.

abort_all_streams(LinkPid, PrimalStream, undefined, Code, Message) ->
    abort_stream_if_open(LinkPid, PrimalStream, Code, Message);
abort_all_streams(LinkPid, PrimalStream, #chunk{lanes = undefined}, Code, Message) ->
    abort_stream_if_open(LinkPid, PrimalStream, Code, Message);
abort_all_streams(LinkPid, _PrimalStream, #chunk{lanes = Lanes}, Code, Message) ->
    lists:foreach(fun(#lane{stream = S}) ->
        abort_stream_if_open(LinkPid, S, Code, Message)
    end, Lanes).

%%%===================================================================
%%% Connect worker — resolve link, open the FIRST content stream. A
%%% single-block transfer keeps running in this same process; a
%%% chunked one hands off to the gen_server as soon as `content_link'
%%% is sent (see `handle_info/2' above) and this function returns
%%% right after — any additional streams a chunked transfer wants are
%%% opened by the gen_server itself (see `content_link_chunked/4' /
%%% `open_extra_streams/2'), not by this worker.
%%%===================================================================

-spec connect_and_run(pid(), kind(), dial(), binary()) -> term().
connect_and_run(Parent, Kind, Dial, Payload) ->
    case connect(Dial) of
        {ok, LinkPid, Stream} ->
            Parent ! {content_link, LinkPid, Stream},
            run_if_single_block(is_chunked(Kind, Payload), Parent, Kind, LinkPid, Stream, Payload);
        {error, _} = E ->
            Parent ! {content_result, E}
    end.

run_if_single_block(true, _Parent, _Kind, _LinkPid, _Stream, _Payload) ->
    ok;
run_if_single_block(false, Parent, Kind, LinkPid, Stream, Payload) ->
    Result = transfer(Kind, LinkPid, Stream, Payload),
    catch macula_station_link:close_content_stream(LinkPid, Stream),
    Parent ! {content_result, Result}.

connect({pooled, Pool}) ->
    open_on_link(macula_client:pick_connected_link(Pool));
connect({station, Pool, Station, TimeoutMs, LinkOpts}) ->
    open_on_link(macula_client:ensure_content_link(Pool, Station, LinkOpts, TimeoutMs)).

open_on_link({error, _} = E) -> E;
open_on_link({ok, LinkPid}) ->
    stream_opened(macula_station_link:open_content_stream(LinkPid), LinkPid).

stream_opened({ok, Stream}, LinkPid) -> {ok, LinkPid, Stream};
stream_opened({error, _} = E, _LinkPid) -> E.

%% @doc Known upfront, no network needed: put by size against the
%% chunk threshold, get by the MCID's own codec byte.
is_chunked(put, Bytes) -> byte_size(Bytes) > macula_manifest:default_chunk_size();
is_chunked(get, <<1, 16#56, _/binary>>) -> true;
is_chunked(get, <<1, 16#55, _/binary>>) -> false.

transfer(put, LinkPid, Stream, Bytes) -> put_single_block(LinkPid, Stream, Bytes);
transfer(get, LinkPid, Stream, Mcid)  -> get_single_block(LinkPid, Stream, Mcid).

%%%===================================================================
%%% Single block — one wire round trip, runs entirely in the connect
%%% worker exactly as Phase 1 shipped it. Untouched by Phases 2-3:
%%% there is no "between chunks" or "another stream" for either to mean
%%% anything here.
%%%===================================================================

put_single_block(LinkPid, Stream, Bytes) ->
    Hash = macula_blake3_nif:hash(Bytes),
    MCID = <<1, 16#55, Hash/binary>>,
    classify_put_content(put_block(LinkPid, Stream, MCID, Bytes), MCID).

put_block(LinkPid, Stream, MCID, Bytes) ->
    call_on_stream_with_retry(LinkPid, Stream, ?CONTENT_PUT_BLOCK_PROC,
                              #{mcid => MCID, payload => Bytes},
                              ?CONTENT_BLOCK_TIMEOUT_MS).

classify_put_content({ok, ok},                MCID) -> {ok, MCID};
classify_put_content({ok, hash_mismatch},     _MCID) -> {error, hash_mismatch};
classify_put_content({ok, Reply},             _MCID) -> {error, {unexpected_reply, Reply}};
classify_put_content({error, _} = E,          _MCID) -> E.

get_single_block(LinkPid, Stream, MCID) ->
    classify_get_content(get_block(LinkPid, Stream, MCID), MCID).

get_block(LinkPid, Stream, MCID) ->
    call_on_stream_with_retry(LinkPid, Stream, ?CONTENT_GET_BLOCK_PROC,
                              #{mcid => MCID}, ?CONTENT_BLOCK_TIMEOUT_MS).

classify_get_content({ok, not_found}, _MCID)        -> {error, not_found};
classify_get_content({ok, Bin}, MCID) when is_binary(Bin) ->
    verify_block_hash(MCID, Bin);
classify_get_content({ok, Reply}, _MCID)            -> {error, {unexpected_reply, Reply}};
classify_get_content({error, _} = E, _MCID)         -> E.

%% The station verified this block's hash at PUT time; a station
%% fetched FROM (especially via `start_get_station/5', deliberately
%% dialing a caller-chosen peer) is not necessarily the one that stored
%% it, so re-verify client-side rather than trusting whoever answered.
%% Chunked content gets the equivalent check from `macula_manifest:
%% verify/2' over the reassembled whole (see `dispatch_terminal_step/1''s
%% get clause below); single-block content had no client-side check at
%% all before this (fixed pre-Phase-1, carried forward here unchanged
%% — see `macula_content_block_hash_tests').
-spec verify_block_hash(macula:mcid(), binary()) ->
        {ok, binary()} | {error, hash_mismatch | invalid_mcid}.
verify_block_hash(<<1, 16#55, Hash:32/binary>>, Bin) ->
    hash_result(macula_blake3_nif:hash(Bin) =:= Hash, Bin);
verify_block_hash(_MCID, _Bin) ->
    {error, invalid_mcid}.

hash_result(true, Bin)   -> {ok, Bin};
hash_result(false, _Bin) -> {error, hash_mismatch}.

%%%===================================================================
%%% Chunked — driven step by step by THIS process (the gen_server), not
%%% the connect worker. A get starts on the ONE stream connect already
%%% opened (fetching the manifest — see `dispatch_get_manifest_step/1')
%%% since the chunk count isn't known before that; a put knows its
%%% chunk count upfront and sets up every stream immediately (see
%%% `content_link_chunked/4'). Once streams — "lanes" — exist, each
%%% runs its own independent chunk queue: `dispatch_lanes_or_finish/1'
%%% starts a worker for any idle lane that still has work, one short-
%%% lived worker per single network call, so `cancel/3' can always
%%% kill whichever ones are in flight and `pause'/`resume' (checked
%%% right here, uniformly, whether there's one lane or several) gate
%%% every lane the same way. Once every lane is empty and idle, the one
%%% remaining step — put the manifest, or (pure, no network)
%%% reassemble+verify a get — runs and finalizes the transfer.
%%% PLAN_PUSH_UPLOAD.md Phases 2-3.
%%%===================================================================

content_link_chunked(put, LinkPid, Stream, Bytes, #state{stream_count = DesiredN} = State) ->
    {ok, Manifest, Chunks} = macula_manifest:create(Bytes),
    Lanes = setup_put_lanes(LinkPid, Stream, DesiredN, Chunks),
    Chunk = #chunk{manifest = Manifest, lanes = Lanes, chunk_count = undefined, acc = #{}},
    {noreply, State#state{worker = undefined, chunk = Chunk}, {continue, next_step}};
content_link_chunked(get, _LinkPid, _Stream, _Mcid, State) ->
    %% Manifest not known yet — fetching it (on the one stream we have
    %% so far) IS the first step; lanes get set up once it's back (see
    %% `get_manifest_result/2').
    Chunk = #chunk{manifest = undefined, lanes = undefined, chunk_count = undefined, acc = #{}},
    {noreply, State#state{chunk = Chunk}, {continue, next_step}}.

setup_put_lanes(LinkPid, Stream0, DesiredN, Chunks) ->
    N = max(1, min(DesiredN, length(Chunks))),
    Streams = [Stream0 | open_extra_streams(LinkPid, N - 1)],
    Items = lists:zip(lists:seq(0, length(Chunks) - 1), Chunks),
    distribute_lanes(Streams, Items, fun({Index, _Bytes}) -> Index end).

setup_get_lanes(LinkPid, Stream0, DesiredN, ChunkCount) ->
    N = max(1, min(DesiredN, ChunkCount)),
    Streams = [Stream0 | open_extra_streams(LinkPid, N - 1)],
    Items = lists:seq(0, ChunkCount - 1),
    distribute_lanes(Streams, Items, fun(Index) -> Index end).

%% Opening an extra stream is a local QUIC operation on an already-live
%% connection (allocate a stream id, no peer round trip) — fast and
%% ordinarily infallible, but if one DOES fail this degrades to fewer
%% streams rather than failing the whole transfer over it.
open_extra_streams(_LinkPid, N) when N =< 0 -> [];
open_extra_streams(LinkPid, N) ->
    lists:filtermap(fun(_) -> try_open_content_stream(LinkPid) end, lists:seq(1, N)).

try_open_content_stream(LinkPid) ->
    case macula_station_link:open_content_stream(LinkPid) of
        {ok, S} -> {true, S};
        {error, _} -> false
    end.

%% Round-robin `Items' across `Streams' by `KeyFun(Item) rem length(Streams)',
%% each lane's own queue kept in ascending original order.
distribute_lanes(Streams, Items, KeyFun) ->
    NumStreams = length(Streams),
    Grouped = lists:foldl(fun(Item, Acc) -> group_by_lane(Item, KeyFun, NumStreams, Acc) end,
                           #{}, Items),
    [#lane{stream = S, remaining = lists:reverse(maps:get(I, Grouped, [])),
          in_flight = undefined, worker = undefined}
     || {I, S} <- lists:zip(lists:seq(0, NumStreams - 1), Streams)].

group_by_lane(Item, KeyFun, NumStreams, Acc) ->
    LaneIdx = KeyFun(Item) rem NumStreams,
    maps:update_with(LaneIdx, fun(L) -> [Item | L] end, [Item], Acc).

dispatch_get_manifest_step(#state{kind = get, payload = Mcid} = State) ->
    start_single_step(State, fun(Self, LinkPid, Stream) ->
        step_get_manifest(Self, LinkPid, Stream, Mcid)
    end).

%% For every lane with no worker currently in flight and work left,
%% start one step (one network call) on it. Idempotent to call
%% redundantly — a lane already busy, or already empty, is a no-op —
%% which is what lets every lane's own completion just re-trigger this
%% uniformly rather than needing to know about any other lane's state.
dispatch_lanes_or_finish(#state{chunk = #chunk{lanes = Lanes} = Chunk} = State) ->
    NewLanes = [maybe_start_lane(State, L) || L <- Lanes],
    NewState = State#state{chunk = Chunk#chunk{lanes = NewLanes}},
    finish_if_all_lanes_done(NewState).

maybe_start_lane(_State, #lane{worker = W} = Lane) when is_pid(W) ->
    Lane;
maybe_start_lane(_State, #lane{remaining = []} = Lane) ->
    Lane;
maybe_start_lane(#state{kind = Kind, link_pid = LinkPid, chunk = #chunk{manifest = Manifest}},
                 #lane{stream = Stream, remaining = [Item | Rest]} = Lane) ->
    Self = self(),
    Worker = spawn_link(fun() -> run_lane_step(Self, Kind, LinkPid, Stream, Manifest, Item) end),
    Lane#lane{remaining = Rest, in_flight = Item, worker = Worker}.

run_lane_step(Self, put, LinkPid, Stream, Manifest, {Index, Bytes}) ->
    {ok, ChunkMcid} = macula_manifest:chunk_mcid(Manifest, Index, blake3),
    Outcome = put_chunk_outcome(classify_put_content(put_block(LinkPid, Stream, ChunkMcid, Bytes), ChunkMcid)),
    Self ! {lane_step_result, Stream, Outcome};
run_lane_step(Self, get, LinkPid, Stream, Manifest, Index) ->
    {ok, ChunkMcid} = macula_manifest:chunk_mcid(Manifest, Index, blake3),
    Outcome = classify_get_content(get_block(LinkPid, Stream, ChunkMcid), ChunkMcid),
    Self ! {lane_step_result, Stream, Outcome}.

put_chunk_outcome({ok, _})       -> ok;
put_chunk_outcome({error, _} = E) -> E.

finish_if_all_lanes_done(#state{chunk = #chunk{lanes = Lanes}} = State) ->
    case lists:all(fun lane_done/1, Lanes) of
        true  -> dispatch_terminal_step(State);
        false -> {noreply, State}
    end.

lane_done(#lane{remaining = [], in_flight = undefined, worker = undefined}) -> true;
lane_done(_) -> false.

dispatch_terminal_step(#state{kind = put, chunk = #chunk{manifest = Manifest}} = State) ->
    start_single_step(State, fun(Self, LinkPid, Stream) ->
        step_put_manifest(Self, LinkPid, Stream, Manifest)
    end);
dispatch_terminal_step(#state{kind = get,
                              chunk = #chunk{manifest = Manifest, chunk_count = N, acc = Acc}} = State) ->
    Reassembled = iolist_to_binary([maps:get(I, Acc) || I <- lists:seq(0, N - 1)]),
    finalize(State, verify_result(macula_manifest:verify(Manifest, Reassembled), Reassembled)).

%% A non-lane step: the get-manifest-fetch bootstrap, or the
%% put-manifest finalize — both use the transfer's own primal stream
%% directly (for a chunked put that's also lane 0's stream; for a get
%% it's the stream the connect step opened, before any lanes exist).
start_single_step(#state{link_pid = LinkPid, stream = Stream} = State, StepFun) ->
    Self = self(),
    Worker = spawn_link(fun() -> StepFun(Self, LinkPid, Stream) end),
    {noreply, State#state{worker = Worker}}.

step_put_manifest(Self, LinkPid, Stream, #{mcid := MCID} = Manifest) ->
    Outcome = classify_put_manifest(
      call_on_stream_with_retry(LinkPid, Stream, ?CONTENT_PUT_MANIFEST_PROC,
                                #{manifest => Manifest}, ?CONTENT_MANIFEST_TIMEOUT_MS),
      MCID),
    Self ! {step_result, Outcome}.

classify_put_manifest({ok, ok},      MCID) -> {ok, MCID};
classify_put_manifest({ok, Reply},  _MCID) -> {error, {unexpected_reply, Reply}};
classify_put_manifest({error, _} = E, _MCID) -> E.

step_get_manifest(Self, LinkPid, Stream, Mcid) ->
    Outcome = classify_get_manifest_step(
      call_on_stream_with_retry(LinkPid, Stream, ?CONTENT_GET_MANIFEST_PROC,
                                #{mcid => Mcid}, ?CONTENT_MANIFEST_TIMEOUT_MS)),
    Self ! {step_result, Outcome}.

classify_get_manifest_step({ok, not_found})          -> {error, not_found};
classify_get_manifest_step({ok, Wire}) when is_map(Wire) -> macula_manifest:from_wire(Wire);
classify_get_manifest_step({ok, Reply})              -> {error, {unexpected_reply, Reply}};
classify_get_manifest_step({error, _} = E)            -> E.

%% The ONLY non-lane step for a get is the manifest fetch; for a put
%% it's the manifest put, which always finalizes (success or failure)
%% since it's the transfer's last step by construction.
step_result(Outcome, #state{kind = get} = State) ->
    get_manifest_result(Outcome, State);
step_result(Outcome, #state{kind = put} = State) ->
    finalize(State, Outcome).

get_manifest_result({ok, Manifest}, #state{link_pid = LinkPid, stream = Stream0,
                                           stream_count = DesiredN} = State) ->
    #{chunk_count := N} = Manifest,
    Lanes = setup_get_lanes(LinkPid, Stream0, DesiredN, N),
    NewChunk = #chunk{manifest = Manifest, lanes = Lanes, chunk_count = N, acc = #{}},
    {noreply, State#state{chunk = NewChunk}, {continue, next_step}};
get_manifest_result({error, _} = E, State) ->
    finalize(State, E).

%% A lane's step outcome. `false' (no matching lane) is a stray/stale
%% message — lanes live for the whole chunk phase, so this should never
%% happen in practice; treated as a defensive no-op rather than a crash.
lane_result(false, _Outcome, State) ->
    {noreply, State};
lane_result(#lane{} = Lane, {error, _} = E, State) ->
    fail_chunked(State, Lane, E);
lane_result(#lane{in_flight = Item} = Lane, Outcome, #state{kind = Kind, chunk = Chunk} = State) ->
    {NewLane, NewChunk} = apply_lane_success(Kind, Lane, Item, Outcome, Chunk),
    FinalChunk = replace_lane(NewChunk, NewLane),
    {noreply, State#state{chunk = FinalChunk}, {continue, next_step}}.

apply_lane_success(put, Lane, _Item, ok, Chunk) ->
    {Lane#lane{in_flight = undefined, worker = undefined}, Chunk};
apply_lane_success(get, Lane, Index, {ok, Bin}, #chunk{acc = Acc} = Chunk) ->
    {Lane#lane{in_flight = undefined, worker = undefined}, Chunk#chunk{acc = Acc#{Index => Bin}}}.

replace_lane(#chunk{lanes = Lanes} = Chunk, #lane{stream = S} = NewLane) ->
    Chunk#chunk{lanes = lists:keyreplace(S, #lane.stream, Lanes, NewLane)}.

%% One stream's chunk genuinely failed (not crashed) — the whole
%% transfer fails with it. Every OTHER lane's in-flight worker (if any)
%% is killed; every lane's stream, including the failed one's, is
%% closed by `finalize/2' below exactly as a successful chunked
%% transfer's would be.
fail_chunked(#state{chunk = #chunk{lanes = Lanes}} = State, FailedLane, Error) ->
    OtherLanes = [L || L <- Lanes, L#lane.stream =/= FailedLane#lane.stream],
    lists:foreach(fun(#lane{worker = W}) -> kill_worker(W) end, OtherLanes),
    finalize(State, Error).

%% Chunked terminal outcome — mirrors `content_result''s job for the
%% single-block path, but the close happens here instead of in a
%% worker (a chunked transfer's step workers never held a stream open
%% past their own one call).
finalize(#state{link_pid = LinkPid, stream = Stream, chunk = Chunk, waiters = Waiters} = State, Outcome) ->
    close_all_streams(LinkPid, Stream, Chunk),
    [gen_server:reply(From, Outcome) || From <- Waiters],
    {noreply, State#state{result = Outcome, waiters = [], worker = undefined}}.

close_all_streams(LinkPid, PrimalStream, undefined) ->
    close_stream_safely(LinkPid, PrimalStream);
close_all_streams(LinkPid, PrimalStream, #chunk{lanes = undefined}) ->
    close_stream_safely(LinkPid, PrimalStream);
close_all_streams(LinkPid, _PrimalStream, #chunk{lanes = Lanes}) ->
    lists:foreach(fun(#lane{stream = S}) -> close_stream_safely(LinkPid, S) end, Lanes).

close_stream_safely(LinkPid, Stream) when is_pid(LinkPid), is_reference(Stream) ->
    catch macula_station_link:close_content_stream(LinkPid, Stream);
close_stream_safely(_LinkPid, _Stream) ->
    ok.

verify_result(ok, Reassembled)      -> {ok, Reassembled};
verify_result({error, _} = E, _Bin) -> E.

%%%===================================================================
%%% Retry — a `_content.*' CALL on the transfer's pinned dedicated
%%% stream, retried on a BOLT#4 error whose OWN retry policy says to.
%%% Verbatim port of macula:call_on_stream_with_retry/5,6.
%%%===================================================================

call_on_stream_with_retry(LinkPid, Stream, Procedure, Payload, TimeoutMs) ->
    call_on_stream_with_retry(LinkPid, Stream, Procedure, Payload, TimeoutMs, 3).

call_on_stream_with_retry(LinkPid, Stream, Procedure, Payload, TimeoutMs,
                          AttemptsLeft) ->
    retry_stream_result(
      macula_station_link:call_on_stream(LinkPid, Stream, ?CONTENT_REALM,
                                         Procedure, Payload, TimeoutMs),
      LinkPid, Stream, Procedure, Payload, TimeoutMs, AttemptsLeft).

retry_stream_result({error, {call_error, Code, _Name}} = E, LinkPid, Stream,
                    Procedure, Payload, TimeoutMs, AttemptsLeft)
        when AttemptsLeft > 1 ->
    retry_stream_if_retryable(macula_bolt4:is_retryable(Code), E, LinkPid,
                              Stream, Procedure, Payload, TimeoutMs,
                              AttemptsLeft);
retry_stream_result(Result, _LinkPid, _Stream, _Procedure, _Payload,
                    _TimeoutMs, _AttemptsLeft) ->
    Result.

retry_stream_if_retryable(true, _E, LinkPid, Stream, Procedure, Payload,
                          TimeoutMs, AttemptsLeft) ->
    timer:sleep(?CONTENT_RETRY_BACKOFF_MS),
    call_on_stream_with_retry(LinkPid, Stream, Procedure, Payload, TimeoutMs,
                              AttemptsLeft - 1);
retry_stream_if_retryable(false, E, _LinkPid, _Stream, _Procedure, _Payload,
                          _TimeoutMs, _AttemptsLeft) ->
    E.
