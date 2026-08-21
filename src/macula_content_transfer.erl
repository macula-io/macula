%%%-------------------------------------------------------------------
%%% @doc Addressable content-store put/get, with a real, peer-visible
%%% abort and (for chunked content) real pause/resume — the foundation
%%% `macula_feeder'/`macula_download' (and, later, `macula_pusher'/
%%% `macula_upload') build on. See PLAN_PUSH_UPLOAD.md, Phases 1-2.
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
%%% `cancel/3' resets the open content stream's send side via
%%% `macula_quic:reset_stream/2' (through `macula_station_link:
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
%%% is whether the NEXT chunk starts once the current one finishes.
%%% Internally this is a `handle_continue/2' step the gen_server
%%% re-triggers itself between chunks, checking `paused' each time;
%%% `resume/1' re-arms it from exactly the next un-sent/un-fetched
%%% chunk, never from the start. `cancel/1,3' still works at any point,
%%% paused or not — if a chunk step happens to be in flight it is
%%% killed and the stream reset exactly as Phase 1 describes above; if
%%% paused between chunks (no step in flight), there is simply nothing
%%% to kill and `cancel' resets the stream directly.
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

-type kind() :: put | get.
-type dial() :: {pooled, macula:pool()}
              | {station, macula:pool(), macula_client:seed(), pos_integer(), map()}.

%% Chunked-transfer driving state — `undefined` in `#state.chunk` for a
%% single-block transfer (nothing to drive between chunks) and for a
%% chunked one until its content stream is open.
%%
%% put: `manifest'/`remaining' are both known upfront (pure,
%% `macula_manifest:create/1', no network) — `remaining' shrinks by one
%% chunk per successful step until `[]', which triggers the final
%% put_manifest step.
%%
%% get: `manifest' starts `undefined' — fetching it IS the first step.
%% Once known, `chunk_count' bounds the loop and `acc' accumulates
%% fetched chunks in REVERSE order (cheap `[H|T]`, reversed once at
%% reassembly).
-record(chunk, {
    manifest    :: map() | undefined,
    remaining   :: [binary()],
    next_index  :: non_neg_integer(),
    chunk_count :: non_neg_integer() | undefined,
    acc         :: [binary()]
}).

-record(state, {
    kind     :: kind(),
    payload  :: binary(),         % put: Bytes; get: Mcid
    share_id :: binary(),
    link_pid :: pid() | undefined,
    stream   :: reference() | undefined,
    worker   :: pid() | undefined,
    result   :: {ok, term()} | {error, term()} | undefined,
    waiters  :: [gen_server:from()],
    paused   :: boolean(),
    chunk    :: #chunk{} | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start an addressable put through the pool's own connected
%% link (whichever `macula_client:pick_connected_link/1' picks).
-spec start_put(macula:pool(), binary()) -> {ok, pid()}.
start_put(Pool, Bytes) -> start_put(Pool, Bytes, #{}).

%% @doc As `start_put/2'. `Opts' may carry `share_id' (binary,
%% overrides the minted default).
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

%% @doc As `start_put_station/4'. `Opts' may carry `share_id' plus a
%% per-call TLS trust override for this dial — `verify',
%% `expected_node_id', `pin_tls_cert' (see `macula:put_content_station/5').
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

%% @doc As `start_get/2'. `Opts' may carry `share_id'.
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

%% @doc Cancel `Pid''s transfer and reap the process. If a content
%% stream is already open, resets it with `Code' — genuinely
%% peer-visible, see the moduledoc. `Message' is local-only (logged at
%% the link; QUIC RESET_STREAM carries only the numeric code on the
%% wire). If the transfer already resolved (success or failure), this
%% is a pure reap — nothing left to abort. Either way `await/1,2'
%% answers `{error, cancelled}' to anyone still waiting.
-spec cancel(pid(), non_neg_integer(), binary()) -> ok.
cancel(Pid, Code, Message)
  when is_integer(Code), Code >= 0, is_binary(Message) ->
    gen_server:call(Pid, {cancel, Code, Message}).

%% @doc Pause a chunked transfer between chunks — the in-flight chunk
%% (if any) still completes, the stream stays open, but the next chunk
%% does not start until `resume/1'. A no-op on a single-block transfer
%% or one that has already resolved (nothing to pause either way).
-spec pause(pid()) -> ok.
pause(Pid) -> gen_server:call(Pid, pause).

%% @doc Resume a transfer paused via `pause/1', continuing from the
%% next un-sent/un-fetched chunk. A no-op if not actually paused, not
%% chunked, or already resolved.
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
    ok = macula_content_transfer_registry:register_share(ShareId, self()),
    Self = self(),
    Worker = spawn_link(fun() -> connect_and_run(Self, Kind, Dial, Payload) end),
    {ok, #state{kind = Kind, payload = Payload, share_id = ShareId,
               worker = Worker, waiters = [], paused = false, chunk = undefined}}.

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
                  waiters = Waiters} = State) ->
    kill_worker(Worker),
    abort_stream_if_open(LinkPid, Stream, Code, Message),
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
%% hands off: this process starts driving the chunk-by-chunk loop.
handle_info({content_link, LinkPid, Stream}, #state{kind = Kind, payload = Payload} = State) ->
    NewState = State#state{link_pid = LinkPid, stream = Stream},
    case is_chunked(Kind, Payload) of
        true  -> {noreply, NewState#state{chunk = init_chunk(Kind, Payload)}, {continue, next_step}};
        false -> {noreply, NewState}
    end;
%% Single-block path only (chunked finalizes via `finalize/2' instead).
handle_info({content_result, Result}, #state{waiters = Waiters} = State) ->
    [gen_server:reply(From, Result) || From <- Waiters],
    {noreply, State#state{result = Result, waiters = []}};
%% One chunk step's outcome. Which step it was is inferred from
%% `chunk''s current shape — there is only ever one step in flight.
handle_info({step_result, Outcome}, #state{worker = Worker} = State)
        when is_pid(Worker) ->
    step_result(Outcome, State#state{worker = undefined});
handle_info({'EXIT', Worker, Reason}, #state{worker = Worker, result = undefined} = State)
        when Reason =/= normal ->
    {stop, {worker_crashed, Reason}, State};
handle_info(_Msg, State) ->
    {noreply, State}.

%% @private
handle_continue(next_step, #state{paused = true} = State) ->
    {noreply, State};
handle_continue(next_step, State) ->
    dispatch_next_step(State).

kill_worker(Worker) when is_pid(Worker) ->
    unlink(Worker),
    exit(Worker, kill);
kill_worker(undefined) ->
    ok.

abort_stream_if_open(LinkPid, Stream, Code, Message)
  when is_pid(LinkPid), is_reference(Stream) ->
    macula_station_link:abort_content_stream(LinkPid, Stream, Code, Message);
abort_stream_if_open(_LinkPid, _Stream, _Code, _Message) ->
    ok.

%%%===================================================================
%%% Connect worker — resolve link, open stream. A single-block
%%% transfer keeps running in this same process; a chunked one hands
%%% off to the gen_server as soon as `content_link' is sent (see
%%% `handle_info/2' above) and this function returns right after.
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
%%% worker exactly as Phase 1 shipped it. Untouched by Phase 2: there
%%% is no "between chunks" for pause/resume to mean anything here.
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
%% verify/2' over the reassembled whole (see `dispatch_next_step/1''s
%% get-reassembly clause below); single-block content had no
%% client-side check at all before this (fixed pre-Phase-1, carried
%% forward here unchanged — see `macula_content_block_hash_tests').
-spec verify_block_hash(macula:mcid(), binary()) ->
        {ok, binary()} | {error, hash_mismatch | invalid_mcid}.
verify_block_hash(<<1, 16#55, Hash:32/binary>>, Bin) ->
    hash_result(macula_blake3_nif:hash(Bin) =:= Hash, Bin);
verify_block_hash(_MCID, _Bin) ->
    {error, invalid_mcid}.

hash_result(true, Bin)   -> {ok, Bin};
hash_result(false, _Bin) -> {error, hash_mismatch}.

%%%===================================================================
%%% Chunked — driven step by step by THIS process (the gen_server),
%%% not the connect worker. Each step (one chunk put/get, or the
%%% manifest put/get) runs in its own short-lived linked worker, so
%%% `cancel/3' can always kill whichever one is currently in flight;
%%% between steps `dispatch_next_step/1' checks `paused' before
%%% starting the next one — that check is Phase 2's entire pause/resume
%%% mechanism. PLAN_PUSH_UPLOAD.md Phase 2.
%%%===================================================================

init_chunk(put, Bytes) ->
    {ok, Manifest, Chunks} = macula_manifest:create(Bytes),
    #chunk{manifest = Manifest, remaining = Chunks, next_index = 0, acc = []};
init_chunk(get, _Mcid) ->
    %% Manifest not known yet — fetching it IS the first step.
    #chunk{manifest = undefined, remaining = [], next_index = 0, acc = []}.

%% get: manifest not fetched yet.
dispatch_next_step(#state{kind = get, payload = Mcid,
                          chunk = #chunk{manifest = undefined}} = State) ->
    start_step(State, fun(Self, LinkPid, Stream) ->
        step_get_manifest(Self, LinkPid, Stream, Mcid)
    end);
%% get: every chunk fetched — reassemble + verify. Pure, no network,
%% so this runs inline rather than in a worker.
dispatch_next_step(#state{kind = get,
                          chunk = #chunk{manifest = Manifest, next_index = Index,
                                        chunk_count = N, acc = Acc}} = State)
        when N =/= undefined, Index >= N ->
    Reassembled = iolist_to_binary(lists:reverse(Acc)),
    finalize(State, verify_result(macula_manifest:verify(Manifest, Reassembled), Reassembled));
%% get: fetch the next chunk.
dispatch_next_step(#state{kind = get,
                          chunk = #chunk{manifest = Manifest, next_index = Index}} = State) ->
    start_step(State, fun(Self, LinkPid, Stream) ->
        step_get_chunk(Self, LinkPid, Stream, Manifest, Index)
    end);
%% put: every chunk sent — put the manifest. Finalizes via its own
%% step_result (unlike get's reassembly, this one IS a network call).
dispatch_next_step(#state{kind = put, chunk = #chunk{manifest = Manifest, remaining = []}} = State) ->
    start_step(State, fun(Self, LinkPid, Stream) ->
        step_put_manifest(Self, LinkPid, Stream, Manifest)
    end);
%% put: send the next chunk.
dispatch_next_step(#state{kind = put,
                          chunk = #chunk{manifest = Manifest, remaining = [Chunk | _],
                                        next_index = Index}} = State) ->
    start_step(State, fun(Self, LinkPid, Stream) ->
        step_put_chunk(Self, LinkPid, Stream, Manifest, Chunk, Index)
    end).

start_step(#state{link_pid = LinkPid, stream = Stream} = State, StepFun) ->
    Self = self(),
    Worker = spawn_link(fun() -> StepFun(Self, LinkPid, Stream) end),
    {noreply, State#state{worker = Worker}}.

step_put_chunk(Self, LinkPid, Stream, Manifest, Chunk, Index) ->
    {ok, ChunkMcid} = macula_manifest:chunk_mcid(Manifest, Index, blake3),
    Outcome = put_chunk_outcome(classify_put_content(put_block(LinkPid, Stream, ChunkMcid, Chunk), ChunkMcid)),
    Self ! {step_result, Outcome}.

put_chunk_outcome({ok, _})       -> ok;
put_chunk_outcome({error, _} = E) -> E.

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

step_get_chunk(Self, LinkPid, Stream, Manifest, Index) ->
    {ok, ChunkMcid} = macula_manifest:chunk_mcid(Manifest, Index, blake3),
    Outcome = classify_get_content(get_block(LinkPid, Stream, ChunkMcid), ChunkMcid),
    Self ! {step_result, Outcome}.

%% A step worker's outcome. Which step it was is inferred from
%% `chunk''s current shape — same discriminants `dispatch_next_step/1'
%% just used to decide what to dispatch, since there is only ever one
%% step in flight at a time.
step_result(Outcome, #state{kind = get, chunk = #chunk{manifest = undefined}} = State) ->
    get_manifest_result(Outcome, State);
step_result(Outcome, #state{kind = get,
                            chunk = #chunk{next_index = Index, acc = Acc} = Chunk} = State) ->
    get_chunk_result(Outcome, Index, Acc, Chunk, State);
step_result(Outcome, #state{kind = put, chunk = #chunk{remaining = []}} = State) ->
    finalize(State, Outcome);
step_result(Outcome, #state{kind = put,
                            chunk = #chunk{remaining = [_ | Rest], next_index = Index} = Chunk} = State) ->
    put_chunk_result(Outcome, Rest, Index, Chunk, State).

get_manifest_result({ok, Manifest}, State) ->
    #{chunk_count := N} = Manifest,
    NewChunk = #chunk{manifest = Manifest, remaining = [], next_index = 0,
                      chunk_count = N, acc = []},
    {noreply, State#state{chunk = NewChunk}, {continue, next_step}};
get_manifest_result({error, _} = E, State) ->
    finalize(State, E).

get_chunk_result({ok, Bin}, Index, Acc, Chunk, State) when is_binary(Bin) ->
    NewChunk = Chunk#chunk{next_index = Index + 1, acc = [Bin | Acc]},
    {noreply, State#state{chunk = NewChunk}, {continue, next_step}};
get_chunk_result({error, _} = E, _Index, _Acc, _Chunk, State) ->
    finalize(State, E).

put_chunk_result(ok, Rest, Index, Chunk, State) ->
    NewChunk = Chunk#chunk{remaining = Rest, next_index = Index + 1},
    {noreply, State#state{chunk = NewChunk}, {continue, next_step}};
put_chunk_result({error, _} = E, _Rest, _Index, _Chunk, State) ->
    finalize(State, E).

%% Chunked terminal outcome — mirrors `content_result''s job for the
%% single-block path, but the close happens here instead of in a
%% worker (Phase 2's step workers never held the stream open past
%% their own one call).
finalize(#state{link_pid = LinkPid, stream = Stream, waiters = Waiters} = State, Outcome) ->
    catch macula_station_link:close_content_stream(LinkPid, Stream),
    [gen_server:reply(From, Outcome) || From <- Waiters],
    {noreply, State#state{result = Outcome, waiters = [], worker = undefined}}.

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
