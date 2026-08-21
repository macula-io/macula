%%%-------------------------------------------------------------------
%%% @doc Addressable content-store put/get, with a real, peer-visible
%%% abort — the foundation `macula_feeder'/`macula_download' (and,
%%% later, `macula_pusher'/`macula_upload') build on. See
%%% PLAN_PUSH_UPLOAD.md, Phase 1.
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
%%% resolve/dial/transfer sequence runs in a linked worker. `await/1,2'
%%% blocks for the outcome (`{ok, Mcid}' / `{ok, Bytes}' /
%%% `{error, Reason}') — repeatable and from any process; the result
%%% is cached once known. The process does NOT self-terminate on
%%% completion (a second `await/1' after success must still answer) —
%%% call `cancel/1' when done with the handle, whether the transfer
%%% succeeded, failed, or is still in flight; on an already-resolved
%%% transfer this is a pure reap (nothing left to abort).
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
-export([await/1, await/2, cancel/1, cancel/3, share_id/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

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

-record(state, {
    kind     :: kind(),
    payload  :: binary(),         % put: Bytes; get: Mcid
    share_id :: binary(),
    link_pid :: pid() | undefined,
    stream   :: reference() | undefined,
    worker   :: pid(),
    result   :: {ok, term()} | {error, term()} | undefined,
    waiters  :: [gen_server:from()]
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
    Worker = spawn_link(fun() -> run(Self, Kind, Dial, Payload) end),
    {ok, #state{kind = Kind, payload = Payload, share_id = ShareId,
               worker = Worker, waiters = []}}.

%% @private
handle_call(await, From, #state{result = undefined, waiters = Waiters} = State) ->
    {noreply, State#state{waiters = [From | Waiters]}};
handle_call(await, _From, #state{result = Result} = State) ->
    {reply, Result, State};
handle_call(share_id, _From, #state{share_id = Id} = State) ->
    {reply, Id, State};
handle_call({cancel, _Code, _Message}, _From, #state{result = Result} = State)
        when Result =/= undefined ->
    {stop, normal, ok, State};
handle_call({cancel, Code, Message}, _From,
            #state{worker = Worker, link_pid = LinkPid, stream = Stream,
                  waiters = Waiters} = State) ->
    unlink(Worker),
    exit(Worker, kill),
    abort_stream_if_open(LinkPid, Stream, Code, Message),
    [gen_server:reply(From, {error, cancelled}) || From <- Waiters],
    {stop, normal, ok, State#state{result = {error, cancelled}, waiters = []}};
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info({content_link, LinkPid, Stream}, State) ->
    {noreply, State#state{link_pid = LinkPid, stream = Stream}};
handle_info({content_result, Result}, #state{waiters = Waiters} = State) ->
    [gen_server:reply(From, Result) || From <- Waiters],
    {noreply, State#state{result = Result, waiters = []}};
handle_info({'EXIT', Worker, Reason}, #state{worker = Worker, result = undefined} = State)
        when Reason =/= normal ->
    {stop, {worker_crashed, Reason}, State};
handle_info(_Msg, State) ->
    {noreply, State}.

abort_stream_if_open(LinkPid, Stream, Code, Message)
  when is_pid(LinkPid), is_reference(Stream) ->
    macula_station_link:abort_content_stream(LinkPid, Stream, Code, Message);
abort_stream_if_open(_LinkPid, _Stream, _Code, _Message) ->
    ok.

%%%===================================================================
%%% Worker — resolve link, open stream, run the transfer
%%%===================================================================

-spec run(pid(), kind(), dial(), binary()) -> term().
run(Parent, Kind, Dial, Payload) ->
    case connect(Dial) of
        {ok, LinkPid, Stream} ->
            Parent ! {content_link, LinkPid, Stream},
            Result = transfer(Kind, LinkPid, Stream, Payload),
            catch macula_station_link:close_content_stream(LinkPid, Stream),
            Parent ! {content_result, Result};
        {error, _} = E ->
            Parent ! {content_result, E}
    end.

connect({pooled, Pool}) ->
    open_on_link(macula_client:pick_connected_link(Pool));
connect({station, Pool, Station, TimeoutMs, LinkOpts}) ->
    open_on_link(macula_client:ensure_content_link(Pool, Station, LinkOpts, TimeoutMs)).

open_on_link({error, _} = E) -> E;
open_on_link({ok, LinkPid}) ->
    stream_opened(macula_station_link:open_content_stream(LinkPid), LinkPid).

stream_opened({ok, Stream}, LinkPid) -> {ok, LinkPid, Stream};
stream_opened({error, _} = E, _LinkPid) -> E.

transfer(put, LinkPid, Stream, Bytes) -> run_put(LinkPid, Stream, Bytes);
transfer(get, LinkPid, Stream, Mcid)  -> run_get(LinkPid, Stream, Mcid).

%%%===================================================================
%%% Put — single block or chunked (mirrors macula:put_content/2's
%%% former internals byte-for-byte; see PLAN_PUSH_UPLOAD.md Phase 1)
%%%===================================================================

run_put(LinkPid, Stream, Bytes) ->
    put_content_by_size(byte_size(Bytes) =< macula_manifest:default_chunk_size(),
                        LinkPid, Stream, Bytes).

put_content_by_size(true, LinkPid, Stream, Bytes) ->
    put_single_block(LinkPid, Stream, Bytes);
put_content_by_size(false, LinkPid, Stream, Bytes) ->
    put_chunked(LinkPid, Stream, Bytes).

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

%% Split into chunks, upload each block, then the manifest. A chunk
%% failure short-circuits WITHOUT putting the manifest — a manifest
%% naming missing chunks would resolve but never reassemble, which is
%% worse than a clean error now.
put_chunked(LinkPid, Stream, Bytes) ->
    {ok, Manifest, Chunks} = macula_manifest:create(Bytes),
    put_chunks(LinkPid, Stream, Manifest, Chunks, 0).

put_chunks(LinkPid, Stream, Manifest, [], _Index) ->
    put_manifest(LinkPid, Stream, Manifest);
put_chunks(LinkPid, Stream, Manifest, [Chunk | Rest], Index) ->
    {ok, ChunkMcid} = macula_manifest:chunk_mcid(Manifest, Index, blake3),
    chunk_put_result(put_block(LinkPid, Stream, ChunkMcid, Chunk), LinkPid,
                     Stream, Manifest, Rest, Index + 1).

chunk_put_result({ok, ok}, LinkPid, Stream, Manifest, Rest, NextIndex) ->
    put_chunks(LinkPid, Stream, Manifest, Rest, NextIndex);
chunk_put_result({ok, hash_mismatch}, _LinkPid, _Stream, _Manifest, _Rest,
                 _NextIndex) ->
    {error, hash_mismatch};
chunk_put_result({ok, Reply}, _LinkPid, _Stream, _Manifest, _Rest, _NextIndex) ->
    {error, {unexpected_reply, Reply}};
chunk_put_result({error, _} = E, _LinkPid, _Stream, _Manifest, _Rest, _NextIndex) ->
    E.

put_manifest(LinkPid, Stream, #{mcid := MCID} = Manifest) ->
    classify_put_manifest(
      call_on_stream_with_retry(LinkPid, Stream, ?CONTENT_PUT_MANIFEST_PROC,
                                #{manifest => Manifest},
                                ?CONTENT_MANIFEST_TIMEOUT_MS),
      MCID).

classify_put_manifest({ok, ok},      MCID) -> {ok, MCID};
classify_put_manifest({ok, Reply},  _MCID) -> {error, {unexpected_reply, Reply}};
classify_put_manifest({error, _} = E, _MCID) -> E.

%%%===================================================================
%%% Get — single block or chunked
%%%===================================================================

run_get(LinkPid, Stream, <<1, 16#55, _:32/binary>> = MCID) ->
    classify_get_content(get_block(LinkPid, Stream, MCID), MCID);
run_get(LinkPid, Stream, <<1, 16#56, _:32/binary>> = MCID) ->
    get_chunked(LinkPid, Stream, MCID).

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
%% Chunked content already gets this from `macula_manifest:verify/2'
%% over the reassembled whole; single-block content had no client-side
%% check at all before this (fixed pre-Phase-1, carried forward here
%% unchanged — see `macula_content_block_hash_tests').
-spec verify_block_hash(macula:mcid(), binary()) ->
        {ok, binary()} | {error, hash_mismatch | invalid_mcid}.
verify_block_hash(<<1, 16#55, Hash:32/binary>>, Bin) ->
    hash_result(macula_blake3_nif:hash(Bin) =:= Hash, Bin);
verify_block_hash(_MCID, _Bin) ->
    {error, invalid_mcid}.

hash_result(true, Bin)   -> {ok, Bin};
hash_result(false, _Bin) -> {error, hash_mismatch}.

get_chunked(LinkPid, Stream, MCID) ->
    classify_get_manifest(
      call_on_stream_with_retry(LinkPid, Stream, ?CONTENT_GET_MANIFEST_PROC,
                                #{mcid => MCID}, ?CONTENT_MANIFEST_TIMEOUT_MS),
      LinkPid, Stream).

classify_get_manifest({ok, not_found}, _LinkPid, _Stream) ->
    {error, not_found};
classify_get_manifest({ok, Wire}, LinkPid, Stream) when is_map(Wire) ->
    manifest_decoded(macula_manifest:from_wire(Wire), LinkPid, Stream);
classify_get_manifest({ok, Reply}, _LinkPid, _Stream) ->
    {error, {unexpected_reply, Reply}};
classify_get_manifest({error, _} = E, _LinkPid, _Stream) ->
    E.

manifest_decoded({error, invalid_manifest}, _LinkPid, _Stream) ->
    {error, invalid_manifest};
manifest_decoded({ok, #{chunk_count := N} = Manifest}, LinkPid, Stream) ->
    get_chunks(LinkPid, Stream, Manifest, 0, N, []).

get_chunks(_LinkPid, _Stream, Manifest, Index, N, Acc) when Index >= N ->
    reassembled(Manifest, iolist_to_binary(lists:reverse(Acc)));
get_chunks(LinkPid, Stream, Manifest, Index, N, Acc) ->
    {ok, ChunkMcid} = macula_manifest:chunk_mcid(Manifest, Index, blake3),
    chunk_get_result(get_block(LinkPid, Stream, ChunkMcid), LinkPid, Stream,
                     Manifest, Index, N, Acc).

chunk_get_result({ok, Bin}, LinkPid, Stream, Manifest, Index, N, Acc)
        when is_binary(Bin) ->
    get_chunks(LinkPid, Stream, Manifest, Index + 1, N, [Bin | Acc]);
chunk_get_result({ok, not_found}, _LinkPid, _Stream, _Manifest, _Index, _N,
                 _Acc) ->
    {error, not_found};
chunk_get_result({ok, Reply}, _LinkPid, _Stream, _Manifest, _Index, _N, _Acc) ->
    {error, {unexpected_reply, Reply}};
chunk_get_result({error, _} = E, _LinkPid, _Stream, _Manifest, _Index, _N,
                 _Acc) ->
    E.

reassembled(Manifest, Reassembled) ->
    verify_result(macula_manifest:verify(Manifest, Reassembled), Reassembled).

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
