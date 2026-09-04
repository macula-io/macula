%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised content pushes (the sender side of a
%%% push-initiated upload — PLAN_PUSH_UPLOAD.md Phase 6).
%%%
%%% `start_link/5,6' returns immediately with a pid, delivers the
%%% outcome to `Module:handle_pushed/2', and publishes
%%% `sharing.push_started_v1' / `sharing.push_completed_v1' mesh facts
%%% around the transfer — including `outcome => cancelled' if the
%%% pusher is stopped before the push resolves.
%%%
%%% Unlike `macula_feeder' (which puts into content-addressed storage
%%% for a downloader to discover and pull later), this actively pushes
%%% bytes AT a specific, already-known recipient advertising an upload
%%% procedure (`macula_upload:advertise/5,6') — the `client_stream'
%%% mode `STREAMING_GUIDE.md' names for exactly this ("an upload, a
%%% batch submit"), wrapped with `macula_feeder'/`macula_download''s
%%% own integrity machinery: `macula_manifest:create/2' chunks and
%%% hashes `Bytes' up front, the manifest rides the stream's open-time
%%% `Args' (an out-of-band channel, not an in-band header chunk — see
%%% `macula_manifest''s `from_wire/1'), and chunks are sent in order
%%% over the ONE `client_stream' the recipient reads from.
%%%
%%% == No multi-stream parallelism here — a deliberate correction ==
%%%
%%% An earlier draft of this plan said this module "sends chunks via
%%% the Phase 3 multi-stream engine." Traced why that can't be true:
%%% Phase 3's multi-stream engine lives entirely inside
%%% `macula_content_transfer', built on content-sharing's OWN dedicated
%%% content-stream bookkeeping (`macula_station_link''s
%%% `content_stream_bufs' / `open_content_stream') — a wire mechanism
%%% streaming RPC's `client_stream'/`macula_stream' doesn't have and
%%% was never meant to. This plan's own scope-decision section says so
%%% explicitly: "Multi-stream parallel chunk transfer is a
%%% content-sharing-only concern. It does NOT extend to
%%% `macula_streamer'/`macula_stream_sink'." Chunks here go out
%%% sequentially over the one stream `macula:call_stream/5' /
%%% `macula_direct_dial:call_stream/5' opens — the same shape a hand
%%% written `client_stream' caller would use, just chunked and hashed
%%% for you.
%%%
%%% == The terminal reply ==
%%%
%%% The recipient's own `macula_upload' verifies the reassembled bytes
%%% against the manifest (receiver-side, never sender-trusted — the
%%% sender's claimed manifest proves nothing on its own) and reports
%%% the outcome back over `client_stream''s own terminal-reply channel
%%% (`macula_stream:set_reply/2' / `set_error/2', surfaced here via
%%% `macula_streamer''s new `handle_eof/1' callback — see that
%%% module's doc). This pusher blocks on `macula:await_reply/1' for
%%% it, so `handle_pushed/2' only ever sees `{ok, Mcid}' once the
%%% recipient has actually verified the bytes, never merely "the local
%%% `send/2,3' calls all returned `ok''" (which would only prove bytes
%%% were accepted onto the wire, not that they arrived correctly).
%%%
%%% == Real cancel ==
%%%
%%% Holds the raw stream pid directly (a `stream' state field,
%%% alongside the lightweight open+send+await proxy `worker' that
%%% reports it back) so `cancel/1' reaches it for a real, peer-visible
%%% `macula_stream:abort/3' STREAM_ERROR — not a blunt local kill that
%%% leaves the recipient inferring cancellation from the connection
%%% simply going away. Mirrors `macula_feeder''s own `content_transfer'
%%% field / `reap_content_transfer/1' pattern exactly, one layer down
%%% (a raw stream instead of a `macula_content_transfer' pid).
%%%
%%% == Direct-dial ==
%%%
%%% A correction from the plan's literal wording, worth recording:
%%% "mirrors `macula_feeder''s shape exactly" holds for the WRAPPER
%%% (supervised gen_server, cancel via a held handle, mesh facts) but
%%% not for direct-dial's DIAL semantics. Content-sharing's direct-dial
%%% targets a named STATION (`macula_feeder:start_link_direct/5,6'
%%% takes an explicit `Station' pubkey, resolved via its own signed
%%% `station_endpoint' record) because content storage has no notion of
%%% "advertised procedures." A push targets a specific ADVERTISED
%%% PROCEDURE instead (`macula_upload:advertise_direct/6,7''s
%%% `procedure_advertisement'), so `start_link_direct/5,6' mirrors
%%% `macula_stream_sink:start_link_direct/5,6''s shape instead — same
%%% `Procedure'-based resolve, no `Station' parameter — and reuses
%%% `macula_direct_dial:call_stream/5' as-is, which already resolves
%%% and dials as one step (unlike content-transfer's lower-level
%%% primitives, there is no separate resolve step for this module to
%%% drive itself).
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(doc_pusher).
%%% -behaviour(macula_pusher).
%%% -export([init/1, handle_pushed/2]).
%%%
%%% init(Parent) -> {ok, Parent}.
%%%
%%% handle_pushed(Result, Parent) ->
%%%     Parent ! {pushed, Result},
%%%     {stop, normal, Parent}.
%%% '''
%%%
%%% ```
%%% {ok, Pid} = macula_pusher:start_link(doc_pusher, Pool, Realm,
%%%     <<"bulk.ingest">>, Bytes, self()).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pusher).

-behaviour(gen_server).

-export([start_link/5, start_link/6]).
-export([start_link_direct/5, start_link_direct/6]).
-export([cancel/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_pushed(Result :: {ok, macula:mcid()} | {error, term()}, State :: term()) ->
    {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-define(PUSH_STARTED, <<"sharing.push_started_v1">>).
-define(PUSH_COMPLETED, <<"sharing.push_completed_v1">>).
-define(CANCEL_CODE, <<"cancelled">>).

-record(pstate, {
    module    :: module(),
    pool      :: macula:pool(),
    realm     :: macula:realm(),
    announce  :: boolean(),
    share_id  :: binary(),
    worker    :: pid(),
    stream    :: pid() | undefined,
    completed :: boolean(),
    user      :: term()
}).

%% @doc Start a pusher. Pushes `Bytes' to `Procedure' on `(Realm)' via
%% `Pool'.
-spec start_link(module(), macula:pool(), macula:realm(), macula:procedure(),
                 binary()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Procedure, Bytes) ->
    start_link(Module, Pool, Realm, Procedure, Bytes, undefined).

%% @doc As `start_link/5', with `Args' passed to `Module:init/1'.
-spec start_link(module(), macula:pool(), macula:realm(), macula:procedure(),
                 binary(), term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Procedure, Bytes, Args) ->
    gen_server:start_link(?MODULE,
        {pooled, Module, Pool, Realm, Procedure, Bytes, true, Args}, []).

%% @doc As `start_link/5', but resolves `Procedure''s
%% `procedure_advertisement' from the DHT and dials its provider
%% directly instead of pushing through the pool's existing links. See
%% the "Direct-dial" section above. Requires the recipient to have
%% advertised via `macula_upload:advertise_direct/6,7', not plain
%% `advertise/5,6'.
-spec start_link_direct(module(), macula:pool(), macula:realm(),
                        macula:procedure(), binary()) ->
    {ok, pid()} | {error, term()}.
start_link_direct(Module, Pool, Realm, Procedure, Bytes) ->
    start_link_direct(Module, Pool, Realm, Procedure, Bytes, undefined).

%% @doc As `start_link_direct/5', with `Args' passed to `Module:init/1'.
-spec start_link_direct(module(), macula:pool(), macula:realm(),
                        macula:procedure(), binary(), term()) ->
    {ok, pid()} | {error, term()}.
start_link_direct(Module, Pool, Realm, Procedure, Bytes, Args) ->
    gen_server:start_link(?MODULE,
        {direct, Module, Pool, Realm, Procedure, Bytes, true, Args}, []).

%% @doc Cancel an in-flight push. Publishes `sharing.push_completed_v1'
%% with `outcome => cancelled' if the push had not resolved yet.
-spec cancel(pid()) -> ok.
cancel(Pid) -> gen_server:stop(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({DialMode, Module, Pool, Realm, Procedure, Bytes, Announce, InitArgs}) ->
    process_flag(trap_exit, true),
    case Module:init(InitArgs) of
        {ok, UserState} ->
            ShareId = crypto:strong_rand_bytes(16),
            publish(Announce, Pool, Realm, ?PUSH_STARTED,
                    #{share_id => ShareId, size => byte_size(Bytes)}),
            Worker = spawn_worker(DialMode, Pool, Realm, Procedure, Bytes),
            {ok, #pstate{module = Module, pool = Pool, realm = Realm,
                        announce = Announce, share_id = ShareId,
                        worker = Worker, stream = undefined,
                        completed = false, user = UserState}};
        {stop, Reason} ->
            {stop, Reason}
    end.

%% The lightweight proxy: chunk+hash `Bytes', open the stream, report
%% its pid back immediately (so `terminate/2' can reach it even if
%% this proxy itself gets killed mid-flight), send every chunk in
%% order, half-close, block for the recipient's verified terminal
%% reply, report the outcome. Bails out early (without half-closing or
%% awaiting a reply that will never usefully arrive) the moment any
%% `send/2,3' fails — the stream is already gone or going.
spawn_worker(DialMode, Pool, Realm, Procedure, Bytes) ->
    Parent = self(),
    spawn_link(fun() -> pusher_worker_run(DialMode, Pool, Realm, Procedure, Bytes, Parent) end).

pusher_worker_run(DialMode, Pool, Realm, Procedure, Bytes, Parent) ->
    {ok, Manifest, Chunks} = macula_manifest:create(Bytes),
    case open_stream(DialMode, Pool, Realm, Procedure, Manifest) of
        {ok, Stream} ->
            Parent ! {stream, Stream},
            Result = push_chunks(Stream, Chunks),
            Parent ! {push_result, Result};
        {error, Reason} ->
            Parent ! {push_result, {error, Reason}}
    end.

open_stream(pooled, Pool, Realm, Procedure, Manifest) ->
    macula:call_stream(Pool, Realm, Procedure, Manifest, #{mode => client_stream});
open_stream(direct, Pool, Realm, Procedure, Manifest) ->
    macula_direct_dial:call_stream(Pool, Realm, Procedure, Manifest,
                                   #{mode => client_stream}).

push_chunks(Stream, Chunks) ->
    case send_all(Stream, Chunks) of
        ok ->
            ok = macula:close_send(Stream),
            macula:await_reply(Stream);
        {error, _} = Error ->
            Error
    end.

send_all(_Stream, []) -> ok;
send_all(Stream, [Chunk | Rest]) ->
    case macula:send(Stream, Chunk) of
        ok -> send_all(Stream, Rest);
        {error, _} = Error -> Error
    end.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info({stream, Stream}, State) ->
    {noreply, State#pstate{stream = Stream}};
handle_info({push_result, Result}, State) ->
    NewState = announce_completed(State, Result),
    #pstate{module = Module, user = User} = NewState,
    deliver(Module:handle_pushed(Result, User), NewState#pstate{stream = undefined});
handle_info({'EXIT', Worker, Reason}, #pstate{worker = Worker} = State)
        when Reason =/= normal ->
    {stop, {worker_crashed, Reason}, State};
handle_info(_Msg, State) ->
    {noreply, State}.

deliver({noreply, NewUser}, State) -> {noreply, State#pstate{user = NewUser}};
deliver({stop, Reason, NewUser}, State) -> {stop, Reason, State#pstate{user = NewUser}}.

%% @private
terminate(_Reason, #pstate{worker = Worker, completed = true}) ->
    unlink(Worker),
    exit(Worker, kill),
    ok;
terminate(_Reason, #pstate{stream = Stream} = State) ->
    unlink(State#pstate.worker),
    exit(State#pstate.worker, kill),
    reap_stream(Stream),
    _ = announce_completed(State, {error, cancelled}),
    ok.

%% Killing the proxy `worker' does NOT cascade into aborting the
%% stream it opened — `macula_stream''s owner defaults to whichever
%% process called `call_stream/5' (the proxy), and an owner's death
%% only stops the stream quietly (`{stop, normal, _}', no frame ever
%% sent to the peer) — see `macula_stream:terminate/2', a no-op. This
%% is the actual fix: reach in and abort it explicitly, the same
%% peer-visible signal `macula_streamer'/`macula_stream_sink' send on
%% their own non-normal stops (Phase 5). `undefined' covers the window
%% before `{stream, Stream}' has arrived yet (still resolving/dialing).
%% `catch' covers the benign race between the proxy's own natural
%% completion and an external `cancel/1' landing at the same time.
reap_stream(undefined) -> ok;
reap_stream(Stream) ->
    try macula_stream:abort(Stream, ?CANCEL_CODE, <<"push cancelled">>)
    catch _:_ -> ok end,
    ok.

announce_completed(#pstate{completed = true} = State, _Result) ->
    State;
announce_completed(#pstate{pool = Pool, realm = Realm, announce = Announce,
                           share_id = ShareId} = State, Result) ->
    publish(Announce, Pool, Realm, ?PUSH_COMPLETED,
            outcome_fields(#{share_id => ShareId}, Result)),
    State#pstate{completed = true}.

outcome_fields(Base, {ok, Mcid}) ->
    Base#{outcome => completed, mcid => Mcid};
outcome_fields(Base, {error, cancelled}) ->
    Base#{outcome => cancelled};
outcome_fields(Base, {error, Reason}) ->
    Base#{outcome => failed, reason => Reason}.

publish(false, _, _, _, _) -> ok;
publish(true, Pool, Realm, Topic, Payload) ->
    _ = macula:publish(Pool, Realm, Topic, Payload), ok.
