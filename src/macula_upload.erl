%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised content uploads (the receiver side of
%%% a push-initiated upload — PLAN_PUSH_UPLOAD.md Phase 6, the
%%% recipient `macula_pusher' pushes at).
%%%
%%% `advertise/5,6' registers an upload procedure. Each inbound push
%%% starts one supervised, ephemeral receiver: it reads the manifest
%%% `macula_pusher' passed as the stream's open-time `Args' (decoded
%%% via `macula_manifest:from_wire/1', the same decode content
%%% sharing's own `_content.get_manifest' path already relies on),
%%% accumulates pushed chunks, and once the sender half-closes,
%%% reassembles and verifies them against the manifest with
%%% `macula_manifest:verify/2' — receiver-side verification, never
%%% sender-trusted, matching content-sharing's existing "content is
%%% self-verifying by hash" model exactly; the sender's claimed
%%% manifest proves nothing on its own. Delivers
%%% `{ok, Mcid, Bytes} | {error, _}' to `Module:handle_uploaded/2', and
%%% publishes `sharing.upload_started_v1' / `sharing.upload_completed_v1'
%%% mesh facts around each push's lifetime.
%%%
%%% == A correction from the plan's literal wording ==
%%%
%%% The plan that named this module described it as "mirroring
%%% `macula_download''s shape" while, in the same sentence, also
%%% saying its public API is `advertise'/`advertise_direct' —
%%% `macula_download' doesn't have those; it has `start_link/4,5' (a
%%% one-shot, caller-initiated fetch). What this module actually is —
%%% a long-lived, ADVERTISED provider spawning one ephemeral child per
%%% inbound push — mirrors `macula_streamer''s shape, not
%%% `macula_download''s. Built directly on top of it: this module IS a
%%% `macula_streamer' callback module internally (`?MODULE' is passed
%%% as `macula_streamer:advertise/6''s own `Module' argument, closing
%%% over the caller's `Module'/`Args' in its own state), reusing
%%% Phase 5's supervision, `client_stream' receive loop
%%% (`handle_chunk/2'), and abort-wired cancel for free — "inherits...
%%% cancel from day one, nothing to retrofit later," per the plan's own
%%% intro to this phase.
%%%
%%% == The terminal reply, and why `handle_uploaded/2' return values
%%% are ignored ==
%%%
%%% Once the sender half-closes, `macula_streamer' calls this module's
%%% `handle_eof/1' (Phase 6's new optional callback on that module) —
%%% the one place with access to the raw stream needed to set a
%%% terminal reply. Verification happens right there: `{reply, {ok,
%%% Mcid}, NewState}' on success or `{reply, {error, Reason}, NewState}'
%%% on failure, which `macula_streamer' turns into
%%% `macula_stream:set_reply/2' / `set_error/2' — exactly the channel
%%% `macula_pusher''s own `macula:await_reply/1' blocks on. The Mcid
%%% itself needs no separate round trip: it's a field already present
%%% in the manifest both sides hold, deterministic from the same bytes
%%% the sender is pushing (`macula_manifest' mirrors macula-station's
%%% own algorithm BYTE-FOR-BYTE) — trustworthy to echo back specifically
%%% BECAUSE verification against the actually-received bytes already
%%% passed, not despite it.
%%%
%%% `Module:handle_uploaded/2' — the LOCAL delivery, to whatever
%%% application registered this upload handler — fires separately, from
%%% this module's own `terminate/2' (after the wire-level reply is
%%% already set, so a slow local callback can never block it). Because
%%% `terminate/2' can no longer act on a `{stop, _, _}' return the way a
%%% live `handle_info' clause could, `handle_uploaded/2''s return value
%%% is ignored — `any()', not `{noreply,_} | {stop,_,_}' — the same
%%% shape `macula_stream_sink''s own `handle_close/2' already uses for
%%% the identical reason (it too only ever fires from `terminate/2').
%%% This is a deliberate divergence from `macula_feeder'/`macula_download''s
%%% `handle_fed'/`handle_downloaded' callbacks, which DO get a
%%% meaningful `{stop,_,_}' because they fire from a live `handle_info'
%%% clause, not `terminate/2' — "mirrors macula_feeder's shape exactly"
%%% does not hold for this specific callback's return contract, traced
%%% to WHERE each one is actually invoked from.
%%%
%%% A malicious or buggy sender that never half-closes, or that pushes
%%% more chunks than its own manifest declared, is bounded: exceeding
%%% the declared `chunk_count' stops the receiver (a genuine
%%% `{error, too_many_chunks}', abort-wired same as any other non-normal
%%% stop) rather than accumulating chunks without limit — the manifest
%%% is a system-boundary input from an untrusted remote peer, worth
%%% guarding explicitly, unlike `macula_manifest:verify/2''s own
%%% eventual `size_mismatch', which would only catch this after the
%%% fact.
%%%
%%% == Direct-dial ==
%%%
%%% `advertise_direct/6,7' is `macula_streamer:advertise_direct/6,7'
%%% verbatim, `mode => client_stream' folded into `Opts' before
%%% forwarding — see that module's own "Direct-dial" section. (Building
%%% this surfaced a real, separate bug in `macula_streamer:advertise_direct/7'
%%% itself: it called the arity-5 `advertise/5', silently discarding
%%% whatever `mode' `Opts' carried, so ANY direct-dial-advertised
%%% `client_stream' procedure — not just this module's — would have been
%%% served as `server_stream' instead, with no error anywhere to say
%%% so. Fixed at the source, in `macula_streamer.erl' itself, per this
%%% project's "fix bugs in owned libraries immediately" rule — not
%%% specific to Phase 6, but found while building it.)
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(doc_upload).
%%% -behaviour(macula_upload).
%%% -export([init/1, handle_uploaded/2]).
%%%
%%% init(Parent) -> {ok, Parent}.
%%%
%%% handle_uploaded(Result, Parent) ->
%%%     Parent ! {uploaded, Result},
%%%     ok.
%%% '''
%%%
%%% ```
%%% {ok, _Sup} = macula_upload:advertise(Pool, Realm,
%%%     <<"bulk.ingest">>, doc_upload, self()).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_upload).

-behaviour(macula_streamer).

-export([advertise/5, advertise/6, advertise_direct/6, advertise_direct/7,
        unadvertise/3]).
-export([init/1, handle_open/2, handle_chunk/2, handle_eof/1, terminate/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_uploaded(
    Result :: {ok, macula:mcid(), binary()} | {error, term()},
    State :: term()) -> any().

-define(UPLOAD_STARTED, <<"sharing.upload_started_v1">>).
-define(UPLOAD_COMPLETED, <<"sharing.upload_completed_v1">>).

-record(ustate, {
    module    :: module(),
    pool      :: macula:pool(),
    realm     :: macula:realm(),
    announce  :: boolean(),
    share_id  :: binary() | undefined,
    manifest  :: map() | {error, term()} | undefined,
    acc       = [] :: [binary()],
    result    :: {ok, macula:mcid(), binary()} | {error, term()} | undefined,
    user      :: term()
}).

%% @doc Advertise `Procedure' on `Pool'/`Realm' as an upload target.
%% Each push sent at it starts one supervised, ephemeral receiver,
%% threading state through `Module:init/1' and delivering the final
%% outcome to `Module:handle_uploaded/2'.
-spec advertise(macula:pool(), macula:realm(), macula:procedure(),
                module(), term()) -> {ok, pid()} | {error, term()}.
advertise(Pool, Realm, Procedure, Module, Args) ->
    advertise(Pool, Realm, Procedure, Module, Args, #{}).

%% @doc As `advertise/5'. `Opts' may include `announce' (default
%% `true') for this module's OWN `sharing.upload_*' facts.
-spec advertise(macula:pool(), macula:realm(), macula:procedure(),
                module(), term(), map()) -> {ok, pid()} | {error, term()}.
advertise(Pool, Realm, Procedure, Module, Args, Opts) ->
    Announce = maps:get(announce, Opts, true),
    macula_streamer:advertise(Pool, Realm, Procedure, ?MODULE,
                              {Module, Pool, Realm, Announce, Args},
                              #{mode => client_stream, announce => false}).

%% @doc As `advertise/5', and additionally publishes a signed
%% `procedure_advertisement' DHT record naming this pool's connected
%% station as the server, so `macula_pusher:start_link_direct/5,6' can
%% resolve and dial here directly. See `macula_streamer:advertise_direct/6,7'.
-spec advertise_direct(macula:pool(), macula:realm(), macula:procedure(),
                       module(), term(), macula_identity:key_pair()) ->
    {ok, pid()} | {error, term()}.
advertise_direct(Pool, Realm, Procedure, Module, Args, Identity) ->
    advertise_direct(Pool, Realm, Procedure, Module, Args, Identity, #{}).

%% @doc As `advertise_direct/6', with `Opts' forwarded to
%% `macula_direct_dial:publish_advertisement/5' — e.g. `cert_chain =>
%% ChainPem' (Slice 7c Direction B, managed realms only).
-spec advertise_direct(macula:pool(), macula:realm(), macula:procedure(),
                       module(), term(), macula_identity:key_pair(), map()) ->
    {ok, pid()} | {error, term()}.
advertise_direct(Pool, Realm, Procedure, Module, Args, Identity, Opts) ->
    Announce = maps:get(announce, Opts, true),
    macula_streamer:advertise_direct(Pool, Realm, Procedure, ?MODULE,
                                     {Module, Pool, Realm, Announce, Args},
                                     Identity,
                                     Opts#{mode => client_stream, announce => false}).

%% @doc Stop advertising `Procedure'.
-spec unadvertise(macula:pool(), macula:realm(), macula:procedure()) -> ok.
unadvertise(Pool, Realm, Procedure) ->
    macula_streamer:unadvertise(Pool, Realm, Procedure).

%%%===================================================================
%%% macula_streamer callbacks (internal — this module IS the streamer
%%% callback module; the caller's OWN Module/Args are closed over
%%% above and delegated to below)
%%%===================================================================

%% @private
init({Module, Pool, Realm, Announce, InitArgs}) ->
    case Module:init(InitArgs) of
        {ok, UserState} ->
            {ok, #ustate{module = Module, pool = Pool, realm = Realm,
                        announce = Announce, user = UserState}};
        {stop, Reason} ->
            {stop, Reason}
    end.

%% @private A manifest that fails to decode is NOT rejected via
%% `{stop, Reason, State}' here — a `handle_open/2' stop makes
%% `macula_streamer:init/1' itself return `{stop, Reason}', a genuine
%% gen_server init failure, and OTP never calls `terminate/2' for a
%% process that failed to start. That would silently drop this push:
%% `Module:handle_uploaded/2' never fires, no
%% `sharing.upload_completed_v1' fact publishes, AND (traced, not just
%% theorized) the sender's own `macula:await_reply/1' would hang or
%% crash rather than seeing a clean `{error,_}' — nothing settles its
%% wire-level reply either, since the stream never reaches
%% `handle_eof/1'. Accepting the stream and stashing the decode error
%% in `manifest' instead means `handle_eof/1' below — the ONE place
%% already wired to set a terminal reply — reports it correctly on
%% both sides once the sender closes, exactly like any other failure.
handle_open(StreamArgs, State) ->
    case macula_manifest:from_wire(StreamArgs) of
        {ok, Manifest} ->
            {ok, announce_started(State#ustate{manifest = Manifest})};
        {error, Reason} ->
            {ok, State#ustate{manifest = {error, {invalid_manifest, Reason}}}}
    end.

announce_started(#ustate{pool = Pool, realm = Realm, announce = Announce,
                         manifest = Manifest} = State) ->
    ShareId = crypto:strong_rand_bytes(16),
    publish(Announce, Pool, Realm, ?UPLOAD_STARTED,
            #{share_id => ShareId, mcid => maps:get(mcid, Manifest),
              size => maps:get(size, Manifest)}),
    State#ustate{share_id = ShareId}.

%% @private Chunks pushed against a manifest that never decoded are
%% dropped, not accumulated — there's no `chunk_count' to bound
%% against, and no point: `handle_eof/1' will report the real error
%% once the sender closes, same as if no chunks had arrived at all.
%% Otherwise bounded: a sender exceeding its own manifest's declared
%% `chunk_count' is stopped rather than accumulated without limit —
%% see the module doc.
handle_chunk(_Chunk, #ustate{manifest = {error, _}} = State) ->
    {noreply, State};
handle_chunk(Chunk, #ustate{manifest = #{chunk_count := ChunkCount},
                            acc = Acc} = State)
        when length(Acc) >= ChunkCount ->
    {stop, too_many_chunks, State#ustate{acc = [Chunk | Acc]}};
handle_chunk(Chunk, #ustate{acc = Acc} = State) ->
    {noreply, State#ustate{acc = [Chunk | Acc]}}.

%% @private The sender is done. Reassemble, verify, and hand the
%% terminal reply straight to `macula_streamer' — see the module doc's
%% "terminal reply" section for why this can't happen anywhere else.
handle_eof(#ustate{manifest = {error, Reason}} = State) ->
    {reply, {error, Reason}, State#ustate{result = {error, Reason}}};
handle_eof(#ustate{manifest = Manifest, acc = Acc} = State) ->
    Data = iolist_to_binary(lists:reverse(Acc)),
    reply_for(macula_manifest:verify(Manifest, Data), Manifest, Data, State).

reply_for(ok, Manifest, Data, State) ->
    Mcid = maps:get(mcid, Manifest),
    {reply, {ok, Mcid}, State#ustate{result = {ok, Mcid, Data}}};
reply_for({error, Reason}, _Manifest, _Data, State) ->
    {reply, {error, Reason}, State#ustate{result = {error, Reason}}}.

%% @private
terminate(Reason, #ustate{module = Module, pool = Pool, realm = Realm,
                          announce = Announce, share_id = ShareId,
                          result = Result, user = UserState}) ->
    announce_completed(ShareId, Announce, Pool, Realm, Reason, Result),
    _ = Module:handle_uploaded(final_result(Reason, Result), UserState),
    ok.

%% No `sharing.upload_started_v1' ever fired (a bad manifest failed
%% `handle_open/2' before `share_id' was ever minted) — nothing to
%% close out.
announce_completed(undefined, _Announce, _Pool, _Realm, _Reason, _Result) ->
    ok;
announce_completed(ShareId, Announce, Pool, Realm, Reason, Result) ->
    publish(Announce, Pool, Realm, ?UPLOAD_COMPLETED,
            outcome_fields(#{share_id => ShareId}, final_result(Reason, Result))).

final_result(_Reason, {ok, _Mcid, _Data} = R) -> R;
final_result(_Reason, {error, _} = R) -> R;
final_result(Reason, undefined) -> {error, Reason}.

outcome_fields(Base, {ok, Mcid, Data}) ->
    Base#{outcome => completed, mcid => Mcid, size => byte_size(Data)};
outcome_fields(Base, {error, Reason}) ->
    Base#{outcome => failed, reason => Reason}.

publish(false, _, _, _, _) -> ok;
publish(true, Pool, Realm, Topic, Payload) ->
    _ = macula:publish(Pool, Realm, Topic, Payload), ok.
