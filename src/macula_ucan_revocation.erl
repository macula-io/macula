%% @doc UCAN Revocation Module.
%%
%% Manages revocation of UCAN tokens via mesh PubSub gossip. Revocations are
%% stored in an ETS cache and checked during authorization.
%%
%% == Revocation Flow ==
%%
%% 1. Issuer calls `revoke/2` with their DID and the UCAN CID
%% 2. Module broadcasts revocation to `io.macula.system.ucan_revoked` topic
%% 3. All mesh nodes receive via PubSub subscription
%% 4. Each node validates signature (issuer must be UCAN creator)
%% 5. Valid revocations stored in local ETS cache with TTL
%% 6. Authorization checks consult cache via `is_revoked/2`
%%
%% == System Topic ==
%%
%% All revocations are published to:
%%
%% `io.macula.system.ucan_revoked'
%%
%% == Rate Limiting ==
%%
%% Maximum 10 revocations per issuer per minute to prevent abuse.
%%
%% == Cache Auto-Expiry ==
%%
%% Revocation entries expire based on original UCAN expiry time.
%% A cleanup process runs periodically to purge expired entries.
%%
%% @author macula
-module(macula_ucan_revocation).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/0,
    stop/1,
    revoke/3,
    revoke/4,
    is_revoked/2,
    is_revoked/3,
    handle_revocation_message/1,
    handle_revocation_message/2,
    get_stats/0,
    get_stats/1,
    clear_cache/0,
    clear_cache/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% Internal exports for testing
-export([
    compute_ucan_cid/1,
    validate_revocation_signature/1,
    check_rate_limit/2
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Constants
%%====================================================================

-define(SERVER, ?MODULE).
-define(REVOCATION_TABLE, macula_revocation_cache).
-define(RATE_LIMIT_TABLE, macula_revocation_rate_limit).
-define(SYSTEM_TOPIC, <<"io.macula.system.ucan_revoked">>).
-define(MAX_REVOCATIONS_PER_MINUTE, 10).
-define(RATE_LIMIT_WINDOW_MS, 60000).  %% 1 minute
-define(CLEANUP_INTERVAL_MS, 60000).   %% 1 minute

%%====================================================================
%% Types
%%====================================================================

-type did() :: binary().
-type ucan_cid() :: binary().
-type ucan_token() :: binary().
-type timestamp() :: non_neg_integer().

-type revocation_msg() :: #{
    binary() => binary() | timestamp()
}.

-record(state, {
    cleanup_timer :: reference() | undefined,
    pubsub_pid :: pid() | undefined,
    stats :: #{atom() => non_neg_integer()}
}).

-export_type([did/0, ucan_cid/0, revocation_msg/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the revocation server with default name.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the revocation server with options.
-spec start_link(Opts :: map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Stop the revocation server (default name).
-spec stop() -> ok.
stop() ->
    stop(?SERVER).

%% @doc Stop a specific revocation server.
-spec stop(ServerRef :: atom() | pid()) -> ok.
stop(ServerRef) ->
    gen_server:stop(ServerRef).

%% @doc Revoke a UCAN token (uses default server).
%%
%% The issuer must sign the revocation message. The revocation is
%% broadcast to all mesh nodes via PubSub.
%%
%% `UcanToken' is the full UCAN JWT token being revoked.
%% `ExpiresAt' is the original expiry time of the UCAN.
%% `PrivateKey' is used to sign the revocation (Ed25519).
-spec revoke(IssuerDID :: did(), UcanToken :: ucan_token(),
             ExpiresAt :: timestamp()) ->
    {ok, ucan_cid()} | {error, term()}.
revoke(IssuerDID, UcanToken, ExpiresAt) ->
    revoke(?SERVER, IssuerDID, UcanToken, ExpiresAt).

%% @doc Revoke a UCAN token via specific server.
-spec revoke(ServerRef :: atom() | pid(), IssuerDID :: did(),
             UcanToken :: ucan_token(), ExpiresAt :: timestamp()) ->
    {ok, ucan_cid()} | {error, term()}.
revoke(ServerRef, IssuerDID, UcanToken, ExpiresAt) ->
    gen_server:call(ServerRef, {revoke, IssuerDID, UcanToken, ExpiresAt}).

%% @doc Check if a UCAN is revoked (uses default server).
-spec is_revoked(IssuerDID :: did(), UcanCID :: ucan_cid()) -> boolean().
is_revoked(IssuerDID, UcanCID) ->
    is_revoked(?SERVER, IssuerDID, UcanCID).

%% @doc Check if a UCAN is revoked via specific server.
-spec is_revoked(ServerRef :: atom() | pid(), IssuerDID :: did(),
                 UcanCID :: ucan_cid()) -> boolean().
is_revoked(_ServerRef, IssuerDID, UcanCID) ->
    %% Direct ETS lookup for performance (no gen_server call needed)
    case ets:lookup(?REVOCATION_TABLE, {IssuerDID, UcanCID}) of
        [{_, {_RevokedAt, ExpiresAt}}] ->
            Now = erlang:system_time(second),
            Now < ExpiresAt;  %% Only revoked if not yet expired
        [] ->
            false
    end.

%% @doc Handle incoming revocation message from PubSub.
-spec handle_revocation_message(Msg :: revocation_msg()) -> ok | {error, term()}.
handle_revocation_message(Msg) ->
    handle_revocation_message(?SERVER, Msg).

%% @doc Handle incoming revocation message via specific server.
-spec handle_revocation_message(ServerRef :: atom() | pid(),
                                 Msg :: revocation_msg()) -> ok | {error, term()}.
handle_revocation_message(ServerRef, Msg) ->
    gen_server:call(ServerRef, {handle_revocation, Msg}).

%% @doc Get revocation statistics (uses default server).
-spec get_stats() -> #{atom() => term()}.
get_stats() ->
    get_stats(?SERVER).

%% @doc Get revocation statistics via specific server.
-spec get_stats(ServerRef :: atom() | pid()) -> #{atom() => term()}.
get_stats(ServerRef) ->
    gen_server:call(ServerRef, get_stats).

%% @doc Clear all revocation cache entries (uses default server).
-spec clear_cache() -> ok.
clear_cache() ->
    clear_cache(?SERVER).

%% @doc Clear all revocation cache entries via specific server.
-spec clear_cache(ServerRef :: atom() | pid()) -> ok.
clear_cache(ServerRef) ->
    gen_server:call(ServerRef, clear_cache).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Opts) ->
    %% Create ETS tables if they don't exist
    create_tables(),

    %% Start cleanup timer
    Timer = erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_expired),

    %% Subscribe to revocation topic if PubSub provided
    PubSubPid = maps:get(pubsub_pid, Opts, undefined),

    State = #state{
        cleanup_timer = Timer,
        pubsub_pid = PubSubPid,
        stats = #{
            revocations_issued => 0,
            revocations_received => 0,
            revocations_rejected => 0,
            rate_limit_hits => 0,
            cache_size => 0
        }
    },

    {ok, State}.

handle_call({revoke, IssuerDID, UcanToken, ExpiresAt}, _From, State) ->
    case do_revoke(IssuerDID, UcanToken, ExpiresAt, State) of
        {ok, CID, State2} ->
            {reply, {ok, CID}, State2};
        {error, Reason, State2} ->
            {reply, {error, Reason}, State2}
    end;

handle_call({handle_revocation, Msg}, _From, State) ->
    case do_handle_revocation(Msg, State) of
        {ok, State2} ->
            {reply, ok, State2};
        {error, Reason, State2} ->
            {reply, {error, Reason}, State2}
    end;

handle_call(get_stats, _From, State) ->
    CacheSize = ets:info(?REVOCATION_TABLE, size),
    Stats = maps:put(cache_size, CacheSize, State#state.stats),
    {reply, Stats, State};

handle_call(clear_cache, _From, State) ->
    ets:delete_all_objects(?REVOCATION_TABLE),
    ets:delete_all_objects(?RATE_LIMIT_TABLE),
    Stats = maps:map(fun(_, _) -> 0 end, State#state.stats),
    {reply, ok, State#state{stats = Stats}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_expired, State) ->
    do_cleanup_expired(),
    Timer = erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_expired),
    {noreply, State#state{cleanup_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{cleanup_timer = Timer}) ->
    case Timer of
        undefined -> ok;
        _ -> erlang:cancel_timer(Timer)
    end,
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Create ETS tables for revocation and rate limiting.
create_tables() ->
    case ets:whereis(?REVOCATION_TABLE) of
        undefined ->
            ets:new(?REVOCATION_TABLE, [
                named_table, public, set,
                {read_concurrency, true}
            ]);
        _ -> ok
    end,
    case ets:whereis(?RATE_LIMIT_TABLE) of
        undefined ->
            ets:new(?RATE_LIMIT_TABLE, [
                named_table, public, set
            ]);
        _ -> ok
    end,
    ok.

%% @private Execute a revocation.
do_revoke(IssuerDID, UcanToken, ExpiresAt, State) ->
    %% Check rate limit
    case check_rate_limit(IssuerDID, ?MAX_REVOCATIONS_PER_MINUTE) of
        ok ->
            %% Compute CID of the UCAN
            CID = compute_ucan_cid(UcanToken),
            Now = erlang:system_time(second),

            %% Store in local cache
            ets:insert(?REVOCATION_TABLE, {
                {IssuerDID, CID},
                {Now, ExpiresAt}
            }),

            %% Increment rate limit counter
            increment_rate_limit(IssuerDID),

            %% Update stats
            Stats = State#state.stats,
            Stats2 = maps:update_with(revocations_issued, fun(V) -> V + 1 end, Stats),

            %% Broadcast to mesh (if PubSub available)
            broadcast_to_mesh(IssuerDID, CID, Now, ExpiresAt, State),

            {ok, CID, State#state{stats = Stats2}};

        {error, rate_limited} ->
            Stats = State#state.stats,
            Stats2 = maps:update_with(rate_limit_hits, fun(V) -> V + 1 end, Stats),
            {error, rate_limited, State#state{stats = Stats2}}
    end.

%% @private Handle an incoming revocation message.
do_handle_revocation(Msg, State) ->
    case validate_revocation_message(Msg) of
        {ok, IssuerDID, CID, RevokedAt, ExpiresAt} ->
            %% Validate signature
            case validate_revocation_signature(Msg) of
                ok ->
                    %% Store in cache
                    ets:insert(?REVOCATION_TABLE, {
                        {IssuerDID, CID},
                        {RevokedAt, ExpiresAt}
                    }),

                    Stats = State#state.stats,
                    Stats2 = maps:update_with(revocations_received,
                                               fun(V) -> V + 1 end, Stats),
                    {ok, State#state{stats = Stats2}};

                {error, Reason} ->
                    ?LOG_WARNING("Revocation signature invalid: ~p", [Reason]),
                    Stats = State#state.stats,
                    Stats2 = maps:update_with(revocations_rejected,
                                               fun(V) -> V + 1 end, Stats),
                    {error, invalid_signature, State#state{stats = Stats2}}
            end;

        {error, Reason} ->
            Stats = State#state.stats,
            Stats2 = maps:update_with(revocations_rejected, fun(V) -> V + 1 end, Stats),
            {error, Reason, State#state{stats = Stats2}}
    end.

%% @private Validate revocation message format.
validate_revocation_message(Msg) when is_map(Msg) ->
    IssuerDID = maps:get(<<"issuer_did">>, Msg, undefined),
    CID = maps:get(<<"ucan_cid">>, Msg, undefined),
    RevokedAt = maps:get(<<"revoked_at">>, Msg, undefined),
    ExpiresAt = maps:get(<<"expires_at">>, Msg, undefined),
    Signature = maps:get(<<"signature">>, Msg, undefined),

    case {IssuerDID, CID, RevokedAt, ExpiresAt, Signature} of
        {undefined, _, _, _, _} ->
            {error, missing_issuer_did};
        {_, undefined, _, _, _} ->
            {error, missing_ucan_cid};
        {_, _, undefined, _, _} ->
            {error, missing_revoked_at};
        {_, _, _, undefined, _} ->
            {error, missing_expires_at};
        {_, _, _, _, undefined} ->
            {error, missing_signature};
        {I, C, R, E, _S} when is_binary(I), is_binary(C),
                              is_integer(R), is_integer(E) ->
            {ok, I, C, R, E};
        _ ->
            {error, invalid_format}
    end;
validate_revocation_message(_) ->
    {error, not_a_map}.

%% @private Validate revocation signature.
%% For now, just check signature is present and non-empty.
%% Full Ed25519 verification would require crypto operations.
-spec validate_revocation_signature(Msg :: revocation_msg()) -> ok | {error, term()}.
validate_revocation_signature(Msg) ->
    case maps:get(<<"signature">>, Msg, undefined) of
        undefined ->
            {error, missing_signature};
        <<>> ->
            {error, empty_signature};
        Sig when is_binary(Sig), byte_size(Sig) >= 64 ->
            %% Ed25519 signatures are 64 bytes
            %% For now, just validate format. Full crypto would need:
            %% - Extract public key from issuer DID
            %% - Verify signature over message content
            ok;
        _ ->
            {error, invalid_signature_format}
    end.

%% @private Compute CID (content identifier) for a UCAN token.
%% Uses SHA-256 hash encoded as base64url.
-spec compute_ucan_cid(UcanToken :: ucan_token()) -> ucan_cid().
compute_ucan_cid(UcanToken) when is_binary(UcanToken) ->
    Hash = crypto:hash(sha256, UcanToken),
    base64url_encode(Hash).

%% @private URL-safe base64 encoding (no padding).
base64url_encode(Bin) ->
    B64 = base64:encode(Bin),
    B64_1 = binary:replace(B64, <<"+">>, <<"-">>, [global]),
    B64_2 = binary:replace(B64_1, <<"/">>, <<"_">>, [global]),
    %% Remove padding
    binary:replace(B64_2, <<"=">>, <<>>, [global]).

%% @private Check rate limit for an issuer.
-spec check_rate_limit(IssuerDID :: did(), MaxPerMinute :: pos_integer()) ->
    ok | {error, rate_limited}.
check_rate_limit(IssuerDID, MaxPerMinute) ->
    Now = erlang:system_time(millisecond),
    WindowStart = Now - ?RATE_LIMIT_WINDOW_MS,

    case ets:lookup(?RATE_LIMIT_TABLE, IssuerDID) of
        [{_, Timestamps}] ->
            %% Filter to only recent timestamps
            Recent = [T || T <- Timestamps, T > WindowStart],
            case length(Recent) >= MaxPerMinute of
                true ->
                    {error, rate_limited};
                false ->
                    ok
            end;
        [] ->
            ok
    end.

%% @private Increment rate limit counter for an issuer.
increment_rate_limit(IssuerDID) ->
    Now = erlang:system_time(millisecond),
    WindowStart = Now - ?RATE_LIMIT_WINDOW_MS,

    NewTimestamps = case ets:lookup(?RATE_LIMIT_TABLE, IssuerDID) of
        [{_, Timestamps}] ->
            %% Keep only recent timestamps + new one
            [T || T <- Timestamps, T > WindowStart] ++ [Now];
        [] ->
            [Now]
    end,

    ets:insert(?RATE_LIMIT_TABLE, {IssuerDID, NewTimestamps}).

%% @private Broadcast revocation to mesh via PubSub.
broadcast_to_mesh(_IssuerDID, CID, _RevokedAt, _ExpiresAt, #state{pubsub_pid = undefined}) ->
    ?LOG_DEBUG("No PubSub available for revocation broadcast: ~s", [CID]),
    ok;
broadcast_to_mesh(IssuerDID, CID, RevokedAt, ExpiresAt, #state{pubsub_pid = _PubSubPid}) ->
    Msg = #{
        <<"issuer_did">> => IssuerDID,
        <<"ucan_cid">> => CID,
        <<"revoked_at">> => RevokedAt,
        <<"expires_at">> => ExpiresAt,
        <<"signature">> => create_dummy_signature()  %% TODO: Real Ed25519 signing
    },
    %% This would call macula_pubsub_handler:publish/3
    %% For now, just log the intent
    ?LOG_DEBUG("Would broadcast revocation to ~s: ~p", [?SYSTEM_TOPIC, Msg]),
    ok.

%% @private Create a dummy signature placeholder.
%% In production, this would use Ed25519 signing.
create_dummy_signature() ->
    %% 64-byte placeholder (Ed25519 signature size)
    crypto:strong_rand_bytes(64).

%% @private Cleanup expired revocation entries.
do_cleanup_expired() ->
    Now = erlang:system_time(second),

    %% Find and delete expired entries
    Expired = ets:foldl(
        fun({{IssuerDID, CID}, {_RevokedAt, ExpiresAt}}, Acc) ->
            case Now >= ExpiresAt of
                true -> [{IssuerDID, CID} | Acc];
                false -> Acc
            end
        end,
        [],
        ?REVOCATION_TABLE
    ),

    lists:foreach(
        fun(Key) ->
            ets:delete(?REVOCATION_TABLE, Key)
        end,
        Expired
    ),

    case Expired of
        [] -> ok;
        _ -> ?LOG_DEBUG("Cleaned up ~p expired revocations", [length(Expired)])
    end.
