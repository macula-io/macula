%% @doc Pubsub surface for the V2 SDK.
%%
%% Thin delegation over `macula_client' (the pool). The pool owns
%% the link state machine, replication, replay, and dedup; this
%% module is the named public entry point that consumers reach for
%% (or, more often, the `macula' facade re-exports of the same
%% functions).
%%
%% == Realm-per-call ==
%%
%% Per `PLAN_V2_PARITY' Q2 §2: every call carries its own 32-byte
%% realm tag. There is no connect-time default realm. A single pool
%% can multiplex any number of realms with no extra plumbing.
%%
%% == Quick start ==
%%
%% ```
%% {ok, Pool} = macula:connect(Seeds, ConnectOpts),
%% ok          = macula_pubsub:publish(Pool, Realm, Topic, Payload),
%% {ok, Sub}   = macula_pubsub:subscribe(Pool, Realm, Topic, self()),
%% receive
%%     {macula_event, Sub, Topic, Payload, Meta} -> ok
%% end,
%% ok          = macula_pubsub:unsubscribe(Pool, Sub).
%% '''
%%
%% See `docs/guides/PUBSUB_GUIDE.md' for a full guide and
%% `docs/migrations/V1_TO_V2_PUBSUB.md' for the breaking changes
%% from the pre-3.11.0 facade.
-module(macula_pubsub).

-export([publish/4, publish/5,
         subscribe/4, subscribe/5,
         unsubscribe/2]).

%% @doc Publish to `(Realm, Topic)' on `Pool'. Equivalent to
%% `publish/5' with empty opts.
-spec publish(macula_client:pool(), <<_:256>>, binary(), term()) ->
    ok | {error, term()}.
publish(Pool, Realm, Topic, Payload) ->
    publish(Pool, Realm, Topic, Payload, #{}).

%% @doc Publish to `(Realm, Topic)' on `Pool'.
%%
%% `Opts' currently honored:
%% <ul>
%%   <li>`timeout_ms' — gen_server call timeout (default 5_000).
%%       Most apps leave this as default.</li>
%% </ul>
%%
%% Returns `ok' as soon as one configured station accepts the
%% PUBLISH frame (partial success = success, per
%% `PLAN_V2_PARITY' §5.1.1). Returns
%% `{error, {transient, no_healthy_station}}' when the pool has no
%% spawned links; the caller may retry.
-spec publish(macula_client:pool(), <<_:256>>, binary(), term(), map()) ->
    ok | {error, term()}.
publish(Pool, Realm, Topic, Payload, Opts)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Topic),
       is_map(Opts) ->
    macula_client:publish(Pool, Realm, Topic, Payload, Opts).

%% @doc Subscribe `Subscriber' to `(Realm, Topic)' via `Pool'.
%% Equivalent to `subscribe/5' with empty opts.
-spec subscribe(macula_client:pool(), <<_:256>>, binary(), pid()) ->
    {ok, reference()}.
subscribe(Pool, Realm, Topic, Subscriber) ->
    subscribe(Pool, Realm, Topic, Subscriber, #{}).

%% @doc Subscribe `Subscriber' to `(Realm, Topic)' via `Pool'.
%%
%% Returns `{ok, SubRef}'. `Subscriber' subsequently receives
%% `{macula_event, SubRef, Topic, Payload, Meta}' for each delivered
%% event; `Meta' is a map carrying `realm', `publisher', `seq', and
%% `delivered_via'. Stores receive `{macula_event_gone, SubRef,
%% Reason}' once when the subscription terminates (pool close,
%% subscriber pid death).
%%
%% `Opts' is a forward-compatible map; Phase 1 honors no
%% subscribe-time options. Future phases (history replay, server-
%% side filters) will add named keys.
-spec subscribe(macula_client:pool(), <<_:256>>, binary(), pid(), map()) ->
    {ok, reference()}.
subscribe(Pool, Realm, Topic, Subscriber, Opts)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Topic),
       is_pid(Subscriber),
       is_map(Opts) ->
    macula_client:subscribe(Pool, Realm, Topic, Subscriber, Opts).

%% @doc Drop a subscription. Idempotent — unknown `SubRef' is a
%% no-op.
-spec unsubscribe(macula_client:pool(), reference()) -> ok.
unsubscribe(Pool, SubRef) when is_pid(Pool), is_reference(SubRef) ->
    macula_client:unsubscribe(Pool, SubRef).
