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
%% See `docs/guides/pubsub/PUBSUB_GUIDE.md' for a full guide.
-module(macula_pubsub).

-export([publish/4, publish/5,
         subscribe/4, subscribe/5,
         subscribe_callback/4,
         unsubscribe/2,
         event_meta/2]).

-export_type([callback/0, event_meta/0]).

%% Callback shape accepted by `subscribe_callback/4'. Invoked once
%% per inbound event in a separate receiver process so a slow
%% callback does not back-pressure the pool.
-type callback() :: fun((Topic :: binary(),
                          Payload :: term(),
                          Meta :: event_meta()) -> any()).

%% The delivery context a subscriber receives with each event, taken
%% from the publication its link verified: its realm, its publisher's
%% node_id, its seq and published_at, how this copy arrived, and
%% publication_hash, the SHA-384 of its tbs, with expires_at, on which
%% the pool delivers each publication once. `sealed' is 1 for an event
%% opened with a sealed group's epoch, named by `seal_key_id', and 0 for
%% one that travelled in the clear.
-type event_meta() :: #{
    realm            := <<_:256>>,
    publisher        := <<_:256>>,
    seq              := non_neg_integer(),
    published_at     := non_neg_integer(),
    delivered_via    := macula_frame:delivery_channel(),
    publication_hash := <<_:384>>,
    expires_at       := non_neg_integer(),
    sealed           => 0 | 1,
    seal_key_id      => <<_:64>>
}.

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
%%   <li>`group' — a sealed group's prefix, which `Topic' must be under
%%       (plans/DESIGN_E2E_SEALED_PUBSUB.md): the payload is sealed under
%%       the group's current epoch, its key pulled from the org's
%%       distributor first. A refusal fails the publish closed as
%%       `{error, {group, Reason}}'; a topic outside the prefix, or a
%%       prefix with no org segment, is `{error, {invalid_option, group}}'.
%%       `ucan_token' carries the org's grant to the distributor, and
%%       `distributor' pins its node_id.</li>
%% </ul>
%%
%% Without `group', a topic under a group this node holds is refused as
%% `{error, {confidentiality, {group_held, Prefix}}}' rather than sent in
%% the clear.
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
    {ok, reference()} | {error, {text_too_long | invalid_text, topic}}.
subscribe(Pool, Realm, Topic, Subscriber) ->
    subscribe(Pool, Realm, Topic, Subscriber, #{}).

%% @doc Subscribe `Subscriber' to `(Realm, Topic)' via `Pool'.
%%
%% Returns `{ok, SubRef}'. `Subscriber' subsequently receives
%% `{macula_event, SubRef, Topic, Payload, Meta}' for each delivered
%% event, where `Meta' is an `event_meta()'. Only a publication that
%% verified is delivered. `Subscriber' also receives
%% `{macula_event_gone, SubRef, Reason}' once when the subscription
%% terminates (pool close, subscriber pid death).
%%
%% `Opts' honors `delivery' (see `macula:subscribe/5') and `group', a
%% sealed group's prefix `Topic' (or every topic a pattern matches) must
%% be under, with `ucan_token' and `distributor' as for `publish/5'. The
%% group is joined before the subscription is made, and a refusal fails
%% it closed as `{error, {group, Reason}}'. Under a group, a sealed event
%% arrives opened, its meta saying `sealed => 1' and `seal_key_id'; one
%% that cannot be opened arrives once as `{macula_event_unopened, SubRef,
%% Topic, #{publisher, seal_key_id, reason}}', and nothing of its payload.
%% `reason' is one of `unknown_epoch', `epoch_expired', `not_a_member',
%% `membership_unknown', `no_distributor', `no_group' (a sealed event on
%% a subscription that named no group) and `tag_invalid'. A clear event
%% carries `sealed => 0', and one under a prefix this node holds as
%% `required' is refused, counted and logged naming its publisher.
-spec subscribe(macula_client:pool(), <<_:256>>, binary(), pid(), map()) ->
    {ok, reference()}
    | {error, {text_too_long | invalid_text, topic} | {invalid_option, group | distributor | ucan_token}
              | {group, macula_group_keyring:reason()}}.
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

%% @doc The meta a subscriber receives with an event, built from a
%% publication that verified and the EVENT's `delivered_via'. A link
%% calls this for every event it delivers, so the meta has one producer.
-spec event_meta(macula_frame:verified_publication(),
                 macula_frame:delivery_channel()) -> event_meta().
event_meta(#{realm := Realm, publisher := Publisher, seq := Seq,
             published_at := PublishedAt, publication_hash := Hash,
             expires_at := ExpiresAt}, DeliveredVia)
  when DeliveredVia =:= plumtree; DeliveredVia =:= direct ->
    #{realm => Realm, publisher => Publisher, seq => Seq,
      published_at => PublishedAt, delivered_via => DeliveredVia,
      publication_hash => Hash, expires_at => ExpiresAt}.

%% @doc Subscribe with a callback function instead of a receiver pid.
%% Spawns a small receiver process internally that drives the
%% callback for every inbound event. The receiver monitors the
%% caller; if the caller dies, the receiver follows and the
%% subscription is cleaned up by the pool's standard subscriber-DOWN
%% path. It monitors the pool too, and ends when the pool dies: a pool
%% that is killed sends no `macula_event_gone'.
%%
%% A crashing callback does NOT kill the receiver — the exception is
%% logged and the next event is delivered. This is intentional: a
%% transient bug in event handler N should not lose events N+1..M.
%%
%% Caller cleanup: invoke `unsubscribe(Pool, SubRef)' with the
%% returned ref. The receiver shuts down on the resulting
%% `macula_event_gone' message.
-spec subscribe_callback(macula_client:pool(), <<_:256>>, binary(),
                          callback()) ->
    {ok, reference()} | {error, term()}.
subscribe_callback(Pool, Realm, Topic, Callback)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Topic),
       is_function(Callback, 3) ->
    Caller = self(),
    {Receiver, Mon} = spawn_monitor(
        fun() -> receiver_init(Caller, Pool, Realm, Topic, Callback) end),
    await_init(Receiver, Mon).

await_init(Receiver, Mon) ->
    receive
        {?MODULE, started, Receiver, SubRef} ->
            erlang:demonitor(Mon, [flush]),
            {ok, SubRef};
        {?MODULE, failed, Receiver, Err} ->
            erlang:demonitor(Mon, [flush]),
            Err;
        {'DOWN', Mon, process, Receiver, Reason} ->
            {error, {receiver_died, Reason}}
    after 5_000 ->
        exit(Receiver, init_timeout),
        erlang:demonitor(Mon, [flush]),
        {error, init_timeout}
    end.

receiver_init(Caller, Pool, Realm, Topic, Callback) ->
    Mons = {erlang:monitor(process, Caller), erlang:monitor(process, Pool)},
    on_subscribe(macula_client:subscribe(Pool, Realm, Topic, self(), #{}),
                 Caller, Mons, Callback).

on_subscribe({ok, SubRef}, Caller, Mons, Callback) ->
    Caller ! {?MODULE, started, self(), SubRef},
    receiver_loop(SubRef, Mons, Callback);
on_subscribe({error, _} = E, Caller, {CallerMon, PoolMon}, _Callback) ->
    Caller ! {?MODULE, failed, self(), E},
    erlang:demonitor(CallerMon, [flush]),
    erlang:demonitor(PoolMon, [flush]).

receiver_loop(SubRef, {CallerMon, PoolMon} = Mons, Callback) ->
    receive
        {macula_event, SubRef, Topic, Payload, Meta} ->
            invoke(Callback, Topic, Payload, Meta),
            receiver_loop(SubRef, Mons, Callback);
        %% A sealed event this subscription could not open: the callback
        %% takes payloads, and this one has none, so it is logged, never
        %% left in the mailbox.
        {macula_event_unopened, SubRef, Topic, Info} ->
            ok = macula_diagnostics:bounded_event(warning, <<"_macula.pubsub.event_unopened">>,
                                                  Info#{topic => Topic}),
            receiver_loop(SubRef, Mons, Callback);
        {macula_event_gone, SubRef, _Reason} ->
            ok;
        {'DOWN', CallerMon, process, _, _} ->
            ok;
        {'DOWN', PoolMon, process, _Pool, _Reason} ->
            ok
    end.

%% Guard the user callback so a transient handler bug does not
%% wedge the entire subscription stream. This is the rare place where
%% try/catch is the right tool: the SDK is owning a long-lived
%% receiver on behalf of an opaque consumer fun.
invoke(Callback, Topic, Payload, Meta) ->
    try Callback(Topic, Payload, Meta) of
        _ -> ok
    catch
        Class:Reason:Stack ->
            logger:warning("[macula_pubsub] callback crashed: ~p:~p~n~p",
                           [Class, Reason, Stack])
    end.
