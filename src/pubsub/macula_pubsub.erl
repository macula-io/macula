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
%% Every call carries its own 32-byte
%% realm tag. There is no connect-time default realm. A single pool
%% can multiplex any number of realms with no extra plumbing.
%%
%% == Quick start ==
%%
%% ```
%% {ok, Pool} = macula:connect(Seeds, ConnectOpts),
%% ok          = macula_pubsub:publish(Pool, Realm, Topic, Payload),
%% {ok, Sub}   = macula_pubsub:subscribe(Pool, Realm, Topic, self(),
%%                                          #{sources => members}),
%% receive
%%     {macula_event, Sub, Topic, Payload, Meta} -> ok
%% end,
%% ok          = macula_pubsub:unsubscribe(Pool, Sub).
%% '''
%%
%% See `docs/guides/pubsub/PUBSUB_GUIDE.md' for a full guide.
-module(macula_pubsub).

-export([publish/4, publish/5,
         subscribe/5,
         subscribe_callback/5,
         unsubscribe/2,
         event_meta/2]).

-export_type([callback/0, event_meta/0]).

%% Callback shape accepted by `subscribe_callback/5'. Invoked once
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
    realm              := <<_:256>>,
    publisher          := <<_:256>>,
    %% Whether the publisher's signature was checked against `publisher'
    %% (macula#147). The link delivers an EVENT whose publication did not verify
    %% with `false' in lenient mode, and its `publisher' is then a claim: a
    %% subscription's declared sources refuse it.
    publisher_verified := boolean(),
    seq                := non_neg_integer(),
    published_at       := non_neg_integer(),
    delivered_via      := macula_frame:delivery_channel(),
    publication_hash   := <<_:384>>,
    expires_at         := non_neg_integer(),
    sealed             => 0 | 1,
    seal_key_id        => <<_:64>>
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
%%       (docs/design/DESIGN_E2E_SEALED_PUBSUB.md): the payload is sealed under
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
%% PUBLISH frame (partial success = success). Returns
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
%%
%% Returns `{ok, SubRef}'. `Subscriber' subsequently receives
%% `{macula_event, SubRef, Topic, Payload, Meta}' for each delivered
%% event, where `Meta' is an `event_meta()'. Only a publication that
%% verified is delivered. `Subscriber' also receives
%% `{macula_event_gone, SubRef, Reason}' once when the subscription
%% terminates (pool close, subscriber pid death).
%%
%% The pool carries a subscription on every station link it holds. When the
%% last link carrying it ends, whatever its station had written to that link
%% is lost, and `Subscriber' receives
%% `{macula_delivery_interrupted, SubRef, #{from_ms, reason}}': `from_ms' is
%% the last time that link heard from its station, `reason' the name of why it
%% ended. When a station accepts the subscription again, it receives
%% `{macula_delivery_gap, SubRef, #{from_ms, to_ms, reason}}': events on the
%% subscription may have been missed between the two times. Both are this
%% node's wall-clock milliseconds, a hint, not a filter: answer a gap by
%% resyncing what the subscription feeds, never by dropping events whose
%% `published_at' falls outside it. While another link still carries the
%% subscription nothing was missed and nothing is sent (macula-station#129).
%%
%% `Opts' honors `delivery' (see `macula:subscribe/5'), `sources' (see below) and
%% `group', a sealed group's prefix `Topic' (or every topic a pattern matches) must
%% be under, with `ucan_token' and `distributor' as for `publish/5'. The
%% group is joined before the subscription is made, and a refusal fails
%% it closed as `{error, {group, Reason}}'. Under a group, a sealed event
%% arrives opened, its meta saying `sealed => 1' and `seal_key_id'; one
%% that cannot be opened arrives once as `{macula_event_unopened, SubRef,
%% Topic, #{publisher, seal_key_id, reason}}', and nothing of its payload.
%% `reason' is one of `unknown_epoch', `epoch_expired', `not_authorized', `not_a_member',
%% `membership_unknown', `no_distributor', `no_group' (a sealed event on
%% a subscription that named no group) and `tag_invalid'. A clear event
%% carries `sealed => 0', and one under a prefix this node holds as
%% `required' is refused, counted and logged naming its publisher.
%%
%% `sources' declares who may deliver to this subscription, and there is NO
%% default: a subscription that names none is refused `{error, {sources, required}}'
%% rather than delivered to by anyone (macula#147). The forms are
%% `members' (any realm member), `{org, Organisation}' (a node that organisation
%% delegated), `{nodes, [NodeId]}' (exactly those publishers, and no lookup at
%% all), or `open' (an explicit opt-out). The subscriber's own SDK enforces it on
%% the publisher its link verified, before the handler runs, on both delivery
%% channels; `members' and `{org, _}' are refused `{error, {source_class,
%% no_realm_key}}' in a realm whose key the pool never pinned rather than served
%% open. A fact from a source that cannot be resolved is dropped and counted, and
%% the pool's `status/1' keeps three separate counters: refused (a verdict),
%% dropped (a budget) and held (waiting behind a lookup).
-spec subscribe(macula_client:pool(), <<_:256>>, binary(), pid(), map()) ->
    {ok, reference()}
    | {error, {text_too_long | invalid_text, topic} | {invalid_option, group | distributor | ucan_token}
              | {group, macula_group_keyring:reason()}
              | {sources, required} | {source_class, term()}}.
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
    #{realm => Realm, publisher => Publisher, publisher_verified => true, seq => Seq,
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
%% `Opts' is `subscribe/5's, and names `sources' like this arity's
%% receiver-pid sibling: there is no default. Two more options take the
%% notices a receiver pid gets when the subscription may be missing events
%% (see `subscribe/5'): `on_interrupted' and `on_gap', each a function of the
%% notice's map. Without them a notice is only logged. Any other value is
%% `{error, {invalid_option, on_interrupted | on_gap}}'.
%%
%% A crashing callback does NOT kill the receiver — the exception is
%% logged and the next event is delivered. This is intentional: a
%% transient bug in event handler N should not lose events N+1..M.
%%
%% Caller cleanup: invoke `unsubscribe(Pool, SubRef)' with the
%% returned ref. The receiver shuts down on the resulting
%% `macula_event_gone' message.
-spec subscribe_callback(macula_client:pool(), <<_:256>>, binary(),
                          callback(), map()) ->
    {ok, reference()} | {error, term()}.
subscribe_callback(Pool, Realm, Topic, Callback, Opts)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Topic),
       is_function(Callback, 3),
       is_map(Opts) ->
    callback_subscribed(invalid_notice([on_interrupted, on_gap], Opts), Pool, Realm, Topic, Callback, Opts).

%% A notice option is a function of one argument, or absent.
invalid_notice([], _Opts) -> none;
invalid_notice([Key | Keys], Opts) -> notice_option(maps:find(Key, Opts), Key, Keys, Opts).

notice_option({ok, Fun}, _Key, Keys, Opts) when is_function(Fun, 1) -> invalid_notice(Keys, Opts);
notice_option(error, _Key, Keys, Opts) -> invalid_notice(Keys, Opts);
notice_option({ok, _NotAFun}, Key, _Keys, _Opts) -> Key.

callback_subscribed(none, Pool, Realm, Topic, Callback, Opts) ->
    Caller = self(),
    {Receiver, Mon} = spawn_monitor(
        fun() -> receiver_init(Caller, Pool, Realm, Topic, Callback, Opts) end),
    await_init(Receiver, Mon);
callback_subscribed(Key, _Pool, _Realm, _Topic, _Callback, _Opts) ->
    {error, {invalid_option, Key}}.

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

receiver_init(Caller, Pool, Realm, Topic, Callback, Opts) ->
    Mons = {erlang:monitor(process, Caller), erlang:monitor(process, Pool)},
    on_subscribe(macula_client:subscribe(Pool, Realm, Topic, self(), Opts),
                 Caller, Mons, {Callback, maps:with([on_interrupted, on_gap], Opts)}).

on_subscribe({ok, SubRef}, Caller, Mons, Handlers) ->
    Caller ! {?MODULE, started, self(), SubRef},
    receiver_loop(SubRef, Mons, Handlers);
on_subscribe({error, _} = E, Caller, {CallerMon, PoolMon}, _Handlers) ->
    Caller ! {?MODULE, failed, self(), E},
    erlang:demonitor(CallerMon, [flush]),
    erlang:demonitor(PoolMon, [flush]).

receiver_loop(SubRef, {CallerMon, PoolMon} = Mons, {Callback, Notices} = Handlers) ->
    receive
        {macula_event, SubRef, Topic, Payload, Meta} ->
            invoke(Callback, Topic, Payload, Meta),
            receiver_loop(SubRef, Mons, Handlers);
        %% The subscription may be missing events (macula-station#129): told
        %% to the option's fun, else only logged, which the pool already did.
        {macula_delivery_interrupted, SubRef, Info} ->
            ok = notice_told(maps:find(on_interrupted, Notices), Info),
            receiver_loop(SubRef, Mons, Handlers);
        {macula_delivery_gap, SubRef, Info} ->
            ok = notice_told(maps:find(on_gap, Notices), Info),
            receiver_loop(SubRef, Mons, Handlers);
        %% A sealed event this subscription could not open: the callback
        %% takes payloads, and this one has none, so it is logged, never
        %% left in the mailbox.
        {macula_event_unopened, SubRef, Topic, Info} ->
            ok = macula_diagnostics:bounded_event(warning, <<"_macula.pubsub.event_unopened">>,
                                                  Info#{topic => Topic}),
            receiver_loop(SubRef, Mons, Handlers);
        {macula_event_gone, SubRef, _Reason} ->
            ok;
        {'DOWN', CallerMon, process, _, _} ->
            ok;
        {'DOWN', PoolMon, process, _Pool, _Reason} ->
            ok
    end.

notice_told({ok, Fun}, Info) ->
    try Fun(Info) of
        _ -> ok
    catch
        Class:Reason:Stack ->
            logger:warning("[macula_pubsub] notice callback crashed: ~p:~p~n~p", [Class, Reason, Stack])
    end;
notice_told(error, _Info) ->
    ok.

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
