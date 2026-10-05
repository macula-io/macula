%% @doc The process that opens a sealed group's events for one subscription (docs/design/DESIGN_E2E_SEALED_PUBSUB.md §6, §7).
%%
%% The pool hands it every event the subscription's ordering releases, in order, and it answers the subscriber:
%%
%% <ul>
%%   <li>a sealed event under an epoch the keyring holds, or pulls by id, and that opens: `{macula_event, SubRef,
%%       Topic, Payload, Meta}' with `sealed => 1' and `seal_key_id' in the meta;</li>
%%   <li>a sealed event that cannot be opened: `{macula_event_unopened, SubRef, Topic, #{publisher, seal_key_id,
%%       reason}}' once, and nothing of the payload. `reason' is `unknown_epoch', `epoch_expired', `not_a_member',
%%       `membership_unknown', `no_distributor', `no_group' (the group was left) or `tag_invalid';</li>
%%   <li>a clear event: delivered with `sealed => 0', unless this node holds `required' for a prefix covering its
%%       topic, when it is refused, counted and logged naming its publisher.</li>
%% </ul>
%%
%% It runs apart from the pool because opening may pull a missed epoch from the distributor, over the pool, for as
%% long as that call's deadline: the pool never waits on it. It ends when the pool does, or when told to stop.
-module(macula_group_opener).

-export([start/1, open/4, stop/1, delivered/4]).

-type options() :: #{keyring := macula_group_keyring:keyring(), subscriber := pid(), sub_ref := reference(),
                     realm := <<_:256>>, prefix := binary(), pool := pid()}.
-export_type([options/0]).

%% @doc Start an opener; the caller monitors it.
-spec start(options()) -> pid().
start(#{pool := Pool} = Opts) ->
    spawn(fun() -> loop(Opts#{pool_mon => erlang:monitor(process, Pool)}) end).

%% @doc Hand the opener one event the subscription's ordering released.
-spec open(pid(), binary(), term(), map()) -> ok.
open(Opener, Topic, Payload, Meta) ->
    Opener ! {open, Topic, Payload, Meta},
    ok.

-spec stop(pid()) -> ok.
stop(Opener) ->
    Opener ! stop,
    ok.

loop(#{pool_mon := PoolMon} = Opts) ->
    receive
        {open, Topic, Payload, Meta} ->
            _ = deliver(Topic, Payload, Meta, Opts),
            loop(Opts);
        stop ->
            ok;
        {'DOWN', PoolMon, process, _Pool, _Reason} ->
            ok
    end.

deliver(Topic, _Payload, #{sealed := #{key_id := Id}, publisher := Publisher} = Meta,
        #{keyring := Keyring, realm := Realm, prefix := Prefix} = Opts) ->
    opened(epoch_opened(macula_group_keyring:open_epoch(Keyring, Realm, Prefix, Id, Publisher), Topic, Meta),
           Topic, Id, Meta, Opts);
deliver(Topic, Payload, Meta, #{keyring := Keyring, subscriber := Subscriber, sub_ref := SubRef}) ->
    delivered(Keyring, Topic, Meta, fun(Clear) -> Subscriber ! {macula_event, SubRef, Topic, Payload, Clear} end).

epoch_opened({ok, Epoch}, Topic, #{realm := Realm, publisher := Publisher, seq := Seq, published_at := PublishedAt,
                                   sealed := Sealed}) ->
    macula_group_event:open(Epoch, #{realm => Realm, topic => Topic, publisher => Publisher, seq => Seq,
                                     published_at => PublishedAt, sealed => Sealed});
epoch_opened({error, not_joined}, _Topic, _Meta) ->
    {error, no_group};
epoch_opened({error, _Reason} = Refused, _Topic, _Meta) ->
    Refused.

opened({ok, Payload}, Topic, Id, Meta, #{subscriber := Subscriber, sub_ref := SubRef}) ->
    Subscriber ! {macula_event, SubRef, Topic, Payload, (maps:remove(sealed, Meta))#{sealed => 1, seal_key_id => Id}};
opened({error, Reason}, Topic, Id, #{publisher := Publisher}, #{subscriber := Subscriber, sub_ref := SubRef}) ->
    Subscriber ! {macula_event_unopened, SubRef, Topic, #{publisher => Publisher, seal_key_id => Id, reason => Reason}}.

%% @doc A clear event, as every subscription on this node takes it: handed to `Deliver' with `sealed => 0', unless the
%% node holds `required' for a prefix covering `Topic', when it is refused, counted and logged naming its publisher.
%% Reads the keyring's table only, so the pool calls it too.
-spec delivered(macula_group_keyring:keyring(), binary(), map(), fun((map()) -> term())) -> ok.
delivered(Keyring, Topic, #{realm := Realm} = Meta, Deliver) ->
    required_or_clear(macula_group_keyring:policy(Keyring, Realm, Topic), Topic, Meta, Deliver).

required_or_clear({ok, Prefix, required}, Topic, #{realm := Realm, publisher := Publisher}, _Deliver) ->
    macula_diagnostics:bounded_event(warning, <<"_macula.group.clear_event_refused">>,
                                     #{realm => Realm, prefix => Prefix, topic => Topic, publisher => Publisher});
required_or_clear(_NotRequired, _Topic, Meta, Deliver) ->
    _ = Deliver(Meta#{sealed => 0}),
    ok.
