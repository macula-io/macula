%% @private
%% @doc Which station links carry each pool subscription, and when a
%% subscription may have missed events (macula-station#129, phase 1).
%%
%% The pool subscribes every `(Realm, Topic)' on every link it holds. A link
%% covers a subscription from the moment its station accepts it (an accepting
%% REGISTER_ACK) until the link ends or its station refuses it. While at least
%% one link covers a subscription, every publication reached the pool on one of
%% them. When the last covering link ends, whatever its station wrote to it
%% since it was last heard from is lost, and the subscription is interrupted
%% from that time. When a station accepts it again on a link, the interruption
%% ends, and the subscription may have missed events between the two.
%%
%% A link is heard from when it connects, when its station answers a
%% registration, and when it reports the last frame it received as it
%% disconnects. An interruption starts at the latest time any link that covered
%% the subscription was heard from, never later, so the window it names is never
%% shorter than the loss.
%%
%% Pure state: the pool keeps it, and sends the notices it returns.
-module(macula_client_coverage).
-export([new/0, heard/3, ending/4, accepted/4, refused/4, link_ended/3, withdrawn/2]).

-export_type([t/0, key/0, interruption/0, gap/0]).

-type key() :: {<<_:256>>, binary()}.
%% `from_ms' is when the subscription was last known covered, `reason' the
%% name of why its last covering link ended, both kept until a station accepts
%% it again.
-type interruption() :: #{from_ms := integer(), reason := binary()}.
-type gap() :: #{from_ms := integer(), to_ms := integer(), reason := binary()}.

-record(coverage, {
    %% key → the links covering it now, and the latest time a link that
    %% stopped covering it since it was last accepted was heard from.
    covering = #{} :: #{key() => {sets:set(pid()), integer() | undefined}},
    %% key → its interruption, while no link covers it.
    interrupted = #{} :: #{key() => interruption()},
    %% link → the last time it was heard from, in milliseconds of wall-clock time.
    heard = #{} :: #{pid() => integer()},
    %% link → why it is ending, as it said before it stopped.
    ending = #{} :: #{pid() => binary()}
}).
-opaque t() :: #coverage{}.

%% @doc No subscription covered, none interrupted.
-spec new() -> t().
new() ->
    #coverage{}.

%% @doc `Link' was heard from at `AtMs'. A time earlier than one already
%% recorded for it changes nothing.
-spec heard(pid(), integer(), t()) -> t().
heard(Link, AtMs, #coverage{heard = Heard} = C) when is_pid(Link), is_integer(AtMs) ->
    C#coverage{heard = Heard#{Link => max(AtMs, maps:get(Link, Heard, AtMs))}}.

%% @doc `Link' is ending, named `Reason', and last received a frame from its
%% station at `LastHeardMs' (`undefined' when it never did).
-spec ending(pid(), binary(), integer() | undefined, t()) -> t().
ending(Link, Reason, LastHeardMs, #coverage{ending = Ending} = C) when is_pid(Link), is_binary(Reason) ->
    heard_at(LastHeardMs, Link, C#coverage{ending = Ending#{Link => Reason}}).

heard_at(undefined, _Link, C) -> C;
heard_at(AtMs, Link, C) -> heard(Link, AtMs, C).

%% @doc `Link''s station accepted the subscription on `Key' at `NowMs'. The
%% link covers it from now on. Returns the gap to tell the subscription's
%% subscribers when it was interrupted, else `none'.
-spec accepted(key(), pid(), integer(), t()) -> {gap() | none, t()}.
accepted(Key, Link, NowMs, #coverage{covering = Covering, interrupted = Interrupted} = C) ->
    {Links, _LeftHeard} = maps:get(Key, Covering, {sets:new(), undefined}),
    Covered = C#coverage{covering = Covering#{Key => {sets:add_element(Link, Links), undefined}},
                         interrupted = maps:remove(Key, Interrupted)},
    {gap_of(maps:find(Key, Interrupted), NowMs), heard(Link, NowMs, Covered)}.

gap_of({ok, Interruption}, NowMs) -> Interruption#{to_ms => NowMs};
gap_of(error, _NowMs) -> none.

%% @doc `Link''s station refused the subscription on `Key' (named `Reason'):
%% it no longer covers it. Returns the interruption to tell its subscribers
%% when no link covers it any more, else `none'.
-spec refused(key(), pid(), binary(), t()) -> {interruption() | none, t()}.
refused(Key, Link, Reason, C) ->
    left(maps:find(Key, C#coverage.covering), Key, Link, Reason, C).

%% @doc `Link' ended. Returns every subscription it was the last link to
%% cover, each with its interruption, and forgets the link. The interruption is
%% named by the reason the link gave as it was ending, else by `ExitReason'.
-spec link_ended(pid(), binary(), t()) -> {[{key(), interruption()}], t()}.
link_ended(Link, ExitReason, #coverage{covering = Covering, ending = Ending} = C) ->
    Reason = maps:get(Link, Ending, ExitReason),
    {Interruptions, Left} =
        lists:foldl(fun({Key, {Links, _}}, {Acc, Cov}) ->
                            covered_by(sets:is_element(Link, Links), Key, Link, Reason, Acc, Cov)
                    end, {[], C}, maps:to_list(Covering)),
    {lists:reverse(Interruptions), Left#coverage{heard = maps:remove(Link, Left#coverage.heard),
                                                 ending = maps:remove(Link, Ending)}}.

covered_by(false, _Key, _Link, _Reason, Acc, C) ->
    {Acc, C};
covered_by(true, Key, Link, Reason, Acc, C) ->
    interrupted_into(left(maps:find(Key, C#coverage.covering), Key, Link, Reason, C), Key, Acc).

interrupted_into({none, C}, _Key, Acc) -> {Acc, C};
interrupted_into({Interruption, C}, Key, Acc) -> {[{Key, Interruption} | Acc], C}.

%% @doc The last local subscriber of `Key' left: nothing about it is kept.
-spec withdrawn(key(), t()) -> t().
withdrawn(Key, #coverage{covering = Covering, interrupted = Interrupted} = C) ->
    C#coverage{covering = maps:remove(Key, Covering), interrupted = maps:remove(Key, Interrupted)}.

%% `Link' stops covering `Key'. The latest time a link that covered it was heard
%% from is kept, so the interruption starts there once the last one has gone.
left(error, _Key, _Link, _Reason, C) ->
    {none, C};
left({ok, {Links, LeftHeard}}, Key, Link, Reason, C) ->
    carried(sets:is_element(Link, Links), Key, Link, Links, LeftHeard, Reason, C).

%% A link that never carried `Key' takes nothing from it.
carried(false, _Key, _Link, _Links, _LeftHeard, _Reason, C) ->
    {none, C};
carried(true, Key, Link, Links, LeftHeard, Reason, #coverage{covering = Covering, heard = Heard} = C) ->
    Rest = sets:del_element(Link, Links),
    Latest = latest(LeftHeard, maps:get(Link, Heard)),
    emptied(sets:is_empty(Rest), Key, Latest, Reason, C#coverage{covering = Covering#{Key => {Rest, Latest}}}).

emptied(false, _Key, _Latest, _Reason, C) ->
    {none, C};
emptied(true, Key, Latest, Reason, #coverage{interrupted = Interrupted} = C) ->
    Interruption = #{from_ms => Latest, reason => Reason},
    {Interruption, C#coverage{interrupted = Interrupted#{Key => Interruption}}}.

latest(undefined, B) -> B;
latest(A, B) -> max(A, B).
