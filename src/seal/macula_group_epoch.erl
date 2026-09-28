%% @doc A sealed group's epochs (plans/DESIGN_E2E_SEALED_PUBSUB.md §4).
%%
%% An epoch is a group key for a stretch of time: a 32-byte key drawn at random,
%% independent of every other epoch's, so holding one says nothing about the
%% next; an 8-byte id drawn at random, which names the epoch on every sealed
%% event (`seal_key_id') and is bound to the key only by the distributor's
%% signed reply; and its times. Epochs are contiguous: the next one is issued
%% where this one stops publishing. A publisher seals under an epoch until its
%% `publish_until'; a subscriber accepts events under it until its
%% `accept_until', which is `publish_until' plus the longest a publication
%% lives (60 minutes of ttl plus 5 of tolerance, as `macula_frame' verifies a
%% publication), and with the same 5-minute clock tolerance. After that the key
%% has no use and is erased.
%%
%% The last third of an epoch is its ahead window: a pull in it is also handed
%% the next epoch, and every holder re-pulls at a random instant in it, so a
%% rotation is spread over that third rather than asked for in one instant.
%%
%% Everything here is a pure function of the epochs and the time it is given.
-module(macula_group_epoch).

-export([new/2, next/2, ahead_window/2, in_ahead_window/3, repull_at/3, for_publish/2, acceptable/2, live/2,
         default_rotate_after_ms/0]).
-export_type([epoch/0]).

-type epoch() :: #{id := <<_:64>>, key := <<_:256>>, issued_at := integer(), publish_until := integer(),
                   accept_until := integer()}.

-define(DEFAULT_ROTATE_AFTER_MS, 15 * 60000).
%% The longest a publication lives, as macula_frame verifies one: at most 60
%% minutes of ttl, plus 5 minutes of tolerance.
-define(EVENT_LIFE_MS, 65 * 60000).
-define(TOLERANCE_MS, 5 * 60000).

%% @doc A fresh epoch issued at `IssuedAt' (milliseconds), publishing for
%% `RotateAfterMs'.
-spec new(integer(), pos_integer()) -> epoch().
new(IssuedAt, RotateAfterMs) when is_integer(IssuedAt), is_integer(RotateAfterMs), RotateAfterMs > 0 ->
    PublishUntil = IssuedAt + RotateAfterMs,
    #{id => crypto:strong_rand_bytes(8), key => crypto:strong_rand_bytes(32), issued_at => IssuedAt,
      publish_until => PublishUntil, accept_until => PublishUntil + ?EVENT_LIFE_MS}.

%% @doc The epoch after `Epoch': issued where it stops publishing, with a fresh
%% key and id.
-spec next(epoch(), pos_integer()) -> epoch().
next(#{publish_until := PublishUntil}, RotateAfterMs) ->
    new(PublishUntil, RotateAfterMs).

%% @doc `Epoch''s ahead window, `{From, To}': the last third of its publishing
%% stretch, `To' excluded.
-spec ahead_window(epoch(), pos_integer()) -> {integer(), integer()}.
ahead_window(#{publish_until := PublishUntil}, RotateAfterMs) ->
    {PublishUntil - RotateAfterMs div 3, PublishUntil}.

%% @doc Whether `Now' falls in `Epoch''s ahead window.
-spec in_ahead_window(epoch(), pos_integer(), integer()) -> boolean().
in_ahead_window(Epoch, RotateAfterMs, Now) ->
    {From, To} = ahead_window(Epoch, RotateAfterMs),
    Now >= From andalso Now < To.

%% @doc The instant a holder re-pulls: `Fraction' (0.0 inclusive to 1.0
%% exclusive, drawn uniformly by the caller) of the way through `Epoch''s ahead
%% window.
-spec repull_at(epoch(), pos_integer(), float()) -> integer().
repull_at(Epoch, RotateAfterMs, Fraction) when is_float(Fraction), Fraction >= 0.0, Fraction < 1.0 ->
    {From, To} = ahead_window(Epoch, RotateAfterMs),
    From + trunc((To - From) * Fraction).

%% @doc The epoch a publisher seals under at `Now': the newest held epoch that
%% has been issued and has not stopped publishing, or `{error,
%% no_current_epoch}' when none has (so the publisher fails closed until a pull
%% succeeds).
-spec for_publish([epoch()], integer()) -> {ok, epoch()} | {error, no_current_epoch}.
for_publish(Held, Now) ->
    newest([E || #{issued_at := I, publish_until := P} = E <- Held, I =< Now, Now < P]).

newest([]) -> {error, no_current_epoch};
newest(Current) -> {ok, lists:last(lists:sort(fun by_issue/2, Current))}.

by_issue(#{issued_at := A}, #{issued_at := B}) -> A =< B.

%% @doc Whether an event under `Epoch' is accepted at `Now': until its
%% `accept_until', with the 5-minute clock tolerance a publication gets.
-spec acceptable(epoch(), integer()) -> boolean().
acceptable(#{accept_until := AcceptUntil}, Now) ->
    Now =< AcceptUntil + ?TOLERANCE_MS.

%% @doc The held epochs still of use at `Now'; the others are erased.
-spec live([epoch()], integer()) -> [epoch()].
live(Held, Now) ->
    [E || E <- Held, acceptable(E, Now)].

%% @doc How long an epoch publishes by default: 15 minutes.
-spec default_rotate_after_ms() -> pos_integer().
default_rotate_after_ms() -> ?DEFAULT_ROTATE_AFTER_MS.
