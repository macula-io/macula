%%% @doc A caller's stream holds no longer than its provider's authorization (macula#120).
%%%
%%% A stream opened against a provider's verified advertisement of an org procedure was opened under that provider's
%%% authorization: the realm-signed org directory and the org-signed procedure delegation, which end at
%%% `macula_record:authorization_ends_at/3'. A provider the org stops delegating to, or the realm stops listing, is
%%% not renewed, so its authorization ends and the stream with it. A provider in good standing renews its chain
%%% before then (D32), so the follower looks for that renewal at a third of the time left: the ends of the provider's
%%% renewed advertisements, verified and trusted as at the open, and moves the stream's end to the latest of them. Only
%%% a later end moves it. A lookup that finds nothing, only earlier ends, or fails, leaves the end where it is: a
%%% withheld renewal can only let the stream reach its end, never keep it open past it. At the end the stream is aborted
%%% `unauthorized'. The follower watches the stream and ends with it.
%%%
%%% Given a standing reader, the follower also re-reads the provider's delegation slot every re-check (60 s by default,
%%% the `standing_recheck_ms' application env): the org's tombstone of the delegation, `{error, withdrawn}', aborts the
%%% stream at once. Any other answer, a failed lookup or a reader that raises included, is no evidence of revocation and
%%% leaves the stream to its end.
-module(macula_provider_authority_follower).

-export([follow/3, follow/4]).

%% Close to the end, the follower looks again no more often than this.
-define(MIN_LOOK_MS, 1_000).
%% A lookup waits at most this long, and never past the end.
-define(LOOKUP_MS, 10_000).
%% How often the provider's standing is read, unless the options or the application env say otherwise.
-define(STANDING_RECHECK_MS, 60_000).

-type standing() :: fun(() -> ok | {error, term()}).
-type opts() :: #{standing => standing(), recheck_ms => pos_integer()}.
-export_type([opts/0]).

%% @doc follow/4 without a standing re-check.
-spec follow(pid(), integer(), fun((integer()) -> [integer()])) -> pid().
follow(Stream, EndsAt, Renewed) ->
    follow(Stream, EndsAt, Renewed, #{}).

%% @doc Follow `Stream' until `EndsAt' (milliseconds), moving the end to the latest of the ends `Renewed(Deadline)'
%% answers, by `Deadline' (milliseconds), when it is later. With a `standing' reader, read it every `recheck_ms' and
%% abort the stream when it answers `{error, withdrawn}'. Returns the follower's pid.
-spec follow(pid(), integer(), fun((integer()) -> [integer()]), opts()) -> pid().
follow(Stream, EndsAt, Renewed, Opts)
  when is_pid(Stream), is_integer(EndsAt), is_function(Renewed, 1), is_map(Opts) ->
    Standing = maps:get(standing, Opts, none),
    RecheckMs = maps:get(recheck_ms, Opts, recheck_default()),
    spawn(fun() ->
              Now = now_ms(),
              followed(#{mon => erlang:monitor(process, Stream), stream => Stream, ends_at => EndsAt,
                         renewed => Renewed, look_at => Now + look_in(EndsAt - Now), standing => Standing,
                         recheck_ms => RecheckMs, check_at => check_at(Standing, Now, RecheckMs)})
          end).

recheck_default() ->
    application:get_env(macula, standing_recheck_ms, ?STANDING_RECHECK_MS).

check_at(none, _Now, _RecheckMs) -> infinity;
check_at(_Standing, Now, RecheckMs) -> Now + RecheckMs.

%% The follower sleeps until its next event, the earliest of the end, the next look and the next standing check.
followed(#{mon := Mon, stream := Stream, ends_at := EndsAt, look_at := LookAt, check_at := CheckAt} = F) ->
    receive
        {'DOWN', Mon, process, Stream, _Reason} -> ok
    after max(0, min(min(EndsAt, LookAt), CheckAt) - now_ms()) ->
        due(now_ms(), F)
    end.

look_in(LeftMs) when LeftMs =< ?MIN_LOOK_MS -> max(0, LeftMs);
look_in(LeftMs) -> max(?MIN_LOOK_MS, LeftMs div 3).

due(Now, #{stream := Stream, ends_at := EndsAt}) when Now >= EndsAt ->
    aborted(Stream, <<"the provider's authorization ended">>);
due(Now, #{check_at := CheckAt, standing := Standing} = F) when Now >= CheckAt ->
    checked(read_standing(Standing), Now, F);
due(Now, #{look_at := LookAt} = F) when Now >= LookAt ->
    looked(Now, F);
due(_Now, F) ->
    followed(F).

checked({error, withdrawn}, _Now, #{stream := Stream}) ->
    aborted(Stream, <<"the org withdrew the provider's delegation">>);
checked(_Standing, Now, #{recheck_ms := RecheckMs} = F) ->
    due(Now, F#{check_at := Now + RecheckMs}).

looked(Now, #{ends_at := EndsAt, renewed := Renewed} = F) ->
    Renewed1 = lists:max([EndsAt | renewed_ends(Renewed, Now + min(?LOOKUP_MS, EndsAt - Now))]),
    After = now_ms(),
    followed(F#{ends_at := Renewed1, look_at := After + look_in(Renewed1 - After)}).

aborted(Stream, Message) ->
    _ = macula_stream:abort(Stream, <<"unauthorized">>, Message),
    ok.

read_standing(Standing) ->
    try Standing()
    catch _:_ -> {error, standing_raised}
    end.

renewed_ends(Renewed, Deadline) ->
    try [End || End <- Renewed(Deadline), is_integer(End)]
    catch _:_ -> []
    end.

now_ms() ->
    erlang:system_time(millisecond).
