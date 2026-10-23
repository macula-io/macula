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
-module(macula_provider_authority_follower).

-export([follow/3]).

%% Close to the end, the follower looks again no more often than this.
-define(MIN_LOOK_MS, 1_000).
%% A lookup waits at most this long, and never past the end.
-define(LOOKUP_MS, 10_000).

%% @doc Follow `Stream' until `EndsAt' (milliseconds), moving the end to the latest of the ends `Renewed(Deadline)'
%% answers, by `Deadline' (milliseconds), when it is later. Returns the follower's pid.
-spec follow(pid(), integer(), fun((integer()) -> [integer()])) -> pid().
follow(Stream, EndsAt, Renewed) when is_pid(Stream), is_integer(EndsAt), is_function(Renewed, 1) ->
    spawn(fun() -> followed(erlang:monitor(process, Stream), Stream, EndsAt, Renewed) end).

followed(Mon, Stream, EndsAt, Renewed) ->
    receive
        {'DOWN', Mon, process, Stream, _Reason} -> ok
    after look_in(EndsAt - now_ms()) ->
        due(EndsAt - now_ms(), Mon, Stream, EndsAt, Renewed)
    end.

look_in(LeftMs) when LeftMs =< ?MIN_LOOK_MS -> max(0, LeftMs);
look_in(LeftMs) -> max(?MIN_LOOK_MS, LeftMs div 3).

due(LeftMs, _Mon, Stream, _EndsAt, _Renewed) when LeftMs =< 0 ->
    _ = macula_stream:abort(Stream, <<"unauthorized">>, <<"the provider's authorization ended">>),
    ok;
due(LeftMs, Mon, Stream, EndsAt, Renewed) ->
    followed(Mon, Stream, lists:max([EndsAt | renewed_ends(Renewed, now_ms() + min(?LOOKUP_MS, LeftMs))]), Renewed).

renewed_ends(Renewed, Deadline) ->
    try [End || End <- Renewed(Deadline), is_integer(End)]
    catch _:_ -> []
    end.

now_ms() ->
    erlang:system_time(millisecond).
