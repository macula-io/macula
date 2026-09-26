%% @private
%% @doc Subscription replay helper for `macula_client'.
%%
%% When a station link dies and the pool respawns it, the new link
%% has no wire-level subscriptions — the station saw the previous
%% peering connection drop and dropped its bookkeeping with it. The
%% pool re-issues a SUBSCRIBE frame for every distinct `(Realm,
%% Topic)' it is currently tracking against the new link.
%%
%% This module is a thin wrapper around `macula_station_link:
%% subscribe/4' — extracted so the pool's gen_server stays focused
%% on its state machine and so the replay path is independently
%% testable.
-module(macula_client_replay).
-export([subs_to/2, advs_to/3, stream_advs_to/3, link_subscribe/4]).

%% @doc Re-issue a SUBSCRIBE frame for every distinct `{Realm, Topic}'
%% in `TopicIndex' against `LinkPid', and return the SubRef the link
%% gave each one it accepted, so the pool can unsubscribe that link
%% later. The calling process (pool's gen_server pid via `self()')
%% receives subsequent EVENT messages from the link.
%%
%% Errors from individual link subscriptions are swallowed — the
%% pool's `{macula_peering, disconnected}' path will fire if the
%% link can't establish, triggering another respawn cycle.
-spec subs_to(pid(), #{{<<_:256>>, binary()} => sets:set(reference())}) ->
    #{{<<_:256>>, binary()} => reference()}.
subs_to(LinkPid, TopicIndex) when is_pid(LinkPid), is_map(TopicIndex) ->
    PoolPid = self(),
    maps:from_list(
      [{{R, T}, LinkSubRef}
       || {R, T} <- maps:keys(TopicIndex),
          {ok, LinkSubRef} <- [link_subscribe(LinkPid, R, T, PoolPid)]]).

%% @doc Subscribe `LinkPid' to `{Realm, Topic}' for the pool `PoolPid', as
%% `macula_station_link:subscribe/4' does, except that a link that does not
%% answer within that call's 5 s, or is gone, is skipped rather than taking
%% the calling pool down with every subscription, advertisement and pending
%% call it holds (macula#44). A skipped link is logged at warning with its
%% reason; the next respawn of that link replays the subscription.
-spec link_subscribe(pid(), <<_:256>>, binary(), pid()) -> {ok, reference()} | {error, term()}.
link_subscribe(LinkPid, Realm, Topic, PoolPid) ->
    try macula_station_link:subscribe(LinkPid, Realm, Topic, PoolPid)
    catch exit:Reason -> skipped_subscribe(LinkPid, Realm, Topic, Reason)
    end.

skipped_subscribe(LinkPid, Realm, Topic, Reason) ->
    macula_diagnostics:event(warning, <<"_macula.client.link_subscribe_skipped">>,
                             #{link => LinkPid, realm => Realm, topic => Topic,
                               reason => macula_reason_name:text(Reason)}),
    {error, {link_subscribe_skipped, Reason}}.

%% @doc Register on `LinkPid' every advertised procedure in `Procs' whose
%% stations include `Station', the station that link pins (`all' names every
%% station). Mirrors `subs_to/2' for the RPC surface: the pool restores a
%% respawned station link's registrations with it.
%%
%% Errors from individual link advertise calls are swallowed: the
%% next link respawn cycle re-tries.
-spec advs_to(pid(), <<_:256>> | undefined, #{{<<_:256>>, binary()} => map()}) -> ok.
advs_to(LinkPid, Station, Procs) when is_pid(LinkPid), is_map(Procs) ->
    maps:foreach(
      fun({Realm, Procedure}, #{handler := Handler, policy := Policy, ad := EncodedAd}) ->
          _ = macula_station_link:advertise(LinkPid, Realm, Procedure, Handler, Policy, EncodedAd)
      end, for_station(Station, Procs)),
    ok.

%% @doc Register on `LinkPid' every streaming procedure in `StreamProcs'
%% whose stations include `Station', as `advs_to/3' does for procedures. The
%% registration keeps its mode, so the link dispatches an inbound STREAM_OPEN
%% correctly, and its auth policy.
%%
%% Errors are swallowed (same policy as `advs_to/3').
-spec stream_advs_to(pid(), <<_:256>> | undefined, #{{<<_:256>>, binary()} => map()}) -> ok.
stream_advs_to(LinkPid, Station, StreamProcs) when is_pid(LinkPid), is_map(StreamProcs) ->
    maps:foreach(
      fun({Realm, Procedure}, #{mode := Mode, handler := Handler, policy := Policy, ad := EncodedAd}) ->
          _ = macula_station_link:advertise_stream(LinkPid, Realm, Procedure, Mode, Handler, Policy, EncodedAd)
      end, for_station(Station, StreamProcs)),
    ok.

for_station(Station, Registrations) ->
    maps:filter(fun(_Key, #{stations := all}) -> true;
                   (_Key, #{stations := Stations}) -> lists:member(Station, Stations)
                end, Registrations).
