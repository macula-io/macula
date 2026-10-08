%% @private
%% @doc Subscription replay helper for `macula_client'.
%%
%% When a station link dies and the pool respawns it, the new link
%% has no wire-level subscriptions — the station saw the previous
%% peering connection drop and dropped its bookkeeping with it. The
%% pool re-issues a SUBSCRIBE frame for every distinct `(Realm,
%% Topic)' it is currently tracking against the new link.
%%
%% Each registration is handed to the link without waiting
%% (macula#45). Kept apart so the pool's gen_server stays focused on its
%% state machine and the replay path is testable on its own.
-module(macula_client_replay).
-export([subs_to/2, advs_to/3, stream_advs_to/3]).

%% @doc Hand `LinkPid' a subscribe for every distinct `{Realm, Topic}' in
%% `TopicIndex', for the calling process (the pool), which then receives
%% the link's EVENT messages. Waits on nothing (macula#45): a busy link
%% takes them when it resumes. The pool unsubscribes a link by topic, so
%% it keeps nothing back.
-spec subs_to(pid(), #{{<<_:256>>, binary()} => sets:set(reference())}) -> ok.
subs_to(LinkPid, TopicIndex) when is_pid(LinkPid), is_map(TopicIndex) ->
    PoolPid = self(),
    lists:foreach(fun({R, T}) -> ok = macula_station_link:subscribe_async(LinkPid, R, T, PoolPid) end,
                  maps:keys(TopicIndex)).

%% @doc Register on `LinkPid' every advertised procedure in `Procs' whose
%% stations include `Station', the station that link pins (`all' names every
%% station). Mirrors `subs_to/2' for the RPC surface: the pool restores a
%% respawned station link's registrations with it.
%%
%% Handed without waiting (macula#45); a link that is gone is the next
%% respawn's to replay.
-spec advs_to(pid(), <<_:256>> | undefined, #{{<<_:256>>, binary()} => map()}) -> ok.
advs_to(LinkPid, Station, Procs) when is_pid(LinkPid), is_map(Procs) ->
    maps:foreach(
      fun({Realm, Procedure}, #{handler := Handler, policy := Policy, ad := EncodedAd}) ->
          ok = macula_station_link:advertise_async(LinkPid, Realm, Procedure, Handler, Policy, EncodedAd)
      end, for_station(Station, Procs)),
    ok.

%% @doc Register on `LinkPid' every streaming procedure in `StreamProcs'
%% whose stations include `Station', as `advs_to/3' does for procedures. The
%% registration keeps its mode, so the link dispatches an inbound STREAM_OPEN
%% correctly, and its auth policy.
%%
%% Handed without waiting, as `advs_to/3' does.
-spec stream_advs_to(pid(), <<_:256>> | undefined, #{{<<_:256>>, binary()} => map()}) -> ok.
stream_advs_to(LinkPid, Station, StreamProcs) when is_pid(LinkPid), is_map(StreamProcs) ->
    maps:foreach(
      fun({Realm, Procedure}, #{mode := Mode, handler := Handler, policy := Policy, ad := EncodedAd}) ->
          ok = macula_station_link:advertise_stream_async(LinkPid, Realm, Procedure, Mode, Handler, Policy, EncodedAd)
      end, for_station(Station, StreamProcs)),
    ok.

for_station(Station, Registrations) ->
    maps:filter(fun(_Key, #{stations := all}) -> true;
                   (_Key, #{stations := Stations}) -> lists:member(Station, Stations)
                end, Registrations).
