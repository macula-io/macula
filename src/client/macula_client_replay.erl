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
-export([subs_to/2, advs_to/2, stream_advs_to/2]).

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
          {ok, LinkSubRef} <- [macula_station_link:subscribe(LinkPid, R, T, PoolPid)]]).

%% @doc Register the handler of every advertised procedure in `Procs'
%% on `LinkPid'. Mirrors `subs_to/2' for the RPC surface — used by the
%% pool to restore a respawned station link's handlers.
%%
%% Errors from individual link advertise calls are swallowed: the
%% next link respawn cycle re-tries.
-spec advs_to(pid(), #{{<<_:256>>, binary()} =>
                        {macula_station_link:handler(),
                         macula_client:auth_policy()}}) -> ok.
advs_to(LinkPid, Procs) when is_pid(LinkPid), is_map(Procs) ->
    maps:foreach(
      fun({Realm, Procedure}, {Handler, Policy, EncodedAd}) ->
          _ = macula_station_link:advertise(LinkPid, Realm,
                                             Procedure, Handler, Policy,
                                             EncodedAd)
      end, Procs),
    ok.

%% @doc Register the handler of every streaming procedure in
%% `StreamProcs' on `LinkPid'. Mirrors `advs_to/2' for the
%% streaming RPC surface (SDK 3.17+). Stored shape is
%% `{Mode, Handler, Policy}' so the receiving link dispatches inbound
%% STREAM_OPEN frames with the correct mode and keeps enforcing the
%% procedure's auth policy.
%%
%% Errors are swallowed (same policy as `advs_to/2').
-spec stream_advs_to(pid(),
                     #{{<<_:256>>, binary()} =>
                       {macula_frame:stream_mode(),
                        macula_station_link:stream_handler(),
                        macula_client:auth_policy()}}) -> ok.
stream_advs_to(LinkPid, StreamProcs)
  when is_pid(LinkPid), is_map(StreamProcs) ->
    maps:foreach(
      fun({Realm, Procedure}, {Mode, Handler, Policy, EncodedAd}) ->
          _ = macula_station_link:advertise_stream(LinkPid, Realm,
                                                    Procedure, Mode,
                                                    Handler, Policy,
                                                    EncodedAd)
      end, StreamProcs),
    ok.
