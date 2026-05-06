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
-export([subs_to/2, advs_to/2]).

%% @doc Re-issue a SUBSCRIBE frame for every distinct `{Realm, Topic}'
%% in `TopicIndex' against `LinkPid'. The calling process (pool's
%% gen_server pid via `self()') receives subsequent EVENT messages
%% from the link.
%%
%% Errors from individual link subscriptions are swallowed — the
%% pool's `{macula_peering, disconnected}' path will fire if the
%% link can't establish, triggering another respawn cycle.
-spec subs_to(pid(), #{{<<_:256>>, binary()} => sets:set(reference())}) ->
    ok.
subs_to(LinkPid, TopicIndex) when is_pid(LinkPid), is_map(TopicIndex) ->
    PoolPid = self(),
    Pairs = maps:keys(TopicIndex),
    [_ = macula_station_link:subscribe(LinkPid, R, T, PoolPid)
     || {R, T} <- Pairs],
    ok.

%% @doc Re-issue an ADVERTISE frame for every advertised procedure
%% in `Procs' against `LinkPid'. Mirrors `subs_to/2' for the RPC
%% surface — used by the pool to restore wire-level advertisement
%% bindings whenever a station link respawns.
%%
%% Errors from individual link advertise calls are swallowed: the
%% next link respawn cycle re-tries.
-spec advs_to(pid(), #{{<<_:256>>, binary()} =>
                        macula_station_link:handler()}) -> ok.
advs_to(LinkPid, Procs) when is_pid(LinkPid), is_map(Procs) ->
    maps:foreach(
      fun({Realm, Procedure}, Handler) ->
          _ = macula_station_link:advertise(LinkPid, Realm,
                                             Procedure, Handler)
      end, Procs),
    ok.
