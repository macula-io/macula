%% @doc Acceptance tests for realm-join admission (Part 6 §9.6 / Part
%% 3 §7.1). Exercises the full JOIN → verify-endorsement → admit
%% path end-to-end across two in-VM stations, over the frame codec,
%% deterministically and without a real network.
-module(macula_hyparview_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([realm_join_admits_new_member/1,
         realm_join_rejects_bogus_endorsement/1]).

all() ->
    [realm_join_admits_new_member,
     realm_join_rejects_bogus_endorsement].

init_per_suite(Cfg) -> Cfg.
end_per_suite(_Cfg) -> ok.

%%---------------------------------------------------------------------
%% Realm-join admission
%%---------------------------------------------------------------------

realm_join_admits_new_member(_Cfg) ->
    #{realm := Realm, realm_key := RealmKey} = RealmSpec = realm(),
    Net = hyparview_fleet_helper:start_fleet([seed, joiner], [RealmSpec]),
    try
        Endorsement = hyparview_fleet_helper:endorse(Net, RealmKey, Realm, joiner),
        ok = hyparview_fleet_helper:join(Net, joiner, seed, Realm, Endorsement),
        Joiner = hyparview_fleet_helper:node_id_of(Net, joiner),
        true = lists:member(Joiner, hyparview_fleet_helper:active_view(Net, seed, Realm))
    after
        hyparview_fleet_helper:stop_fleet(Net)
    end.

%% An endorsement signed by any key but the realm's is refused; the realm's own still admits afterwards.
realm_join_rejects_bogus_endorsement(_Cfg) ->
    #{realm := Realm, realm_key := RealmKey} = RealmSpec = realm(),
    {ok, Impostor} = macula_node_keys:generate(realm, pq_pure),
    Net = hyparview_fleet_helper:start_fleet([seed, joiner], [RealmSpec]),
    try
        Joiner = hyparview_fleet_helper:node_id_of(Net, joiner),
        Bogus = hyparview_fleet_helper:endorse(Net, Impostor, Realm, joiner),
        ok = hyparview_fleet_helper:join(Net, joiner, seed, Realm, Bogus),
        false = lists:member(Joiner, hyparview_fleet_helper:active_view(Net, seed, Realm)),
        Real = hyparview_fleet_helper:endorse(Net, RealmKey, Realm, joiner),
        ok = hyparview_fleet_helper:join(Net, joiner, seed, Realm, Real),
        true = lists:member(Joiner, hyparview_fleet_helper:active_view(Net, seed, Realm))
    after
        hyparview_fleet_helper:stop_fleet(Net)
    end.

%%=====================================================================
%% Helpers
%%=====================================================================

%% A realm: its 32-byte id, and the key its endorsements are signed with.
realm() ->
    {ok, RealmKey} = macula_node_keys:generate(realm, pq_pure),
    #{realm => crypto:strong_rand_bytes(32), realm_key => RealmKey}.
