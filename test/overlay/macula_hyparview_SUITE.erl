%% @doc Acceptance tests for realm-join admission (Part 6 §9.6 / Part
%% 3 §7.1). Exercises the full JOIN → verify-endorsement → admit
%% path end-to-end across two in-VM stations, deterministically and
%% without a real network.
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
    AdminKp = macula_identity:generate(),
    Realm   = macula_identity:public(AdminKp),
    Net = hyparview_fleet_helper:start_fleet([seed, joiner], [Realm], #{}),
    try
        End = hyparview_fleet_helper:endorse(Net, AdminKp, Realm, joiner),
        hyparview_fleet_helper:join(Net, joiner, seed, Realm, End),
        Active = hyparview_fleet_helper:active_view(Net, seed, Realm),
        JoinerPub = pubkey_of(Net, joiner),
        true = lists:member(JoinerPub, Active)
    after
        hyparview_fleet_helper:stop_fleet(Net)
    end.

realm_join_rejects_bogus_endorsement(_Cfg) ->
    RealAdmin = macula_identity:generate(),
    Realm     = macula_identity:public(RealAdmin),
    Impostor  = macula_identity:generate(),
    Net = hyparview_fleet_helper:start_fleet([seed, joiner], [Realm], #{}),
    try
        %% Build endorsement but sign with the impostor — verify must fail.
        RealEnd = hyparview_fleet_helper:endorse(Net, RealAdmin, Realm, joiner),
        JoinerPub = pubkey_of(Net, joiner),
        Bogus0 = macula_record:realm_member_endorsement(
                   Realm,
                   #{realm => Realm, member_node => JoinerPub,
                     roles => [<<"peer">>]}),
        Bogus = macula_record:sign(Bogus0, Impostor),
        hyparview_fleet_helper:join(Net, joiner, seed, Realm, Bogus),
        %% Seed must NOT admit the bogus joiner.
        Active = hyparview_fleet_helper:active_view(Net, seed, Realm),
        false = lists:member(JoinerPub, Active),
        %% Sanity: a subsequent valid endorsement still admits.
        hyparview_fleet_helper:join(Net, joiner, seed, Realm, RealEnd),
        Active2 = hyparview_fleet_helper:active_view(Net, seed, Realm),
        true  = lists:member(JoinerPub, Active2)
    after
        hyparview_fleet_helper:stop_fleet(Net)
    end.

%%=====================================================================
%% Helpers
%%=====================================================================

pubkey_of(Net, Name) -> hyparview_fleet_helper:pubkey_of(Net, Name).
