%% @doc Acceptance tests for Plumtree gossip + realm-scoped PubSub
%% (Part 3 §7.3 / Part 6 §6). Covers end-to-end delivery across a
%% chain topology and cross-realm isolation, deterministically and
%% without a real network.
-module(macula_plumtree_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([plumtree_delivers_to_all_subscribers/1,
         cross_realm_isolation/1]).

all() ->
    [plumtree_delivers_to_all_subscribers,
     cross_realm_isolation].

init_per_suite(Cfg) -> Cfg.
end_per_suite(_Cfg) -> ok.

%%---------------------------------------------------------------------
%% Plumtree + PubSub end-to-end delivery
%%---------------------------------------------------------------------

plumtree_delivers_to_all_subscribers(_Cfg) ->
    AdminKp = macula_identity:generate(),
    Realm   = macula_identity:public(AdminKp),
    Net = plumtree_fleet_helper:start_fleet([a, b, c, d, e], [Realm], #{}),
    try
        %% Chain topology: a — b — c — d — e. Every message must
        %% traverse intermediate hops.
        ok = plumtree_fleet_helper:connect(Net, a, b, Realm),
        ok = plumtree_fleet_helper:connect(Net, b, c, Realm),
        ok = plumtree_fleet_helper:connect(Net, c, d, Realm),
        ok = plumtree_fleet_helper:connect(Net, d, e, Realm),

        [ok = plumtree_fleet_helper:subscribe(Net, N, Realm, <<"chat">>)
         || N <- [a, b, c, d, e]],

        ok = plumtree_fleet_helper:publish(Net, a, Realm, <<"chat">>, <<"hello">>),
        timer:sleep(100),

        %% Each station must see exactly one delivery of the event.
        [begin
             Payloads = plumtree_fleet_helper:deliveries(Net, N, {Realm, <<"chat">>}),
             1 = length(Payloads),
             [<<"hello">>] = Payloads
         end || N <- [a, b, c, d, e]]
    after
        plumtree_fleet_helper:stop_fleet(Net)
    end.

%%---------------------------------------------------------------------
%% Cross-realm isolation
%%---------------------------------------------------------------------

cross_realm_isolation(_Cfg) ->
    AdminR1 = macula_identity:generate(), R1 = macula_identity:public(AdminR1),
    AdminR2 = macula_identity:generate(), R2 = macula_identity:public(AdminR2),
    %% Both realms share the same fleet identities; wiring is per-realm.
    Net = plumtree_fleet_helper:start_fleet([a, b, c], [R1, R2], #{}),
    try
        %% In R1: a—b connected. In R2: b—c connected.
        ok = plumtree_fleet_helper:connect(Net, a, b, R1),
        ok = plumtree_fleet_helper:connect(Net, b, c, R2),

        ok = plumtree_fleet_helper:subscribe(Net, a, R1, <<"feed">>),
        ok = plumtree_fleet_helper:subscribe(Net, b, R1, <<"feed">>),
        ok = plumtree_fleet_helper:subscribe(Net, b, R2, <<"feed">>),
        ok = plumtree_fleet_helper:subscribe(Net, c, R2, <<"feed">>),

        ok = plumtree_fleet_helper:publish(Net, a, R1, <<"feed">>, <<"r1-only">>),
        timer:sleep(100),

        %% Delivered on R1 subscribers in R1:
        1 = length(plumtree_fleet_helper:deliveries(Net, a, {R1, <<"feed">>})),
        1 = length(plumtree_fleet_helper:deliveries(Net, b, {R1, <<"feed">>})),
        %% NOT delivered on R2 subscribers — not even b (same station,
        %% different realm) — and definitely not c (other realm).
        0 = length(plumtree_fleet_helper:deliveries(Net, b, {R2, <<"feed">>})),
        0 = length(plumtree_fleet_helper:deliveries(Net, c, {R2, <<"feed">>}))
    after
        plumtree_fleet_helper:stop_fleet(Net)
    end.
