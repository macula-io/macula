%%%-------------------------------------------------------------------
%%% @doc Phase 3 §8 acceptance #2 + #3 — daemon lifecycle.
%%%
%%% These two acceptance criteria test the *host-side* lifecycle of a
%%% hosted daemon's record in the DHT:
%%%
%%%   #2 Graceful removed on detachment — after explicit detach, the
%%%      host stops including that daemon in its hosted_records()
%%%      and stops publishing fresh copies on the refresh tick. The
%%%      old record then ages out of the DHT via TTL.
%%%   #3 Reattachment-to-different-station propagates via gossip —
%%%      Alice detaches from Helsinki, attaches to Nuremberg.
%%%      Nuremberg publishes a new hosted_address_map record signed
%%%      by Nuremberg. Both records share the same `storage_key`
%%%      (sha256("hosted_address_map" || daemon_addr)) by spec §4,
%%%      so the new PUT overwrites the old. A resolver fetching the
%%%      address now gets Nuremberg's signed redirect, not Helsinki's.
%%%
%%% Both are exercised here in isolation against a tiny in-memory DHT
%%% so the test runs fast and the assertions are crisp. The full
%%% integration story (sender's cache invalidation, transport reroute)
%%% is covered by the Phase 3.6 / Alice↔Bob e2e suites.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_net_phase3_lifecycle_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<16#11:256>>).
-define(DHT,   macula_net_phase3_lifecycle_dht).

%% =============================================================================
%% Top-level
%% =============================================================================

lifecycle_test_() ->
    {inorder, [
        {timeout, 10, fun acceptance_2_detach_drops_record/0},
        {timeout, 10, fun acceptance_3_reattach_overwrites_redirect/0}
    ]}.

%% =============================================================================
%% Mock DHT — same interface used elsewhere; keyed by storage_key.
%% =============================================================================

start_dht() ->
    catch unregister(?DHT),
    Tab = ets:new(?DHT, [set, public]),
    Pid = spawn(fun() -> dht_loop(Tab) end),
    register(?DHT, Pid),
    Pid.

stop_dht(Pid) ->
    catch (Pid ! stop),
    catch unregister(?DHT),
    ok.

dht_loop(Tab) ->
    receive
        {put, Record, From} ->
            ets:insert(Tab, {macula_record:storage_key(Record), Record}),
            From ! {ok, put},
            dht_loop(Tab);
        {find, Key, From} ->
            Reply = case ets:lookup(Tab, Key) of
                [{_, R}] -> {ok, R};
                []       -> {error, not_found}
            end,
            From ! {find_reply, Reply},
            dht_loop(Tab);
        stop ->
            catch ets:delete(Tab),
            ok
    end.

dht_put(Record) ->
    ?DHT ! {put, Record, self()},
    receive {ok, put} -> ok after 1000 -> {error, dht_timeout} end.

dht_find(Key) ->
    ?DHT ! {find, Key, self()},
    receive {find_reply, R} -> R after 1000 -> {error, dht_timeout} end.

%% =============================================================================
%% Helpers
%% =============================================================================

new_actor() ->
    Kp = macula_identity:generate(),
    Pk = macula_identity:public(Kp),
    Addr = macula_address:derive(?REALM, Pk),
    #{kp => Kp, pk => Pk, addr => Addr}.

start_host(Host) ->
    {ok, _Pid} = macula_host_identity:start_link(#{
        realm_pubkey => ?REALM,
        host_pubkey  => maps:get(pk, Host),
        host_privkey => maps:get(kp, Host),
        put_fn       => fun dht_put/1,
        refresh_ms   => 250
    }),
    ok.

stop_host() ->
    catch macula_host_identity:stop(),
    case ets:info(macula_host_identity_table) of
        undefined -> ok;
        _         -> ets:delete(macula_host_identity_table)
    end.

build_delegation(#{kp := DKp, pk := DPk}, #{pk := HPk}) ->
    Now = erlang:system_time(millisecond),
    macula_record:sign_host_delegation(
        macula_record:host_delegation(DPk, HPk, ?REALM, Now, Now + 60_000),
        DKp).

storage_key_for(DaemonAddr) ->
    %% Mirrors macula_record's hosted_address_map storage_key
    %% derivation. The resolver would compute the same key via
    %% redirect_key/1 — we duplicate it here to keep the test
    %% independent of resolver internals.
    crypto:hash(sha256,
        <<"hosted_address_map", DaemonAddr/binary>>).

%% =============================================================================
%% Acceptance #2 — graceful detach
%% =============================================================================

acceptance_2_detach_drops_record() ->
    DhtPid  = start_dht(),
    Alice   = new_actor(),
    Helsinki = new_actor(),
    ok = start_host(Helsinki),

    %% Alice attaches; host publishes the redirect record.
    Conn = make_ref(),
    ok = macula_host_identity:attach(
            maps:get(addr, Alice), maps:get(pk, Alice),
            build_delegation(Alice, Helsinki), Conn),

    Key = storage_key_for(maps:get(addr, Alice)),
    ?assertMatch({ok, _}, dht_find(Key)),
    ?assertEqual([maps:get(addr, Alice)],
                 macula_host_identity:hosted_addresses()),

    %% Detach: Alice is no longer in the hosted set, host stops
    %% advertising her on subsequent refreshes.
    ok = macula_host_identity:detach(maps:get(addr, Alice)),
    ?assertNot(macula_host_identity:hosted(maps:get(addr, Alice))),
    ?assertEqual([], macula_host_identity:hosted_addresses()),

    %% Trigger an explicit refresh and confirm hosted_records is empty
    %% — this is what the periodic refresh tick would do, only sooner.
    ok = macula_host_identity:refresh_now(),
    ?assertEqual([], macula_host_identity:hosted_records()),

    stop_host(),
    stop_dht(DhtPid),
    ok.

%% =============================================================================
%% Acceptance #3 — reattachment-to-different-station overwrites
%% =============================================================================

acceptance_3_reattach_overwrites_redirect() ->
    DhtPid    = start_dht(),
    Alice     = new_actor(),
    Helsinki  = new_actor(),
    Nuremberg = new_actor(),

    %% Phase 1: Alice attaches to Helsinki.
    ok = start_host(Helsinki),
    ok = macula_host_identity:attach(
            maps:get(addr, Alice), maps:get(pk, Alice),
            build_delegation(Alice, Helsinki), make_ref()),
    Key = storage_key_for(maps:get(addr, Alice)),
    {ok, R1} = dht_find(Key),
    ?assertEqual(maps:get(pk, Helsinki), macula_record:key(R1)),

    %% Alice detaches from Helsinki + Helsinki shuts down.
    ok = macula_host_identity:detach(maps:get(addr, Alice)),
    stop_host(),

    %% Phase 2: Alice attaches to Nuremberg. The new redirect carries
    %% Nuremberg's pubkey as the signing key, the same storage_key as
    %% before, so the DHT entry is overwritten in place.
    ok = start_host(Nuremberg),
    ok = macula_host_identity:attach(
            maps:get(addr, Alice), maps:get(pk, Alice),
            build_delegation(Alice, Nuremberg), make_ref()),

    {ok, R2} = dht_find(Key),
    ?assertEqual(maps:get(pk, Nuremberg), macula_record:key(R2)),
    ?assertNotEqual(macula_record:key(R1), macula_record:key(R2)),

    %% A resolver doing find(Key) now gets Nuremberg's signed record;
    %% Helsinki's earlier version is no longer reachable.
    {ok, R3} = dht_find(Key),
    ?assertEqual(maps:get(pk, Nuremberg), macula_record:key(R3)),

    stop_host(),
    stop_dht(DhtPid),
    ok.
