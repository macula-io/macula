%%%-------------------------------------------------------------------
%%% @doc Tests for macula_host_identity (Phase 3 §6.1, milestone 3.3).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_host_identity_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<16#11:256>>).
-define(SINK, macula_host_identity_tests_sink).

new_actor() ->
    Kp = macula_identity:generate(),
    Pk = macula_identity:public(Kp),
    Addr = macula_address:derive(?REALM, Pk),
    #{kp => Kp, pk => Pk, addr => Addr}.

now_ms() -> erlang:system_time(millisecond).

valid_delegation(#{kp := DKp, pk := DPk}, #{pk := HPk}) ->
    macula_record:sign_host_delegation(
      macula_record:host_delegation(DPk, HPk, ?REALM,
                                     now_ms(), now_ms() + 60_000),
      DKp).

setup() ->
    Host = new_actor(),
    #{host => Host}.

cleanup(_) ->
    catch macula_host_identity:stop(),
    catch unregister(?SINK),
    flush(),
    case ets:info(macula_host_identity_table) of
        undefined -> ok;
        _ -> ets:delete(macula_host_identity_table)
    end.

flush() ->
    receive _ -> flush() after 0 -> ok end.

register_sink_fn() ->
    catch unregister(?SINK),
    true = register(?SINK, self()),
    fun(R) -> ?SINK ! {put, R}, ok end.

start_host(#{host := #{kp := Kp, pk := Pk}}) ->
    PutFn = register_sink_fn(),
    {ok, _} = macula_host_identity:start_link(#{
        realm_pubkey  => ?REALM,
        host_pubkey   => Pk,
        host_privkey  => Kp,
        put_fn        => PutFn,
        refresh_ms    => 60_000
    }).

drain(N) -> drain(N, []).
drain(0, Acc) -> lists:reverse(Acc);
drain(N, Acc) ->
    receive {put, R} -> drain(N-1, [R | Acc])
    after 1000 -> lists:reverse(Acc)
    end.

%% =============================================================================
%% Tests
%% =============================================================================

host_identity_test_() ->
    {inorder, [
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun attach_succeeds_and_publishes/1}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun attach_rejects_mismatched_address/1}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun attach_rejects_wrong_host_in_delegation/1}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun attach_rejects_wrong_realm_in_delegation/1}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun attach_rejects_unsigned_delegation/1}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun lookup_and_hosted_after_attach/1}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun detach_removes_entry/1}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun hosted_records_returns_signed_set/1}}
    ]}.

attach_succeeds_and_publishes(Ctx) ->
    fun() ->
        start_host(Ctx),
        Daemon = new_actor(),
        Conn = make_ref(),
        ok = macula_host_identity:attach(
               maps:get(addr, Daemon),
               maps:get(pk, Daemon),
               valid_delegation(Daemon, maps:get(host, Ctx)),
               Conn),
        [Record] = drain(1),
        ?assertEqual(16#14, macula_record:type(Record)),
        ?assertMatch({ok, _}, macula_record:verify(Record))
    end.

attach_rejects_mismatched_address(Ctx) ->
    fun() ->
        start_host(Ctx),
        Daemon = new_actor(),
        WrongAddr = <<16#fd, 0:120>>,
        ?assertEqual({error, address_does_not_match_daemon_pubkey},
                     macula_host_identity:attach(
                       WrongAddr, maps:get(pk, Daemon),
                       valid_delegation(Daemon, maps:get(host, Ctx)),
                       make_ref()))
    end.

attach_rejects_wrong_host_in_delegation(Ctx) ->
    fun() ->
        start_host(Ctx),
        Daemon = new_actor(),
        OtherHost = new_actor(),
        Delegation = valid_delegation(Daemon, OtherHost),
        ?assertEqual({error, delegation_host_mismatch},
                     macula_host_identity:attach(
                       maps:get(addr, Daemon),
                       maps:get(pk, Daemon),
                       Delegation, make_ref()))
    end.

attach_rejects_wrong_realm_in_delegation(Ctx) ->
    fun() ->
        start_host(Ctx),
        Daemon = new_actor(),
        BadRealm = <<16#22:256>>,
        BadDelegation = macula_record:sign_host_delegation(
                          macula_record:host_delegation(
                            maps:get(pk, Daemon),
                            maps:get(pk, maps:get(host, Ctx)),
                            BadRealm,
                            now_ms(), now_ms() + 60_000),
                          maps:get(kp, Daemon)),
        ?assertEqual({error, delegation_realm_mismatch},
                     macula_host_identity:attach(
                       maps:get(addr, Daemon),
                       maps:get(pk, Daemon),
                       BadDelegation, make_ref()))
    end.

attach_rejects_unsigned_delegation(Ctx) ->
    fun() ->
        start_host(Ctx),
        Daemon = new_actor(),
        Unsigned = macula_record:host_delegation(
                     maps:get(pk, Daemon),
                     maps:get(pk, maps:get(host, Ctx)),
                     ?REALM, now_ms(), now_ms() + 60_000),
        ?assertMatch({error, _},
                     macula_host_identity:attach(
                       maps:get(addr, Daemon),
                       maps:get(pk, Daemon),
                       Unsigned, make_ref()))
    end.

lookup_and_hosted_after_attach(Ctx) ->
    fun() ->
        start_host(Ctx),
        Daemon = new_actor(),
        Conn = make_ref(),
        ok = macula_host_identity:attach(
               maps:get(addr, Daemon), maps:get(pk, Daemon),
               valid_delegation(Daemon, maps:get(host, Ctx)),
               Conn),
        ?assert(macula_host_identity:hosted(maps:get(addr, Daemon))),
        ?assertEqual({ok, Conn}, macula_host_identity:lookup(maps:get(addr, Daemon))),
        ?assertEqual([maps:get(addr, Daemon)],
                     macula_host_identity:hosted_addresses())
    end.

detach_removes_entry(Ctx) ->
    fun() ->
        start_host(Ctx),
        Daemon = new_actor(),
        ok = macula_host_identity:attach(
               maps:get(addr, Daemon), maps:get(pk, Daemon),
               valid_delegation(Daemon, maps:get(host, Ctx)),
               make_ref()),
        ?assert(macula_host_identity:hosted(maps:get(addr, Daemon))),
        ok = macula_host_identity:detach(maps:get(addr, Daemon)),
        ?assertNot(macula_host_identity:hosted(maps:get(addr, Daemon))),
        ?assertEqual(not_found,
                     macula_host_identity:lookup(maps:get(addr, Daemon)))
    end.

hosted_records_returns_signed_set(Ctx) ->
    fun() ->
        start_host(Ctx),
        Alice = new_actor(),
        Bob   = new_actor(),
        ok = macula_host_identity:attach(
               maps:get(addr, Alice), maps:get(pk, Alice),
               valid_delegation(Alice, maps:get(host, Ctx)), make_ref()),
        ok = macula_host_identity:attach(
               maps:get(addr, Bob), maps:get(pk, Bob),
               valid_delegation(Bob, maps:get(host, Ctx)), make_ref()),
        _ = drain(2),
        Records = macula_host_identity:hosted_records(),
        ?assertEqual(2, length(Records)),
        lists:foreach(fun(R) ->
            ?assertEqual(16#14, macula_record:type(R)),
            ?assertMatch({ok, _}, macula_record:verify(R))
        end, Records)
    end.
