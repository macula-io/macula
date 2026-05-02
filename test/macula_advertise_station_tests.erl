%%%-------------------------------------------------------------------
%%% @doc Tests for macula_advertise_station (Phase 2 §4.1).
%%%
%%% Verifies the gen_server publishes a signed station_endpoint plus
%%% one address_pubkey_map per configured address, that the records
%%% verify, and that the timer-driven refresh actually re-publishes.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_advertise_station_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<16#11:256>>).

%% =============================================================================
%% Fixtures
%% =============================================================================

%% Setup runs in a different process from the test body, so we
%% register a known name and the put_fn delivers there. Avoids
%% capturing the (wrong) setup process pid in a closure.
-define(SINK, macula_advertise_station_tests_sink).

setup() ->
    Kp   = macula_identity:generate(),
    Pk   = macula_identity:public(Kp),
    Addr = macula_address:derive(?REALM, Pk),
    #{kp => Kp, pk => Pk, addr => Addr}.

cleanup(_) ->
    catch macula_advertise_station:stop(),
    catch unregister(?SINK),
    flush_mailbox().

flush_mailbox() ->
    receive _ -> flush_mailbox() after 0 -> ok end.

%% Called inside each test body — registers the body's pid as the
%% sink and returns a put_fn that delivers there.
register_sink_fn() ->
    catch unregister(?SINK),
    true = register(?SINK, self()),
    fun(R) -> ?SINK ! {put, R}, ok end.

base_config(#{kp := Kp, pk := Pk, addr := Addr}, PutFn) ->
    #{
        realm_pubkey     => ?REALM,
        identity_pubkey  => Pk,
        identity_privkey => Kp,
        quic_port        => 4400,
        addresses        => [Addr],
        put_fn           => PutFn,
        refresh_ms       => 200    %% short for testing
    }.

drain(N) ->
    drain(N, []).
drain(0, Acc) -> lists:reverse(Acc);
drain(N, Acc) ->
    receive {put, R} -> drain(N-1, [R | Acc])
    after 1000 -> lists:reverse(Acc)
    end.

%% =============================================================================
%% Tests — wrapped in `inorder' so the singleton gen_server doesn't
%% race across cases.
%% =============================================================================

advertise_station_test_() ->
    {inorder, [
        {timeout, 10,
         {setup, fun setup/0, fun cleanup/1,
          fun publishes_two_records_per_addr/1}},
        {timeout, 10,
         {setup, fun setup/0, fun cleanup/1,
          fun records_verify/1}},
        {timeout, 10,
         {setup, fun setup/0, fun cleanup/1,
          fun redirect_address_matches/1}},
        {timeout, 10,
         {setup, fun setup/0, fun cleanup/1,
          fun multiple_addresses/1}},
        {timeout, 10,
         {setup, fun setup/0, fun cleanup/1,
          fun refresh_republishes/1}},
        {timeout, 10,
         {setup, fun setup/0, fun cleanup/1,
          fun put_fn_failure_does_not_crash/1}},
        {timeout, 10,
         {setup, fun setup/0, fun cleanup/1,
          fun current_records_accessor/1}}
    ]}.

publishes_two_records_per_addr(Ctx) ->
    fun() ->
        PutFn = register_sink_fn(),
        {ok, _} = macula_advertise_station:start_link(base_config(Ctx, PutFn)),
        Records = drain(2),
        ?assertEqual(2, length(Records)),
        ?assertEqual([16#12, 16#13], [macula_record:type(R) || R <- Records])
    end.

records_verify(Ctx) ->
    fun() ->
        PutFn = register_sink_fn(),
        {ok, _} = macula_advertise_station:start_link(base_config(Ctx, PutFn)),
        Records = drain(2),
        lists:foreach(fun(R) ->
            ?assertMatch({ok, _}, macula_record:verify(R))
        end, Records)
    end.

redirect_address_matches(#{pk := Pk, addr := Addr} = Ctx) ->
    fun() ->
        PutFn = register_sink_fn(),
        {ok, _} = macula_advertise_station:start_link(base_config(Ctx, PutFn)),
        Records = drain(2),
        [Redirect] = [R || R <- Records, macula_record:type(R) =:= 16#13],
        Payload = macula_record:payload(Redirect),
        ?assertEqual(Addr, maps:get({text, <<"addr">>}, Payload)),
        ?assertEqual(Addr,
                     macula_address:derive(?REALM,
                                           macula_record:key(Redirect))),
        ?assertEqual(Pk, macula_record:key(Redirect))
    end.

multiple_addresses(#{kp := Kp, pk := Pk}) ->
    fun() ->
        PutFn = register_sink_fn(),
        Addrs = [<<16#fd, 0:120>>, <<16#fd, 1:120>>, <<16#fd, 2:120>>],
        Config = #{
            realm_pubkey     => ?REALM,
            identity_pubkey  => Pk,
            identity_privkey => Kp,
            quic_port        => 4400,
            addresses        => Addrs,
            put_fn           => PutFn,
            refresh_ms       => 5_000
        },
        {ok, _} = macula_advertise_station:start_link(Config),
        Records = drain(4),
        ?assertEqual(4, length(Records)),
        Redirects = [R || R <- Records, macula_record:type(R) =:= 16#13],
        ?assertEqual(3, length(Redirects)),
        PublishedAddrs = lists:sort([
            maps:get({text, <<"addr">>}, macula_record:payload(R))
            || R <- Redirects]),
        ?assertEqual(lists:sort(Addrs), PublishedAddrs)
    end.

refresh_republishes(Ctx) ->
    fun() ->
        PutFn = register_sink_fn(),
        Config = (base_config(Ctx, PutFn))#{refresh_ms => 150},
        {ok, _} = macula_advertise_station:start_link(Config),
        First  = drain(2),
        ?assertEqual(2, length(First)),
        Second = drain(2),
        ?assertEqual(2, length(Second)),
        [#{version := V1}|_] = [R || R <- First,  macula_record:type(R) =:= 16#12],
        [#{version := V2}|_] = [R || R <- Second, macula_record:type(R) =:= 16#12],
        ?assertNotEqual(V1, V2)
    end.

put_fn_failure_does_not_crash(#{kp := Kp, pk := Pk, addr := Addr}) ->
    fun() ->
        catch unregister(?SINK),
        true = register(?SINK, self()),
        BadPut = fun(R) ->
            ?SINK ! {tried, R},
            {error, simulated_failure}
        end,
        Config = #{
            realm_pubkey     => ?REALM,
            identity_pubkey  => Pk,
            identity_privkey => Kp,
            quic_port        => 4400,
            addresses        => [Addr],
            put_fn           => BadPut,
            refresh_ms       => 5_000
        },
        {ok, Pid} = macula_advertise_station:start_link(Config),
        receive {tried, _} -> ok after 1000 -> ?assert(false) end,
        ?assert(is_process_alive(Pid))
    end.

current_records_accessor(Ctx) ->
    fun() ->
        PutFn = register_sink_fn(),
        {ok, _} = macula_advertise_station:start_link(base_config(Ctx, PutFn)),
        _ = drain(2),
        Records = macula_advertise_station:current_records(),
        ?assertEqual(2, length(Records))
    end.
