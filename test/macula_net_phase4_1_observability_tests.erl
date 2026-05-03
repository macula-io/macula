%%%-------------------------------------------------------------------
%%% @doc Phase 4.1 §8 acceptance — observability hot-path counters.
%%%
%%% Drives a real route_packet egress + deliver_packet ingress flow
%%% in-process, asserts that the canonical metric set increments,
%%% and that latency histograms record samples.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_net_phase4_1_observability_tests).

-include_lib("eunit/include/eunit.hrl").

-define(OWN,    <<16#fd, 1:40, 16#01:80>>).
-define(REMOTE, <<16#fd, 1:40, 16#aa:80>>).

phase4_1_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
        fun egress_dispatch_increments_envelopes_forwarded_egress/1,
        fun no_route_increments_dropped_with_reason/1,
        fun ingress_handle_envelope_increments_envelopes_forwarded_ingress/1,
        fun ingress_unknown_dst_increments_dropped/1,
        fun resolve_event_records_outcome_and_histogram/1
     ]}.

setup() ->
    {ok, _} = application:ensure_all_started(telemetry),
    {ok, _} = macula_metrics:start_link(#{install_default_gauges => false}),
    macula_metrics:reset_all(),
    ok.

teardown(_) ->
    drain(),
    case whereis(macula_metrics) of
        undefined -> ok;
        _ -> macula_metrics:stop()
    end,
    ok.

%% Each test re-configures with `self()' captured at test-process scope
%% so messages from callbacks land in this test's mailbox. eunit foreach
%% spawns a fresh process per test, so a Self captured in setup/0 would
%% not match the test process.
configure_with_self() ->
    Self = self(),
    Sender = fun(StationId, Cbor) ->
        Self ! {sent, StationId, Cbor}, ok
    end,
    Stations = [#{address => ?REMOTE,
                  station => <<"station-a">>,
                  send => Sender}],
    ok = macula_route_packet:configure(
           #{own_address => ?OWN, stations => Stations}),
    TunWriter = fun(_Pkt) -> Self ! tun_written, ok end,
    ok = macula_deliver_packet:configure(#{
        local_addresses => [?OWN],
        tun_writer => TunWriter
    }),
    ok.

drain() ->
    receive _ -> drain() after 0 -> ok end.

%% --- §8 #3 — egress counter -----------------------------------------

egress_dispatch_increments_envelopes_forwarded_egress(_) ->
    fun() ->
        configure_with_self(),
        Pkt = ipv6_packet(?OWN, ?REMOTE),
        {ok, _} = macula_route_packet:dispatch(Pkt),
        receive {sent, _, _} -> ok after 1000 -> ?assert(false) end,
        Egress = counter_value(<<"macula_net_envelopes_forwarded_total">>,
                               [{kind, <<"data">>}, {direction, <<"egress">>}]),
        ?assertEqual(1, Egress),
        %% Egress histogram should also have one observation.
        H = pick_metric(<<"macula_net_egress_latency_seconds">>),
        [Sample] = maps:get(samples, H),
        ?assertEqual(1, maps:get(count, Sample))
    end.

no_route_increments_dropped_with_reason(_) ->
    fun() ->
        configure_with_self(),
        Pkt = ipv6_packet(?OWN, <<0:128>>),
        {error, no_route} = macula_route_packet:dispatch(Pkt),
        Dropped = counter_value(<<"macula_net_envelopes_dropped_total">>,
                                [{reason, <<"no_route">>}]),
        ?assertEqual(1, Dropped)
    end.

%% --- §8 #3 — ingress counter ----------------------------------------

ingress_handle_envelope_increments_envelopes_forwarded_ingress(_) ->
    fun() ->
        configure_with_self(),
        Cbor = macula_route_packet:encapsulate(<<1,2,3>>, ?REMOTE, ?OWN),
        ok = macula_deliver_packet:handle_envelope(Cbor),
        receive tun_written -> ok after 1000 -> ?assert(false) end,
        Ingress = counter_value(<<"macula_net_envelopes_forwarded_total">>,
                                [{kind, <<"data">>}, {direction, <<"ingress">>}]),
        ?assertEqual(1, Ingress)
    end.

ingress_unknown_dst_increments_dropped(_) ->
    fun() ->
        configure_with_self(),
        Cbor = macula_route_packet:encapsulate(<<9,9,9>>, ?REMOTE, ?REMOTE),
        {error, no_route} = macula_deliver_packet:handle_envelope(Cbor),
        Dropped = counter_value(<<"macula_net_envelopes_dropped_total">>,
                                [{reason, <<"no_route">>}]),
        ?assertEqual(1, Dropped)
    end.

%% --- §8 #4 — resolve histogram -------------------------------------

resolve_event_records_outcome_and_histogram(_) ->
    fun() ->
        %% FindFn that always returns not_found — drives the "miss"
        %% outcome through the resolve telemetry path without needing
        %% a real DHT.
        Find = fun(_K) -> {error, not_found} end,
        Realm = <<0:256>>,
        Addr  = ?REMOTE,
        {error, not_found} = macula_resolve_address:resolve(Addr, Realm, Find),
        Miss = counter_value(<<"macula_net_resolve_total">>,
                             [{outcome, <<"miss">>}]),
        ?assertEqual(1, Miss),
        H = pick_metric(<<"macula_net_resolve_latency_seconds">>),
        [Sample] = maps:get(samples, H),
        ?assertEqual(1, maps:get(count, Sample))
    end.

%% --- helpers --------------------------------------------------------

ipv6_packet(Src, Dst) ->
    Header = <<6:4, 0:8, 0:20, 8:16, 59:8, 64:8, Src/binary, Dst/binary>>,
    <<Header/binary, 1, 2, 3, 4, 5, 6, 7, 8>>.

counter_value(Name, LabelsRaw) ->
    Labels = lists:keysort(1, [{K, to_bin(V)} || {K, V} <- LabelsRaw]),
    M = pick_metric(Name),
    Hits = [V || #{labels := L, value := V} <- maps:get(samples, M),
                 L =:= Labels],
    sum(Hits).

pick_metric(Name) ->
    [M] = [M || #{name := N} = M <- macula_metrics:gather(), N =:= Name],
    M.

sum([])  -> 0;
sum(L)   -> lists:sum(L).

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_atom(V)   -> atom_to_binary(V, utf8).
