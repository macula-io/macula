%%%-------------------------------------------------------------------
%%% @doc Eunit for macula_packet_trace — Phase 4.1 §8 #5.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_packet_trace_tests).

%% Logger handler callback (this module IS a logger handler in tests).
-export([log/2]).

-include_lib("eunit/include/eunit.hrl").

-define(A, <<16#fd, 1:40, 16#aa:80>>).
-define(B, <<16#fd, 1:40, 16#bb:80>>).
-define(C, <<16#fd, 1:40, 16#cc:80>>).

trace_test_() ->
    {foreach,
     fun setup/0, fun teardown/1,
     [
        fun disabled_by_default/1,
        fun enable_addr_makes_it_enabled_for_that_addr_only/1,
        fun enable_all_makes_everything_enabled/1,
        fun disable_addr_unsets_it/1,
        fun disable_all_resets/1,
        fun trace_envelope_logs_when_src_or_dst_matches/1,
        fun trace_envelope_silent_when_disabled/1
     ]}.

setup() ->
    macula_packet_trace:disable_all(),
    ok.

teardown(_) ->
    macula_packet_trace:disable_all(),
    _ = uninstall_capture(),
    ok.

disabled_by_default(_) ->
    fun() ->
        ?assertEqual(false, macula_packet_trace:is_enabled_for(?A))
    end.

enable_addr_makes_it_enabled_for_that_addr_only(_) ->
    fun() ->
        ok = macula_packet_trace:enable(?A),
        ?assert(macula_packet_trace:is_enabled_for(?A)),
        ?assertNot(macula_packet_trace:is_enabled_for(?B))
    end.

enable_all_makes_everything_enabled(_) ->
    fun() ->
        ok = macula_packet_trace:enable_all(),
        ?assert(macula_packet_trace:is_enabled_for(?A)),
        ?assert(macula_packet_trace:is_enabled_for(?B))
    end.

disable_addr_unsets_it(_) ->
    fun() ->
        ok = macula_packet_trace:enable(?A),
        ok = macula_packet_trace:enable(?B),
        ok = macula_packet_trace:disable(?A),
        ?assertNot(macula_packet_trace:is_enabled_for(?A)),
        ?assert(macula_packet_trace:is_enabled_for(?B))
    end.

disable_all_resets(_) ->
    fun() ->
        ok = macula_packet_trace:enable(?A),
        ok = macula_packet_trace:enable(?B),
        ok = macula_packet_trace:disable_all(),
        ?assertNot(macula_packet_trace:is_enabled_for(?A)),
        ?assertNot(macula_packet_trace:is_enabled_for(?B))
    end.

%% logger handler intercept: install a per-test handler that captures
%% all `_macula.net.packet_trace.envelope' events to a counter.
trace_envelope_logs_when_src_or_dst_matches(_) ->
    fun() ->
        ok = install_capture(),
        ok = macula_packet_trace:enable(?A),
        ok = macula_packet_trace:trace_envelope(egress, ?A, ?B, 64),
        ok = macula_packet_trace:trace_envelope(ingress, ?B, ?A, 32),
        ok = macula_packet_trace:trace_envelope(egress, ?B, ?C, 16), % no match
        timer:sleep(50),
        ?assertEqual(2, captured_count()),
        ok = uninstall_capture()
    end.

trace_envelope_silent_when_disabled(_) ->
    fun() ->
        ok = install_capture(),
        ok = macula_packet_trace:trace_envelope(egress, ?A, ?B, 64),
        timer:sleep(50),
        ?assertEqual(0, captured_count()),
        ok = uninstall_capture()
    end.

%% --- logger handler helpers ----------------------------------------

-define(HANDLER, packet_trace_capture).

install_capture() ->
    _ = uninstall_capture(),
    ets_new(),
    OldPrimary = logger:get_primary_config(),
    persistent_term:put({?MODULE, old_primary}, OldPrimary),
    ok = logger:set_primary_config(level, debug),
    Config = #{level => debug,
               filter_default => stop,
               filters => [
                   {pkt, {fun packet_filter/2, []}}
               ],
               formatter => {logger_formatter, #{}},
               config => #{type => standard_io}},
    ok = logger:add_handler(?HANDLER,
                            ?MODULE,
                            Config),
    ok.

uninstall_capture() ->
    catch logger:remove_handler(?HANDLER),
    case persistent_term:get({?MODULE, old_primary}, undefined) of
        undefined -> ok;
        #{level := L} ->
            _ = logger:set_primary_config(level, L),
            persistent_term:erase({?MODULE, old_primary}),
            ok
    end,
    ok.

log(Event, _Config) ->
    capture(Event),
    ok.

capture(#{msg := {report, #{event := <<"_macula.net.packet_trace.envelope">>}}}) ->
    ets:update_counter(?HANDLER, count, {2, 1}, {count, 0});
capture(_) ->
    ok.

%% Filter that just accepts everything for inspection in log/2.
packet_filter(LogEvent, _Extra) -> LogEvent.

ets_new() ->
    case ets:info(?HANDLER) of
        undefined -> _ = ets:new(?HANDLER, [named_table, public, set]), ok;
        _ -> ets:delete_all_objects(?HANDLER), ok
    end.

captured_count() ->
    case ets:lookup(?HANDLER, count) of
        [{_, N}] -> N;
        []       -> 0
    end.
