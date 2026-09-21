%%%-------------------------------------------------------------------
%%% @doc `pin_tls_cert' is refused, not accepted and ignored.
%%%
%%% This exists so that a reader of the SDK cannot be told a certificate
%%% is being pinned when none is.
%%%
%%% The option had a reader until 11.0.0 and has had none since. The
%%% published RPC guide meanwhile documented a default of `true'. macula#15
%%% is that gap. These tests pin the decision taken there: `true' is
%%% refused at every public entry point that takes a per-dial trust map,
%%% and `false' keeps working because it is the honest value and because
%%% live callers pass it.
%%%
%%% Every test runs against a REAL pool rather than a bare pid, so that
%%% against the pre-refusal code each one fails with a value mismatch
%%% (`{error, not_connected}' instead of the refusal) rather than crashing
%%% on a missing application. A test that goes red by crashing has not
%%% shown it is connected to the behaviour it claims to protect.
%%%
%%% PLACEMENT. The refusal lives in two places, `macula' (the facade, for a
%%% synchronous error to the caller) and `macula_station_link:seed_checked/4'
%%% (the gate every seed map passes through). It is NOT in
%%% `macula_peering_conn'.
%%%
%%% ⚠ An earlier version of this comment said a refusal in the peering
%%% target "would fail every station-to-station dial on the fleet". THAT WAS
%%% WRONG, and wrong in the flattering direction. macula-station passes
%%% `pin_tls_cert => false' and this refusal matches only `true', so moving
%%% it into the peering layer would not break the fleet at all. The claim
%%% asserted nothing and no test guarded it, which is the same defect the
%%% refusal itself exists to fix.
%%%
%%% What IS true, and what `fleet_value_survives_the_gate_test' guards: the
%%% fleet's value must keep working wherever a check is placed. A refusal
%%% keyed on the KEY'S PRESENCE rather than on `true' is the move that would
%%% break every station dial, and that test goes red if anyone makes it.
%%% The facade and the seed gate are the right homes for a different reason:
%%% macula-station reaches neither, calling `macula_peering:connect/1'
%%% directly, so a check in either cannot touch the fleet by construction.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pin_tls_cert_refusal_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REFUSAL, {error, {pin_tls_cert, no_pin_primitive_for_mldsa87_identity}}).
-define(NODE, <<7:256>>).
-define(REALM, <<0:256>>).
-define(SEED, <<"quic://127.0.0.1:4433">>).
-define(MCID, <<2, 16#55, 0:384>>).
-define(PIN, #{expected_node_id => ?NODE, pin_tls_cert => true}).

%% `foreach', not `setup': each test gets its own pool. Under a shared
%% fixture the first failure cancels the rest, so a module run against
%% code that fails them ALL reports one failure and says nothing about
%% the others. That is the difference between a red run and a useful one.
pin_tls_cert_test_() ->
    {foreach,
     fun start_pool/0,
     fun stop_pool/1,
     refusals() ++ accepted() ++ unchanged()}.

start_pool() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula:connect([], #{}),
    Pool.

stop_pool(Pool) ->
    catch macula:close(Pool),
    ok.

%%------------------------------------------------------------------
%% `true' is refused, at every entry point that takes the trust map
%%------------------------------------------------------------------

%% Against the pre-refusal code every one of these returns
%% `{error, not_connected}' from the empty pool instead, which is the
%% mismatch that shows the assertion is load-bearing.
refusals() ->
    [fun(Pool) ->
         {"call_station refuses a requested pin",
          ?_assertEqual(?REFUSAL,
                        macula:call_station(Pool, ?SEED, ?NODE, ?REALM, <<"x.y">>,
                                            #{}, 300, ?PIN))}
     end,
     fun(Pool) ->
         %% `dial_timeout_ms' is not decoration: this entry point takes no
         %% positional timeout, so against code that does NOT refuse, the
         %% call blocks on an empty pool until eunit's own timeout. It then
         %% reports `*timed out*' and cancels a sibling, which is a red run
         %% that proves less than it looks. With a short dial timeout the
         %% pre-refusal code returns a value and the assertion states a
         %% mismatch.
         {"call_stream_station refuses a requested pin",
          ?_assertEqual(?REFUSAL,
                        macula:call_stream_station(Pool, ?SEED, ?NODE, ?REALM,
                                                   <<"x.y">>, #{},
                                                   maps:merge(?PIN, #{dial_timeout_ms => 200})))}
     end,
     fun(Pool) ->
         {"put_content_station refuses a requested pin",
          ?_assertEqual(?REFUSAL,
                        macula:put_content_station(Pool, ?SEED, <<"bytes">>, 300, ?PIN))}
     end,
     fun(Pool) ->
         {"get_content_station refuses a requested pin",
          ?_assertEqual(?REFUSAL,
                        macula:get_content_station(Pool, ?SEED, ?MCID, 300, ?PIN))}
     end,
     fun(_Pool) ->
         {"connect refuses a requested pin, before any pool is started",
          ?_assertEqual(?REFUSAL, macula:connect([], #{pin_tls_cert => true}))}
     end].

%%------------------------------------------------------------------
%% `false' and absence pass
%%------------------------------------------------------------------

%% These start their own pool, because the point is that the value gets
%% PAST the check and reaches the ordinary code path. Asserting only that
%% the result is not the refusal would pass against a function that
%% refused everything for some other reason.
accepted() ->
    [fun(_Pool) ->
         {"`false' is accepted: it is what every caller in the fleet passes "
          "today, and refusing it would break a live caller for asking for "
          "the safe thing",
          fun() -> pool_starts(#{pin_tls_cert => false}) end}
     end,
     fun(_Pool) ->
         {"an absent key is accepted", fun() -> pool_starts(#{}) end}
     end,
     fun(_Pool) ->
         {"only the exact atom `true' is refused: a non-boolean is not "
          "quietly treated as a request to pin",
          fun() -> pool_starts(#{pin_tls_cert => <<"yes">>}) end}
     end].

pool_starts(Opts) ->
    {ok, Pool} = macula:connect([], Opts),
    ?assert(is_process_alive(Pool)),
    ok = macula:close(Pool).

%%------------------------------------------------------------------
%% Nothing else moved
%%------------------------------------------------------------------

unchanged() ->
    [fun(Pool) ->
         {"an invalid MCID is still an invalid MCID: the refusal does not "
          "swallow a fault that was already being reported",
          ?_assertEqual({error, invalid_mcid},
                        macula:get_content_station(Pool, ?SEED, <<"not an mcid">>,
                                                   300, ?PIN))}
     end,
     %% `macula_station_link:add_tls_opts/2' folded `pin_tls_cert' into
     %% the seed map for a consumer that no longer exists. A seed still
     %% carrying it would put the dead key back on the peering target.
     fun(_Pool) ->
         {"a seed built for a bare IP carries no pin key",
          fun() ->
              Station = #{host_advertised => [<<"2600:3c04::1">>], quic_port => 4433,
                          node_id => <<1:256>>},
              {true, {Seed, <<1:256>>}} = macula_client:station_seed(Station),
              ?assertNot(maps:is_key(pin_tls_cert, Seed))
          end}
     end].

%%------------------------------------------------------------------
%% The option's HISTORICAL home is the seed/station map, not `Opts'
%%------------------------------------------------------------------

%% ⚠ These exist because the first version of this refusal inspected only
%% `Opts', and the seed map is where the option actually lived: the
%% CHANGELOG on hexdocs calls it a connect/link opt and macula-station
%% carries it in exactly that map. A reader who learned it from our own
%% documentation puts it on the seed, gets `{ok, Pool}', and has been told
%% a check happened that did not. That is worse than shipping nothing,
%% which is the argument the refusal was justified with.
%%
%% `macula_station_link:parse_seed/1' returns a map seed UNFILTERED and
%% `seed_checked/4' looked only at `expected_node_id', so the key rode
%% through into the peering target, where `connect_opts()' ends in
%% `_ => _' and nothing noticed.
-define(PINNED_SEED, #{host => <<"127.0.0.1">>, port => 4433,
                       expected_node_id => ?NODE, pin_tls_cert => true}).

seed_map_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
     fun(_) -> ok end,
     [{"a map SEED carrying a requested pin is refused by connect/2",
       ?_assertEqual(?REFUSAL, macula:connect([?PINNED_SEED], #{}))},
      {"a map seed carrying `false' still connects",
       fun() ->
           Seed = maps:put(pin_tls_cert, false, ?PINNED_SEED),
           {ok, Pool} = macula:connect([Seed], #{}),
           ?assert(is_process_alive(Pool)),
           ok = macula:close(Pool)
       end},
      {"a map STATION carrying a requested pin is refused by call_station/8",
       fun() ->
           {ok, Pool} = macula:connect([], #{}),
           try ?assertEqual(?REFUSAL,
                            macula:call_station(Pool, ?PINNED_SEED, ?NODE, ?REALM,
                                                <<"x.y">>, #{}, 300, #{}))
           after ok = macula:close(Pool)
           end
       end},
      %% `seed_checked/4' is the choke point every seed map passes through,
      %% and the fleet does NOT: macula-station calls
      %% `macula_peering:connect/1' and never builds a station_link.
      {"seed_checked/4 refuses the seed itself, as the backstop under the facade",
       ?_assertEqual({error, {seed, {pin_tls_cert, no_pin_primitive_for_mldsa87_identity}}},
                     macula_station_link:seed_checked(?PINNED_SEED, key, profile, self()))}]}.

%% The supervised start is the path the facade's own documentation tells
%% production callers to use, and it pointed at `macula_client:connect/2',
%% so it bypassed the facade and every check on it.
child_spec_start_goes_through_the_facade_test() ->
    ?assertMatch(#{start := {macula, connect, [[], #{}]}},
                 macula:child_spec(a_pool, [], #{})).

%%------------------------------------------------------------------
%% The fleet's value survives the gate
%%------------------------------------------------------------------

%% macula-station's `do_dial/1' builds its target with
%% `pin_tls_cert => false' on every dial. Any refusal anywhere must let
%% that through. This goes red the moment a check is keyed on the key's
%% presence instead of on `true', which is the change that would take the
%% fleet down.
fleet_value_survives_the_gate_test() ->
    FleetShape = #{host => <<"127.0.0.1">>, port => 4433,
                   expected_node_id => ?NODE, pin_tls_cert => false},
    ?assertMatch({ok, _Seed, _Key, _Profile, _Issuer},
                 macula_station_link:seed_checked(FleetShape, key, profile, self())).

%%------------------------------------------------------------------
%% The gate's ordinary path: a seed with NO trust keys at all
%%------------------------------------------------------------------

%% ⚠ This is the highest-risk clause on the branch. The `pin_tls_cert'
%% refusal is now the FIRST clause of `macula_station_link:seed_checked/4',
%% a function EVERY seed map in the SDK passes through. If it is wrong it
%% is wrong for every link the SDK starts, not only for a caller who passes
%% the option.
%%
%% The rest of the suite exercises this path constantly, but that is
%% coverage of a system that happens to route through the clause, not
%% coverage OF the clause: nothing there would localise a fault to it, and
%% nothing asserts the seed comes back unaltered.
%%
%% Three properties, each of which a plausible mistake in that clause
%% breaks:
%%   1. a seed naming no trust keys is ACCEPTED (a clause matching too
%%      broadly refuses every link in the SDK);
%%   2. the seed comes back BYTE-IDENTICAL (a clause that normalises or
%%      strips a key would silently drop a pin);
%%   3. the pre-existing `expected_node_id_required' refusal still fires
%%      (a new first clause can shadow the one below it).
plain_seed_passes_the_gate_unaltered_test() ->
    Seed = #{host => <<"127.0.0.1">>, port => 4433, expected_node_id => ?NODE},
    ?assertEqual({ok, Seed, a_key, a_profile, self()},
                 macula_station_link:seed_checked(Seed, a_key, a_profile, self())).

seed_without_a_pin_is_still_refused_by_the_clause_below_test() ->
    ?assertEqual({error, {seed, expected_node_id_required}},
                 macula_station_link:seed_checked(#{host => <<"127.0.0.1">>, port => 4433},
                                                  a_key, a_profile, self())).
