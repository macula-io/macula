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
%%% What IS true, and what `station_target_shape_reaches_hello_test' guards:
%%% a station's dial target on 12, `pin_tls_cert => false' included, must
%%% complete a handshake through `macula_peering:connect/1'. A refusal of
%%% `pin_tls_cert' added in the peering layer and keyed on the KEY'S
%%% PRESENCE would break every station dial, and that test goes red if
%%% anyone makes it. `verify', which macula-station's 11.x target also
%%% carries, IS refused by the peering layer from 12.0.0: it chose a TLS
%%% mode, and there is one (plan WP 1.6 lists it among the station's
%%% changes for 12). `seed_checked/4' is an SDK gate the
%%% fleet never reaches, so a test there guards SDK callers and nothing
%%% about the fleet.
%%% The facade and the seed gate are the right homes for a different reason:
%%% macula-station reaches neither, calling `macula_peering:connect/1'
%%% directly, so a check in either cannot touch the fleet by construction.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pin_tls_cert_refusal_tests).

-include_lib("eunit/include/eunit.hrl").

%% Wrapped in `refused' on purpose: nothing is sent and every candidate
%% refuses it identically, which is `request' scope in the dial taxonomy.
%% Bare, it would fall to a catch-all and be classified `provider', meaning
%% "may have reached a provider", which is wrong for a refusal that sent
%% nothing.
-define(REFUSAL, {error, {refused, {pin_tls_cert, no_pin_primitive_for_mldsa87_identity}}}).
-define(SEED_REFUSAL, {error, {seed, {pin_tls_cert, no_pin_primitive_for_mldsa87_identity}}}).
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
       ?_assertEqual(?SEED_REFUSAL,
                     macula_station_link:seed_checked(?PINNED_SEED, key, profile, self()))},
      %% R2: `call_station/7' takes no option map, so it was not covered by
      %% the `Opts'-shaped checks and a pinned STATION rode straight past.
      {"call_station/7, which takes no option map, still refuses a pinned station",
       fun() ->
           {ok, Pool} = macula:connect([], #{}),
           try ?assertEqual(?REFUSAL,
                            macula:call_station(Pool, ?PINNED_SEED, ?NODE, ?REALM,
                                                <<"x.y">>, #{}, 300))
           after ok = macula:close(Pool)
           end
       end},
      {"join_mesh/1 refuses a pinned relay",
       ?_assertEqual(?REFUSAL, macula:join_mesh(#{relays => [?PINNED_SEED]}))}]}.

%%------------------------------------------------------------------
%% The backstop refusal must be PERMANENT, not transient
%%------------------------------------------------------------------

%% ⚠ This is the defect the seed gate INTRODUCED. `permanent_refusal/1`
%% had one permanent clause and a transient catch-all, so the new refusal
%% fell through, `start_refused(transient, ...)` scheduled a respawn every
%% second, the gate refused the same seed again, and the pool looped
%% forever on a link that can never start: uncounted, absent from
%% `status/1', and reported to a caller only as `not_connected'.
%%
%% Exercised through `macula_client:connect/2', deliberately. The facade
%% refuses this seed outright, so the only way to reach the gate with it
%% is the path that bypasses the facade, which is exactly the path that
%% would have looped in production.
backstop_refusal_is_permanent_and_counted_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([?PINNED_SEED], #{}),
    try
        %% Longer than ?LINK_RESPAWN_DELAY_MS (1s): if the refusal were
        %% transient there would be repeated start attempts in this window.
        timer:sleep(1500),
        {ok, #{refused_dials := Refused}} = macula_client:status(Pool),
        ?assertEqual(1, maps:get(pin_tls_cert_refused, Refused, 0))
    after ok = macula_client:close(Pool)
    end.

%% The supervised start is the path the facade's own documentation tells
%% production callers to use, and it pointed at `macula_client:connect/2',
%% so it bypassed the facade and every check on it.
%%
%% ⚠ Asserted on BEHAVIOUR, not on the spec's shape. An earlier version
%% matched `#{start := {macula, connect, _}}', which passes against a
%% facade that checks nothing: it pins the data and proves nothing about
%% what happens when the supervisor runs it. The legitimate start is
%% already covered by `macula_client_pool_keys_tests'.
child_spec_with_a_pinned_seed_fails_to_start_test() ->
    {ok, _} = application:ensure_all_started(macula),
    Spec = macula:child_spec(a_pinned_pool, [?PINNED_SEED], #{}),
    process_flag(trap_exit, true),
    Result = supervisor:start_link(macula_test_one_for_one, [Spec]),
    process_flag(trap_exit, false),
    ?assertMatch({error, {shutdown, {failed_to_start_child, a_pinned_pool,
                                     {refused, {pin_tls_cert, _}}}}},
                 Result).

%%------------------------------------------------------------------
%% The fleet's value survives the gate
%%------------------------------------------------------------------

%% ⚠ NAMED FOR WHAT IT COVERS. This exercises `seed_checked/4', which is
%% an SDK gate the FLEET NEVER REACHES: macula-station calls
%% `macula_peering:connect/1' directly. A presence-keyed refusal here
%% breaks SDK callers, not the fleet, so this test says nothing about the
%% fleet and an earlier name and comment claimed that it did.
%%
%% The fleet's guarantee is `station_target_shape_reaches_hello_test'
%% below, which drives macula-station's exact target through a real
%% handshake.
sdk_seed_with_false_passes_seed_checked_test() ->
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

%%------------------------------------------------------------------
%% The fleet's guarantee, through a real handshake
%%------------------------------------------------------------------

%% ⚠ THIS IS THE TEST THE PLACEMENT CLAIM NEEDS, and the third attempt at
%% it. Round one asked for it by name; round two found that the stand-in
%% called `seed_checked/4', which the fleet never reaches, so it could not
%% detect the change it claimed to guard against.
%%
%% macula-station's `do_dial/1' builds exactly this target and hands it to
%% `macula_peering:connect/1', never touching `macula_station_link'. So
%% this drives that target through the loopback pair and asserts the
%% handshake completes. A refusal added anywhere in the peering layer and
%% keyed on `pin_tls_cert' being PRESENT rather than on `true' would take
%% every station-to-station dial on the fleet down, and would turn this
%% red.
station_target_shape_reaches_hello_test_() ->
    {setup,
     fun macula_peering_handshake_tests:setup/0,
     fun macula_peering_handshake_tests:cleanup/1,
     fun(Ctx) ->
         [{"macula-station's exact dial target completes a handshake",
           {timeout, 30, fun() -> station_target_reaches_hello(Ctx) end}}]
     end}.

station_target_reaches_hello(Ctx) ->
    World = macula_peering_handshake_tests:world(Ctx, #{}),
    %% The shape `macula_station_outbound_link:do_dial/1' builds, less the
    %% `verify' key its `maybe_verify/2' adds on 11.x and 12 refuses.
    StationShape = #{pin_tls_cert => false},
    {ClientPid, ServerPid} =
        macula_peering_handshake_tests:connect(
          World, #{mode => off, accept_owner => self(),
                   target_extra => StationShape}),
    expect_connected(ClientPid),
    expect_connected(ServerPid),
    [catch macula_peering:close(P, test_cleanup) || P <- [ClientPid, ServerPid]],
    macula_peering_handshake_tests:forget_world(World).

expect_connected(Pid) ->
    receive
        {macula_peering, connected, Pid, _PeerNodeId} -> ok
    after 5_000 ->
        erlang:error({no_connected_notification, Pid})
    end.
