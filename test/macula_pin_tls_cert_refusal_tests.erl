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
%%% Placement matters and is asserted here on purpose. The refusal is at
%%% the `macula' facade, NOT in `macula_peering_conn'. macula-station's
%%% outbound links call `macula_peering:connect/1' directly with a target
%%% carrying `pin_tls_cert => false'; a refusal in the peering target
%%% would fail every station-to-station dial on the fleet.
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
