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
-define(SEED, <<"quic://127.0.0.1:4433">>).

%%------------------------------------------------------------------
%% `true' is refused, before anything is dialled
%%------------------------------------------------------------------

%% No pool is started and no socket is opened: the refusal happens on the
%% option map, so these need nothing running. A `connect/2' that got past
%% the check would return `{ok, Pid}' and leak a process.
connect_refuses_a_requested_pin_test() ->
    ?assertEqual(?REFUSAL, macula:connect([], #{pin_tls_cert => true})).

call_station_refuses_a_requested_pin_test() ->
    ?assertEqual(?REFUSAL,
                 macula:call_station(self(), ?SEED, ?NODE, <<0:256>>, <<"x.y">>,
                                     #{}, 1_000,
                                     #{expected_node_id => ?NODE,
                                       pin_tls_cert => true})).

call_stream_station_refuses_a_requested_pin_test() ->
    ?assertEqual(?REFUSAL,
                 macula:call_stream_station(self(), ?SEED, ?NODE, <<0:256>>, <<"x.y">>,
                                            #{}, #{expected_node_id => ?NODE,
                                                   pin_tls_cert => true})).

put_content_station_refuses_a_requested_pin_test() ->
    ?assertEqual(?REFUSAL,
                 macula:put_content_station(self(), ?SEED, <<"bytes">>, 1_000,
                                            #{expected_node_id => ?NODE,
                                              pin_tls_cert => true})).

%% The MCID guard runs first on this one, so the map must reach the
%% refusal through a well-formed MCID or the test proves nothing.
get_content_station_refuses_a_requested_pin_test() ->
    Mcid = <<2, 16#55, 0:384>>,
    ?assertEqual(?REFUSAL,
                 macula:get_content_station(self(), ?SEED, Mcid, 1_000,
                                            #{expected_node_id => ?NODE,
                                              pin_tls_cert => true})).

%% An invalid MCID is still an invalid MCID: the refusal does not swallow
%% a fault that was already being reported.
get_content_station_still_reports_an_invalid_mcid_test() ->
    ?assertEqual({error, invalid_mcid},
                 macula:get_content_station(self(), ?SEED, <<"not an mcid">>, 1_000,
                                            #{pin_tls_cert => true})).

%%------------------------------------------------------------------
%% `false' and absence pass
%%------------------------------------------------------------------

%% These start a real pool, because the point is that the value gets PAST
%% the check and reaches the ordinary code path. Asserting only that the
%% result is not the refusal would pass against a function that refused
%% everything for some other reason.
accepted_values_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
     fun(_) -> ok end,
     [{"`false' is accepted: it is what every caller in the fleet passes "
       "today, and refusing it would break a live caller for asking for "
       "the safe thing",
       fun() -> pool_starts(#{pin_tls_cert => false}) end},
      {"an absent key is accepted",
       fun() -> pool_starts(#{}) end},
      {"only the exact atom `true' is refused: a non-boolean is not "
       "quietly treated as a request to pin",
       fun() -> pool_starts(#{pin_tls_cert => <<"yes">>}) end}]}.

pool_starts(Opts) ->
    {ok, Pool} = macula:connect([], Opts),
    ?assert(is_process_alive(Pool)),
    ok = macula:close(Pool).

%%------------------------------------------------------------------
%% The key does not reach the link
%%------------------------------------------------------------------

%% `macula_station_link:add_tls_opts/2' folded `pin_tls_cert' into the
%% seed map for a consumer that no longer exists. A seed that still
%% carried it would put the dead key back on the peering target.
seed_built_for_a_bare_ip_carries_no_pin_key_test() ->
    Station = #{host_advertised => [<<"2600:3c04::1">>], quic_port => 4433,
                node_id => <<1:256>>},
    {true, {Seed, <<1:256>>}} = macula_client:station_seed(Station),
    ?assertNot(maps:is_key(pin_tls_cert, Seed)).
