%%%-------------------------------------------------------------------
%%% @doc `verify' is refused, in any value, at every public entry point that
%%% takes a seed, a station or a per-dial trust map.
%%%
%%% It chose a TLS verification mode, a webpki chain check or none, and
%%% 12.0.0 has one mode (plan decisions D12 and D16): a station's handshake
%%% signature under the key of its ML-DSA-87 certificate, with its identity
%%% proved by the signed handshake. Accepting the key and ignoring it would
%%% let a caller believe a check of its choosing ran (macula#15), so the
%%% facade refuses it before anything is dialled, as it refuses
%%% `pin_tls_cert => true'. The peering layer refuses it on a dial target
%%% too (`macula_peering_handshake_tests').
%%% @end
%%%-------------------------------------------------------------------
-module(macula_verify_option_refusal_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REFUSAL, {error, {refused, {verify, one_verification_mode}}}).
-define(NODE, <<7:256>>).
-define(REALM, <<0:256>>).
-define(SEED, <<"quic://127.0.0.1:4433">>).
-define(MCID, <<2, 16#55, 0:384>>).
-define(VERIFY, #{expected_node_id => ?NODE, verify => none}).

%% `foreach': each test gets its own pool, so one failure does not cancel
%% the rest.
verify_option_test_() ->
    {foreach,
     fun start_pool/0,
     fun stop_pool/1,
     refusals()}.

start_pool() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula:connect([], #{}),
    Pool.

stop_pool(Pool) ->
    catch macula:close(Pool),
    ok.

%% Against code that does not refuse, the per-call ones return
%% `{error, not_connected}' from the empty pool, and `connect/2' a pool.
refusals() ->
    [fun(Pool) ->
         {"call_station refuses verify in its options",
          ?_assertEqual(?REFUSAL,
                        macula:call_station(Pool, ?SEED, ?NODE, ?REALM, <<"x.y">>,
                                            #{}, 300, ?VERIFY))}
     end,
     fun(Pool) ->
         {"call_station refuses verify on the station it dials",
          ?_assertEqual(?REFUSAL,
                        macula:call_station(Pool, #{host => <<"127.0.0.1">>, port => 4433,
                                                    verify => webpki},
                                            ?NODE, ?REALM, <<"x.y">>, #{}, 300))}
     end,
     fun(Pool) ->
         {"call_stream_station refuses verify",
          ?_assertEqual(?REFUSAL,
                        macula:call_stream_station(Pool, ?SEED, ?NODE, ?REALM, <<"x.y">>, #{},
                                                   maps:merge(?VERIFY, #{dial_timeout_ms => 200})))}
     end,
     fun(Pool) ->
         {"put_content_station refuses verify",
          ?_assertEqual(?REFUSAL,
                        macula:put_content_station(Pool, ?SEED, <<"bytes">>, 300, ?VERIFY))}
     end,
     fun(Pool) ->
         {"get_content_station refuses verify",
          ?_assertEqual(?REFUSAL,
                        macula:get_content_station(Pool, ?SEED, ?MCID, 300, ?VERIFY))}
     end,
     fun(_Pool) ->
         {"connect refuses verify in its options, before any pool is started",
          ?_assertEqual(?REFUSAL, macula:connect([], #{verify => webpki}))}
     end,
     fun(_Pool) ->
         {"macula_client:connect/2, which the services call directly, refuses verify too",
          [?_assertEqual({error, {seeds, {verify, one_verification_mode}}},
                         macula_client:connect([], #{verify => none})),
           ?_assertEqual({error, {seeds, {verify, one_verification_mode}}},
                         macula_client:connect([#{host => <<"127.0.0.1">>, port => 4433,
                                                  expected_node_id => ?NODE, verify => webpki}], #{}))]}
     end,
     fun(_Pool) ->
         {"connect refuses verify on a seed",
          ?_assertEqual(?REFUSAL,
                        macula:connect([#{host => <<"127.0.0.1">>, port => 4433,
                                          expected_node_id => ?NODE, verify => none}], #{}))}
     end].
