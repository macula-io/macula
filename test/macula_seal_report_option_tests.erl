%%%-------------------------------------------------------------------
%%% @doc `report' asks for a call's seal report (DESIGN_E2E_SEAL_REPORT) and is
%%% a boolean. Any other value is refused before anything is dialled or looked
%%% up, at both entry points that take it: `call/6' and `call_station/8', which
%%% the pool's own direct dial calls through, so it honours the option rather
%%% than ignoring it (§4).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_seal_report_option_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REFUSAL, {error, {invalid_option, report}}).
-define(NODE, <<7:256>>).
-define(REALM, <<0:256>>).
-define(SEED, <<"quic://127.0.0.1:4433">>).

report_option_test_() ->
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

%% Against code that ignores `report', call_station/8 goes on to seal from
%% its options and dial (`{error, not_connected}' from the empty pool).
refusals() ->
    [fun(Pool) ->
         {"call_station refuses a report that is not a boolean",
          ?_assertEqual(?REFUSAL,
                        macula:call_station(Pool, ?SEED, ?NODE, ?REALM, <<"x.y">>, #{}, 300,
                                            #{expected_node_id => ?NODE, confidential => off, report => yes}))}
     end,
     fun(Pool) ->
         %% A valid report reaches the pool's station call: from the empty pool, `not_connected', with or without it.
         {"call_station carries a report down to the pool",
          ?_assertEqual([{error, not_connected}, {error, not_connected}],
                        [macula:call_station(Pool, ?SEED, ?NODE, ?REALM, <<"x.y">>, #{}, 300,
                                             #{expected_node_id => ?NODE, confidential => off, report => Report})
                         || Report <- [true, false]])}
     end,
     fun(Pool) ->
         %% A stream reports through stream_report/1: `report' on its open means nothing and is refused, whatever its
         %% value, as the C ABI refuses it (an option accepted and ignored is 13.0.1's lesson).
         {"a stream open refuses report, whatever its value",
          ?_assertEqual([?REFUSAL, ?REFUSAL, ?REFUSAL],
                        [macula:call_stream(Pool, ?REALM, <<"x.y">>, #{}, #{report => true}),
                         macula:call_stream(Pool, ?REALM, <<"x.y">>, #{}, #{report => false}),
                         macula:call_stream_station(Pool, ?SEED, ?NODE, ?REALM, <<"x.y">>, #{},
                                                    #{expected_node_id => ?NODE, confidential => off,
                                                      report => true})])}
     end,
     fun(Pool) ->
         {"call refuses a report that is not a boolean",
          ?_assertEqual(?REFUSAL, macula:call(Pool, ?REALM, <<"x.y">>, #{}, 300, #{report => 1}))}
     end].
