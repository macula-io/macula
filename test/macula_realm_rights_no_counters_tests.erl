%% EUnit tests for macula_realm_rights with no counter table at all (macula#186). The counters belong to the peering
%% run -- `macula_peering_sup' creates the table and its life is the run's -- and a caller can outlive it: the
%% station's peer observer lives in the station's own supervision tree and is still handed frames while a station
%% shuts down. There is then no table, and a decision still has to be decided.
-module(macula_realm_rights_no_counters_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EU_TIMEOUT, 120).
-define(REALM_NAME, <<"io.example">>).
-define(TOPIC, <<"io.example/acme/_org/orders/placed_v1">>).
-define(PATTERN, <<"io.example/acme/_org/orders/*">>).
-define(PROCEDURE, <<"io.example/acme/place_order">>).

realm() -> crypto:hash(sha256, ?REALM_NAME).

%% The table is macula_peering_sup's, so only stopping the application removes it. The fixture starts the
%% application afterwards whatever happens, so the module that runs next still finds it up.
%%
%% Before the fix every case below raised `badarg' out of the counting write: `ets:member' and then
%% `ets:update_counter' on a named table that does not exist.
without_the_counter_table_test_() ->
    {timeout, ?EU_TIMEOUT,
     {setup,
      fun() ->
          {ok, _} = application:ensure_all_started(macula),
          ok = application:stop(macula),
          ?assertEqual(undefined, ets:whereis(macula_realm_rights)),
          ok
      end,
      fun(_) ->
          {ok, _} = application:ensure_all_started(macula),
          ok
      end,
      fun(_) ->
          [{"a SUBSCRIBE is still decided, and allowed, where there is nowhere to count it",
            fun() -> ?assertEqual(allow, macula_realm_rights:subscribe(counting, realm(), none, ?PATTERN)) end},
           {"a PUBLISH is still decided, and allowed, where there is nowhere to count it",
            fun() -> ?assertEqual(allow, macula_realm_rights:publish(counting, realm(), none, ?TOPIC)) end},
           {"a refusal is still a refusal, with its reason, where there is nowhere to count it",
            fun() ->
                ?assertMatch({refuse, #{reason := no_token}},
                             macula_realm_rights:publish(refusing, realm(), none, ?TOPIC))
            end},
           {"a SERVE is still decided where there is nowhere to count it",
            fun() ->
                ?assertEqual(allow, macula_realm_rights:serve(counting, realm(), ?PROCEDURE,
                                                              {error, not_delegated}))
            end},
           {"the counters read as empty rather than raising",
            fun() -> ?assertEqual(#{}, macula_realm_rights:counters()) end}]
      end}}.
