%% EUnit tests for the two peering counter tables with no table at all (macula#186, folding macula#187).
%%
%% macula_wire_tolerance and macula_peer_versions are created by `macula_peering_sup:init/1' beside
%% macula_realm_rights, and go down in the same teardown. A caller can outlive that run: the station's peer observer
%% sits in the station's own supervision tree and is still handed frames while the station shuts down. The tolerance
%% counters are read from the frame decoder, whose callers match the result (`ok = macula_wire_tolerance:count(...)'),
%% so an absent table was a crash in the decoder and not a lost count.
-module(macula_peering_counters_no_table_tests).

-include_lib("eunit/include/eunit.hrl").

-export([floor/2, raise/3]).

-define(EU_TIMEOUT, 120).
-define(PEER, <<3:256>>).
-define(STORE_VERSION, 3).

%% A store that already holds a floor for every peer, so the read path can be shown to fall back to it.
floor(?STORE_VERSION, _NodeId) -> ?STORE_VERSION.
raise(?STORE_VERSION, _NodeId, _Version) -> ok.

%% Only stopping the application removes tables their owner owns. The fixture starts it again whatever happens, so
%% the module that runs next still finds it up.
%%
%% Before the fix every case below raised `badarg' against a named table that does not exist.
without_the_counter_tables_test_() ->
    {timeout, ?EU_TIMEOUT,
     {setup,
      fun() ->
          {ok, _} = application:ensure_all_started(macula),
          ok = application:stop(macula),
          ?assertEqual(undefined, ets:whereis(macula_wire_tolerance)),
          ?assertEqual(undefined, ets:whereis(macula_peer_versions)),
          ok
      end,
      fun(_) ->
          {ok, _} = application:ensure_all_started(macula),
          ok
      end,
      fun(_) ->
          [{"a frame of a type this node does not know is counted nowhere, and the decoder is not failed",
            fun() -> ?assertEqual(ok, macula_wire_tolerance:count(wire_unknown_frame_type)) end},
           {"an ignored extension is counted nowhere, and the decoder is not failed",
            fun() -> ?assertEqual(ok, macula_wire_tolerance:count(wire_ext_ignored, <<"io.macula/ext">>)) end},
           {"a refused critical extension is counted nowhere, and the decoder is not failed",
            fun() -> ?assertEqual(ok, macula_wire_tolerance:count(wire_ext_refused, <<"io.macula/ext">>)) end},
           {"an unknown enum value is counted nowhere, and the decoder is not failed",
            fun() -> ?assertEqual(ok, macula_wire_tolerance:count(wire_enum_fallback, some_field)) end},
           {"the tolerance counters read as empty rather than raising",
            fun() -> ?assertEqual(#{}, macula_wire_tolerance:counters()) end},
           {"a handshake event is counted nowhere, and the link is not failed",
            fun() -> ?assertEqual(ok, macula_peer_versions:count(v5_connections)) end},
           {"a peer with no table and no store holds no floor",
            fun() -> ?assertEqual(none, macula_peer_versions:floor(?PEER, none)) end},
           {"a peer with no table takes its floor from the store, which is what outlives the run",
            fun() -> ?assertEqual(?STORE_VERSION, macula_peer_versions:floor(?PEER, {?MODULE, ?STORE_VERSION})) end},
           {"a raise with no table still reaches the store",
            fun() -> ?assertEqual(ok, macula_peer_versions:raise(?PEER, ?STORE_VERSION + 1,
                                                                {?MODULE, ?STORE_VERSION})) end},
           {"the handshake counters read as zero rather than raising",
            fun() ->
                Counters = macula_peer_versions:counters(),
                ?assertEqual(0, maps:get(v5_connections, Counters)),
                ?assertEqual([], [N || N <- maps:values(Counters), N =/= 0])
            end}]
      end}}.
