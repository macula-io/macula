%% EUnit tests for `macula_content_fetch': fetching content from the node that shares it (D27). The DHT answer, the
%% station endpoint resolution and the stream dial are an io map; the sharers are real serving handlers over local
%% in-process streams, one per announced node, so a fetch reads DATA bodies exactly as over a station link. A
%% tampering sharer answers with bodies that do not match what was asked.
-module(macula_content_fetch_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).
-define(STATION, <<16#51:256>>).
-define(CHUNK, 262144).

fetch_test_() ->
    {foreach,
     fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
     fun(_) -> ok end,
     [{"a raw root is fetched", fun a_raw_root_is_fetched/0},
      {"a manifest root is fetched chunk by chunk and assembled", fun a_manifest_is_fetched/0},
      {"a block that does not match its content id is refused, and the next sharer serves",
       fun a_tampered_block_moves_on/0},
      {"a manifest that does not match its content id is refused", fun a_tampered_manifest_is_refused/0},
      {"a manifest past the chunk bound is refused before any chunk is asked for",
       fun a_manifest_past_the_chunk_bound_is_refused/0},
      {"content past the caller's byte bound is refused", fun content_past_max_bytes_is_refused/0},
      {"an announcement naming another node's procedure is ignored", fun a_foreign_procedure_is_ignored/0},
      {"a dead first sharer moves the fetch to the next", fun a_dead_sharer_moves_on/0},
      {"every sharer failing names each", fun every_sharer_failing_names_each/0},
      {"content nobody announces is not_shared", fun content_nobody_announces_is_not_shared/0},
      {"a fetch in a realm uses only that realm's announcements", fun a_fetch_keeps_to_its_realm/0}]}.

a_fetch_keeps_to_its_realm() ->
    {Sharer, MCID} = honest_sharer(<<"hello">>),
    ?assertEqual({ok, <<"hello">>}, fetch([Sharer], MCID, #{realm => ?REALM})),
    ?assertEqual({error, not_shared}, fetch([Sharer], MCID, #{realm => <<8:256>>})).

a_raw_root_is_fetched() ->
    {Sharer, MCID} = honest_sharer(<<"hello, mesh">>),
    ?assertEqual({ok, <<"hello, mesh">>}, fetch([Sharer], MCID, #{})).

a_manifest_is_fetched() ->
    Bytes = crypto:strong_rand_bytes(?CHUNK * 2 + 99),
    {Sharer, MCID} = honest_sharer(Bytes),
    ?assertEqual({ok, Bytes}, fetch([Sharer], MCID, #{})).

a_tampered_block_moves_on() ->
    {Honest, MCID} = honest_sharer(<<"the real bytes">>),
    Liar = sharer(fun(Stream, _Args) ->
                      ok = macula:send(Stream, #{kind => block, mcid => MCID, bytes => <<"other bytes">>}, msgpack),
                      macula:close_stream(Stream)
                  end),
    ?assertEqual({ok, <<"the real bytes">>}, fetch([Liar, Honest], MCID, #{order => as_given})).

a_tampered_manifest_is_refused() ->
    {_Honest, MCID} = honest_sharer(crypto:strong_rand_bytes(?CHUNK + 1)),
    {ok, Other, _} = macula_manifest:create(crypto:strong_rand_bytes(?CHUNK + 2)),
    Liar = sharer(fun(Stream, _Args) ->
                      ok = macula:send(Stream, #{kind => manifest, mcid => MCID, manifest => Other}, msgpack),
                      macula:close_stream(Stream)
                  end),
    ?assertMatch({error, {unavailable, [{_, manifest_mcid_mismatch}]}}, fetch([Liar], MCID, #{})).

a_manifest_past_the_chunk_bound_is_refused() ->
    Bytes = crypto:strong_rand_bytes(?CHUNK * 3),
    {Sharer, MCID} = honest_sharer(Bytes),
    ?assertMatch({error, {unavailable, [{_, {too_many_chunks, 3}}]}}, fetch([Sharer], MCID, #{max_chunks => 2})),
    %% Only the root was asked for.
    ?assertEqual(1, asks(Sharer)).

content_past_max_bytes_is_refused() ->
    Bytes = crypto:strong_rand_bytes(?CHUNK + 10),
    {Sharer, MCID} = honest_sharer(Bytes),
    ?assertMatch({error, {unavailable, [{_, {too_large, _}}]}}, fetch([Sharer], MCID, #{max_bytes => ?CHUNK})),
    {Small, SmallMCID} = honest_sharer(<<"0123456789">>),
    ?assertMatch({error, {unavailable, [{_, {too_large, 10}}]}}, fetch([Small], SmallMCID, #{max_bytes => 5})).

a_foreign_procedure_is_ignored() ->
    {Sharer, MCID} = honest_sharer(<<"hello">>),
    Foreign = Sharer#{procedure => <<"~", (hex(<<16#EE:256>>))/binary, "/content_v1">>},
    ?assertEqual({error, not_shared}, fetch([Foreign], MCID, #{})).

a_dead_sharer_moves_on() ->
    {Honest, MCID} = honest_sharer(<<"still here">>),
    Dead = (sharer(fun(_S, _A) -> ok end))#{dead => true},
    ?assertEqual({ok, <<"still here">>}, fetch([Dead, Honest], MCID, #{order => as_given})).

every_sharer_failing_names_each() ->
    {Honest, MCID} = honest_sharer(<<"x">>),
    DeadA = (sharer(fun(_S, _A) -> ok end))#{dead => true},
    DeadB = (sharer(fun(_S, _A) -> ok end))#{dead => true},
    {error, {unavailable, Failures}} = fetch([DeadA, DeadB], MCID, #{}),
    ?assertEqual(lists:sort([maps:get(node, DeadA), maps:get(node, DeadB)]),
                 lists:sort([Node || {Node, _Reason} <- Failures])),
    _ = Honest.

content_nobody_announces_is_not_shared() ->
    ?assertEqual({error, not_shared}, fetch([], <<2, 16#55, 0:384>>, #{})).

%%------------------------------------------------------------------
%% Sharers and the io
%%------------------------------------------------------------------

%% A sharer serving `Bytes' from its own store, and the content id.
honest_sharer(Bytes) ->
    {MCID, Store} = macula_content_store:added(Bytes, #{}, macula_content_store:new()),
    Lookup = fun(Want, M) -> macula_content_serve:lookup(Want, M, Store) end,
    {sharer(fun(Stream, Args) -> macula_content_serve:serve(Stream, Args, Lookup) end), MCID}.

%% A node that announces through ?STATION and serves `Handler' on a fresh local procedure, counting what it is asked.
sharer(Handler) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Node} = macula_node_keys:node_id(Key),
    Local = <<"t.fetch_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Counter = counters:new(1, []),
    ok = macula:advertise_stream(Local, server_stream,
                                 fun(S, A) -> counters:add(Counter, 1, 1), Handler(S, A) end),
    #{node => Node, key => Key, local => Local, asks => Counter,
      procedure => <<"~", (hex(Node))/binary, "/content_v1">>}.

asks(#{asks := Counter}) -> counters:get(Counter, 1).

%% Fetch `MCID' from the announcements of `Sharers', dialing each sharer's local procedure.
fetch(Sharers, MCID, Opts) ->
    Announced = [announcement(S, MCID) || S <- Sharers],
    ByNode = maps:from_list([{maps:get(node, S), S} || S <- Sharers]),
    Io = #{find_records => fun(_Pool, Key, _T) ->
                               Key = macula_record:content_key(MCID),
                               {ok, Announced}
                           end,
           resolve_station_endpoint => fun(_Pool, ?STATION, _T) -> {ok, <<"quic://station.test:4433">>} end,
           call_stream_station => fun(_Pool, _Url, Target, ?REALM, _Proc, Args, #{expected_node_id := ?STATION}) ->
                                      dialed(maps:get(Target, ByNode), Args)
                                  end},
    macula_content_fetch:get(self(), MCID, maps:merge(#{io => Io}, Opts)).

dialed(#{dead := true}, _Args) -> {error, not_connected};
dialed(#{local := Local}, Args) -> macula:call_stream(Local, Args).

announcement(#{node := Node, key := Key, procedure := Procedure}, MCID) ->
    macula_record:refresh(macula_record:content_announcement(
                            Node, MCID,
                            #{realm_id => ?REALM, serving_station => ?STATION, procedure => Procedure}), Key).

hex(Node) -> binary:encode_hex(Node, lowercase).
