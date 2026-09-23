%% EUnit tests for why a pool's links last went down, as `macula_client:links/1' reports it per seed.
%%
%% A link that disconnects is respawned by the pool about a second later, so by the time anyone looks, the link that
%% knew the reason is gone and a healthy looking one stands in its place. The pool keeps the last reason per seed and
%% every `link_info()' carries it as `last_disconnect', so a realm showing its seeds on a health page can say WHY a seed
%% keeps dropping, not only that it did.
%%
%% Same technique as `macula_link_respawn_replay_tests': real station links against an unreachable seed. The pool is
%% handed its node identity key, so no stored identity is read or written.
-module(macula_client_last_disconnect_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SEED, #{host => <<"127.0.0.1">>, port => 1, expected_node_id => <<16#AA:256>>}).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).

last_disconnect_test_() ->
    {foreach, fun pool/0, fun close/1,
     [fun(Pool) -> {"a link that has not gone down reports no last disconnect",
                    fun() -> no_disconnect_yet(Pool) end} end,
      fun(Pool) -> {"a mismatch is kept across the respawn, with both node ids",
                    {timeout, 15, fun() -> mismatch_outlives_the_link(Pool) end}} end,
      fun(Pool) -> {"a link that dies without disconnecting is kept by its exit reason's name",
                    {timeout, 15, fun() -> a_killed_link_is_named(Pool) end}} end]}.

no_disconnect_yet(Pool) ->
    ?assertMatch({ok, [#{last_disconnect := undefined}]}, macula_client:links(Pool)).

mismatch_outlives_the_link(Pool) ->
    {ok, [#{pid := Link}]} = macula_client:links(Pool),
    Peer = fake_peer(Link),
    Link ! {macula_peering, disconnected, Peer,
            {peer_identity_mismatch, #{expected => <<16#AA:256>>, derived => <<16#BB:256>>}}},
    #{last_disconnect := Last} = respawned(Pool, Link),
    ?assertMatch(#{reason := <<"peer_identity_mismatch">>, at_ms := At} when is_integer(At), Last),
    ?assertEqual({hex(<<16#AA:256>>), hex(<<16#BB:256>>)},
                 {maps:get(expected_node_id, Last), maps:get(presented_node_id, Last)}),
    Peer ! stop.

a_killed_link_is_named(Pool) ->
    {ok, [#{pid := Link}]} = macula_client:links(Pool),
    exit(Link, kill),
    #{last_disconnect := Last} = respawned(Pool, Link),
    ?assertEqual(<<"killed">>, maps:get(reason, Last)),
    ?assertEqual([at_ms, reason], lists:sort(maps:keys(Last))).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

pool() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Pool} = macula_client:connect([?SEED], #{node_identity => Key}),
    ok = wait_for_link(Pool, 50),
    Pool.

close(Pool) ->
    unlink(Pool),
    ok = macula_client:close(Pool).

%% A process standing in for the link's peering connection, so the link takes a disconnect notice from it.
fake_peer(Link) ->
    Peer = spawn(fun() -> receive stop -> ok end end),
    _ = sys:replace_state(Link, fun(S) -> setelement(?PEER_PID_INDEX, S, Peer) end),
    Peer.

wait_for_link(_Pool, 0) -> erlang:error(no_link);
wait_for_link(Pool, Left) ->
    linked(macula_client:links(Pool), Pool, Left).

linked({ok, [_]}, _Pool, _Left) -> ok;
linked(_None, Pool, Left) -> timer:sleep(100), wait_for_link(Pool, Left - 1).

%% The pool's view once a new link stands where `Old' was.
respawned(Pool, Old) ->
    respawned(Pool, Old, 60).

respawned(_Pool, _Old, 0) -> erlang:error(link_not_respawned);
respawned(Pool, Old, Left) ->
    replaced(macula_client:links(Pool), Pool, Old, Left).

replaced({ok, [#{pid := Pid} = Info]}, _Pool, Old, _Left) when Pid =/= Old -> Info;
replaced(_Same, Pool, Old, Left) -> timer:sleep(100), respawned(Pool, Old, Left - 1).

hex(Id) -> binary:encode_hex(Id, lowercase).
