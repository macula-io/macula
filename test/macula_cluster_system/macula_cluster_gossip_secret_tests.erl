%%%-------------------------------------------------------------------
%%% @doc Tests for gossip discovery with a required shared secret.
%%%
%%% Gossip refuses to start without a secret of at least 32 bytes, given as
%%% the secret option or in MACULA_GOSSIP_SECRET, and its socket is bound to
%%% the multicast group address, so no unicast packet reaches it. A
%%% macula_cluster start that would use gossip refuses the same way. A packet
%%% counts only when its tag is 64 hex characters that verify against the
%%% secret: a malformed or forged packet is dropped and the server keeps
%%% running. A node name has to have the shape of one before any atom is
%%% made for it, and the atoms the server makes for node names have a cap. A
%%% node is dialled outside the gossip server, so a host that does not answer
%%% leaves the server serving. Scenarios that need a distributed node run in
%%% a peer node of their own.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cluster_gossip_secret_tests).

-include_lib("eunit/include/eunit.hrl").

%% Scenario, run in a peer node.
-export([dial_that_blocks/0, cluster_without_secret/1]).

-define(GROUP, {230, 1, 1, 251}).
%% 32 bytes.
-define(SECRET, <<"0123456789abcdef0123456789abcdef">>).
-define(PREFIX, <<"MACULA_GOSSIP:">>).
%% Atoms one gossip server makes for node names.
-define(CAP, 1024).
%% Names whose atoms exist before the server sees them.
-define(EXISTING, 10).
-define(ANSWER_MS, 200).
%% Longer than ANSWER_MS: how long the peer's test epmd module takes to
%% resolve the slow host.
-define(SLOW_HOST, "slow.invalid").
-define(EVENT_TIMEOUT_MS, 10_000).
-define(SCENARIO_TIMEOUT_MS, 60_000).

start_test_() ->
    {foreach, fun no_server/0, fun stop_server/1,
     [{"gossip does not start without a secret", fun refuses_without_secret/0},
      {"gossip does not start with an empty MACULA_GOSSIP_SECRET", fun refuses_empty_env_secret/0},
      {"gossip does not start with a 31-byte secret option", fun refuses_short_option_secret/0},
      {"gossip does not start with a 31-byte MACULA_GOSSIP_SECRET", fun refuses_short_env_secret/0},
      {"gossip starts with a 32-byte MACULA_GOSSIP_SECRET", fun starts_with_env_secret/0},
      {"the socket is bound to the multicast group address", fun bound_to_the_group/0}]}.

packet_test_() ->
    {foreach, fun server/0, fun stop_server/1,
     [fun(S) -> {"a tag of odd length is dropped", ?_test(odd_length_tag(S))} end,
      fun(S) -> {"a tag of 62 hex characters is dropped", ?_test(short_tag(S))} end,
      fun(S) -> {"a tag of 64 characters that are not hex is dropped", ?_test(non_hex_tag(S))} end,
      fun(S) -> {"a forged tag is dropped", ?_test(forged_tag(S))} end,
      fun(S) -> {"a verified name longer than 255 bytes is dropped", ?_test(overlong_name(S))} end,
      fun(S) -> {"a verified name without the shape of a node name makes no atom",
                 ?_test(name_without_node_shape(S))} end,
      fun(S) -> {"a verified node name is discovered", ?_test(verified_name(S))} end,
      fun(S) -> {"a new name past the atom cap makes no atom",
                 {timeout, 60, ?_test(past_the_cap(S))}} end,
      fun(S) -> {"names whose atoms exist do not count against the atom cap",
                 {timeout, 60, ?_test(existing_atoms_uncounted(S))}} end]}.

start_cluster_test_() ->
    Refused = {ok, {error, {gossip_strategy_failed, secret_required}}},
    [{"start_cluster/0 does not start gossip without a secret",
      {timeout, 60, ?_assertEqual(Refused, in_peer(cluster_without_secret, [start_cluster]))}},
     {"start_cluster/1 with no strategy given does not start gossip without a secret",
      {timeout, 60, ?_assertEqual(Refused, in_peer(cluster_without_secret, [#{}]))}},
     {"start_cluster/1 with the gossip strategy does not start gossip without a secret",
      {timeout, 60, ?_assertEqual(Refused,
                                  in_peer(cluster_without_secret, [#{strategy => gossip}]))}}].

parser_test_() ->
    [{"no packet of random bytes makes the parser raise or verify",
      fun random_packets_are_refused/0},
     {"a verified node name parses to that name", fun verified_name_parses/0},
     {"a verified node name with an IPv6 host parses to that name",
      fun verified_ipv6_name_parses/0},
     {"a verified name that is not a node name is refused",
      fun verified_non_node_names_are_refused/0}].

dial_test_() ->
    {"a dial to a host that does not answer leaves the gossip server serving",
     {timeout, 90, fun dial_does_not_block_the_server/0}}.

%%%===================================================================
%%% Start
%%%===================================================================

refuses_without_secret() ->
    ?assertEqual({error, secret_required},
                 macula_cluster_gossip:start_link(#{port => free_udp_port()})).

refuses_empty_env_secret() ->
    os:putenv("MACULA_GOSSIP_SECRET", ""),
    ?assertEqual({error, secret_required},
                 macula_cluster_gossip:start_link(#{port => free_udp_port()})).

refuses_short_option_secret() ->
    ?assertEqual({error, {secret_too_short, #{bytes => 31, required => 32}}},
                 macula_cluster_gossip:start_link(#{port => free_udp_port(),
                                                    secret => binary:part(?SECRET, 0, 31)})).

refuses_short_env_secret() ->
    os:putenv("MACULA_GOSSIP_SECRET", binary_to_list(binary:part(?SECRET, 0, 31))),
    ?assertEqual({error, {secret_too_short, #{bytes => 31, required => 32}}},
                 macula_cluster_gossip:start_link(#{port => free_udp_port()})).

starts_with_env_secret() ->
    os:putenv("MACULA_GOSSIP_SECRET", binary_to_list(?SECRET)),
    ?assertMatch({ok, _Pid}, macula_cluster_gossip:start_link(#{port => free_udp_port()})).

bound_to_the_group() ->
    Port = free_udp_port(),
    {ok, Pid} = macula_cluster_gossip:start_link(#{port => Port, secret => ?SECRET}),
    ?assertEqual({ok, {?GROUP, Port}}, inet:sockname(server_socket(Pid))).

%%%===================================================================
%%% Packets
%%%===================================================================

odd_length_tag(Server) ->
    ?assertEqual({running, []},
                 deliver(Server, tagged(node_name(), binary:copy(<<"A">>, 63)))).

short_tag(Server) ->
    ?assertEqual({running, []},
                 deliver(Server, tagged(node_name(), binary:copy(<<"AB">>, 31)))).

non_hex_tag(Server) ->
    ?assertEqual({running, []},
                 deliver(Server, tagged(node_name(), binary:copy(<<"Z">>, 64)))).

forged_tag(Server) ->
    ?assertEqual({running, []},
                 deliver(Server, tagged(node_name(), binary:copy(<<"AB">>, 32)))).

overlong_name(Server) ->
    Name = <<(binary:copy(<<"a">>, 300))/binary, "@127.0.0.1">>,
    ?assertEqual({running, []}, deliver(Server, verified(Name))).

name_without_node_shape(Server) ->
    Name = <<"no at sign ", (unique())/binary>>,
    ?assertEqual({{running, []}, no_atom}, {deliver(Server, verified(Name)), atom_made(Name)}).

verified_name(Server) ->
    Name = node_name(),
    ?assertEqual({running, [Name]}, deliver(Server, verified(Name))).

%% CAP new names make CAP atoms, and the name after them makes none.
past_the_cap(Server) ->
    _ = [deliver(Server, verified(node_name())) || _ <- lists:seq(1, ?CAP)],
    Past = node_name(),
    {running, Discovered} = deliver(Server, verified(Past)),
    ?assertEqual({?CAP, no_atom}, {length(Discovered), atom_made(Past)}).

%% Names whose atoms exist are discovered without the server making an atom,
%% so CAP new names after them still make theirs.
existing_atoms_uncounted(Server) ->
    Existing = [node_name() || _ <- lists:seq(1, ?EXISTING)],
    _ = [binary_to_atom(Name) || Name <- Existing],
    _ = [deliver(Server, verified(Name)) || Name <- Existing],
    New = [node_name() || _ <- lists:seq(1, ?CAP)],
    _ = [deliver(Server, verified(Name)) || Name <- New],
    Past = node_name(),
    {running, Discovered} = deliver(Server, verified(Past)),
    ?assertEqual({?EXISTING + ?CAP, atom_made, no_atom},
                 {length(Discovered), atom_made(lists:last(New)), atom_made(Past)}).

%%%===================================================================
%%% Cluster start
%%%===================================================================

%% In a peer node without MACULA_GOSSIP_SECRET: start_cluster/0 when Start is
%% start_cluster, otherwise start_cluster/1 with Start as its options.
cluster_without_secret(Start) ->
    os:unsetenv("MACULA_GOSSIP_SECRET"),
    started_cluster(Start).

started_cluster(start_cluster) -> macula_cluster:start_cluster();
started_cluster(Opts) -> macula_cluster:start_cluster(Opts).

%%%===================================================================
%%% Parser
%%%===================================================================

random_packets_are_refused() ->
    Results = [macula_cluster_gossip:parse_gossip_packet(random_packet(N), ?SECRET)
               || N <- lists:seq(1, 5_000)],
    ?assertEqual([], [R || R <- Results, not refused(R)]).

refused({error, _Reason}) -> true;
refused(_Other) -> false.

%% Random bytes, or random bytes shaped like a packet: the prefix, a name,
%% "|" and a tag of random length.
random_packet(N) when N rem 2 =:= 0 ->
    crypto:strong_rand_bytes(rand:uniform(300) - 1);
random_packet(_N) ->
    <<?PREFIX/binary, (crypto:strong_rand_bytes(rand:uniform(300) - 1))/binary, "|",
      (crypto:strong_rand_bytes(rand:uniform(100) - 1))/binary>>.

verified_name_parses() ->
    Name = node_name(),
    ?assertEqual({ok, Name}, macula_cluster_gossip:parse_gossip_packet(verified(Name), ?SECRET)).

verified_ipv6_name_parses() ->
    Name = <<"peer_", (unique())/binary, "@fd00::1">>,
    ?assertEqual({ok, Name}, macula_cluster_gossip:parse_gossip_packet(verified(Name), ?SECRET)).

verified_non_node_names_are_refused() ->
    ?assertEqual([{error, bad_name}, {error, bad_name}, {error, bad_name}],
                 [macula_cluster_gossip:parse_gossip_packet(verified(Name), ?SECRET)
                  || Name <- [<<"no_host@">>, <<"two@at@signs">>, <<"space in@host">>]]).

%%%===================================================================
%%% Dial
%%%===================================================================

dial_does_not_block_the_server() ->
    ?assertEqual({ok, answered}, in_peer(dial_that_blocks, [])).

%% In a peer node whose epmd module takes longer than ANSWER_MS to resolve
%% SLOW_HOST: gossip discovers a node on that host and dials it, and the
%% gossip server still answers within ANSWER_MS.
dial_that_blocks() ->
    {ok, Pid} = macula_cluster_gossip:start_link(#{port => free_udp_port(), secret => ?SECRET,
                                                   broadcast_interval => 60_000}),
    Server = #{pid => Pid, socket => server_socket(Pid)},
    Pid ! {udp, maps:get(socket, Server), {127, 0, 0, 1}, 45_000,
           verified(<<"far@", ?SLOW_HOST>>)},
    answered(answer_within(fun() -> macula_cluster_gossip:get_discovered(Pid) end, ?ANSWER_MS)).

answered({answered, _Value}) -> answered;
answered(no_answer) -> no_answer.

answer_within(Fun, Ms) ->
    Asker = self(),
    Pid = spawn(fun() -> Asker ! {answer, self(), Fun()} end),
    receive
        {answer, Pid, Value} -> {answered, Value}
    after Ms ->
        exit(Pid, kill),
        no_answer
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

no_server() ->
    process_flag(trap_exit, true),
    os:unsetenv("MACULA_GOSSIP_SECRET"),
    none.

server() ->
    process_flag(trap_exit, true),
    os:unsetenv("MACULA_GOSSIP_SECRET"),
    {ok, Pid} = macula_cluster_gossip:start_link(#{port => free_udp_port(), secret => ?SECRET,
                                                   broadcast_interval => 60_000}),
    #{pid => Pid, socket => server_socket(Pid)}.

stop_server(_Started) ->
    _ = (catch macula_cluster_gossip:stop()),
    os:unsetenv("MACULA_GOSSIP_SECRET"),
    ok.

%% Hands the server a packet as its socket would, then asks it for the
%% discovered nodes, which it answers once it has handled the packet.
deliver(#{pid := Pid, socket := Socket}, Packet) ->
    Pid ! {udp, Socket, {127, 0, 0, 1}, 45_000, Packet},
    discovered(Pid).

discovered(Pid) ->
    try macula_cluster_gossip:get_discovered(Pid) of
        Nodes -> {running, lists:sort([atom_to_binary(N) || N <- Nodes])}
    catch
        exit:Reason -> {crashed, Reason}
    end.

%% The packet a node with the shared secret sends for `Name'.
verified(Name) ->
    Data = <<?PREFIX/binary, Name/binary>>,
    <<Data/binary, "|", (binary:encode_hex(crypto:mac(hmac, sha256, ?SECRET, Data)))/binary>>.

tagged(Name, Tag) ->
    <<?PREFIX/binary, Name/binary, "|", Tag/binary>>.

node_name() ->
    <<"peer_", (unique())/binary, "@127.0.0.1">>.

unique() ->
    integer_to_binary(erlang:unique_integer([positive])).

atom_made(Name) ->
    try binary_to_existing_atom(Name) of
        _Atom -> atom_made
    catch
        error:badarg -> no_atom
    end.

%% The UDP socket the gossip server owns.
server_socket(Pid) ->
    [Socket] = [Port || Port <- erlang:ports(), port_is(Port, Pid, "udp_inet")],
    Socket.

port_is(Port, Pid, Name) ->
    {erlang:port_info(Port, connected), erlang:port_info(Port, name)} =:=
        {{connected, Pid}, {name, Name}}.

free_udp_port() ->
    {ok, Sock} = gen_udp:open(0, [binary]),
    {ok, Port} = inet:port(Sock),
    ok = gen_udp:close(Sock),
    Port.

%%%===================================================================
%%% Peer node
%%%===================================================================

in_peer(Scenario, Args) ->
    Name = "gossip_peer_" ++ integer_to_list(erlang:unique_integer([positive])),
    Started = peer:start_link(#{name => Name,
                                host => "127.0.0.1",
                                longnames => true,
                                connection => standard_io,
                                args => ["-epmd_module", "macula_cluster_gossip_slow_epmd",
                                         "-start_epmd", "false",
                                         "-pa" | code:get_path()]}),
    Peer = element(2, Started),
    OsPid = peer:call(Peer, os, getpid, [], 5_000),
    try peer:call(Peer, ?MODULE, Scenario, Args, ?SCENARIO_TIMEOUT_MS) of
        Result -> {ok, Result}
    catch
        Class:Reason -> {error, {Class, Reason}}
    after
        _ = os:cmd("kill -9 " ++ OsPid),
        try peer:stop(Peer) catch _:_ -> ok end
    end.
