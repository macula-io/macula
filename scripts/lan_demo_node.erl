%%%-------------------------------------------------------------------
%%% @doc One macula-net node for the two-netns LAN demo.
%%%
%%% Run via:
%%%   ip netns exec maculaA erl ... -noshell -s lan_demo_node start_a
%%%   ip netns exec maculaB erl ... -noshell -s lan_demo_node start_b
%%%
%%% The node configures `macula_net' with deterministic test
%%% identities, opens a TUN device, peers to the other node over the
%%% veth pair, and idles. ICMPv6 traffic to the peer's macula-net
%%% address routes through the TUN -> QUIC -> peer TUN -> kernel.
%%% @end
%%%-------------------------------------------------------------------
-module(lan_demo_node).

-export([start_a/0, start_b/0]).

%% Same realm pubkey both sides — addresses share the realm /48 prefix.
-define(REALM,    <<16#11:256>>).
-define(IDENT_A,  <<16#aa:256>>).
-define(IDENT_B,  <<16#bb:256>>).

-define(QUIC_PORT, 4400).
-define(TUN,       <<"macula0">>).

start_a() -> start(a).
start_b() -> start(b).

start(Role) ->
    {Self, Peer, PeerHost} = resolve_role(Role),
    SelfAddr = macula_address:derive(?REALM, Self),
    PeerAddr = macula_address:derive(?REALM, Peer),

    SelfFmt = macula_address:format(SelfAddr),
    PeerFmt = macula_address:format(PeerAddr),
    io:format("[~s] self  = ~s~n", [Role, SelfFmt]),
    io:format("[~s] peer  = ~s @ ~s:~p~n",
              [Role, PeerFmt, PeerHost, ?QUIC_PORT]),

    {ok, _} = macula_net:start(#{
        realm_pubkey    => ?REALM,
        identity_pubkey => Self,
        tun_name        => ?TUN,
        tun_mtu         => 1280,
        quic_port       => ?QUIC_PORT,
        peers           => [#{station   => <<"peer">>,
                              host      => PeerHost,
                              port      => ?QUIC_PORT,
                              addresses => [PeerAddr]}]
    }),

    %% Configure the kernel so ICMPv6 actually flows: assign self_addr
    %% to the TUN, mark it up, and add a /48 realm route via TUN so
    %% packets to the peer go through macula-net.
    ok = configure_kernel(SelfFmt, realm_prefix(SelfAddr)),

    io:format("[~s] up; idling.  ping6 from the other netns.~n", [Role]),

    %% Block forever — kill -TERM cleans up.
    timer:sleep(infinity).

%% Peer host is taken from the MACULA_PEER_HOST env var when set
%% (real-LAN demo: each side points at the other box's LAN IP).
%% Falls back to the netns demo's hard-coded veth peer addresses.
resolve_role(Role) ->
    {Self, Peer, DefaultHost} = role_config(Role),
    Host = case os:getenv("MACULA_PEER_HOST") of
        false -> DefaultHost;
        ""    -> DefaultHost;
        V     -> list_to_binary(V)
    end,
    {Self, Peer, Host}.

role_config(a) -> {?IDENT_A, ?IDENT_B, <<"10.99.0.2">>};
role_config(b) -> {?IDENT_B, ?IDENT_A, <<"10.99.0.1">>}.

%% Realm /48 prefix (first 48 bits of address) as text for `ip route'.
realm_prefix(<<P:48, _/bitstring>>) ->
    A = (P bsr 32) band 16#ffff,
    B = (P bsr 16) band 16#ffff,
    C = P band 16#ffff,
    iolist_to_binary(io_lib:format("~4.16.0b:~4.16.0b:~4.16.0b::/48",
                                    [A, B, C])).

configure_kernel(SelfAddrBin, RealmPrefix) ->
    %% These commands must succeed in `ip netns exec ... root' context.
    Steps = [
        {"link up", io_lib:format("ip link set ~s up", [?TUN])},
        {"addr add", io_lib:format("ip -6 addr add ~s/128 dev ~s",
                                    [SelfAddrBin, ?TUN])},
        {"route add", io_lib:format("ip -6 route add ~s dev ~s",
                                     [RealmPrefix, ?TUN])}
    ],
    lists:foreach(fun({Label, Cmd}) ->
        Out = os:cmd(lists:flatten(Cmd) ++ " 2>&1"),
        io:format("[~s] ~s~n", [Label, string:trim(Out)])
    end, Steps),
    ok.
