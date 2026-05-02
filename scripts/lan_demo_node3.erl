%%%-------------------------------------------------------------------
%%% @doc Phase 2 macula-net demo node — three-station netns mesh.
%%%
%%% Wires the Phase 2 slices (advertise_station + resolve_address +
%%% cache_route + route_packet:dht-mode) directly. Each station opens
%%% a TUN, advertises its endpoint into the shared mock DHT
%%% ({@link lan_demo_dht}), and dispatches outbound packets through
%%% the DHT-resolved path. ICMPv6 and TCP (curl) work end-to-end.
%%%
%%% Run via:
%%%   sudo ip netns exec maculaA erl ... -s lan_demo_node3 start_a
%%%
%%% The DHT's host/port comes from MACULA_DHT_HOST + MACULA_DHT_PORT
%%% env vars; defaults to 10.99.0.254:5555 (the bridge gateway in
%%% scripts/netns3-demo.sh).
%%% @end
%%%-------------------------------------------------------------------
-module(lan_demo_node3).

-export([start_a/0, start_b/0, start_c/0]).

-define(REALM, <<16#11:256>>).
-define(IDENT_A, <<16#aa:256>>).
-define(IDENT_B, <<16#bb:256>>).
-define(IDENT_C, <<16#cc:256>>).
-define(QUIC_PORT, 4400).
-define(TUN, <<"macula0">>).

start_a() -> start(a).
start_b() -> start(b).
start_c() -> start(c).

start(Role) ->
    try start_unsafe(Role)
    catch
        Class:Reason:Stack ->
            io:format("[~s] FAILED: ~p:~p~nstack=~p~n",
                      [Role, Class, Reason, Stack]),
            erlang:halt(1)
    end.

start_unsafe(Role) ->
    %% Note: we use deterministic identity privkeys derived from a
    %% fixed seed so the demo is reproducible. Real stations generate
    %% with macula_identity:generate/0.
    Kp = deterministic_keypair(Role),
    Pk = macula_identity:public(Kp),
    SelfAddr = macula_address:derive(?REALM, Pk),
    SelfFmt  = macula_address:format(SelfAddr),
    SelfIP   = veth_ip(Role),

    io:format("[~s] self  = ~s~n", [Role, SelfFmt]),
    io:format("[~s] ip    = ~s, quic_port=~p~n", [Role, SelfIP, ?QUIC_PORT]),

    %% Phase 2 wiring (no static peers; pure DHT).
    io:format("[~s] starting transport_quic...~n", [Role]),
    {ok, _} = macula_net_transport_quic:start_link(#{port => ?QUIC_PORT}),
    ok = macula_net_transport_quic:set_handler(
            fun(Cbor, _StreamRef) ->
                macula_deliver_packet:handle_envelope(Cbor)
            end),
    io:format("[~s] starting cache_route...~n", [Role]),
    {ok, _} = macula_cache_route:start_link(#{sweep_ms => 30_000}),

    %% TUN comes up before deliver_packet so we have a writer.
    TunHandle = open_tun_or_die(),
    ok = macula_deliver_packet:configure(#{
        local_addresses => [SelfAddr],
        tun_writer      => fun(Pkt) -> macula_tun:write(TunHandle, Pkt) end
    }),
    start_tun_reader(TunHandle),

    %% DHT client functions.
    DhtHost = list_to_binary(getenv("MACULA_DHT_HOST", "10.99.0.254")),
    DhtPort = list_to_integer(getenv("MACULA_DHT_PORT", "5555")),
    PutFn  = make_put_fn(DhtHost, DhtPort),
    FindFn = make_find_fn(DhtHost, DhtPort),

    io:format("[~s] starting advertise_station...~n", [Role]),
    {ok, _} = macula_advertise_station:start_link(#{
        realm_pubkey     => ?REALM,
        identity_pubkey  => Pk,
        identity_privkey => Kp,
        quic_port        => ?QUIC_PORT,
        addresses        => [SelfAddr],
        advertised_ips   => [SelfIP],
        put_fn           => PutFn,
        refresh_ms       => 60_000
    }),

    ConnectFn = fun(StationId, Host, Port) ->
        case macula_net_transport_quic:connect(StationId, Host, Port) of
            ok -> ok;
            {error, already_connected} -> ok;
            {error, _} = E -> E
        end
    end,
    SendFn = fun(StationId, Cbor) ->
        macula_net_transport_quic:send(StationId, Cbor)
    end,
    ok = macula_route_packet:configure(#{
        own_address => SelfAddr,
        resolver    => #{realm_pubkey => ?REALM,
                         find_fn      => FindFn,
                         connect_fn   => ConnectFn,
                         send_fn      => SendFn}
    }),

    %% Kernel: address + realm route.
    ok = configure_kernel(SelfFmt, realm_prefix(SelfAddr)),

    io:format("[~s] up; idling.~n", [Role]),
    timer:sleep(infinity).

%% =============================================================================
%% DHT client over gen_tcp (matches scripts/lan_demo_dht.erl wire format)
%% =============================================================================

make_put_fn(Host, Port) ->
    fun(Record) -> dht_call(Host, Port, {put, Record}) end.

make_find_fn(Host, Port) ->
    fun(Key) -> dht_call(Host, Port, {find, Key}) end.

dht_call(Host, Port, Term) ->
    HostStr = case Host of
        H when is_binary(H) -> binary_to_list(H);
        H -> H
    end,
    case gen_tcp:connect(HostStr, Port, [binary, {packet, 0}, {active, false}], 3_000) of
        {ok, Sock} ->
            Reply = call_once(Sock, Term),
            gen_tcp:close(Sock),
            Reply;
        {error, _} = E -> E
    end.

call_once(Sock, Term) ->
    Bin = term_to_binary(Term),
    ok = gen_tcp:send(Sock, <<(byte_size(Bin)):32/big, Bin/binary>>),
    case gen_tcp:recv(Sock, 4) of
        {ok, <<Len:32/big>>} ->
            case gen_tcp:recv(Sock, Len) of
                {ok, RBin} -> binary_to_term(RBin);
                {error, _} = E -> E
            end;
        {error, _} = E -> E
    end.

%% =============================================================================
%% TUN + kernel
%% =============================================================================

open_tun_or_die() ->
    case macula_tun:open(?TUN, 1280) of
        {ok, H} -> H;
        {error, R} ->
            io:format("FATAL: TUN open failed: ~p~n", [R]),
            erlang:halt(1)
    end.

start_tun_reader(Handle) ->
    {ok, ReaderPid} = macula_tun_reader_proxy:start_link(#{
        dispatch_fn => fun macula_route_packet:dispatch/1
    }),
    ok = macula_tun:start_reader(Handle, ReaderPid).

configure_kernel(SelfAddrBin, RealmPrefix) ->
    %% `nodad' bypasses Duplicate Address Detection so user-space can
    %% bind to the address immediately. DAD on a TUN with no L2 peer
    %% just adds 1+ seconds of "tentative" state with no upside.
    Steps = [
        {"link up", io_lib:format("ip link set ~s up", [?TUN])},
        {"addr add", io_lib:format("ip -6 addr add ~s/128 dev ~s nodad",
                                    [SelfAddrBin, ?TUN])},
        {"route add", io_lib:format("ip -6 route add ~s dev ~s",
                                     [RealmPrefix, ?TUN])}
    ],
    lists:foreach(fun({Label, Cmd}) ->
        Out = os:cmd(lists:flatten(Cmd) ++ " 2>&1"),
        io:format("[~s] ~s~n", [Label, string:trim(Out)])
    end, Steps),
    ok.

realm_prefix(<<P:48, _/bitstring>>) ->
    A = (P bsr 32) band 16#ffff,
    B = (P bsr 16) band 16#ffff,
    C = P band 16#ffff,
    iolist_to_binary(io_lib:format("~4.16.0b:~4.16.0b:~4.16.0b::/48",
                                    [A, B, C])).

%% =============================================================================
%% Identities + IPs (deterministic)
%% =============================================================================

%% Build a deterministic Ed25519 keypair from a fixed 32-byte seed
%% (the role-tagged ?IDENT_X). Real stations should call
%% macula_identity:generate/0.
deterministic_keypair(Role) ->
    Seed = role_seed(Role),
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519, Seed),
    #{public => iolist_to_binary(Pub), private => iolist_to_binary(Priv)}.

role_seed(a) -> ?IDENT_A;
role_seed(b) -> ?IDENT_B;
role_seed(c) -> ?IDENT_C.

veth_ip(a) -> <<"10.99.0.1">>;
veth_ip(b) -> <<"10.99.0.2">>;
veth_ip(c) -> <<"10.99.0.3">>.

getenv(Key, Default) ->
    case os:getenv(Key) of
        false -> Default;
        ""    -> Default;
        V     -> V
    end.
