%%%-------------------------------------------------------------------
%%% @doc Phase 3 host-station demo node (netns).
%%%
%%% Like {@link lan_demo_node3} (Phase 2 station) but additionally
%%% wires:
%%%
%%%   * {@link macula_host_identity} — owns the hosted-daemon table,
%%%     publishes hosted_address_map records into the same DHT
%%%     advertise_station already uses.
%%%   * {@link macula_host_attach_controller} — replaces the plain
%%%     deliver_packet handler. Dispatches attach_v1 frames into
%%%     host_identity, forwards data frames whose dst is hosted to the
%%%     correct attach stream, falls through everything else to
%%%     deliver_packet.
%%%
%%% Run via:
%%%   sudo ip netns exec maculaHelsinki erl ... -s lan_demo_host_node start
%%%
%%% The host's pubkey hex is printed at startup so the daemon
%%% (lan_demo_daemon_node) can pick it up via MACULA_HOST_PUBKEY_HEX.
%%% @end
%%%-------------------------------------------------------------------
-module(lan_demo_host_node).

-export([start/0]).

-define(REALM,      <<16#11:256>>).
-define(QUIC_PORT,  4400).
-define(TUN,        <<"macula0">>).

start() ->
    try start_unsafe()
    catch
        Class:Reason:Stack ->
            io:format("[host] FAILED: ~p:~p~nstack=~p~n",
                      [Class, Reason, Stack]),
            erlang:halt(1)
    end.

start_unsafe() ->
    SeedHex  = getenv("MACULA_HOST_SEED_HEX", "bb"),
    Kp       = deterministic_keypair_from_hex(SeedHex),
    Pk       = macula_identity:public(Kp),
    SelfAddr = macula_address:derive(?REALM, Pk),
    SelfFmt  = macula_address:format(SelfAddr),
    SelfIP   = list_to_binary(getenv("MACULA_HOST_VETH_IP", "10.99.0.2")),

    io:format("[host] self      = ~s~n",          [SelfFmt]),
    io:format("[host] pubkey    = ~s~n",          [hexbin(Pk)]),
    io:format("[host] ip        = ~s, quic_port=~p~n", [SelfIP, ?QUIC_PORT]),

    %% Phase 2 plumbing first — same as lan_demo_node3.
    {ok, _} = macula_net_transport_quic:start_link(#{port => ?QUIC_PORT}),
    {ok, _} = macula_cache_route:start_link(#{sweep_ms => 30_000}),

    TunHandle = open_tun_or_die(),
    ok = macula_deliver_packet:configure(#{
        local_addresses => [SelfAddr],
        tun_writer      => fun(Pkt) -> macula_tun:write(TunHandle, Pkt) end
    }),
    start_tun_reader(TunHandle),

    DhtHost = list_to_binary(getenv("MACULA_DHT_HOST", "10.99.0.254")),
    DhtPort = list_to_integer(getenv("MACULA_DHT_PORT", "5555")),
    PutFn   = make_put_fn(DhtHost, DhtPort),
    FindFn  = make_find_fn(DhtHost, DhtPort),

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

    ok = macula_route_packet:configure(#{
        own_address => SelfAddr,
        resolver    => #{realm_pubkey => ?REALM,
                         find_fn      => FindFn,
                         connect_fn   => fun connect_fn/3,
                         send_fn      => fun macula_net_transport_quic:send/2}
    }),

    ok = configure_kernel(SelfFmt, realm_prefix(SelfAddr)),

    %% Phase 3 add-ons.
    {ok, _} = macula_host_identity:start_link(#{
        realm_pubkey => ?REALM,
        host_pubkey  => Pk,
        host_privkey => Kp,
        put_fn       => PutFn,
        refresh_ms   => 60_000
    }),
    {ok, _} = macula_host_attach_controller:start_link(#{
        realm_pubkey   => ?REALM,
        host_pubkey    => Pk,
        attach_send_fn => fun macula_quic:send/2,
        lookup_fn      => fun macula_host_identity:lookup/1,
        attach_fn      => fun macula_host_identity:attach/4,
        fallback_fn    => fun macula_deliver_packet:handle_envelope/1,
        forward_fn     => fun macula_route_packet:dispatch_envelope/2
    }),

    %% Replace the Phase-2 default handler with the controller — only
    %% AFTER host_identity + controller are up so no inbound frame can
    %% race in front of them.
    ok = macula_net_transport_quic:set_handler(
            fun macula_host_attach_controller:handle/2),

    %% Phase 4.1 — observability. Start metrics + Prometheus endpoint
    %% if MACULA_METRICS_PORT is set in the environment. Bind to the
    %% netns veth IP so curls from the root namespace can scrape it.
    maybe_start_metrics(SelfIP),

    io:format("[host] up; idling.~n"),
    timer:sleep(infinity).

maybe_start_metrics(SelfIP) ->
    case getenv("MACULA_METRICS_PORT", "") of
        "" -> ok;
        PortStr ->
            Port = list_to_integer(PortStr),
            {ok, _} = application:ensure_all_started(telemetry),
            {ok, _} = application:ensure_all_started(inets),
            {ok, _} = macula_metrics:start_link(
                        #{install_default_gauges => true}),
            BindStr = binary_to_list(SelfIP),
            {ok, BindIp} = inet:parse_address(BindStr),
            {ok, _} = macula_metrics_http:start_link(
                        #{port => Port, bind => BindIp}),
            io:format("[host] metrics  = http://~s:~p/metrics~n",
                      [SelfIP, Port])
    end.

%% =============================================================================
%% DHT client (matches lan_demo_dht wire format)
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
    after_connect(gen_tcp:connect(HostStr, Port,
                                   [binary, {packet, 0}, {active, false}],
                                   3_000),
                   Term).

after_connect({error, _} = E, _Term) -> E;
after_connect({ok, Sock}, Term) ->
    Reply = call_once(Sock, Term),
    gen_tcp:close(Sock),
    Reply.

call_once(Sock, Term) ->
    Bin = term_to_binary(Term),
    ok = gen_tcp:send(Sock, <<(byte_size(Bin)):32/big, Bin/binary>>),
    after_recv(gen_tcp:recv(Sock, 4), Sock).

after_recv({error, _} = E, _Sock) -> E;
after_recv({ok, <<Len:32/big>>}, Sock) ->
    after_recv_body(gen_tcp:recv(Sock, Len)).

after_recv_body({error, _} = E)    -> E;
after_recv_body({ok, RBin})        -> binary_to_term(RBin).

%% =============================================================================
%% Connect helper (idempotent)
%% =============================================================================

connect_fn(StationId, Host, Port) ->
    handle_connect(macula_net_transport_quic:connect(StationId, Host, Port)).

handle_connect(ok)                          -> ok;
handle_connect({error, already_connected})  -> ok;
handle_connect({error, _} = E)              -> E.

%% =============================================================================
%% TUN + kernel
%% =============================================================================

open_tun_or_die() ->
    after_tun_open(macula_tun:open(?TUN, 1280)).

after_tun_open({ok, H})        -> H;
after_tun_open({error, R})     ->
    io:format("FATAL: TUN open failed: ~p~n", [R]),
    erlang:halt(1).

start_tun_reader(Handle) ->
    {ok, ReaderPid} = macula_tun_reader_proxy:start_link(#{
        dispatch_fn => fun macula_route_packet:dispatch/1
    }),
    ok = macula_tun:start_reader(Handle, ReaderPid).

configure_kernel(SelfAddrBin, RealmPrefix) ->
    Steps = [
        {"link up",  io_lib:format("ip link set ~s up", [?TUN])},
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
%% Identity + misc
%% =============================================================================

deterministic_keypair_from_hex(Hex) when is_list(Hex) ->
    Seed = decode_seed(Hex),
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519, Seed),
    #{public => iolist_to_binary(Pub), private => iolist_to_binary(Priv)}.

%% Pad a short hex string to a 32-byte seed by treating the hex value
%% as the lowest byte and zero-padding the high bytes. Mirrors the
%% bash demo's <<16#bb:256>> construction.
decode_seed(Hex) when is_list(Hex) ->
    Cleaned = case Hex of
        "0x" ++ Rest -> Rest;
        _            -> Hex
    end,
    decode_seed_padded(length(Cleaned), Cleaned).

decode_seed_padded(64, Hex) ->
    binary:decode_hex(list_to_binary(Hex));
decode_seed_padded(N, Hex) when N > 0, N =< 64 ->
    Padded = string:right(Hex, 64, $0),
    binary:decode_hex(list_to_binary(Padded));
decode_seed_padded(_, _) ->
    erlang:halt(1).

hexbin(Bin) ->
    binary:encode_hex(Bin, lowercase).

getenv(Key, Default) ->
    case os:getenv(Key) of
        false -> Default;
        ""    -> Default;
        V     -> V
    end.
