%%%-------------------------------------------------------------------
%%% @doc macula-net facade.
%%%
%%% Sovereign IPv6 substrate for the macula realm. Crypto-derived
%%% addresses, multi-realm identity, no central allocation authority.
%%% See PLAN_MACULA_NET.md for the full spec.
%%%
%%% This module orchestrates the slices:
%%%
%%% <ul>
%%%   <li>{@link macula_address}             — pubkey -&gt; IPv6</li>
%%%   <li>{@link macula_tun}                 — kernel-side TUN</li>
%%%   <li>{@link macula_net_transport_quic}  — wire transport (Quinn)</li>
%%%   <li>{@link macula_route_packet}        — egress: TUN packet -&gt; station</li>
%%%   <li>{@link macula_deliver_packet}      — ingress: envelope -&gt; TUN</li>
%%% </ul>
%%%
%%% == Lifecycle ==
%%%
%%% ```
%%%   {ok, _} = macula_net:start(#{
%%%       realm_pubkey    => &lt;&lt;...32 bytes...&gt;&gt;,
%%%       identity_pubkey => &lt;&lt;...32 bytes...&gt;&gt;,
%%%       tun_name        => &lt;&lt;"macula0"&gt;&gt;,
%%%       tun_mtu         => 1280,
%%%       quic_port       => 4400,
%%%       peers           => [
%%%           #{station   => &lt;&lt;"beam02"&gt;&gt;,
%%%             host      => "192.168.1.12", port =&gt; 4400,
%%%             addresses => [&lt;&lt;...16 bytes...&gt;&gt;]}
%%%       ]
%%%   }).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_net).

-export([
    start/1,
    stop/0,
    derive_address/2,
    format_address/1,
    own_address/0
]).

-export_type([
    node_config/0,
    peer_spec/0
]).

-type node_config() :: #{
    realm_pubkey    := <<_:256>>,
    identity_pubkey := <<_:256>>,
    tun_name        := binary(),
    tun_mtu         := 1280..65535,
    quic_port       := inet:port_number(),
    peers           := [peer_spec()]
}.

-type peer_spec() :: #{
    station   := binary(),
    host      := binary() | string(),
    port      := inet:port_number(),
    addresses := [<<_:128>>]
}.

-define(OWN_ADDR, macula_net_own_address).

%% =============================================================================
%% Lifecycle
%% =============================================================================

%% @doc Bring up macula-net for this node with the supplied config.
-spec start(node_config()) -> {ok, pid()} | {error, term()}.
start(#{realm_pubkey    := Realm,
        identity_pubkey := Identity,
        tun_name        := TunName,
        tun_mtu         := Mtu,
        quic_port       := QuicPort,
        peers           := Peers}) ->
    OwnAddr = macula_address:derive(Realm, Identity),
    persistent_term:put(?OWN_ADDR, OwnAddr),

    %% 1. QUIC transport listener.
    {ok, _Tr} = ensure_started(macula_net_transport_quic, fun() ->
        macula_net_transport_quic:start_link(#{port => QuicPort})
    end),

    %% 2. TUN device (skipped if no CAP_NET_ADMIN; demo will surface).
    TunHandle = open_tun(TunName, Mtu),

    %% 3. Ingress: which addresses are local + how to write to TUN.
    LocalAddrs = [OwnAddr],
    TunWriter  = make_tun_writer(TunHandle),
    ok = macula_deliver_packet:configure(#{
        local_addresses => LocalAddrs,
        tun_writer      => TunWriter
    }),

    %% 4. Wire the transport's inbound handler to deliver_packet.
    ok = macula_net_transport_quic:set_handler(
            fun macula_deliver_packet:handle_envelope/1),

    %% 5. Connect outbound to each peer + populate egress route table.
    SendFun  = fun(StationId, Cbor) ->
                       macula_net_transport_quic:send(StationId, Cbor)
               end,
    Stations = lists:flatmap(
        fun(#{station := S, host := H, port := P, addresses := As}) ->
            _ = macula_net_transport_quic:connect(S, H, P),  %% best effort
            [#{address => A, station => S, send => SendFun} || A <- As]
        end, Peers),
    ok = macula_route_packet:configure(#{own_address => OwnAddr, stations => Stations}),

    %% 6. Start TUN reader (if we have a TUN). Each packet -> route_packet:dispatch.
    start_tun_reader(TunHandle),

    {ok, self()}.

%% @doc Bring everything down.
-spec stop() -> ok.
stop() ->
    catch macula_net_transport_quic:stop(),
    persistent_term:erase(?OWN_ADDR),
    ok.

%% =============================================================================
%% Convenience helpers
%% =============================================================================

%% @doc Derive a macula-net IPv6 address. See {@link macula_address:derive/2}.
-spec derive_address(<<_:256>>, <<_:256>>) -> <<_:128>>.
derive_address(RealmPk, IdentityPk) ->
    macula_address:derive(RealmPk, IdentityPk).

%% @doc Format a 16-byte IPv6 binary as RFC 5952 lowercase text.
-spec format_address(<<_:128>>) -> binary().
format_address(Addr) ->
    macula_address:format(Addr).

%% @doc Return THIS node's macula-net address (set by {@link start/1}).
-spec own_address() -> <<_:128>> | undefined.
own_address() ->
    persistent_term:get(?OWN_ADDR, undefined).

%% =============================================================================
%% Internals
%% =============================================================================

ensure_started(Name, StartFun) ->
    decide_start(whereis(Name), StartFun).

decide_start(undefined, StartFun) ->
    pid_or_already(StartFun());
decide_start(Pid, _StartFun) ->
    {ok, Pid}.

pid_or_already({ok, Pid})                       -> {ok, Pid};
pid_or_already({error, {already_started, Pid}}) -> {ok, Pid};
pid_or_already(Other)                            -> Other.

%% Try to open the TUN device. If it fails (no CAP_NET_ADMIN, no
%% /dev/net/tun, wrong platform), log and continue — the demo runner
%% will surface the missing device.
open_tun(Name, Mtu) ->
    open_tun_result(macula_tun:open(Name, Mtu), Name, Mtu).

open_tun_result({ok, Handle}, Name, Mtu) ->
    error_logger:info_msg("macula-net: TUN ~s up (mtu ~p)", [Name, Mtu]),
    Handle;
open_tun_result({error, Reason}, _Name, _Mtu) ->
    error_logger:warning_msg(
      "macula-net: TUN open failed: ~p — running without TUN", [Reason]),
    skipped.

make_tun_writer(skipped) ->
    fun(_Pkt) -> {error, no_tun} end;
make_tun_writer(Handle) ->
    fun(Pkt) -> macula_tun:write(Handle, Pkt) end.

start_tun_reader(skipped) ->
    ok;
start_tun_reader(Handle) ->
    ReaderPid = spawn_link(fun reader_proxy_loop/0),
    ok = macula_tun:start_reader(Handle, ReaderPid).

%% Tiny proxy: receives `{macula_net_packet, Handle, Payload}` from
%% the TUN reader thread and forwards each Payload to the egress
%% router. Linked to the caller so it dies with the app.
reader_proxy_loop() ->
    receive
        {macula_net_packet, _Handle, Payload} ->
            _ = macula_route_packet:dispatch(Payload),
            reader_proxy_loop();
        stop ->
            ok
    end.
