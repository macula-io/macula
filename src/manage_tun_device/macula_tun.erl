%%%-------------------------------------------------------------------
%%% @doc TUN device lifecycle for macula-net.
%%%
%%% High-level facade over {@link macula_tun_nif}. The NIF owns all
%%% kernel-side syscalls (open / write / reader-thread / close); this
%%% module keeps the BEAM-side API stable and ergonomic.
%%%
%%% == Usage ==
%%%
%%% ```
%%% {ok, Tun} = macula_tun:open(<<"macula0">>, 1280),
%%% ok        = macula_tun:start_reader(Tun, self()),
%%% ok        = macula_tun:write(Tun, IPv6Packet),
%%% ...
%%% ok        = macula_tun:close(Tun).
%%% '''
%%%
%%% After {@link start_reader/2}, the calling process (or the supplied
%%% Pid) receives messages of shape `{macula_net_packet, Handle, Payload}'
%%% for every packet read from the TUN.
%%%
%%% Requires `CAP_NET_ADMIN'. Without it, {@link open/2} returns
%%% `{error, ...}' and the operator should consult the runbook
%%% (PLAN_MACULA_NET.md §10.3).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_tun).

-export([
    open/2,
    name/1,
    start_reader/2,
    write/2,
    close/1
]).

-export_type([
    handle/0,
    if_name/0,
    mtu/0,
    ipv6_packet/0
]).

-opaque handle()      :: reference().
-type   if_name()     :: binary().     %% e.g. <<"macula0">>
-type   mtu()         :: 1280..65535.
-type   ipv6_packet() :: binary().

%% =============================================================================
%% Public API (thin pass-through to macula_tun_nif:nif_*)
%% =============================================================================

%% @doc Create a TUN device. Kernel may pick a different actual name on
%% conflict; use {@link name/1} to confirm.
-spec open(if_name(), mtu()) -> {ok, handle()} | {error, term()}.
open(IfName, Mtu) ->
    macula_tun_nif:nif_open(IfName, Mtu).

%% @doc Return the actual interface name assigned by the kernel.
-spec name(handle()) -> {ok, binary()} | {error, term()}.
name(Handle) ->
    macula_tun_nif:nif_name(Handle).

%% @doc Spawn the reader thread. Subsequent packets read from the TUN
%% are delivered to `Pid' as `{macula_net_packet, Handle, Payload}'.
-spec start_reader(handle(), pid()) -> ok | {error, term()}.
start_reader(Handle, Pid) ->
    macula_tun_nif:nif_start_reader(Handle, Pid).

%% @doc Write a complete IPv6 packet to the TUN device.
-spec write(handle(), ipv6_packet()) -> ok | {error, term()}.
write(Handle, Packet) ->
    macula_tun_nif:nif_write(Handle, Packet).

%% @doc Close the TUN device. Idempotent; the device is also closed
%% automatically when BEAM GCs the handle.
-spec close(handle()) -> ok | {error, term()}.
close(Handle) ->
    macula_tun_nif:nif_close(Handle).
