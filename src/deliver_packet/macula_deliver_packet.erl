%%%-------------------------------------------------------------------
%%% @doc Ingress delivery for macula-net packets.
%%%
%%% Receives a CBOR-encoded macula-net envelope (typically from
%%% macula_net_transport_quic), decodes it via {@link macula_cbor_nif},
%%% validates against spec, and writes the inner IPv6 packet to the
%%% local TUN device when the destination is one of the locally-hosted
%%% addresses.
%%%
%%% Phase 1 simplification (PLAN_MACULA_NET.md §5.2): single-hop only.
%%% This station receives a packet whose `dst' is expected to be local;
%%% if not, we drop and log. Multi-hop forwarding (decrement TTL,
%%% re-route) lands in Phase 2 alongside the DHT.
%%%
%%% Decoupled from {@link macula_tun} via a TunWriter callback so this
%%% slice has no compile-time dependency on the kernel-side NIF.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_deliver_packet).

-export([
    configure/1,
    handle_envelope/1
]).

-export_type([
    tun_writer/0,
    config/0
]).

-type tun_writer() :: fun((IPv6Packet :: binary()) -> ok | {error, term()}).

-type config() :: #{
    local_addresses := [<<_:128>>],
    tun_writer      := tun_writer()
}.

-define(TABLE,            macula_deliver_packet_table).
-define(SUPPORTED_VSN,    1).
-define(LOCAL_ADDRS_KEY,  local_addresses).
-define(TUN_WRITER_KEY,   tun_writer).

%% =============================================================================
%% Public API
%% =============================================================================

%% @doc Install the local-addresses set + TUN writer callback.
%% Calling configure/1 again replaces the previous config.
-spec configure(config()) -> ok.
configure(#{local_addresses := Addrs, tun_writer := Writer})
  when is_list(Addrs), is_function(Writer, 1) ->
    ensure_table(),
    true = ets:insert(?TABLE, {?LOCAL_ADDRS_KEY, sets:from_list(Addrs)}),
    true = ets:insert(?TABLE, {?TUN_WRITER_KEY, Writer}),
    ok.

%% @doc Handle a single inbound CBOR-encoded macula-net envelope.
%%
%% Returns:
%% <ul>
%%   <li>`ok' — payload delivered to local TUN, OR a non-data control
%%       message accepted but not handled (Phase 1 stub for ctrl/gossip)</li>
%%   <li>`{error, version_unsupported}' — packet's `v' field unknown</li>
%%   <li>`{error, decode_failed}' — CBOR malformed / missing fields</li>
%%   <li>`{error, no_route}' — `dst' not in local-addresses set</li>
%%   <li>`{error, not_configured}' — configure/1 not called yet</li>
%%   <li>`{error, tun_write_failed}' — kernel write returned an error</li>
%% </ul>
-spec handle_envelope(binary()) ->
    ok | {error, version_unsupported | decode_failed | no_route
                | not_configured | tun_write_failed | term()}.
handle_envelope(Cbor) when is_binary(Cbor) ->
    handle_with_config(lookup_config(), Cbor).

%% =============================================================================
%% Internals
%% =============================================================================

handle_with_config({error, _} = Err, _Cbor) ->
    Err;
handle_with_config({ok, LocalAddrs, Writer}, Cbor) ->
    handle_decoded(macula_cbor_nif:unpack(Cbor), LocalAddrs, Writer).

handle_decoded({error, _Reason}, _LocalAddrs, _Writer) ->
    {error, decode_failed};
handle_decoded({ok, #{<<"v">> := V}}, _LocalAddrs, _Writer)
  when V =/= ?SUPPORTED_VSN ->
    {error, version_unsupported};
handle_decoded({ok, #{<<"type">> := <<"data">>,
                      <<"dst">>  := Dst,
                      <<"payload">> := Payload}}, LocalAddrs, Writer) ->
    deliver_local(sets:is_element(Dst, LocalAddrs), Payload, Writer);
handle_decoded({ok, #{<<"type">> := _Other}}, _LocalAddrs, _Writer) ->
    %% ctrl/gossip handlers land in Phase 1.5+. Treat as
    %% unhandled-but-not-error for now.
    ok;
handle_decoded({ok, _Other}, _LocalAddrs, _Writer) ->
    {error, decode_failed}.

deliver_local(false, _Payload, _Writer) ->
    {error, no_route};
deliver_local(true, Payload, Writer) ->
    deliver_via(Writer(Payload)).

deliver_via(ok)            -> ok;
deliver_via({error, _})    -> {error, tun_write_failed}.

lookup_config() ->
    case ets:info(?TABLE) of
        undefined -> {error, not_configured};
        _ ->
            case {ets:lookup(?TABLE, ?LOCAL_ADDRS_KEY),
                  ets:lookup(?TABLE, ?TUN_WRITER_KEY)} of
                {[{_, Addrs}], [{_, Writer}]} ->
                    {ok, Addrs, Writer};
                _ ->
                    {error, not_configured}
            end
    end.

ensure_table() ->
    case ets:info(?TABLE) of
        undefined ->
            _ = ets:new(?TABLE, [named_table, public, set,
                                 {read_concurrency, true}]),
            ok;
        _ ->
            ok
    end.
