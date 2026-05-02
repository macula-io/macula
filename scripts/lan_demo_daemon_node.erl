%%%-------------------------------------------------------------------
%%% @doc Phase 3 daemon-side demo node (netns).
%%%
%%% Runs in its own netns with no TUN device. Brings up the QUIC
%%% transport, dials the configured host station, and attaches via
%%% {@link macula_attach_identity}. Inbound macula-net envelopes
%%% forwarded by the host land in this module's logger so the demo
%%% script can grep for "[daemon] received" as the acceptance signal.
%%%
%%% Run via:
%%%   sudo ip netns exec maculaAlice erl ... -s lan_demo_daemon_node start
%%%
%%% Identity is derived deterministically from a fixed seed so the
%%% demo's expected address is reproducible.
%%% @end
%%%-------------------------------------------------------------------
-module(lan_demo_daemon_node).

-export([start/0]).

-define(REALM,        <<16#11:256>>).
-define(IDENT_DAEMON, <<16#dd:256>>).
-define(QUIC_PORT,    4400).

start() ->
    try start_unsafe()
    catch
        Class:Reason:Stack ->
            io:format("[daemon] FAILED: ~p:~p~nstack=~p~n",
                      [Class, Reason, Stack]),
            erlang:halt(1)
    end.

start_unsafe() ->
    Kp       = deterministic_keypair(),
    Pk       = macula_identity:public(Kp),
    SelfAddr = macula_address:derive(?REALM, Pk),
    SelfFmt  = macula_address:format(SelfAddr),

    HostHost = list_to_binary(getenv("MACULA_HOST_HOST", "10.99.0.2")),
    HostPortStr = getenv("MACULA_HOST_PORT", integer_to_list(?QUIC_PORT)),
    HostPort    = list_to_integer(HostPortStr),
    HostPubHex  = getenv("MACULA_HOST_PUBKEY_HEX", undefined),
    HostPk      = decode_host_pubkey(HostPubHex),

    io:format("[daemon] self      = ~s~n",          [SelfFmt]),
    io:format("[daemon] host      = ~s:~p~n",       [HostHost, HostPort]),

    %% Local QUIC listener — required by the existing transport gen_server
    %% even on the daemon side; outbound-only would also work but the
    %% transport API insists on a port.
    {ok, _} = macula_net_transport_quic:start_link(#{port => 0}),

    HostEndpoint = #{
        station_pubkey => HostPk,
        host           => binary_to_list(HostHost),
        port           => HostPort
    },
    InboundHandler = fun(Cbor) ->
        %% Decode for human-friendly log output. Acceptance line:
        %%   [daemon] received <src>->" <dst>" <len> bytes
        log_envelope(Cbor)
    end,

    {ok, _Handle} = macula_attach_identity:attach(
                       HostEndpoint, ?REALM, Kp,
                       #{inbound_handler => InboundHandler}),

    io:format("[daemon] up; idling.~n"),
    timer:sleep(infinity).

log_envelope(Cbor) ->
    case macula_cbor_nif:unpack(Cbor) of
        {ok, #{<<"type">> := <<"data">>,
               <<"src">>  := Src,
               <<"dst">>  := Dst,
               <<"payload">> := P}} ->
            io:format("[daemon] received ~s -> ~s, ~p byte(s)~n",
                      [macula_address:format(Src),
                       macula_address:format(Dst),
                       byte_size(P)]);
        {ok, Other} ->
            io:format("[daemon] received non-data: ~p~n", [Other]);
        {error, R} ->
            io:format("[daemon] received undecodable cbor: ~p~n", [R])
    end,
    ok.

%% =============================================================================
%% Identity + env helpers
%% =============================================================================

deterministic_keypair() ->
    Seed = ?IDENT_DAEMON,
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519, Seed),
    #{public => iolist_to_binary(Pub), private => iolist_to_binary(Priv)}.

decode_host_pubkey(undefined) ->
    io:format("FATAL: MACULA_HOST_PUBKEY_HEX not set~n"),
    erlang:halt(1);
decode_host_pubkey(Hex) when is_list(Hex) ->
    %% 64 hex chars = 32-byte pubkey.
    binary:decode_hex(list_to_binary(Hex)).

getenv(Key, Default) ->
    case os:getenv(Key) of
        false -> Default;
        ""    -> Default;
        V     -> V
    end.
