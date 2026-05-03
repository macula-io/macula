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
    %% Identity seed is configurable so the same module supports
    %% multiple daemon roles in a single demo (Alice + Bob via
    %% MACULA_DAEMON_SEED_HEX).
    SeedHex = getenv("MACULA_DAEMON_SEED_HEX", "dd"),
    Kp       = deterministic_keypair_from_hex(SeedHex),
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
    InboundHandler = fun(Cbor) -> log_envelope(Cbor) end,

    {ok, Handle} = macula_attach_identity:attach(
                       HostEndpoint, ?REALM, Kp,
                       #{inbound_handler => InboundHandler}),

    %% Optional peer send: if MACULA_DAEMON_PEER_SEED_HEX is set, the
    %% daemon constructs an envelope dst = derive(realm, peer_pubkey)
    %% and sends it via attach_identity. The send waits for the first
    %% refresh_ms tick of the host so Alice's hosted record is in the
    %% DHT by the time the peer's host station tries to forward.
    maybe_send_to_peer(Handle, SelfAddr,
                        getenv("MACULA_DAEMON_PEER_SEED_HEX", undefined),
                        list_to_integer(getenv("MACULA_DAEMON_SEND_DELAY_MS",
                                                "1000"))),

    io:format("[daemon] up; idling.~n"),
    timer:sleep(infinity).

maybe_send_to_peer(_Handle, _SelfAddr, undefined, _DelayMs) ->
    ok;
maybe_send_to_peer(Handle, SelfAddr, PeerSeedHex, DelayMs) ->
    PeerKp = deterministic_keypair_from_hex(PeerSeedHex),
    PeerPk = macula_identity:public(PeerKp),
    PeerAddr = macula_address:derive(?REALM, PeerPk),
    PayloadSize = list_to_integer(getenv("MACULA_DAEMON_PAYLOAD_BYTES", "22")),
    Payload = case PayloadSize of
        22 -> <<"phase 3.7+ alice<->bob">>;
        N  -> crypto:strong_rand_bytes(N)
    end,
    LoopMs = list_to_integer(getenv("MACULA_DAEMON_LOOP_MS", "0")),
    Send = fun() ->
        Envelope = macula_cbor_nif:pack(#{
            <<"v">>       => 1,
            <<"type">>    => <<"data">>,
            <<"src">>     => SelfAddr,
            <<"dst">>     => PeerAddr,
            <<"payload">> => Payload
        }),
        Result = macula_attach_identity:send(Handle, Envelope),
        Sha = binary:part(crypto:hash(sha256, Payload), 0, 8),
        io:format("[daemon] sent to ~s: ~p byte(s) sha8=~s result=~p~n",
                  [macula_address:format(PeerAddr),
                   byte_size(Payload),
                   bin_to_hex(Sha),
                   Result])
    end,
    spawn(fun() ->
        timer:sleep(DelayMs),
        Send(),
        soak_loop(Send, LoopMs)
    end),
    ok.

%% Phase 4.7 — soak workload generator. When MACULA_DAEMON_LOOP_MS > 0
%% the daemon keeps sending after the initial one-shot, so the soak
%% harness has continuous traffic to observe.
soak_loop(_Send, 0) -> ok;
soak_loop(Send, LoopMs) ->
    timer:sleep(LoopMs),
    Send(),
    soak_loop(Send, LoopMs).

bin_to_hex(Bin) ->
    list_to_binary([io_lib:format("~2.16.0b", [B]) || <<B>> <= Bin]).

log_envelope(Cbor) ->
    case macula_cbor_nif:unpack(Cbor) of
        {ok, #{<<"type">> := <<"data">>,
               <<"src">>  := Src,
               <<"dst">>  := Dst,
               <<"payload">> := P}} ->
            Sha = binary:part(crypto:hash(sha256, P), 0, 8),
            io:format("[daemon] received ~s -> ~s, ~p byte(s) sha8=~s~n",
                      [macula_address:format(Src),
                       macula_address:format(Dst),
                       byte_size(P),
                       bin_to_hex(Sha)]);
        {ok, Other} ->
            io:format("[daemon] received non-data: ~p~n", [Other]);
        {error, R} ->
            io:format("[daemon] received undecodable cbor: ~p~n", [R])
    end,
    ok.

%% =============================================================================
%% Identity + env helpers
%% =============================================================================

deterministic_keypair_from_hex(Hex) when is_list(Hex) ->
    Seed = decode_seed(Hex),
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519, Seed),
    #{public => iolist_to_binary(Pub), private => iolist_to_binary(Priv)}.

%% Pad a short hex string (e.g. "dd") to a 32-byte seed by treating
%% the hex value as the lowest byte and zero-padding the high bytes.
%% Matches the bash demo's <<16#dd:256>> construction.
decode_seed(Hex) when is_list(Hex) ->
    %% Strip optional 0x prefix.
    Cleaned = case Hex of
        "0x" ++ Rest -> Rest;
        _ -> Hex
    end,
    case length(Cleaned) of
        64 -> binary:decode_hex(list_to_binary(Cleaned));
        N when N > 0, N =< 64 ->
            %% Right-align: <<16#dd:256>> = 31 zero bytes then 0xdd.
            Padded = string:right(Cleaned, 64, $0),
            binary:decode_hex(list_to_binary(Padded));
        _ ->
            erlang:halt(1)
    end.

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
