%%%-------------------------------------------------------------------
%%% @doc Two-node end-to-end test for the macula-net QUIC transport.
%%%
%%% Spawns a peer BEAM node, starts the QUIC transport on both, opens
%%% a connection from the test node to the peer, and verifies that
%%% CBOR envelopes flow over the wire to the peer's registered handler.
%%%
%%% This is the validation gate for Phase 1.6 LAN demo: if envelopes
%%% don't flow here, they won't flow on the LAN either.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_net_transport_quic_e2e_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LISTEN_PORT_A, 44400).
-define(LISTEN_PORT_B, 44401).
-define(STATION_B,    <<"peer-station">>).
-define(WAIT_MS,      2000).
-define(ENVELOPE_SINK, macula_net_test_envelope_sink).

%% =============================================================================
%% Top-level eunit fixture
%% =============================================================================

quic_envelope_e2e_test_() ->
    {timeout, 30,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun run_envelope_roundtrip/1}}.

%% =============================================================================
%% Setup / cleanup
%% =============================================================================

setup() ->
    %% Start distribution on the test node so peer:start_link works.
    ensure_dist(),
    {ok, PeerPid, PeerNode} = start_peer_node(),
    ok = preload_code_on_peer(PeerNode),
    TestNode = node(),
    %% Spawn a long-lived holder process on the peer node that starts
    %% the QUIC transport. The handler sends to a registered name on
    %% the test node — eunit fixtures run setup and the test body in
    %% different processes, so capturing self() in setup is wrong.
    {ok, Holder} = peer_call(PeerNode, fun() ->
        Caller = self(),
        H = spawn(fun() ->
            process_flag(trap_exit, true),
            {ok, _} = macula_net_transport_quic:start_link(
                        #{port => ?LISTEN_PORT_B}),
            Sink = {?ENVELOPE_SINK, TestNode},
            ok = macula_net_transport_quic:set_handler(
                    fun(Cbor, _StreamRef) -> Sink ! {peer_got, Cbor}, ok end),
            Caller ! {ready, self()},
            holder_loop()
        end),
        receive {ready, H} -> {ok, H} after 5000 -> {error, holder_timeout} end
    end),
    %% On test node: start a listener too (the transport mandates one
    %% to set up its own listening endpoint, even though we only
    %% initiate outbound here).
    {ok, _} = macula_net_transport_quic:start_link(#{port => ?LISTEN_PORT_A}),
    #{peer => PeerPid, peer_node => PeerNode, holder => Holder}.

cleanup(#{peer := PeerPid}) ->
    catch macula_net_transport_quic:stop(),
    catch peer:stop(PeerPid),
    ok.

holder_loop() ->
    receive
        stop -> ok;
        _    -> holder_loop()
    end.

%% =============================================================================
%% Test body
%% =============================================================================

run_envelope_roundtrip(#{peer_node := PeerNode}) ->
    fun() ->
        %% Register the test body pid as the envelope sink so the peer's
        %% handler can deliver via `{Name, Node} ! Msg`.
        catch unregister(?ENVELOPE_SINK),
        true = register(?ENVELOPE_SINK, self()),

        %% First sanity-check: can we receive a message at all from the
        %% peer node? This rules out distribution as the failure mode.
        Self = self(),
        ok = erpc:call(PeerNode, erlang, apply, [
            fun() -> Self ! {peer_smoke, ok}, ok end, []]),
        receive {peer_smoke, ok} -> ok
        after 1000 -> erlang:error(distribution_broken)
        end,

        ok = macula_net_transport_quic:connect(
               ?STATION_B, <<"127.0.0.1">>, ?LISTEN_PORT_B),

        %% Give peer a moment to process new_conn -> async_accept_stream.
        timer:sleep(200),

        Envelope = sample_envelope(),
        ok = macula_net_transport_quic:send(?STATION_B, Envelope),
        receive
            {peer_got, Got} ->
                ?assertEqual(Envelope, Got)
        after ?WAIT_MS ->
            %% Dump peer mailbox and ets info for diagnosis.
            Diag = erpc:call(PeerNode, erlang, apply, [
                fun() ->
                    Pid = whereis(macula_net_transport_quic),
                    {messages, Msgs} = process_info(Pid, messages),
                    State = sys:get_state(Pid, 1000),
                    {Msgs, State}
                end, []]),
            erlang:error({timeout_waiting_envelope, Diag})
        end
    end.


%% =============================================================================
%% Helpers
%% =============================================================================

sample_envelope() ->
    macula_cbor_nif:pack(#{
        <<"v">>       => 1,
        <<"type">>    => <<"data">>,
        <<"src">>     => binary:copy(<<0>>, 16),
        <<"dst">>     => binary:copy(<<1>>, 16),
        <<"ttl">>     => 64,
        <<"payload">> => <<"hello-from-test">>
    }).

ensure_dist() ->
    case net_kernel:get_state() of
        #{started := no} ->
            {ok, _} = net_kernel:start([macula_net_test, shortnames]),
            ok;
        _ ->
            ok
    end.

start_peer_node() ->
    Args = ["-pa" | code:get_path()],
    peer:start_link(#{name => peer:random_name(),
                      args => Args,
                      connection => standard_io,
                      shutdown => 5000}).

preload_code_on_peer(Node) ->
    %% Ensure the peer can resolve modules and the macula NIFs are
    %% loadable in its VM.
    Mods = [macula_net_transport_quic, macula_quic, macula_cbor_nif,
            macula_blake3_nif, macula_crypto_nif, macula_net_transport,
            crypto, public_key],
    lists:foreach(fun(M) ->
        {Mod, Bin, File} = code:get_object_code(M),
        {module, Mod} = erpc:call(Node, code, load_binary, [Mod, File, Bin])
    end, Mods),
    %% Force NIF on_load by calling a harmless function.
    _ = erpc:call(Node, macula_cbor_nif, pack, [#{<<"k">> => 1}]),
    ok.

peer_call(Node, Fun) ->
    erpc:call(Node, erlang, apply, [Fun, []]).
