%%%-------------------------------------------------------------------
%%% @doc Tests for macula_dist_relay — relay-routed Erlang distribution.
%%%
%%% Tests cover:
%%% - Public API: is_relay_mode, register/get mesh client
%%% - Loopback pair creation and packet framing
%%% - Bridge I/O: reader/writer loops via loopback
%%% - Payload extraction
%%% - Tunnel topic construction
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_relay_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests — is_relay_mode
%%%===================================================================

is_relay_mode_off_test() ->
    os:unsetenv("MACULA_DIST_MODE"),
    ?assertNot(macula_dist_relay:is_relay_mode()).

is_relay_mode_on_test() ->
    os:putenv("MACULA_DIST_MODE", "relay"),
    ?assert(macula_dist_relay:is_relay_mode()),
    os:unsetenv("MACULA_DIST_MODE").

is_relay_mode_other_test() ->
    os:putenv("MACULA_DIST_MODE", "direct"),
    ?assertNot(macula_dist_relay:is_relay_mode()),
    os:unsetenv("MACULA_DIST_MODE").

%%%===================================================================
%%% Tests — register/get mesh client
%%%===================================================================

register_mesh_client_test() ->
    Self = self(),
    ok = macula_dist_relay:register_mesh_client(Self),
    ?assertEqual(Self, macula_dist_relay:get_mesh_client()),
    %% Cleanup
    persistent_term:erase(macula_dist_mesh_client).

get_mesh_client_undefined_test() ->
    persistent_term:erase(macula_dist_mesh_client),
    ?assertEqual(undefined, macula_dist_relay:get_mesh_client()).

get_mesh_client_dead_pid_test() ->
    Pid = spawn(fun() -> ok end),
    timer:sleep(50),
    persistent_term:put(macula_dist_mesh_client, Pid),
    ?assertEqual(undefined, macula_dist_relay:get_mesh_client()),
    persistent_term:erase(macula_dist_mesh_client).

%%%===================================================================
%%% Tests — connect error paths
%%%===================================================================

connect_no_mesh_client_test() ->
    persistent_term:erase(macula_dist_mesh_client),
    os:putenv("MACULA_DIST_MODE", "relay"),
    ?assertEqual({error, no_mesh_connection},
                 macula_dist_relay:connect("test@host", "host", 4433)),
    os:unsetenv("MACULA_DIST_MODE").

%%%===================================================================
%%% Tests — accept_dist
%%%===================================================================

accept_dist_returns_tunnel_id_test() ->
    TunnelId = <<"test-tunnel-123">>,
    {ok, ReturnedId} = macula_dist_relay:accept_dist(TunnelId, #{}),
    ?assertEqual(TunnelId, ReturnedId).

%%%===================================================================
%%% Tests — extract_payload
%%%===================================================================

extract_payload_map_test() ->
    ?assertEqual(<<"data">>,
                 macula_dist_relay:extract_payload(#{payload => <<"data">>})).

extract_payload_binary_test() ->
    ?assertEqual(<<"raw">>,
                 macula_dist_relay:extract_payload(<<"raw">>)).

%%%===================================================================
%%% Tests — loopback pair (integration)
%%%===================================================================

loopback_pair_created_test() ->
    %% Access the internal function via module — it's not exported,
    %% so we test it indirectly through the full flow
    %% Instead, test the gen_tcp loopback behavior directly
    Opts = [binary, {active, false}, {reuseaddr, true}, {ip, {127,0,0,1}}],
    {ok, LSock} = gen_tcp:listen(0, Opts),
    {ok, Port} = inet:port(LSock),
    {ok, CSock} = gen_tcp:connect({127,0,0,1}, Port,
                                  [binary, {active, false}, {packet, 2}, {nodelay, true}]),
    {ok, ASock} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    inet:setopts(ASock, [{packet, raw}, {nodelay, true}]),

    %% CSock has {packet, 2}: send prepends 2-byte length
    ok = gen_tcp:send(CSock, <<"hello">>),

    %% ASock is raw: receives the 2-byte header + payload
    {ok, Data} = gen_tcp:recv(ASock, 0, 1000),
    %% {packet, 2} prepends <<0, 5>> (length 5) before "hello"
    ?assertEqual(<<0, 5, "hello">>, Data),

    gen_tcp:close(CSock),
    gen_tcp:close(ASock).

loopback_pair_bidirectional_test() ->
    Opts = [binary, {active, false}, {reuseaddr, true}, {ip, {127,0,0,1}}],
    {ok, LSock} = gen_tcp:listen(0, Opts),
    {ok, Port} = inet:port(LSock),
    {ok, CSock} = gen_tcp:connect({127,0,0,1}, Port,
                                  [binary, {active, false}, {packet, 2}, {nodelay, true}]),
    {ok, ASock} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    inet:setopts(ASock, [{packet, raw}, {nodelay, true}]),

    %% CSock → ASock
    ok = gen_tcp:send(CSock, <<"outbound">>),
    {ok, Out} = gen_tcp:recv(ASock, 0, 1000),
    ?assertEqual(<<0, 8, "outbound">>, Out),

    %% ASock → CSock (raw write, CSock reads with {packet, 2})
    ok = gen_tcp:send(ASock, <<0, 7, "inbound">>),
    {ok, In} = gen_tcp:recv(CSock, 0, 1000),
    ?assertEqual(<<"inbound">>, In),

    gen_tcp:close(CSock),
    gen_tcp:close(ASock).

loopback_pair_packet4_switch_test() ->
    %% Simulates what happens post-handshake when dist_util
    %% switches from {packet, 2} to {packet, 4}
    Opts = [binary, {active, false}, {reuseaddr, true}, {ip, {127,0,0,1}}],
    {ok, LSock} = gen_tcp:listen(0, Opts),
    {ok, Port} = inet:port(LSock),
    {ok, CSock} = gen_tcp:connect({127,0,0,1}, Port,
                                  [binary, {active, false}, {packet, 2}, {nodelay, true}]),
    {ok, ASock} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    inet:setopts(ASock, [{packet, raw}, {nodelay, true}]),

    %% Phase 1: {packet, 2} handshake
    ok = gen_tcp:send(CSock, <<"hs">>),
    {ok, <<0, 2, "hs">>} = gen_tcp:recv(ASock, 0, 1000),

    %% Phase 2: switch CSock to {packet, 4}
    inet:setopts(CSock, [{packet, 4}]),
    ok = gen_tcp:send(CSock, <<"post">>),
    {ok, <<0, 0, 0, 4, "post">>} = gen_tcp:recv(ASock, 0, 1000),

    %% Reverse: raw ASock writes {packet, 4} framed data → CSock decodes
    ok = gen_tcp:send(ASock, <<0, 0, 0, 3, "ack">>),
    {ok, <<"ack">>} = gen_tcp:recv(CSock, 0, 1000),

    gen_tcp:close(CSock),
    gen_tcp:close(ASock).

%%%===================================================================
%%% Tests — bridge I/O via loopback (end-to-end byte flow)
%%%===================================================================

bridge_roundtrip_test() ->
    %% Simulate two loopback pairs connected via in-process message passing
    %% (standing in for relay pub/sub)
    {DistA, BridgeA} = make_loopback_pair(),
    {DistB, BridgeB} = make_loopback_pair(),

    Self = self(),

    %% Bridge A: BridgeA reader → Self → BridgeB writer
    ReaderA = spawn_link(fun() ->
        case gen_tcp:recv(BridgeA, 0, 2000) of
            {ok, Data} -> Self ! {from_a, Data};
            {error, R} -> Self ! {from_a_err, R}
        end
    end),

    %% Send from DistA (with {packet, 2} framing)
    ok = gen_tcp:send(DistA, <<"ping">>),

    %% Reader picks up raw bytes from BridgeA
    receive
        {from_a, RawData} ->
            %% Forward raw bytes to BridgeB (like relay would)
            ok = gen_tcp:send(BridgeB, RawData),
            %% DistB reads with {packet, 2} → gets "ping"
            {ok, <<"ping">>} = gen_tcp:recv(DistB, 0, 1000)
    after 2000 ->
        error(bridge_reader_timeout)
    end,

    exit(ReaderA, normal),
    gen_tcp:close(DistA),
    gen_tcp:close(BridgeA),
    gen_tcp:close(DistB),
    gen_tcp:close(BridgeB).

%%%===================================================================
%%% Tests — advertise_dist_accept
%%%===================================================================

advertise_no_mesh_client_test() ->
    persistent_term:erase(macula_dist_mesh_client),
    ?assertEqual(ok, macula_dist_relay:advertise_dist_accept()).

%%%===================================================================
%%% Helpers
%%%===================================================================

make_loopback_pair() ->
    ListenOpts = [binary, {active, false}, {reuseaddr, true}, {ip, {127,0,0,1}}],
    {ok, LSock} = gen_tcp:listen(0, ListenOpts),
    {ok, Port} = inet:port(LSock),
    {ok, CSock} = gen_tcp:connect({127,0,0,1}, Port,
                                  [binary, {active, false}, {packet, 2}, {nodelay, true}]),
    {ok, ASock} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    inet:setopts(ASock, [{packet, raw}, {nodelay, true}]),
    {CSock, ASock}.

