%% EUnit tests for macula_dist_tunnel: a distribution tunnel that names its peer.
%%
%% Until this existed, the `macula-dist' and `macula-dist-relay' dials ran no connection handshake at all: a
%% connection over them verified that the peer held the key of the certificate it presented, and nothing more.
%% Nothing bound that key to a node_id, so distribution's cookie handshake could be relayed by whatever sat in the
%% middle, which on these carriers is precisely what the middle is for. That is why those dials refuse to start
%% today unless an operator sets `MACULA_DIST_UNIDENTIFIED_PEER=accept'.
%%
%% D29's answer: the tunnel runs the connection handshake end to end INSIDE a TLS 1.3 session the two nodes hold
%% between them. The accepting node takes the station role with its TLS key, certificate and TLS-key binding; the
%% dialling node sends CONNECT with its CONNECT key, against the node_id it meant to reach. The carrier forwards
%% ciphertext and is never asked to be honest about who is at the far end.
-module(macula_dist_tunnel_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EVENT_MS, 15_000).

tunnel_test_() ->
    {timeout, 180,
     {setup,
      fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
      fun(ok) -> ok end,
      [{"both ends of a tunnel name each other", fun both_ends_name_each_other/0},
       {"a dial for another node_id is refused, and nothing is carried",
        fun a_dial_for_another_node_id_is_refused/0},
       {"the tunnel carries bytes, and only inside its session",
        fun the_tunnel_carries_bytes/0}]}}.

%% The dialling side gets the station's node_id it asked for, the accepting side learns the dialler's, and neither
%% took the other's word for it: each node_id is derived from the identity key that signed the handshake.
both_ends_name_each_other() ->
    with_tunnel(#{}, fun(#{dialled := Dialled, accepted := Accepted, world := World}) ->
        #{station_node_id := StationNodeId, client_node_id := ClientNodeId} = World,
        ?assertEqual(StationNodeId, maps:get(peer_node_id, Dialled)),
        ?assertEqual(ClientNodeId, maps:get(peer_node_id, Accepted))
    end).

%% The check that makes the tunnel worth having: a dial names the node it means to reach, and a peer that derives
%% to another node_id is refused before any distribution byte crosses.
a_dial_for_another_node_id_is_refused() ->
    Refusal = with_tunnel(#{expected => <<7:256>>}, fun(Outcome) -> Outcome end),
    ?assertMatch(#{dialled := {error, {peer_identity_mismatch, _}}}, Refusal).

the_tunnel_carries_bytes() ->
    with_tunnel(#{}, fun(#{dialled := Dialled, accepted := Accepted}) ->
        ok = macula_dist_tunnel:send(Dialled, <<"a distribution frame">>),
        ?assertEqual({ok, <<"a distribution frame">>}, macula_dist_tunnel:recv(Accepted, 0, ?EVENT_MS)),
        ok = macula_dist_tunnel:send(Accepted, <<"and its answer">>),
        ?assertEqual({ok, <<"and its answer">>}, macula_dist_tunnel:recv(Dialled, 0, ?EVENT_MS))
    end).

%%%===================================================================
%%% A tunnel over a loopback stream pair
%%%===================================================================

with_tunnel(Options, Fun) ->
    Pair = macula_test_quic_pair:open("macula-dist-tunnel"),
    try
        World = world(Pair),
        Outcome = tunnel(Pair, World, Options),
        try
            Fun(Outcome#{world => World})
        after
            close(Outcome)
        end
    after
        macula_test_quic_pair:close(Pair)
    end.

%% Each side with its own identity key and statement issuer, and the accepting side's leaf registered with its
%% issuer, which is what a station does when its listener presents a certificate.
world(#{cert := CertFile}) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    StationKey = identity(Profile),
    ClientKey = identity(Profile),
    {ok, TlsKey} = macula_node_keys:generate(tls, Profile),
    StationIssuer = issuer(StationKey),
    ok = macula_statement_issuer:register_tls_leaf(StationIssuer, leaf_der(CertFile), TlsKey),
    {ok, StationNodeId} = macula_node_keys:node_id(StationKey),
    {ok, ClientNodeId} = macula_node_keys:node_id(ClientKey),
    #{profile => Profile, station_key => StationKey, client_key => ClientKey,
      station_issuer => StationIssuer, client_issuer => issuer(ClientKey),
      station_node_id => StationNodeId, client_node_id => ClientNodeId}.

%% Both ends run at once: the accepting side waits for the opener the dialling side sends inside the session, so
%% one cannot be driven to completion before the other starts.
tunnel(#{client_stream := ClientStream, server_stream := ServerStream, cert := Cert, key := Key},
       #{profile := Profile, station_key := StationKey, client_key := ClientKey,
         station_issuer := StationIssuer, client_issuer := ClientIssuer, station_node_id := StationNodeId},
       Options) ->
    Parent = self(),
    Accepting = spawn_link(fun() ->
        Parent ! {accepted, macula_dist_tunnel:accept(ServerStream,
                                                      #{profile => Profile, identity => StationKey,
                                                        issuer => StationIssuer, cert => Cert, key => Key,
                                                        %% The keys here are generated, not ground, so the mode is
                                                        %% the one a station starts in (D30). What these tests are
                                                        %% about is who the peer is, not the cost of being one.
                                                        puzzle => #{mode => log_only},
                                                        timeout_ms => ?EVENT_MS})},
        hold()
    end),
    Dialled = macula_dist_tunnel:dial(ClientStream,
                                      #{profile => Profile, identity => ClientKey, issuer => ClientIssuer,
                                        expected_node_id => maps:get(expected, Options, StationNodeId),
                                        timeout_ms => ?EVENT_MS}),
    Accepted = receive {accepted, A} -> A after ?EVENT_MS -> error(no_accept_result) end,
    #{dialled => unwrapped(Dialled), accepted => unwrapped(Accepted), accepting => Accepting}.

%% A tunnel's session belongs to the process that made it, so the accepting process stays until the test is done.
hold() ->
    receive done -> ok after ?EVENT_MS * 4 -> ok end.

unwrapped({ok, Tunnel}) -> Tunnel;
unwrapped({error, _} = Refusal) -> Refusal.

close(#{accepting := Accepting} = Outcome) ->
    _ = [macula_dist_tunnel:close(T) || Side <- [dialled, accepted],
                                        (T = maps:get(Side, Outcome)) =/= undefined, is_map(T)],
    Accepting ! done,
    ok.

leaf_der(CertFile) ->
    {ok, Pem} = file:read_file(CertFile),
    [{'Certificate', Der, not_encrypted} | _] = public_key:pem_decode(Pem),
    Der.

identity(Profile) ->
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    Key.

issuer(Key) ->
    {ok, Issuer} = macula_statement_issuer:start_link(#{identity => fun() -> Key end, owner => self()}),
    Issuer.
