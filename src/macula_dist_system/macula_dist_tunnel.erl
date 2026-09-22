%% @doc A distribution tunnel that names its peer.
%%
%% The `macula-dist' and `macula-dist-relay' carriers forward a tunnel's bytes between two nodes, and forwarding is
%% what they are for: a relay in the middle, or the stations of the pool path. A connection over them proves only
%% that the far end holds the key of the certificate it presented, which binds that key to no node_id at all, so
%% distribution's cookie handshake can be relayed by whatever sits between. That is why those dials refuse to start
%% without `MACULA_DIST_UNIDENTIFIED_PEER=accept'.
%%
%% D29's answer, and this module: the two nodes hold a TLS 1.3 session between them inside the tunnel, and run the
%% connection handshake end to end inside that session. The accepting node takes the station role, with its TLS key,
%% certificate and TLS-key binding; the dialling node sends CONNECT with its CONNECT key, against the node_id it
%% meant to reach. The carrier sees ciphertext, and is never asked to be honest about who is at the far end.
%%
%% The handshake is `macula_handshake', unchanged and shared with the peering connections: the same frames, the same
%% checks, the same refusals. What differs is where the leaf comes from. On a peering connection it is the leaf of
%% the QUIC connection; here it is the leaf of the session inside the tunnel, which is the only one the two nodes
%% hold between them.
-module(macula_dist_tunnel).

-export([dial/2, accept/2, send/2, recv/3, close/1, session/1, peer_node_id/1]).

-export_type([tunnel/0, dial_options/0, accept_options/0]).

-type profile() :: macula_crypto_profile:profile().

-type tunnel() :: #{session := ssl:sslsocket(), peer_node_id := <<_:256>>, peer := map()}.

-type dial_options() :: #{profile := profile(),
                          identity := macula_node_keys:node_key(),
                          issuer := pid(),
                          expected_node_id := <<_:256>>,
                          capabilities => non_neg_integer(),
                          timeout_ms => pos_integer()}.

-type accept_options() :: #{profile := profile(),
                            identity := macula_node_keys:node_key(),
                            issuer := pid(),
                            cert := file:filename_all(),
                            key := file:filename_all(),
                            puzzle := #{mode := macula_handshake:puzzle_mode()},
                            capabilities => non_neg_integer(),
                            timeout_ms => pos_integer()}.

%% The handshake's own timeout (DESIGN_PQ_HANDSHAKE_FRAMES.md), for the session and every frame in it.
-define(TIMEOUT_MS, 30_000).

%% A handshake frame is at most this, as it is on a peering connection: CONNECT is the largest at about 22 KB, and
%% a length above this is read off the header alone, without waiting for bytes that are not coming.
-define(MAX_HANDSHAKE_FRAME, 65_536).

%%------------------------------------------------------------------
%% API
%%------------------------------------------------------------------

%% @doc Dial through Stream and return the tunnel, once the node at the far end has proved it is
%% `expected_node_id'. The refusals are the handshake's own, by name.
-spec dial(reference(), dial_options()) -> {ok, tunnel()} | {error, term()}.
dial(Stream, #{profile := _, identity := _, issuer := _, expected_node_id := <<_:256>>} = Options) ->
    dialled(owned(Stream), Options).

%% @doc Accept the tunnel offered on Stream, in the station role, and return it once the node at the far end has
%% signed for the node_id it claims.
-spec accept(reference(), accept_options()) -> {ok, tunnel()} | {error, term()}.
accept(Stream, #{profile := _, identity := _, issuer := _, cert := _, key := _,
                 puzzle := #{mode := Mode}} = Options)
  when Mode =:= off; Mode =:= log_only; Mode =:= enforce ->
    accepted(owned(Stream), Options).

%% @doc Write on the tunnel. The bytes travel inside its session and nowhere else.
-spec send(tunnel(), iodata()) -> ok | {error, term()}.
send(#{session := Session}, Data) ->
    ssl:send(Session, Data).

-spec recv(tunnel(), non_neg_integer(), timeout()) -> {ok, binary()} | {error, term()}.
recv(#{session := Session}, Length, Timeout) ->
    ssl:recv(Session, Length, Timeout).

-spec close(tunnel()) -> ok.
close(#{session := Session}) ->
    _ = ssl:close(Session),
    ok.

-spec session(tunnel()) -> ssl:sslsocket().
session(#{session := Session}) ->
    Session.

-spec peer_node_id(tunnel()) -> <<_:256>>.
peer_node_id(#{peer_node_id := NodeId}) ->
    NodeId.

%%------------------------------------------------------------------
%% The dialling side
%%------------------------------------------------------------------

dialled({ok, Socket}, Options) ->
    ended(connected(ssl:connect(Socket, client_tls_options(), timeout(Options)), Options), Socket);
dialled({error, _} = Refusal, _Options) ->
    Refusal.

connected({ok, Session}, Options) ->
    with_leaf(ssl:peercert(Session), Session, Options);
connected({error, Reason}, _Options) ->
    {error, {tls_session, Reason}}.

%% The leaf of the session inside the tunnel is what the station's TLS binding must be for. Nothing else in view is
%% a leaf the two nodes share: the carrier's own certificates belong to the carrier.
with_leaf({ok, Leaf}, Session, Options) ->
    opened(write(Session, macula_handshake:opener(), Options), Leaf, Session, Options);
with_leaf({error, Reason}, _Session, _Options) ->
    {error, {no_peer_certificate, Reason}}.

opened(ok, Leaf, Session, Options) ->
    challenged(read(Session, Options), Leaf, Session, Options);
opened({error, _} = Refusal, _Leaf, _Session, _Options) ->
    Refusal.

challenged({ok, Challenge}, Leaf, Session, Options) ->
    answered(macula_handshake:answer_challenge(Challenge, client_session(Leaf, Options)), Session, Options);
challenged({error, _} = Refusal, _Leaf, _Session, _Options) ->
    Refusal.

answered({ok, Connect, Station}, Session, Options) ->
    sent(write(Session, Connect, Options), Station, Session, Options);
answered({error, _} = Refusal, _Session, _Options) ->
    Refusal.

sent(ok, Station, Session, Options) ->
    welcomed(read(Session, Options), Station, Session, Options);
sent({error, _} = Refusal, _Station, _Session, _Options) ->
    Refusal.

welcomed({ok, Hello}, Station, Session, _Options) ->
    hello_read(macula_handshake:read_hello(Hello), Station, Session);
welcomed({error, _} = Refusal, _Station, _Session, _Options) ->
    Refusal.

hello_read({ok, _Fields}, #{node_id := NodeId} = Station, Session) ->
    {ok, #{session => Session, peer_node_id => NodeId, peer => Station}};
hello_read({error, _} = Refusal, _Station, _Session) ->
    Refusal.

client_session(Leaf, #{profile := Profile, identity := Identity, issuer := Issuer,
                       expected_node_id := Expected} = Options) ->
    #{connect_key := Key, connect_binding := Binding, connect_status := Status} =
        macula_statement_issuer:connect_material(Issuer),
    #{profile => Profile, expected_node_id => Expected, leaf => Leaf,
      identity_key => macula_node_keys:public_key(Identity), connect_key => Key,
      connect_binding => Binding, connect_status => Status,
      capabilities => capabilities(Options), now => erlang:system_time(millisecond)}.

%%------------------------------------------------------------------
%% The accepting side
%%------------------------------------------------------------------

accepted({ok, Socket}, Options) ->
    ended(server_session(ssl:handshake(Socket, server_tls_options(Options), timeout(Options)), Options), Socket);
accepted({error, _} = Refusal, _Options) ->
    Refusal.

server_session({ok, Session}, Options) ->
    offered(read(Session, Options), Session, Options);
server_session({error, Reason}, _Options) ->
    {error, {tls_session, Reason}}.

offered({ok, Opener}, Session, Options) ->
    opener_read(macula_handshake:read_opener(Opener), Session, Options);
offered({error, _} = Refusal, _Session, _Options) ->
    Refusal.

opener_read(ok, Session, #{cert := CertFile} = Options) ->
    challenge_sent(station_material(leaf_of(CertFile), Options), Session, Options);
opener_read({error, _} = Refusal, _Session, _Options) ->
    Refusal.

station_material({ok, Leaf}, #{issuer := Issuer, identity := Identity, profile := Profile}) ->
    tls_material(macula_statement_issuer:tls_material(Issuer, crypto:hash(sha384, Leaf)), Leaf, Identity, Profile);
station_material({error, _} = Refusal, _Options) ->
    Refusal.

tls_material({ok, #{tls_binding := Binding, tls_status := Status}}, Leaf, Identity, Profile) ->
    Material = #{profile => Profile, identity_key => macula_node_keys:public_key(Identity),
                 tls_binding => Binding, tls_status => Status},
    {ok, Leaf, macula_handshake:challenge(Material)};
tls_material({error, Reason}, _Leaf, _Identity, _Profile) ->
    {error, {tls_material, Reason}}.

challenge_sent({ok, Leaf, Challenge}, Session, Options) ->
    written(write(Session, Challenge, Options), Leaf, Challenge, Session, Options);
challenge_sent({error, _} = Refusal, _Session, _Options) ->
    Refusal.

written(ok, Leaf, Challenge, Session, Options) ->
    connect_read(read(Session, Options), Leaf, Challenge, Session, Options);
written({error, _} = Refusal, _Leaf, _Challenge, _Session, _Options) ->
    Refusal.

connect_read({ok, Connect}, Leaf, Challenge, Session, Options) ->
    connect_checked(macula_handshake:accept_connect(Connect, station_session(Leaf, Challenge, Options)),
                    Session, Options);
connect_read({error, _} = Refusal, _Leaf, _Challenge, _Session, _Options) ->
    Refusal.

%% A refused CONNECT still gets its HELLO: the far end learns it was refused and one coarse code, and nothing else.
connect_checked({accepted, #{node_id := NodeId} = Client, Hello}, Session, Options) ->
    admitted(write(Session, Hello, Options), NodeId, Client, Session);
connect_checked({refused, Reason, Hello}, Session, Options) ->
    _ = write(Session, Hello, Options),
    {error, Reason}.

admitted(ok, NodeId, Client, Session) ->
    {ok, #{session => Session, peer_node_id => NodeId, peer => Client}};
admitted({error, _} = Refusal, _NodeId, _Client, _Session) ->
    Refusal.

station_session(Leaf, Challenge, #{profile := Profile, puzzle := #{mode := Mode}} = Options) ->
    #{profile => Profile, challenge => Challenge, leaf => Leaf,
      puzzle => #{mode => Mode, difficulty => macula_node_keys:puzzle_difficulty()},
      capabilities => capabilities(Options), now => erlang:system_time(millisecond)}.

%% The leaf this side presents, from the certificate its session is configured with. A station's certificate is
%% strict DER (DESIGN_PQ_HANDSHAKE_FRAMES.md), so these are the bytes the far end received and hashed.
leaf_of(CertFile) ->
    read_leaf(file:read_file(CertFile)).

read_leaf({ok, Pem}) ->
    first_certificate(public_key:pem_decode(Pem));
read_leaf({error, Reason}) ->
    {error, {certificate_unreadable, Reason}}.

first_certificate([{'Certificate', Der, not_encrypted} | _Rest]) ->
    {ok, Der};
first_certificate(_Other) ->
    {error, certificate_unreadable}.

%%------------------------------------------------------------------
%% Shared
%%------------------------------------------------------------------

%% A handshake that ended for any reason leaves nothing open. The socket is what gets closed, not the session:
%% the session may never have formed, and closing the socket takes the stream with it and leaves the session's own
%% process with a transport that has gone, which is the end of it either way.
ended({ok, _Tunnel} = Tunnel, _Socket) ->
    Tunnel;
ended({error, _} = Refusal, Socket) ->
    ok = macula_dist_tunnel_socket:close(Socket),
    Refusal.

owned(Stream) ->
    macula_dist_tunnel_socket:own(Stream).

%% A frame goes out length-prefixed, as it does on a peering connection, and the bytes inside the prefix are the
%% ones the handshake signed and hashed.
write(Session, Frame, _Options) ->
    ssl:send(Session, macula_frame:encode_bytes(Frame)).

%% One frame: its length, then exactly that many bytes. A length above what a handshake frame can be is refused
%% from the header alone, without waiting for bytes that are not coming.
read(Session, Options) ->
    sized(ssl:recv(Session, 4, timeout(Options)), Session, Options).

sized({ok, <<Length:32/big>>}, Session, Options) when Length =< ?MAX_HANDSHAKE_FRAME, Length > 0 ->
    body(ssl:recv(Session, Length, timeout(Options)));
sized({ok, <<_Length:32/big>>}, _Session, _Options) ->
    {error, malformed_frame};
sized({error, Reason}, _Session, _Options) ->
    {error, {tunnel_read, Reason}}.

body({ok, Bytes}) when is_binary(Bytes) ->
    {ok, Bytes};
body({error, Reason}) ->
    {error, {tunnel_read, Reason}}.

%% The session carries a tunnel between two nodes that have already agreed on a profile (D2), so there is nothing
%% to negotiate here beyond TLS 1.3 itself. The leaf is judged by the handshake inside, not by a chain: nothing
%% issues an ML-DSA-87 certificate, so there is no authority for a chain to end at.
client_tls_options() ->
    [binary, {active, false}, {versions, ['tlsv1.3']}, {cb_info, macula_dist_tunnel_socket:cb_info()},
     {verify, verify_peer}, {cacerts, []},
     {verify_fun, {fun leaf_kept/3, []}},
     {customize_hostname_check, [{match_fun, fun(_Ref, _Presented) -> true end}]},
     {server_name_indication, disable}].

server_tls_options(#{cert := Cert, key := Key}) ->
    [binary, {active, false}, {versions, ['tlsv1.3']}, {cb_info, macula_dist_tunnel_socket:cb_info()},
     {certfile, Cert}, {keyfile, Key}].

%% ⚠ This accepts every leaf, and that is correct here and nowhere else: the leaf is checked by the handshake
%% inside the session, against the station's TLS binding and the node_id the dial named. A chain check would refuse
%% every macula certificate, since none is issued by anyone.
leaf_kept(_Cert, {bad_cert, _Reason}, State) -> {valid, State};
leaf_kept(_Cert, {extension, _Extension}, State) -> {unknown, State};
leaf_kept(_Cert, valid, State) -> {valid, State};
leaf_kept(_Cert, valid_peer, State) -> {valid, State}.

capabilities(Options) ->
    maps:get(capabilities, Options, 0).

timeout(Options) ->
    maps:get(timeout_ms, Options, ?TIMEOUT_MS).
