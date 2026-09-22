%%%-------------------------------------------------------------------
%%% @doc Two real `macula_quic' endpoints complete a QUIC handshake, and
%%% exchange bytes over it, while the only key exchange groups either side
%%% offers are post-quantum.
%%%
%%% == What this proves, and what carries the other half ==
%%%
%%% ⚠ ON ITS OWN, "A CONNECTION CAME UP" WOULD BE THE ADJACENT OBJECT. A
%%% connection comes up just as happily over X25519, which is the state this
%%% work exists to leave, so a test that only watched it succeed would stay
%%% green through a change that undid everything.
%%%
%%% The argument has two halves and this file is one of them:
%%%
%%% <ol>
%%%   <li><b>The group list is in force</b>, proved in
%%%       `native/macula_quic/src/config.rs' by a NEGATIVE CONTROL: a peer
%%%       offering only classical groups CANNOT agree with us, whether it
%%%       dials us or we dial it. It can only fail to agree if our list is
%%%       genuinely being applied.</li>
%%%   <li><b>Real endpoints connect under it</b>, which is this file. Since
%%%       `macula_quic' offers post-quantum groups and nothing else, and the
%%%       offering is proved real by (1), a handshake that completes here
%%%       cannot have landed on a classical group. There is nothing left for
%%%       it to have landed on.</li>
%%% </ol>
%%%
%%% <b>THE LIST BELONGS TO `macula-pqc'</b>, the crate on crates.io that every
%%% TLS configuration in `native/macula_quic' is built from:
%%% `SecP384r1MLKEM1024', then `SecP256r1MLKEM768', and nothing classical.
%%% Two endpoints here negotiate `SecP384r1MLKEM1024'; the Rust test
%%% `negotiated_key_exchange_group_is_the_one_we_lead_with' proves it, since
%%% this file cannot see the group. A `macula-pqc' version that changed the
%%% list would reach this NIF through `Cargo.lock', so
%%% `every_configuration_offers_exactly_macula_pqcs_groups' pins the exact
%%% list and fails first.
%%%
%%% ⚠ THE SPLIT IS CONDITIONAL ON THE GROUP LIST STAYING STRICT. It works
%%% because `macula_quic' offers NOTHING classical, so "the handshake
%%% completed" leaves only post-quantum groups it could have completed on.
%%% <b>If a classical fallback group is ever added, in `macula-pqc' or here,
%%% that deduction dies and this file stops proving anything</b>: a completed
%%% handshake would once again be consistent with X25519. Adding a fallback
%%% therefore obliges you to add the negative control HERE, in Erlang,
%%% against a classical-only peer, which needs a per-endpoint key exchange
%%% group option on the NIF. That option was deliberately not added for a
%%% test alone. If you are adding the fallback, it is now part of the work.
%%%
%%% ⛔ IT HAS TO BE SPLIT THAT WAY, and not because anyone preferred it.
%%% `quinn' does not surface the negotiated group: `rustls' has
%%% `CommonState::negotiated_key_exchange_group/1', but
%%% `quinn_proto::crypto::rustls::TlsSession' holds its rustls connection in
%%% a private field and reports only the ALPN protocol and the server name.
%%% <b>No live QUIC connection in this stack can be asked what it
%%% negotiated</b>, so asking it here is not an improvement anyone can make;
%%% it is a day someone can lose. Deducing the answer from what was on offer
%%% is the whole reason the negative control exists.
%%%
%%% == What this does NOT prove ==
%%%
%%% The SDK, and only the SDK. A station, a realm, `mcl-echo' and the fleet
%%% pick this up after a hex release and, for the station, an image rebuild:
%%% its Dockerfile resolves `macula' from hex, not from a checkout. Green
%%% here is not a statement about anything running anywhere.
%%%
%%% And it is key exchange, not signatures. The provider's supported
%%% signature algorithms are ECDSA, Ed25519 and RSA, and `rustls-webpki' has
%%% no ML-DSA at all, so the certificates in this handshake are classically
%%% signed. Say "post-quantum key exchange", never "post-quantum TLS".
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_pq_kx_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

pq_kx_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Identity) ->
         [{"two endpoints complete a QUIC handshake offering only post-quantum "
           "key exchange groups",
           {timeout, 30,
            fun() -> endpoints_handshake_offering_only_pq_groups(Identity) end}},
          {"a stream carries bytes over that connection, so the agreed secret "
           "is usable and not merely agreed",
           {timeout, 30,
            fun() -> a_stream_carries_bytes_over_the_pq_connection(Identity) end}}]
     end}.

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    Dir = macula_test_tmp:dir("macula-quic-pq-kx"),
    identity(Dir).

cleanup(#{dir := Dir}) ->
    ok = file:del_dir_r(Dir),
    drain_quic_messages(),
    ok.

%%%===================================================================
%%% Test bodies
%%%===================================================================

%% The handshake completes between two endpoints that offer post-quantum
%% groups and nothing else, so the shared secret behind this connection is
%% not recoverable from a recording of it by a quantum adversary.
endpoints_handshake_offering_only_pq_groups(#{pub := Pub} = Identity) ->
    {Listener, Port} = listen(Identity),
    {ok, Client} = dial(Port, Identity),
    Server = accepted(),
    %% Both sides finished the handshake, and each holds the leaf the other
    %% presented: a connection that had not got past key exchange has
    %% neither. The client's pin is the server's Ed25519 key, so the leaf it
    %% reports is the one this identity issued and not any leaf at all.
    ?assertMatch({ok, _}, macula_quic:handshake(Client)),
    {ok, ClientSeesLeaf} = macula_quic:peer_leaf(Client),
    {ok, ServerPresentedLeaf} = macula_quic:presented_leaf(Server),
    ?assertEqual(ClientSeesLeaf, ServerPresentedLeaf),
    ?assertEqual(Pub, leaf_pubkey(ClientSeesLeaf)),
    close([Client, Server], Listener).

%% And it carries data, not only a handshake. A key exchange that agrees and
%% then cannot derive working traffic keys would still complete the
%% handshake; bytes arriving is what shows the agreed secret is usable.
a_stream_carries_bytes_over_the_pq_connection(Identity) ->
    {Listener, Port} = listen(Identity),
    {ok, Client} = dial(Port, Identity),
    Server = accepted(),
    ok = macula_quic:async_accept_stream(Server),
    {ok, ClientStream} = macula_quic:open_stream(Client),
    ok = macula_quic:send(ClientStream, <<"post-quantum">>),
    ServerStream = accepted_stream(),
    ok = macula_quic:setopt(ServerStream, active, true),
    ?assertEqual(<<"post-quantum">>, read_bytes(ServerStream, 5000)),
    close([Client, Server], Listener).

%%%===================================================================
%%% Helpers
%%%===================================================================

identity(Dir) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    PubBin = iolist_to_binary(Pub),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            PubBin, iolist_to_binary(Priv), [<<"localhost">>, <<"127.0.0.1">>]),
    Cert = filename:join(Dir, "pq.crt"),
    Key = filename:join(Dir, "pq.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    #{dir => Dir, pub => PubBin, cert => Cert, key => Key}.

listen(#{cert := Cert, key := Key}) ->
    Port = pick_free_port(),
    {ok, Listener} = macula_quic:listen(<<"127.0.0.1">>, Port,
                                        [{cert, Cert}, {key, Key},
                                         {alpn, [<<"macula">>]},
                                         {idle_timeout_ms, 30000},
                                         {keep_alive_interval_ms, 5000}]),
    ok = macula_quic:async_accept(Listener),
    {Listener, Port}.

dial(Port, #{pub := Pub}) ->
    macula_quic:connect(<<"127.0.0.1">>, Port,
                        [{verify_pubkey, Pub}, {alpn, [<<"macula">>]}], 5000).

accepted() ->
    receive
        {quic, new_conn, Conn, _Info} -> Conn
    after 5000 ->
        error(no_accepted_connection)
    end.

accepted_stream() ->
    receive
        {quic, new_stream, Stream, _Props} -> Stream
    after 5000 ->
        error(no_accepted_stream)
    end.

%% Stream data arrives as `{quic, Bin, Stream, Flags}'.
read_bytes(Stream, TimeoutMs) ->
    receive
        {quic, Data, Stream, _Flags} when is_binary(Data) -> Data
    after TimeoutMs ->
        error(no_bytes)
    end.

%% The Ed25519 SubjectPublicKeyInfo of a leaf, so a test can say WHICH
%% identity answered rather than that some certificate came back.
%% ⚠ An Ed25519 key comes back from the OTP decoder wrapped as an
%% `#'ECPoint'{}', not as the raw 32 bytes, so the unwrap is not optional.
leaf_pubkey(Der) ->
    #'OTPCertificate'{tbsCertificate = Tbs} = public_key:pkix_decode_cert(Der, otp),
    #'OTPTBSCertificate'{subjectPublicKeyInfo = Spki} = Tbs,
    #'OTPSubjectPublicKeyInfo'{subjectPublicKey = #'ECPoint'{point = Key}} = Spki,
    Key.

close(Connections, Listener) ->
    lists:foreach(fun macula_quic:close_connection/1, Connections),
    ok = macula_quic:close_listener(Listener).

pick_free_port() ->
    {ok, S} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(S),
    ok = gen_udp:close(S),
    Port.

drain_quic_messages() ->
    receive
        {quic, _, _, _} -> drain_quic_messages()
    after 0 -> ok
    end.
