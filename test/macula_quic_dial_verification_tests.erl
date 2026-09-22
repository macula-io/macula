%%%-------------------------------------------------------------------
%%% @doc The one way a QUIC dial verifies a listener (plan decisions D12
%%% and D16), seen from Erlang.
%%%
%%% A listener presents a self-signed ML-DSA-87 certificate on its TLS key,
%%% and a dial checks the listener's handshake signature under that key and
%%% nothing else. There is no option to choose another check: `verify' and
%%% `verify_pubkey' chose among modes that are gone, and a dial given either
%%% is refused before it starts, rather than run with a check the caller did
%%% not ask for. A listener cannot present a classical certificate at all.
%%%
%%% The signature check itself, including a listener that signs with a key
%%% other than its certificate's, is tested in the NIF (config.rs) and in
%%% macula-pqc, where a certificate and key that disagree can be built.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_dial_verification_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(HOST, <<"127.0.0.1">>).
-define(ALPN, [<<"macula">>]).

dial_verification_test_() ->
    {timeout, 60,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(Ctx) ->
          [{"a dial to an ML-DSA-87 listener connects with no verification option",
            fun() -> dial_connects(Ctx) end},
           {"a dial given verify is refused, whatever its value",
            fun() -> verify_is_refused(Ctx) end},
           {"a dial given verify_pubkey is refused",
            fun() -> verify_pubkey_is_refused(Ctx) end},
           {"an asynchronous dial given either is refused the same way",
            fun() -> async_dial_is_refused(Ctx) end}]
      end}}.

%% A classical certificate cannot even be loaded: the listener's key loader
%% takes an ML-DSA-87 key and nothing else.
a_listener_refuses_a_classical_certificate_test() ->
    #{cert := CertDer, key := KeyTerm} = public_key:pkix_test_root_cert("classical", []),
    Dir = macula_test_tmp:dir("macula-quic-classical-listener"),
    try
        Cert = filename:join(Dir, "listener.crt"),
        Key = filename:join(Dir, "listener.key"),
        ok = file:write_file(Cert, public_key:pem_encode([{'Certificate', CertDer, not_encrypted}])),
        ok = file:write_file(Key, public_key:pem_encode([public_key:pem_entry_encode(key_type(KeyTerm), KeyTerm)])),
        ?assertMatch({error, _}, macula_quic:listen(?HOST, free_udp_port(), [{cert, Cert}, {key, Key}, {alpn, ?ALPN}]))
    after
        ok = file:del_dir_r(Dir)
    end.

%%%===================================================================
%%% Test bodies
%%%===================================================================

dial_connects(#{port := Port}) ->
    {ok, Conn} = macula_quic:connect(?HOST, Port, [{alpn, ?ALPN}], 5_000),
    ok = macula_quic:close_connection(Conn).

verify_is_refused(#{port := Port}) ->
    [?assertEqual({error, {verify_option_removed, verify}},
                  macula_quic:connect(?HOST, Port, [{verify, Mode}, {alpn, ?ALPN}], 5_000))
     || Mode <- [none, webpki]].

verify_pubkey_is_refused(#{port := Port}) ->
    ?assertEqual({error, {verify_option_removed, verify_pubkey}},
                 macula_quic:connect(?HOST, Port, [{verify_pubkey, <<0:256>>}, {alpn, ?ALPN}], 5_000)).

async_dial_is_refused(#{port := Port}) ->
    ?assertEqual({error, {verify_option_removed, verify}},
                 macula_quic:async_connect(?HOST, Port, [{verify, none}], 5_000)),
    ?assertEqual({error, {verify_option_removed, verify_pubkey}},
                 macula_quic:async_connect(?HOST, Port, [{verify_pubkey, <<0:256>>}], 5_000)).

%%%===================================================================
%%% A loopback listener on an ML-DSA-87 certificate
%%%===================================================================

setup() ->
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(macula_test_identity:tls_seed(), [<<"localhost">>, ?HOST]),
    Port = free_udp_port(),
    {ok, Listener} = macula_test_tmp:with_dir("macula-quic-dial-verification",
                                              fun(Dir) -> listen(Dir, Port, CertPem, KeyPem) end),
    ok = macula_quic:async_accept(Listener),
    #{listener => Listener, port => Port}.

%% The listener reads its certificate and key files when it starts
%% listening, so they last only that long.
listen(Dir, Port, CertPem, KeyPem) ->
    Cert = filename:join(Dir, "listener.crt"),
    Key = filename:join(Dir, "listener.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    macula_quic:listen(?HOST, Port, [{cert, Cert}, {key, Key}, {alpn, ?ALPN}]).

cleanup(#{listener := Listener}) ->
    ok = macula_quic:close_listener(Listener),
    drain_quic_messages().

drain_quic_messages() ->
    receive
        {quic, _, _, _} -> drain_quic_messages()
    after 0 ->
        ok
    end.

key_type(#'ECPrivateKey'{}) -> 'ECPrivateKey';
key_type(#'RSAPrivateKey'{}) -> 'RSAPrivateKey'.

free_udp_port() ->
    {ok, Socket} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Socket),
    ok = gen_udp:close(Socket),
    Port.
