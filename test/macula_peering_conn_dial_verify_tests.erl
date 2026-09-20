%%%-------------------------------------------------------------------
%%% @doc What a peering dial actually verifies.
%%%
%%% `macula_station_link''s `opts()' documents a `verify' key on the seed
%%% map, forwarded verbatim into the dial target, and says its default is
%%% `webpki'. These tests pin whether that key reaches TLS: a target that
%%% asks for `webpki' must refuse a self-signed listener, and a target
%%% that says nothing must keep the behaviour the fleet runs on today.
%%%
%%% The listener is self-signed on loopback, so `webpki' can only refuse
%%% it and `none' can only accept it. The two outcomes are therefore a
%%% direct read of which option the dial used.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_conn_dial_verify_tests).

-include_lib("eunit/include/eunit.hrl").

-define(HOST, <<"127.0.0.1">>).
-define(DIAL_TIMEOUT_MS, 5_000).

%%%===================================================================
%%% Tests
%%%===================================================================

dial_verify_test_() ->
    {timeout, 60,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(Ctx) ->
          [{"a target asking for webpki refuses a self-signed listener",
            fun() -> webpki_target_refuses_self_signed(Ctx) end},
           {"a target asking for none accepts a self-signed listener",
            fun() -> none_target_accepts_self_signed(Ctx) end},
           {"a target naming no verify keeps the dial unverified",
            fun() -> silent_target_accepts_self_signed(Ctx) end}]
      end}}.

%% The defect: `verify' is documented, accepted and then dropped, so a
%% caller that asks for chain verification silently gets none. Without
%% the fix this dial connects.
webpki_target_refuses_self_signed(#{port := Port}) ->
    ?assertMatch({error, _}, dial(target(Port, #{verify => webpki}))).

none_target_accepts_self_signed(#{port := Port}) ->
    ?assertMatch({ok, _}, dial(target(Port, #{verify => none}))).

%% The fleet dials with no `verify' in the target and its stations are
%% self-signed, so the default this dial builder applies has to stay
%% `none'. A default of `webpki' here would refuse every station.
silent_target_accepts_self_signed(#{port := Port}) ->
    ?assertMatch({ok, _}, dial(target(Port, #{}))).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% A dial target for the fixture's listener, plus whatever trust keys the
%% case is about.
target(Port, TrustKeys) ->
    maps:merge(#{host => ?HOST, port => Port, timeout_ms => ?DIAL_TIMEOUT_MS},
               TrustKeys).

%% One dial through the peering builder, awaited to its outcome, and the
%% connection closed so nothing outlives the assertion.
dial(Target) ->
    closed(awaited(macula_peering_conn:start_dial(Target))).

awaited({ok, Dial}) ->
    Tag = macula_quic:dial_tag(Dial),
    receive
        {quic, connected, Tag, Conn}       -> {ok, Conn};
        {quic, connect_failed, Tag, Reason} -> {error, Reason}
    after ?DIAL_TIMEOUT_MS + 2_000 ->
        ok = macula_quic:cancel_connect(Dial),
        {error, dial_result_never_arrived}
    end;
awaited({error, _} = Error) ->
    Error.

closed({ok, Conn} = Result) ->
    _ = macula_quic:close_connection(Conn),
    Result;
closed({error, _} = Error) ->
    Error.

%%%===================================================================
%%% A self-signed listener on loopback, for the length of the fixture
%%%===================================================================

setup() ->
    {Pub, Priv} = ephemeral_keypair(),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(Pub, Priv, [<<"localhost">>, ?HOST]),
    Port = pick_free_port(),
    {ok, Listener} =
        macula_test_tmp:with_dir("macula-peering-dial-verify",
                                 fun(Dir) -> listen(Dir, Port, CertPem, KeyPem) end),
    ok = macula_quic:async_accept(Listener),
    #{listener => Listener, port => Port}.

%% The listener reads its certificate and key files while it starts
%% listening, so they need to last only that long.
listen(Dir, Port, CertPem, KeyPem) ->
    Cert = filename:join(Dir, "listener.crt"),
    Key  = filename:join(Dir, "listener.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    macula_quic:listen(?HOST, Port,
                       [{cert, Cert}, {key, Key},
                        {alpn, [<<"macula">>]},
                        {idle_timeout_ms, 30_000},
                        {keep_alive_interval_ms, 5_000}]).

cleanup(#{listener := L}) ->
    try macula_quic:close_listener(L) catch _:_ -> ok end,
    drain_quic_messages(),
    ok.

ephemeral_keypair() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {iolist_to_binary(Pub), iolist_to_binary(Priv)}.

pick_free_port() ->
    {ok, S} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, P} = inet:port(S),
    gen_udp:close(S),
    P.

drain_quic_messages() ->
    receive
        {quic, _, _, _} -> drain_quic_messages()
    after 0 -> ok
    end.
