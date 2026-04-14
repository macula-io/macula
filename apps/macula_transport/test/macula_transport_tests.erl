%% EUnit smoke tests for macula_transport.
%%
%% Real QUIC loopback exercising the full state machine lives in the
%% Phase 1 Common Test suite — these tests cover the wrapper's API
%% surface, NIF load, and immediate-failure cases.
-module(macula_transport_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% NIF presence
%%------------------------------------------------------------------

nif_module_loads_test() ->
    application:ensure_all_started(macula_transport),
    ?assertMatch({module, macula_quic}, code:ensure_loaded(macula_quic)).

nif_exports_listen_test() ->
    ?assert(erlang:function_exported(macula_quic, nif_listen, 9)).

nif_exports_connect_test() ->
    ?assert(erlang:function_exported(macula_quic, nif_connect, 7)).

%%------------------------------------------------------------------
%% Argument validation
%%------------------------------------------------------------------

listen_rejects_out_of_range_port_test() ->
    ?assertError(function_clause,
                 macula_transport:listen(#{
                     bind     => "127.0.0.1",
                     port     => 70000,
                     certfile => "/nonexistent/cert.pem",
                     keyfile  => "/nonexistent/key.pem"
                 })).

connect_rejects_out_of_range_port_test() ->
    ?assertError(function_clause,
                 macula_transport:connect(#{host => "localhost", port => 70000})).

setopt_active_rejects_bad_value_test() ->
    Fake = make_ref(),
    ?assertError(function_clause, macula_transport:setopt_active(Fake, bogus)).

%%------------------------------------------------------------------
%% Failure paths through the NIF — NIF returns {error, BinaryReason}.
%%------------------------------------------------------------------

listen_returns_error_for_missing_certfile_test() ->
    application:ensure_all_started(macula_transport),
    Result = macula_transport:listen(#{
        bind     => <<"127.0.0.1">>,
        port     => 0,
        certfile => <<"/nonexistent/cert.pem">>,
        keyfile  => <<"/nonexistent/key.pem">>
    }),
    ?assertMatch({error, _}, Result).

connect_returns_error_for_unreachable_host_test() ->
    application:ensure_all_started(macula_transport),
    Result = macula_transport:connect(#{
        host       => <<"127.0.0.1">>,
        port       => 1,             %% reserved; nothing listening
        timeout_ms => 500
    }),
    ?assertMatch({error, _}, Result).
