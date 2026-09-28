%% EUnit tests for macula_tls_posture: a node runs handshake v5 only on a TLS configuration that offers exactly the
%% hybrid ML-KEM groups, neither offers nor accepts 0-RTT, and never resumes a session
%% (plans/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md sections 3 and 6).
-module(macula_tls_posture_tests).

-include_lib("eunit/include/eunit.hrl").

%% SecP384r1MLKEM1024 first, then SecP256r1MLKEM768 (IANA TLS Supported Groups).
-define(GROUPS, [16#11ED, 16#11EB]).

good() ->
    #{client_groups => ?GROUPS, server_groups => ?GROUPS, client_early_data => 0, server_max_early_data => 0,
      server_tickets => 0, second_handshake => full, dialler_second_handshake => full}.

this_build_has_the_posture_test() ->
    ?assertEqual({ok, good()}, macula_quic:tls_posture()),
    ?assertEqual(ok, macula_tls_posture:ensure()).

the_expected_posture_passes_test() ->
    ?assertEqual(ok, macula_tls_posture:check(good())).

each_departure_is_refused_by_name_test() ->
    Departures = [{client_groups, [16#11ED, 16#11EB, 16#001D]},
                  {server_groups, [16#11EB, 16#11ED]},
                  {client_groups, [16#11EC]},
                  {client_early_data, 1},
                  {server_max_early_data, 16#FFFFFFFF},
                  {server_tickets, 2},
                  {second_handshake, resumed},
                  {dialler_second_handshake, resumed}],
    [?assertEqual({error, {tls_posture, Field, Got}}, macula_tls_posture:check((good())#{Field := Got}))
     || {Field, Got} <- Departures].
