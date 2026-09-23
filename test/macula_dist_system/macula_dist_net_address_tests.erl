%% EUnit tests for the #net_address{} the dist carrier gives dist_util for a QUIC peer.
%%
%% `macula_quic:peername/1' returns the host as TEXT, a binary such as <<"127.0.0.1">>, not an `inet' address tuple.
%% `#net_address.address' is `{inet:ip_address(), Port}', and `family' names the tuple's kind. The carrier put the
%% binary in as it came and always said `inet', so an IPv6 peer, which is every peer on the AAAA-only fleet, was
%% described as IPv4 with an address nothing in OTP can read.
-module(macula_dist_net_address_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/net_address.hrl").

an_ipv4_peer_is_an_inet_tuple_test() ->
    ?assertMatch(#net_address{address = {{127, 0, 0, 1}, 4433}, family = inet, host = "a@h"},
                 macula_dist:make_net_address({ok, {<<"127.0.0.1">>, 4433}}, 'a@h')).

an_ipv6_peer_is_an_inet6_tuple_test() ->
    ?assertMatch(#net_address{address = {{16#2a01, 16#4f8, 0, 0, 0, 0, 0, 1}, 4433}, family = inet6},
                 macula_dist:make_net_address({ok, {<<"2a01:4f8::1">>, 4433}}, 'a@h')).

a_host_that_is_not_an_address_has_none_test() ->
    ?assertMatch(#net_address{address = undefined, host = "a@h"},
                 macula_dist:make_net_address({ok, {<<"not an address">>, 4433}}, 'a@h')).

an_unknown_peer_has_no_address_test() ->
    ?assertMatch(#net_address{address = undefined},
                 macula_dist:make_net_address({error, closed}, 'a@h')).

%% `inet:peername/1' on a TCP socket already answers with a tuple; that path is unchanged.
a_socket_peer_is_passed_through_test() ->
    ?assertMatch(#net_address{address = {{10, 0, 0, 2}, 9}, family = inet},
                 macula_dist:make_net_address({ok, {{10, 0, 0, 2}, 9}}, 'a@h')).
