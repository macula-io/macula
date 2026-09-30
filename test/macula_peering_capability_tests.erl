%% The capability bits a node declares in CONNECT and HELLO, by name. A frame that an older node would refuse, and
%% then close the connection over, is sent only to a peer that declares the bit for it (macula#59).
-module(macula_peering_capability_tests).

-include_lib("eunit/include/eunit.hrl").

bits_are_fixed_and_distinct_test() ->
    ?assertEqual(1, macula_peering:capability_bit(station)),
    ?assertEqual(2, macula_peering:capability_bit(swim_indirect)).

a_peer_declares_swim_indirect_only_with_its_bit_test() ->
    ?assert(macula_peering:has_capability(swim_indirect, 2#11)),
    ?assert(macula_peering:has_capability(swim_indirect, 2#10)),
    ?assertNot(macula_peering:has_capability(swim_indirect, 2#01)),
    ?assertNot(macula_peering:has_capability(swim_indirect, 0)).
