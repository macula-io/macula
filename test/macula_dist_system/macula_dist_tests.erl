%%%-------------------------------------------------------------------
%%% @doc Unit tests for macula_dist module.
%%%
%%% Tests the QUIC distribution carrier functionality.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/net_address.hrl").

%%%===================================================================
%%% Tests - Node Name Parsing
%%%===================================================================

splitname_test_() ->
    {"Node name splitting tests", [
        ?_assertEqual({4433, "192.168.1.100"}, macula_dist:splitname("4433@192.168.1.100")),
        ?_assertEqual({4433, "192.168.1.100"}, macula_dist:splitname('4433@192.168.1.100')),
        ?_assertEqual({8080, "localhost"}, macula_dist:splitname("8080@localhost")),
        ?_assertEqual({9000, "node1.example.com"}, macula_dist:splitname("9000@node1.example.com")),
        ?_assertEqual(false, macula_dist:splitname("invalid_name")),
        ?_assertEqual(false, macula_dist:splitname("@noport")),
        ?_assertEqual(false, macula_dist:splitname("abc@host")),  % non-numeric port
        ?_assertEqual(false, macula_dist:splitname("0@host")),    % port 0 is invalid
        ?_assertEqual(false, macula_dist:splitname("70000@host")) % port > 65535
    ]}.

is_node_name_test_() ->
    {"Node name validation tests", [
        ?_assert(macula_dist:is_node_name("4433@192.168.1.100")),
        ?_assert(macula_dist:is_node_name("8080@localhost")),
        ?_assertNot(macula_dist:is_node_name("invalid")),
        ?_assertNot(macula_dist:is_node_name("abc@host")),
        ?_assertNot(macula_dist:is_node_name(""))
    ]}.

%%%===================================================================
%%% Tests - Address Handling
%%%===================================================================

address_test_() ->
    {"Address creation tests", [
        ?_test(begin
            Addr = macula_dist:address(),
            ?assertMatch(#net_address{}, Addr),
            ?assertEqual(macula_dist, Addr#net_address.protocol),
            ?assertEqual(inet, Addr#net_address.family)
        end)
    ]}.

%%%===================================================================
%%% Tests - Childspecs
%%%===================================================================

childspecs_test_() ->
    {"Childspecs tests", [
        ?_assertEqual([], macula_dist:childspecs())
    ]}.

%%%===================================================================
%%% Tests - Close
%%%===================================================================

close_test_() ->
    {"Close function tests", [
        ?_assertEqual(ok, macula_dist:close(undefined)),
        ?_assertEqual(ok, macula_dist:close({undefined, undefined})),
        ?_assertEqual(ok, macula_dist:close(make_ref()))
    ]}.

%%%===================================================================
%%% Tests - Select
%%%===================================================================

select_test_() ->
    {"Select function tests", [
        ?_assertEqual(ok, macula_dist:select(undefined)),
        ?_assertEqual(ok, macula_dist:select(any_handle))
    ]}.
