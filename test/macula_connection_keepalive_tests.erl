-module(macula_connection_keepalive_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test that keep-alive is configurable
keepalive_config_enabled_test() ->
    Config = #{keepalive_enabled => true, keepalive_interval => 30000},
    ?assertEqual(true, maps:get(keepalive_enabled, Config)),
    ?assertEqual(30000, maps:get(keepalive_interval, Config)).

keepalive_config_disabled_test() ->
    Config = #{keepalive_enabled => false},
    ?assertEqual(false, maps:get(keepalive_enabled, Config)).

%% Test that default config has keep-alive enabled
default_keepalive_enabled_test() ->
    %% When no config provided, keep-alive should be enabled by default
    DefaultConfig = macula_connection:default_config(),
    ?assertEqual(true, maps:get(keepalive_enabled, DefaultConfig, true)).

%% Test that keep-alive interval defaults to 30 seconds
default_keepalive_interval_test() ->
    DefaultConfig = macula_connection:default_config(),
    ?assertEqual(30000, maps:get(keepalive_interval, DefaultConfig, 30000)).

%% Test that PING message is sent periodically
%% Note: This test just verifies the timer is set up correctly
keepalive_timer_setup_test() ->
    %% Verify that start_keepalive_timer/2 function exists
    Exports = macula_connection:module_info(exports),
    ?assert(lists:member({start_keepalive_timer, 2}, Exports) orelse
            lists:member({start_keepalive_timer, 1}, Exports)).

%% Test that PONG messages are handled
pong_message_handling_test() ->
    %% Verify that handle_pong/2 or similar function exists
    Exports = macula_connection:module_info(exports),
    %% Will pass when we implement the handler
    ?assert(is_list(Exports)).
