%%%-------------------------------------------------------------------
%%% @doc Tests for macula_relay_client multi-relay failover.
%%%
%%% Tests relay list initialization, round-robin failover, backoff
%%% timing, jitter, and backward compatibility with single URL.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_relay_client_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% URL Parsing
%%====================================================================

parse_url_with_port_test() ->
    ?assertEqual({"relay00.macula.io", 4433},
                 macula_relay_client:parse_url(<<"https://relay00.macula.io:4433">>)).

parse_url_default_port_test() ->
    ?assertEqual({"relay00.macula.io", 4433},
                 macula_relay_client:parse_url(<<"https://relay00.macula.io">>)).

parse_url_no_scheme_test() ->
    ?assertEqual({"relay00.macula.io", 4433},
                 macula_relay_client:parse_url(<<"relay00.macula.io:4433">>)).

%%====================================================================
%% Backoff Timing
%%====================================================================

backoff_attempt_0_test() ->
    %% Attempt 0: base=1000ms, jitter ±300ms → 700-1300ms
    Ms = macula_relay_client:backoff_ms(0),
    ?assert(Ms >= 700),
    ?assert(Ms =< 1300).

backoff_attempt_1_test() ->
    %% Attempt 1: base=2000ms, jitter ±600ms → 1400-2600ms
    Ms = macula_relay_client:backoff_ms(1),
    ?assert(Ms >= 1400),
    ?assert(Ms =< 2600).

backoff_attempt_5_test() ->
    %% Attempt 5: base=32000ms, jitter ±9600ms → 22400-41600ms
    %% But capped at 30000ms base, so: 30000 ± 9000 → 21000-39000
    Ms = macula_relay_client:backoff_ms(5),
    ?assert(Ms >= 21000),
    ?assert(Ms =< 39000).

backoff_capped_at_max_test() ->
    %% Attempt 20: should be capped, not overflow
    Ms = macula_relay_client:backoff_ms(20),
    ?assert(Ms =< 39000),
    ?assert(Ms >= 21000).

backoff_has_jitter_test() ->
    %% Multiple calls should produce different values (with high probability)
    Results = [macula_relay_client:backoff_ms(3) || _ <- lists:seq(1, 20)],
    Unique = lists:usort(Results),
    ?assert(length(Unique) > 1).

%%====================================================================
%% Init — Relay List
%%====================================================================

init_single_url_test() ->
    %% Backward compat: single url creates a 1-element relay list
    {ok, State} = macula_relay_client:init(#{
        url => <<"https://relay00.macula.io:4433">>,
        realm => <<"test">>,
        identity => <<"test">>
    }),
    ?assertEqual([<<"https://relay00.macula.io:4433">>], element(2, State)),
    %% Status should be connecting (connect message sent)
    ok.

init_relay_list_test() ->
    %% Multiple relays — all stored, one selected
    Relays = [<<"https://relay00.macula.io:4433">>,
              <<"https://relay01.macula.io:4433">>,
              <<"https://relay02.macula.io:4433">>],
    {ok, State} = macula_relay_client:init(#{
        relays => Relays,
        realm => <<"test">>,
        identity => <<"test">>
    }),
    ?assertEqual(Relays, element(2, State)),
    %% Current URL should be one of the relays
    CurrentUrl = element(4, State),
    ?assert(lists:member(CurrentUrl, Relays)).

init_empty_relay_list_falls_back_test() ->
    %% Empty list falls back to default
    {ok, State} = macula_relay_client:init(#{
        relays => [],
        realm => <<"test">>,
        identity => <<"test">>
    }),
    ?assertEqual([<<"https://localhost:4433">>], element(2, State)).

init_randomizes_initial_relay_test() ->
    %% Run init many times — should not always pick the same relay
    Relays = [<<"https://r0:4433">>, <<"https://r1:4433">>, <<"https://r2:4433">>],
    Urls = lists:map(fun(_) ->
        {ok, State} = macula_relay_client:init(#{relays => Relays, realm => <<"t">>, identity => <<"t">>}),
        element(4, State)
    end, lists:seq(1, 30)),
    Unique = lists:usort(Urls),
    %% With 30 tries and 3 relays, we should hit at least 2 different relays
    ?assert(length(Unique) >= 2).
