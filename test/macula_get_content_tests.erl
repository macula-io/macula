%%%-------------------------------------------------------------------
%%% @doc Regression: `macula:get_content/2' and `get_content_station/5'
%%% used to hand any `MCID' straight to `macula_content_transfer',
%%% whose `is_chunked/2' clauses assume a `<<1, 16#55, _/binary>>' or
%%% `<<1, 16#56, _/binary>>' shape and crash the spawned worker (and
%%% the calling gen_server) with a `FunctionClauseError' on anything
%%% else — reachable with attacker- or corruption-supplied input on
%%% any path that decodes bytes into an MCID before fetching it (a
%%% content-addressed image proxy, a stored reference, a share link).
%%% Pinned here at the boundary, before any pool interaction, so the
%%% valid-shape case doesn't need a live connection to test.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_get_content_tests).

-include_lib("eunit/include/eunit.hrl").

a_malformed_mcid_is_rejected_without_touching_the_pool_test() ->
    ?assertEqual({error, invalid_mcid}, macula:get_content(self(), <<"not-an-mcid">>)).

an_empty_mcid_is_rejected_test() ->
    ?assertEqual({error, invalid_mcid}, macula:get_content(self(), <<>>)).

a_wrong_codec_byte_is_rejected_test() ->
    ?assertEqual({error, invalid_mcid}, macula:get_content(self(), <<1, 16#57, 0:256>>)).

get_content_station_also_rejects_a_malformed_mcid_test() ->
    Result = macula:get_content_station(self(), <<"seed">>, <<"not-an-mcid">>, 1000),
    ?assertEqual({error, invalid_mcid}, Result).

%% The well-formed case (both codec bytes) already has live coverage
%% in macula_content_transfer_tests.erl and macula_download_tests.erl
%% — this suite is only about the rejection boundary this fix added.
