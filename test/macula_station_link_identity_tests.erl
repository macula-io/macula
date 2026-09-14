%%%-------------------------------------------------------------------
%%% @doc Tests that a macula_station_link started without an identity
%%% generates one that passes the station's puzzle check, as the
%%% macula_client pool's default identity does.
%%%
%%% A station that enforces the check refuses a peer whose node id fails
%%% it, so the link's own fallback identity must pass it. Eight links are
%%% started, so a plain key passing by chance cannot make the test pass.
%%% The link's identity is read from its state, as
%%% macula_station_link_tests reads its peer.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_station_link_identity_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SEED, #{host => <<"127.0.0.1">>, port => 1}).
%% The identity field of macula_station_link's state record.
-define(IDENTITY_INDEX, 3).
-define(LINKS, 8).

identity_test_() ->
    {timeout, 30,
     [{"a link started without an identity gets one that passes the puzzle check",
       fun fallback_identity_passes_the_puzzle/0},
      {"a link started with an identity keeps it",
       fun given_identity_is_kept/0}]}.

fallback_identity_passes_the_puzzle() ->
    {ok, _} = application:ensure_all_started(macula),
    Links = [start_link(#{}) || _ <- lists:seq(1, ?LINKS)],
    Passes = [macula_identity:puzzle_valid(macula_identity:public(identity_of(L))) || L <- Links],
    [ok = macula_station_link:stop(L) || L <- Links],
    ?assertEqual(lists:duplicate(?LINKS, true), Passes).

given_identity_is_kept() ->
    {ok, _} = application:ensure_all_started(macula),
    Given = macula_identity:generate(),
    Link = start_link(#{identity => Given}),
    Kept = identity_of(Link),
    ok = macula_station_link:stop(Link),
    ?assertEqual(Given, Kept).

start_link(Opts) ->
    {ok, Pid} = macula_station_link:start_link(Opts#{seed => ?SEED, connect_timeout_ms => 2000}),
    Pid.

identity_of(Link) ->
    element(?IDENTITY_INDEX, sys:get_state(Link)).
