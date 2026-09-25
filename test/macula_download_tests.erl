%%%-------------------------------------------------------------------
%%% @doc Tests for macula_download: a supervised fetch (D27). Each download
%%% runs on the fetch and fact functions its start options give, so no test
%%% replaces a module and no mesh is needed.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_download_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).
-define(MCID, <<2, 16#55, 0:384>>).

-behaviour(macula_download).
-export([init/1, handle_downloaded/2]).

init(Parent) -> {ok, Parent}.

handle_downloaded(Result, Parent) ->
    Parent ! {downloaded, Result},
    {stop, normal, Parent}.

download_test_() ->
    [{spawn, Test} || Test <- [fun a_fetch_reports_its_bytes_and_the_facts/0,
                               fun a_failed_fetch_still_announces_completion/0,
                               fun a_cancel_before_the_fetch_resolves_stops_it/0,
                               fun fetch_opts_and_the_realm_reach_the_fetch/0,
                               fun functions_of_another_shape_are_refused/0]].

a_fetch_reports_its_bytes_and_the_facts() ->
    process_flag(trap_exit, true),
    {ok, _} = macula_download:start_link(?MODULE, self(), ?REALM, ?MCID, self(),
                                         opts(fun(_P, _R, _M, _O) -> {ok, <<"hello">>} end)),
    ?assertEqual({downloaded, {ok, <<"hello">>}}, wait_msg(downloaded)),
    ?assertMatch([{<<"sharing.get_started_v1">>, #{mcid := ?MCID, chunked := false}},
                  {<<"sharing.get_completed_v1">>, #{outcome := completed, size := 5}}],
                 facts()).

a_failed_fetch_still_announces_completion() ->
    process_flag(trap_exit, true),
    {ok, _} = macula_download:start_link(?MODULE, self(), ?REALM, ?MCID, self(),
                                         opts(fun(_P, _R, _M, _O) -> {error, not_shared} end)),
    ?assertEqual({downloaded, {error, not_shared}}, wait_msg(downloaded)),
    ?assertMatch([_, {_, #{outcome := failed, reason := not_shared}}], facts()).

%% A cancel stops the fetch in flight: its worker, and with it the streams it opened.
a_cancel_before_the_fetch_resolves_stops_it() ->
    process_flag(trap_exit, true),
    Test = self(),
    Held = fun(_P, _R, _M, _O) -> Test ! {fetching, self()}, receive never -> ok end end,
    {ok, Pid} = macula_download:start_link(?MODULE, self(), ?REALM, ?MCID, self(), opts(Held)),
    {fetching, Worker} = wait_msg(fetching),
    Mon = erlang:monitor(process, Worker),
    ok = macula_download:cancel(Pid),
    receive {'DOWN', Mon, process, Worker, _} -> ok after 2_000 -> error(fetch_not_stopped) end,
    ?assertMatch([_, {_, #{outcome := cancelled}}], facts()).

fetch_opts_and_the_realm_reach_the_fetch() ->
    process_flag(trap_exit, true),
    Test = self(),
    {ok, _} = macula_download:start_link(?MODULE, self(), ?REALM, ?MCID, self(),
                                         (opts(fun(_P, R, M, O) -> Test ! {asked, R, M, O}, {ok, <<>>} end))
                                             #{fetch_opts => #{max_bytes => 10}}),
    ?assertEqual({asked, ?REALM, ?MCID, #{max_bytes => 10}}, wait_msg(asked)).

functions_of_another_shape_are_refused() ->
    ?assertError(function_clause,
                 macula_download:start_link(?MODULE, self(), ?REALM, ?MCID, self(), #{fetch => fun(_) -> ok end})).

%%%===================================================================
%%% Helpers
%%%===================================================================

opts(Fetch) ->
    Test = self(),
    #{fetch => Fetch, fact_publish => fun(_P, _R, Topic, Payload) -> Test ! {fact, Topic, Payload}, ok end}.

facts() ->
    lists:reverse(facts([])).

facts(Acc) ->
    receive {fact, Topic, Payload} -> facts([{Topic, Payload} | Acc])
    after 200 -> Acc
    end.

wait_msg(Tag) ->
    receive
        {Tag, _} = M -> M;
        {Tag, _, _, _} = M -> M;
        Tag -> Tag
    after 2_000 -> error({no_message, Tag})
    end.
