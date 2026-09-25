%%%-------------------------------------------------------------------
%%% @doc Tests for macula_feeder: a supervised share (D27). Each feeder runs
%%% on the share, unshare and fact functions its start options give, so no
%%% test replaces a module and no mesh is needed.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_feeder_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).

-behaviour(macula_feeder).
-export([init/1, handle_fed/2]).

init(Parent) -> {ok, Parent}.

handle_fed(Result, Parent) ->
    Parent ! {fed, Result},
    {stop, normal, Parent}.

feeder_test_() ->
    [{spawn, Test} || Test <- [fun a_share_reports_its_root_and_the_facts/0,
                               fun a_large_share_reports_chunked/0,
                               fun a_failed_share_still_announces_completion/0,
                               fun a_cancel_before_the_share_resolves_withdraws_it/0,
                               fun share_opts_reach_the_share/0,
                               fun functions_of_another_shape_are_refused/0]].

a_share_reports_its_root_and_the_facts() ->
    process_flag(trap_exit, true),
    Bytes = <<"small">>,
    MCID = <<2, 16#55, (crypto:hash(sha384, Bytes))/binary>>,
    {ok, _} = macula_feeder:start_link(?MODULE, self(), ?REALM, Bytes, self(),
                                       opts(fun(_P, _R, _B, _O) -> {ok, MCID} end)),
    ?assertEqual({fed, {ok, MCID}}, wait_msg(fed)),
    ?assertMatch([{<<"sharing.put_started_v1">>, #{size := 5}},
                  {<<"sharing.put_completed_v1">>, #{outcome := completed, mcid := MCID, chunked := false}}],
                 facts()).

a_large_share_reports_chunked() ->
    process_flag(trap_exit, true),
    MCID = <<2, 16#56, 0:384>>,
    {ok, _} = macula_feeder:start_link(?MODULE, self(), ?REALM, <<"x">>, self(),
                                       opts(fun(_P, _R, _B, _O) -> {ok, MCID} end)),
    ?assertEqual({fed, {ok, MCID}}, wait_msg(fed)),
    ?assertMatch([_, {_, #{outcome := completed, chunked := true}}], facts()).

a_failed_share_still_announces_completion() ->
    process_flag(trap_exit, true),
    {ok, _} = macula_feeder:start_link(?MODULE, self(), ?REALM, <<"x">>, self(),
                                       opts(fun(_P, _R, _B, _O) -> {error, refused} end)),
    ?assertEqual({fed, {error, refused}}, wait_msg(fed)),
    ?assertMatch([_, {_, #{outcome := failed, reason := refused}}], facts()).

%% A cancel landing while the share is in flight withdraws it: the root is known from the bytes before anything is
%% sent, so no content stays shared behind the caller's back.
a_cancel_before_the_share_resolves_withdraws_it() ->
    process_flag(trap_exit, true),
    Test = self(),
    Bytes = <<"held back">>,
    Held = fun(_P, _R, _B, _O) -> Test ! share_started, receive never -> ok end end,
    {ok, Pid} = macula_feeder:start_link(?MODULE, self(), ?REALM, Bytes, self(),
                                         (opts(Held))#{unshare => fun(_P, R, M) -> Test ! {unshared, R, M}, ok end}),
    share_started = wait_msg(share_started),
    ok = macula_feeder:cancel(Pid),
    ?assertEqual({unshared, ?REALM, <<2, 16#55, (crypto:hash(sha384, Bytes))/binary>>}, wait_msg(unshared)),
    ?assertMatch([_, {_, #{outcome := cancelled}}], facts()).

share_opts_reach_the_share() ->
    process_flag(trap_exit, true),
    Test = self(),
    {ok, _} = macula_feeder:start_link(?MODULE, self(), ?REALM, <<"x">>, self(),
                                       (opts(fun(_P, _R, _B, O) -> Test ! {opts, O}, {ok, <<2, 16#55, 0:384>>} end))
                                           #{share_opts => #{org => <<"acme">>}}),
    ?assertEqual({opts, #{org => <<"acme">>}}, wait_msg(opts)).

functions_of_another_shape_are_refused() ->
    ?assertError(function_clause,
                 macula_feeder:start_link(?MODULE, self(), ?REALM, <<"x">>, self(),
                                          #{share => fun(_) -> ok end})).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Start options: the given share, an unshare that does nothing, and a fact publish recording each fact here.
opts(Share) ->
    Test = self(),
    #{share => Share,
      unshare => fun(_P, _R, _M) -> ok end,
      fact_publish => fun(_P, _R, Topic, Payload) -> Test ! {fact, Topic, Payload}, ok end}.

facts() ->
    lists:reverse(facts([])).

facts(Acc) ->
    receive {fact, Topic, Payload} -> facts([{Topic, Payload} | Acc])
    after 200 -> Acc
    end.

wait_msg(Tag) ->
    receive
        {Tag, _} = M -> M;
        {Tag, _, _} = M -> M;
        Tag -> Tag
    after 2_000 -> error({no_message, Tag})
    end.
