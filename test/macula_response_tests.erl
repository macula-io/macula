%%%-------------------------------------------------------------------
%%% @doc Tests for macula_response.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_response_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_response).
-export([init/1, handle_request/2]).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(fail) -> {stop, init_failed};
init(_Args) -> {ok, #{}}.

handle_request(#{a := A, b := B}, State) ->
    {reply, #{result => A + B}, State};
handle_request(#{boom := true}, _State) ->
    error(boom);
handle_request(bad, State) ->
    {error, invalid_payload, State}.

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    meck:new(macula, [passthrough]),
    meck:expect(macula, advertise,
                fun(_Pool, _Realm, _Proc, Handler, _Opts) ->
                    persistent_term:put({?MODULE, handler}, Handler), ok
                end),
    meck:expect(macula, unadvertise, fun(_Pool, _Realm, _Proc) -> ok end),
    meck:expect(macula, publish, fun(_Pool, _Realm, _Topic, _Payload) -> ok end),
    ok.

teardown(_) ->
    persistent_term:erase({?MODULE, handler}),
    catch persistent_term:erase({?MODULE, advertised_opts}),
    meck:unload(macula).

captured_handler() ->
    persistent_term:get({?MODULE, handler}).

%%%===================================================================
%%% Tests
%%%===================================================================

response_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun replies_and_publishes_lifecycle/0,
      fun error_reply_is_surfaced/0,
      fun crash_propagates_to_caller/0,
      fun advertise_failure_is_surfaced/0,
      fun advertise_direct_forwards_opts_to_advertise/0,
      fun reuse_sup_resends_advertise_without_a_new_supervisor/0]}.

%% A station's wire-level registration for a procedure is tied to the
%% connection that sent it, and does not survive that connection being
%% replaced -- a periodic re-advertise is the only way to keep it
%% current. `reuse_sup' is what makes that safe: without it, every
%% re-advertise call starts a fresh factory supervisor, leaking one per
%% tick forever.
reuse_sup_resends_advertise_without_a_new_supervisor() ->
    {ok, Sup1} = macula_response:advertise(pool, <<0:256>>, <<"math.add_v1">>,
                                           ?MODULE, []),
    {ok, Sup2} = macula_response:advertise(pool, <<0:256>>, <<"math.add_v1">>,
                                           ?MODULE, [], #{reuse_sup => Sup1}),
    ?assertEqual(Sup1, Sup2),
    ?assertEqual(2, meck:num_calls(macula, advertise,
                                   [pool, <<0:256>>, <<"math.add_v1">>, '_', '_'])).

%% Regression test for a real bug found while auditing `macula_streamer'
%% for the same pattern (PLAN_PUSH_UPLOAD.md Phase 6 fixed it there;
%% `macula_response' had the identical bug, unfixed): `advertise_direct/7'
%% used to call the arity-5 `advertise/5' (which always defaults `Opts' to
%% `#{}'), silently discarding whatever `announce'/`auth' the caller passed
%% in `Opts' — a direct-dial-advertised procedure could never override
%% either, with no error anywhere to say so.
advertise_direct_forwards_opts_to_advertise() ->
    meck:expect(macula, advertise,
                fun(_Pool, _Realm, _Proc, Handler, Opts) ->
                    persistent_term:put({?MODULE, handler}, Handler),
                    persistent_term:put({?MODULE, advertised_opts}, Opts),
                    ok
                end),
    meck:new(macula_direct_dial, [passthrough]),
    meck:expect(macula_direct_dial, publish_advertisement,
                fun(_Pool, _Realm, _Proc, _Identity, _Opts) -> ok end),
    Identity = macula_identity:generate(),

    {ok, _Sup} = macula_response:advertise_direct(pool, <<0:256>>, <<"math.add_v1">>,
                                                   ?MODULE, [], Identity,
                                                   #{announce => false}),
    ?assertEqual(#{announce => false}, persistent_term:get({?MODULE, advertised_opts})),
    meck:unload(macula_direct_dial).

replies_and_publishes_lifecycle() ->
    {ok, _Sup} = macula_response:advertise(pool, <<0:256>>, <<"math.add_v1">>,
                                            ?MODULE, []),
    Handler = captured_handler(),
    ?assertEqual({ok, #{result => 5}}, Handler(#{a => 2, b => 3})),
    ?assertEqual([<<"rpc.received_v1">>, <<"rpc.replied_v1">>], topics()).

error_reply_is_surfaced() ->
    {ok, _Sup} = macula_response:advertise(pool, <<0:256>>, <<"math.add_v1">>,
                                            ?MODULE, []),
    Handler = captured_handler(),
    ?assertEqual({error, invalid_payload}, Handler(bad)),
    ?assertMatch(#{outcome := failed, reason := invalid_payload}, replied_payload()).

crash_propagates_to_caller() ->
    {ok, _Sup} = macula_response:advertise(pool, <<0:256>>, <<"math.add_v1">>,
                                            ?MODULE, []),
    Handler = captured_handler(),
    ?assertExit(_, Handler(#{boom => true})).

advertise_failure_is_surfaced() ->
    meck:expect(macula, advertise,
                fun(_Pool, _Realm, _Proc, _Handler, _Opts) -> {error, no_healthy_link} end),
    ?assertEqual({error, no_healthy_link},
                 macula_response:advertise(pool, <<0:256>>, <<"math.add_v1">>,
                                            ?MODULE, [])).

%%%===================================================================
%%% Helpers
%%%===================================================================

topics() ->
    [T || {_, {macula, publish, [_Pool, _Realm, T, _Payload]}, ok} <- meck:history(macula)].

replied_payload() ->
    [{_, {macula, publish, [_, _, _, Payload]}, ok}] =
        [E || {_, {macula, publish, [_, _, T, _]}, ok} = E <- meck:history(macula),
              T =:= <<"rpc.replied_v1">>],
    Payload.
