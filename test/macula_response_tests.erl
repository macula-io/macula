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
      fun advertise_failure_is_surfaced/0]}.

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
