%% EUnit tests for how a pool call moves from one link to the next:
%% only a CALL that never went out is tried on another link.
-module(macula_client_call_first_success_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).

call_first_success_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun call_falls_through_to_next_connected_link/0,
      fun a_pool_call_that_timed_out_after_its_write_started_is_not_tried_on_another_link/0]}.

setup() ->
    meck:new(macula_station_link, [passthrough]),
    meck:expect(macula_station_link, is_connected, fun(_Link) -> true end),
    ok.

teardown(_) ->
    meck:unload(macula_station_link).

%% A link whose CALL was never sent passes the call to the next link.
call_falls_through_to_next_connected_link() ->
    Dead = link(),
    Live = link(),
    answer(#{Dead => {error, not_connected}, Live => {ok, <<"from live">>}}),
    ?assertEqual({ok, <<"from live">>},
                 macula_client:call_first_success([Dead, Live], ?REALM, <<"echo.v1">>,
                                                  #{}, 1_000)),
    ?assertEqual(1, calls_on(Live)),
    stop([Dead, Live]).

%% A CALL that may have reached its provider is never sent again, so a
%% provider never runs one call twice.
a_pool_call_that_timed_out_after_its_write_started_is_not_tried_on_another_link() ->
    Silent = link(),
    Live = link(),
    answer(#{Silent => {error, timeout}, Live => {ok, <<"from live">>}}),
    ?assertEqual({error, timeout},
                 macula_client:call_first_success([Silent, Live], ?REALM, <<"echo.v1">>,
                                                  #{}, 1_000)),
    ?assertEqual(0, calls_on(Live)),
    stop([Silent, Live]).

link() ->
    spawn(fun() -> receive stop -> ok end end).

stop(Links) ->
    [Link ! stop || Link <- Links],
    ok.

answer(Answers) ->
    meck:expect(macula_station_link, call,
                fun(Link, _Realm, _Proc, _Payload, _Tmo) -> maps:get(Link, Answers) end).

calls_on(Link) ->
    meck:num_calls(macula_station_link, call, [Link, '_', '_', '_', '_']).
