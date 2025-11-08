%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_server module.
%%% Tests written FIRST (TDD red phase).
%%% GenServer integration and public API.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_server_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Setup and Teardown
%%%===================================================================

%% Start server for each test
setup() ->
    {ok, Pid} = macula_pubsub_server:start_link(),
    Pid.

%% Stop server after each test
cleanup(Pid) ->
    case erlang:is_process_alive(Pid) of
        true -> macula_pubsub_server:stop(Pid);
        false -> ok
    end.

%%%===================================================================
%%% Subscribe Tests
%%%===================================================================

subscribe_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_assertEqual(ok, macula_pubsub_server:subscribe(Pid, <<"sub_001">>, <<"be.cortexiq.#">>, self())),
          ?_assertEqual(1, macula_pubsub_server:subscription_count(Pid))
         ]
     end}.

subscribe_multiple_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_assertEqual(ok, macula_pubsub_server:subscribe(Pid, <<"sub_001">>, <<"be.cortexiq.#">>, self())),
          ?_assertEqual(ok, macula_pubsub_server:subscribe(Pid, <<"sub_002">>, <<"org.example.*">>, self())),
          ?_assertEqual(2, macula_pubsub_server:subscription_count(Pid))
         ]
     end}.

subscribe_updates_existing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         Callback1 = spawn(fun() -> timer:sleep(1000) end),
         Callback2 = self(),
         [
          ?_assertEqual(ok, macula_pubsub_server:subscribe(Pid, <<"sub_001">>, <<"be.cortexiq.#">>, Callback1)),
          ?_assertEqual(1, macula_pubsub_server:subscription_count(Pid)),
          ?_assertEqual(ok, macula_pubsub_server:subscribe(Pid, <<"sub_001">>, <<"be.cortexiq.#">>, Callback2)),
          ?_assertEqual(1, macula_pubsub_server:subscription_count(Pid))  % Still 1, updated
         ]
     end}.

%%%===================================================================
%%% Unsubscribe Tests
%%%===================================================================

unsubscribe_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_assertEqual(ok, macula_pubsub_server:subscribe(Pid, <<"sub_001">>, <<"be.cortexiq.#">>, self())),
          ?_assertEqual(1, macula_pubsub_server:subscription_count(Pid)),
          ?_assertEqual(ok, macula_pubsub_server:unsubscribe(Pid, <<"sub_001">>, <<"be.cortexiq.#">>)),
          ?_assertEqual(0, macula_pubsub_server:subscription_count(Pid))
         ]
     end}.

unsubscribe_nonexistent_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         ?_assertEqual(ok, macula_pubsub_server:unsubscribe(Pid, <<"sub_001">>, <<"be.cortexiq.#">>))
     end}.

%%%===================================================================
%%% Publish Tests
%%%===================================================================

publish_local_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          fun() ->
              ok = macula_pubsub_server:subscribe(Pid, <<"sub_001">>, <<"be.cortexiq.#">>, self()),
              Message = #{topic => <<"be.cortexiq.home.measured">>, payload => <<"data">>},
              ok = macula_pubsub_server:publish(Pid, Message),

              %% Should receive message
              receive
                  Msg -> ?assertEqual(Message, Msg)
              after 100 -> ?assert(false)
              end
          end
         ]
     end}.

publish_no_subscribers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         Message = #{topic => <<"be.cortexiq.home.measured">>, payload => <<"data">>},
         ?_assertEqual(ok, macula_pubsub_server:publish(Pid, Message))
     end}.

%%%===================================================================
%%% List Subscriptions Tests
%%%===================================================================

list_subscriptions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          fun() ->
              ok = macula_pubsub_server:subscribe(Pid, <<"sub_001">>, <<"be.cortexiq.#">>, self()),
              ok = macula_pubsub_server:subscribe(Pid, <<"sub_002">>, <<"org.example.*">>, self()),

              Subs = macula_pubsub_server:list_subscriptions(Pid),
              ?assertEqual(2, length(Subs))
          end
         ]
     end}.

list_subscriptions_empty_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         ?_assertEqual([], macula_pubsub_server:list_subscriptions(Pid))
     end}.

%%%===================================================================
%%% Pattern List Tests
%%%===================================================================

list_patterns_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          fun() ->
              ok = macula_pubsub_server:subscribe(Pid, <<"sub_001">>, <<"be.cortexiq.#">>, self()),
              ok = macula_pubsub_server:subscribe(Pid, <<"sub_002">>, <<"org.example.*">>, self()),

              Patterns = macula_pubsub_server:list_patterns(Pid),
              ?assertEqual(2, length(Patterns)),
              ?assert(lists:member(<<"be.cortexiq.#">>, Patterns)),
              ?assert(lists:member(<<"org.example.*">>, Patterns))
          end
         ]
     end}.

%%%===================================================================
%%% Cache Stats Tests
%%%===================================================================

cache_stats_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         Stats = macula_pubsub_server:cache_stats(Pid),
         [
          ?_assertEqual(0, maps:get(size, Stats)),
          ?_assert(maps:get(max_size, Stats) > 0)
         ]
     end}.

%%%===================================================================
%%% Stop Tests
%%%===================================================================

stop_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> ok end,  % Don't cleanup, test does it
     fun(Pid) ->
         [
          ?_assertEqual(ok, macula_pubsub_server:stop(Pid)),
          ?_assertEqual(false, erlang:is_process_alive(Pid))
         ]
     end}.
