%% EUnit tests for `macula_client' (the pool).
%%
%% These tests exercise the pool's bookkeeping end-to-end without a
%% live QUIC station. The pool spawns real `macula_station_link'
%% workers against unreachable seeds (port 1) — every link stays in
%% the disconnected state. We can still test:
%%
%%   - Pool startup with multiple seeds
%%   - Subscribe/unsubscribe state machine + topic_index ref counting
%%   - Multiple-consumers-one-topic fan-out registration
%%   - Subscriber DOWN tears down the sub
%%   - Pool close emits macula_event_gone to all subscribers
%%   - publish/5 with zero healthy links → transient error
%%   - Synthetic EVENT injected at the pool fans to local subs
%%   - Inbound EVENT dedup across simulated station copies
-module(macula_client_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).
-define(SEED1, #{host => <<"127.0.0.1">>, port => 1}).
-define(SEED2, #{host => <<"127.0.0.1">>, port => 2}).

%%------------------------------------------------------------------
%% Boot
%%------------------------------------------------------------------

connect_with_no_seeds_returns_pool_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([], #{}),
    ?assert(is_process_alive(Pool)),
    ok = macula_client:close(Pool),
    ok.

connect_with_unreachable_seed_returns_pool_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([?SEED1], #{}),
    ?assert(is_process_alive(Pool)),
    ok = macula_client:close(Pool),
    ok.

%%------------------------------------------------------------------
%% subscribe/5 + unsubscribe/2 bookkeeping
%%------------------------------------------------------------------

subscribe_returns_subref_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([], #{}),
    {ok, SubRef} = macula_client:subscribe(Pool, ?REALM,
                                           <<"x.v1">>, self(), #{}),
    ?assert(is_reference(SubRef)),
    ok = macula_client:close(Pool),
    ok.

unsubscribe_is_idempotent_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([], #{}),
    {ok, SubRef} = macula_client:subscribe(Pool, ?REALM,
                                           <<"x.v1">>, self(), #{}),
    ok = macula_client:unsubscribe(Pool, SubRef),
    ok = macula_client:unsubscribe(Pool, SubRef),
    ok = macula_client:unsubscribe(Pool, make_ref()),
    ok = macula_client:close(Pool),
    ok.

%%------------------------------------------------------------------
%% Synthetic EVENT injection — fan-out to local subscribers
%%------------------------------------------------------------------

inbound_event_fans_to_local_subscriber_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         Topic = <<"weather.measured_v1">>,
         {ok, SubRef} = macula_client:subscribe(Pool, ?REALM,
                                                Topic, self(), #{}),
         %% Inject a synthetic inbound event as if a station_link
         %% had received it. The pool's handle_info matches on the
         %% {macula_event, _, Topic, Payload, Meta} shape.
         Pool ! {macula_event, make_ref(), Topic,
                 #{temp => 20},
                 #{realm => ?REALM,
                   publisher => <<1:256>>,
                   seq => 1,
                   delivered_via => direct}},
         receive
             {macula_event, R, T, P, _Meta} ->
                 ?assertEqual(SubRef, R),
                 ?assertEqual(Topic, T),
                 ?assertEqual(#{temp => 20}, P)
         after 2_000 -> erlang:error(no_event_delivered)
         end,
         ok = macula_client:close(Pool),
         ok
     end}.

%%------------------------------------------------------------------
%% Inbound EVENT dedup across simulated station copies
%%------------------------------------------------------------------

inbound_event_dedup_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         Topic = <<"x.v1">>,
         {ok, SubRef} = macula_client:subscribe(Pool, ?REALM,
                                                Topic, self(), #{}),
         %% Three identical events (same realm/publisher/seq).
         Meta = #{realm => ?REALM,
                  publisher => <<1:256>>,
                  seq => 7,
                  delivered_via => direct},
         Pool ! {macula_event, make_ref(), Topic, hello, Meta},
         Pool ! {macula_event, make_ref(), Topic, hello, Meta},
         Pool ! {macula_event, make_ref(), Topic, hello, Meta},
         receive
             {macula_event, SubRef, _, hello, _} -> ok
         after 1_000 -> erlang:error(no_first_delivery)
         end,
         %% No second delivery.
         receive
             {macula_event, SubRef, _, hello, _} ->
                 erlang:error(duplicate_delivered)
         after 200 -> ok
         end,
         ok = macula_client:close(Pool),
         ok
     end}.

%%------------------------------------------------------------------
%% Multiple consumers same topic → both get fan-out
%%------------------------------------------------------------------

multiple_consumers_same_topic_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         Topic = <<"x.v1">>,
         Test = self(),
         Sub1 = spawn(fun() ->
             {ok, R} = macula_client:subscribe(Pool, ?REALM,
                                               Topic, self(), #{}),
             Test ! {ready, self(), R},
             receive
                 {macula_event, _, _, P, _} -> Test ! {got, self(), P}
             after 2_000 -> Test ! {timeout, self()}
             end
         end),
         Sub2 = spawn(fun() ->
             {ok, R} = macula_client:subscribe(Pool, ?REALM,
                                               Topic, self(), #{}),
             Test ! {ready, self(), R},
             receive
                 {macula_event, _, _, P, _} -> Test ! {got, self(), P}
             after 2_000 -> Test ! {timeout, self()}
             end
         end),
         _R1 = wait_ready(Sub1),
         _R2 = wait_ready(Sub2),
         Pool ! {macula_event, make_ref(), Topic,
                 broadcast,
                 #{realm => ?REALM,
                   publisher => <<1:256>>,
                   seq => 1,
                   delivered_via => direct}},
         receive {got, Sub1, broadcast} -> ok
         after 1_000 -> erlang:error(sub1_no_event)
         end,
         receive {got, Sub2, broadcast} -> ok
         after 1_000 -> erlang:error(sub2_no_event)
         end,
         ok = macula_client:close(Pool),
         ok
     end}.

wait_ready(Pid) ->
    receive
        {ready, Pid, R} -> R
    after 1_000 -> erlang:error({sub_not_ready, Pid})
    end.

%%------------------------------------------------------------------
%% Subscriber pid dies → subscription cleared
%%------------------------------------------------------------------

subscriber_down_drops_subscription_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         Topic = <<"x.v1">>,
         Test = self(),
         Sub = spawn(fun() ->
             {ok, R} = macula_client:subscribe(Pool, ?REALM,
                                               Topic, self(), #{}),
             Test ! {ready, self(), R}
         end),
         _R = wait_ready(Sub),
         %% Subscriber has exited. Give the pool a beat to process
         %% the DOWN message.
         timer:sleep(50),
         %% Inject an event — must NOT be delivered anywhere
         %% (consumer is dead, nobody else subscribed).
         Pool ! {macula_event, make_ref(), Topic, ghost,
                 #{realm => ?REALM,
                   publisher => <<1:256>>,
                   seq => 1,
                   delivered_via => direct}},
         receive
             {macula_event, _, _, ghost, _} ->
                 erlang:error(event_to_dead_subscriber)
         after 200 -> ok
         end,
         ok = macula_client:close(Pool),
         ok
     end}.

%%------------------------------------------------------------------
%% Pool close emits macula_event_gone to every subscriber
%%------------------------------------------------------------------

close_notifies_subscribers_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         {ok, SubRef} = macula_client:subscribe(Pool, ?REALM,
                                                <<"x.v1">>, self(), #{}),
         ok = macula_client:close(Pool),
         %% Pattern-match SubRef in the receive so stale event_gone
         %% messages from earlier tests in the same eunit process
         %% are ignored.
         receive
             {macula_event_gone, SubRef, Reason} ->
                 ?assertEqual(pool_closed, Reason)
         after 2_000 -> erlang:error(no_event_gone)
         end,
         ok
     end}.

%%------------------------------------------------------------------
%% publish/5 with zero spawned links → transient error
%%------------------------------------------------------------------

publish_with_no_seeds_is_transient_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         R = macula_client:publish(Pool, ?REALM,
                                   <<"x.v1">>, hello, #{}),
         ?assertEqual({error, {transient, no_healthy_station}}, R),
         ok = macula_client:close(Pool),
         ok
     end}.
