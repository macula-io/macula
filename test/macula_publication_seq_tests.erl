%% EUnit tests for macula_publication_seq: all publications a node signs with one key share one counter, numbered from
%% the wall clock in microseconds (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, Publications).
-module(macula_publication_seq_tests).

-include_lib("eunit/include/eunit.hrl").

-define(KEY, <<1:256>>).
-define(OTHER_KEY, <<2:256>>).
-define(TOPIC, <<"seq.shared_v1">>).

%% Several publishers holding one key draw one rising sequence, with no number repeated or skipped.
publishers_holding_one_key_draw_one_rising_sequence_test() ->
    {Owner, Table} = counter(seq_test_shared, #{}),
    Drawers = [spawn_drawer(Table, 250) || _ <- lists:seq(1, 4)],
    Numbers = lists:sort(lists:append([drawn(D) || D <- Drawers])),
    ?assertEqual(lists:seq(hd(Numbers), hd(Numbers) + 999), Numbers),
    gen_server:stop(Owner).

the_first_number_is_the_wall_clock_in_microseconds_test() ->
    {Owner, Table} = counter(seq_test_wall_clock, #{}),
    Before = erlang:system_time(microsecond),
    First = macula_publication_seq:next(Table, ?KEY),
    After = erlang:system_time(microsecond),
    ?assert(Before =< First andalso First =< After),
    gen_server:stop(Owner).

two_keys_count_independently_test() ->
    {Owner, Table} = counter(seq_test_two_keys, #{clock => fun() -> 1000 end}),
    ?assertEqual(1000, macula_publication_seq:next(Table, ?KEY)),
    ?assertEqual(1001, macula_publication_seq:next(Table, ?KEY)),
    ?assertEqual(1000, macula_publication_seq:next(Table, ?OTHER_KEY)),
    gen_server:stop(Owner).

%% When the table is lost with its owner, the clock seeds the counter again, above every number handed out before.
a_lost_counter_starts_again_above_the_numbers_handed_out_test() ->
    {Owner1, Table} = counter(seq_test_restart, #{clock => fun() -> 1000 end}),
    Before = draw(Table, ?KEY, 5),
    gen_server:stop(Owner1),
    ?assertEqual(undefined, ets:info(Table)),
    {Owner2, Table} = counter(seq_test_restart, #{clock => fun() -> 2000 end}),
    After = macula_publication_seq:next(Table, ?KEY),
    ?assert(After > lists:max(Before)),
    ?assertEqual(2000, After),
    gen_server:stop(Owner2).

%% A pool and a pubsub server signing with one key draw from one counter: a pool publish between two server publishes
%% takes the number between them, even when no link accepts it.
a_pool_and_a_pubsub_server_with_one_key_share_one_counter_test_() ->
    {timeout, 30, fun() ->
        {ok, _} = application:ensure_all_started(macula),
        {ok, Profile} = macula_crypto_profile:configured(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        Realm = crypto:strong_rand_bytes(32),
        {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
        {ok, Server} = hecate_pubsub_server:start_link(#{realm => Realm, identity => Key}),
        First = published_seq(hecate_pubsub_server:publish(Server, ?TOPIC, <<"one">>), Profile),
        {error, _} = macula_client:publish(Pool, Realm, ?TOPIC, <<"two">>, #{}),
        Third = published_seq(hecate_pubsub_server:publish(Server, ?TOPIC, <<"three">>), Profile),
        ?assertEqual(First + 2, Third),
        hecate_pubsub_server:stop(Server),
        ok = macula_client:close(Pool)
    end}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

counter(Table, Options) ->
    {ok, Owner} = macula_publication_seq:start_link(Options#{table => Table}),
    {Owner, Table}.

spawn_drawer(Table, Count) ->
    Test = self(),
    spawn_link(fun() -> Test ! {drawn, self(), draw(Table, ?KEY, Count)} end).

draw(Table, Key, Count) ->
    [macula_publication_seq:next(Table, Key) || _ <- lists:seq(1, Count)].

drawn(Drawer) ->
    receive
        {drawn, Drawer, Numbers} -> Numbers
    after 5000 ->
        erlang:error({no_numbers_from, Drawer})
    end.

published_seq({Event, _Matched}, Profile) ->
    {ok, #{seq := Seq}} = macula_frame:verify_publication(Event, Profile, erlang:system_time(millisecond)),
    Seq.
