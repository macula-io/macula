%% EUnit tests for macula_provider_standing: the streams that follow one provider under one org key share one reading
%% of its delegation slot per re-check (macula#120).
-module(macula_provider_standing_tests).

-include_lib("eunit/include/eunit.hrl").

%% Each test reads under a key of its own, since the table may be the running node's, kept by macula_root.
provider_standing_test_() ->
    {foreach,
     fun() -> owned(ets:whereis(macula_provider_standing)) end,
     fun(Owner) -> stopped(Owner) end,
     [{"many followers of one provider read its slot once per re-check",
       fun many_followers_read_once_per_recheck/0},
      {"a reading older than the re-check is read again, by one follower",
       fun a_stale_reading_is_read_again_once/0},
      {"followers asking during a read wait for its answer",
       fun followers_asking_during_a_read_get_its_answer/0},
      {"a read that raises answers lookup_failed",
       fun a_raising_read_answers_lookup_failed/0},
      {"a reading older than half the re-check is read again",
       fun a_reading_past_half_the_recheck_is_read_again/0},
      {"a claim whose reader died is claimed again after the await",
       fun an_orphaned_claim_is_claimed_again/0},
      {"another provider or org key has its own reading",
       fun another_key_has_its_own_reading/0}]}.

many_followers_read_once_per_recheck() ->
    Key = key(),
    Counter = counters:new(1, []),
    Read = counted_read(Counter, {error, withdrawn}),
    Self = self(),
    _ = [spawn(fun() -> Self ! {read, macula_provider_standing:read(Key, Read, 60_000)} end) || _ <- lists:seq(1, 20)],
    Answers = [receive {read, A} -> A after 1_000 -> timeout end || _ <- lists:seq(1, 20)],
    ?assertEqual(lists:duplicate(20, {error, withdrawn}), Answers),
    ?assertEqual(1, counters:get(Counter, 1)).

a_stale_reading_is_read_again_once() ->
    Key = key(),
    Counter = counters:new(1, []),
    Read = counted_read(Counter, ok),
    ok = macula_provider_standing:read(Key, Read, 100),
    timer:sleep(150),
    _ = [macula_provider_standing:read(Key, Read, 100) || _ <- lists:seq(1, 10)],
    ?assertEqual(2, counters:get(Counter, 1)).

%% The followers that ask while a stale reading is read again wait for the new answer: a withdrawal reaches every
%% follower at the read that finds it, not one re-check later.
followers_asking_during_a_read_get_its_answer() ->
    Key = key(),
    Counter = counters:new(1, []),
    {error, not_found} = macula_provider_standing:read(Key, counted_read(Counter, {error, not_found}), 100),
    timer:sleep(150),
    Slow = fun() -> ok = counters:add(Counter, 1, 1), timer:sleep(200), {error, withdrawn} end,
    Self = self(),
    _ = [spawn(fun() -> Self ! {read, macula_provider_standing:read(Key, Slow, 100)} end) || _ <- lists:seq(1, 5)],
    Answers = [receive {read, A} -> A after 1_000 -> timeout end || _ <- lists:seq(1, 5)],
    ?assertEqual(lists:duplicate(5, {error, withdrawn}), Answers),
    ?assertEqual(2, counters:get(Counter, 1)).

%% A read that raises is stored as lookup_failed, and the next asker after the re-check reads again.
a_raising_read_answers_lookup_failed() ->
    Key = key(),
    ?assertEqual({error, lookup_failed}, macula_provider_standing:read(Key, fun() -> error(dht_down) end, 100)),
    timer:sleep(150),
    ?assertEqual(ok, macula_provider_standing:read(Key, fun() -> ok end, 100)).

%% A reading is served only while younger than half the re-check, so a follower acts on a reading at most one
%% re-check old when it reads again: the withdrawal reaches it within one re-check, not two.
a_reading_past_half_the_recheck_is_read_again() ->
    Key = key(),
    Counter = counters:new(1, []),
    Read = counted_read(Counter, ok),
    ok = macula_provider_standing:read(Key, Read, 400),
    timer:sleep(250),
    ok = macula_provider_standing:read(Key, Read, 400),
    ?assertEqual(2, counters:get(Counter, 1)).

%% A claimant that dies mid-read leaves its claim behind; once it is older than an asker would wait, the next asker
%% claims it and reads, instead of waiting on it until the sweep.
an_orphaned_claim_is_claimed_again() ->
    Key = key(),
    true = ets:insert(macula_provider_standing, {Key, erlang:system_time(millisecond) - 20_000, reading}),
    Started = erlang:monotonic_time(millisecond),
    ?assertEqual({error, withdrawn}, macula_provider_standing:read(Key, fun() -> {error, withdrawn} end, 60_000)),
    ?assert(erlang:monotonic_time(millisecond) - Started < 1_000).

another_key_has_its_own_reading() ->
    {Provider, OrgKey} = key(),
    Counter = counters:new(1, []),
    Read = counted_read(Counter, ok),
    ok = macula_provider_standing:read({Provider, OrgKey}, Read, 60_000),
    ok = macula_provider_standing:read({crypto:strong_rand_bytes(32), OrgKey}, Read, 60_000),
    ok = macula_provider_standing:read({Provider, crypto:strong_rand_bytes(32)}, Read, 60_000),
    ?assertEqual(3, counters:get(Counter, 1)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

key() ->
    {crypto:strong_rand_bytes(32), crypto:strong_rand_bytes(32)}.

counted_read(Counter, Answer) ->
    fun() -> ok = counters:add(Counter, 1, 1), timer:sleep(20), Answer end.

%% The node's owner when macula runs, else one of this test's own, stopped after it.
owned(undefined) ->
    {ok, Owner} = macula_provider_standing:start_link(),
    unlink(Owner),
    Owner;
owned(_Running) ->
    running.

stopped(running) ->
    ok;
stopped(Owner) ->
    exit(Owner, kill),
    wait_down(Owner).

wait_down(Pid) ->
    Mon = erlang:monitor(process, Pid),
    receive {'DOWN', Mon, process, Pid, _} -> ok after 1_000 -> ok end.
