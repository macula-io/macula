%% EUnit tests for macula_provider_standing: the streams that follow one provider under one org key share one reading
%% of its delegation slot per re-check (macula#120).
-module(macula_provider_standing_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PROVIDER, <<1:256>>).
-define(ORG_KEY, <<2:256>>).

provider_standing_test_() ->
    {foreach,
     fun() -> {ok, Owner} = macula_provider_standing:start_link(), unlink(Owner), Owner end,
     fun(Owner) -> exit(Owner, kill), wait_down(Owner) end,
     [{"many followers of one provider read its slot once per re-check",
       fun many_followers_read_once_per_recheck/0},
      {"a reading older than the re-check is read again, by one follower",
       fun a_stale_reading_is_read_again_once/0},
      {"followers asking during a read wait for its answer",
       fun followers_asking_during_a_read_get_its_answer/0},
      {"a read that raises answers lookup_failed",
       fun a_raising_read_answers_lookup_failed/0},
      {"another provider or org key has its own reading",
       fun another_key_has_its_own_reading/0}]}.

many_followers_read_once_per_recheck() ->
    Counter = counters:new(1, []),
    Read = counted_read(Counter, {error, withdrawn}),
    Self = self(),
    _ = [spawn(fun() -> Self ! {read, macula_provider_standing:read({?PROVIDER, ?ORG_KEY}, Read, 60_000)} end)
         || _ <- lists:seq(1, 20)],
    Answers = [receive {read, A} -> A after 1_000 -> timeout end || _ <- lists:seq(1, 20)],
    ?assertEqual(lists:duplicate(20, {error, withdrawn}), Answers),
    ?assertEqual(1, counters:get(Counter, 1)).

a_stale_reading_is_read_again_once() ->
    Counter = counters:new(1, []),
    Read = counted_read(Counter, ok),
    ok = macula_provider_standing:read({?PROVIDER, ?ORG_KEY}, Read, 100),
    timer:sleep(150),
    _ = [macula_provider_standing:read({?PROVIDER, ?ORG_KEY}, Read, 100) || _ <- lists:seq(1, 10)],
    ?assertEqual(2, counters:get(Counter, 1)).

%% The followers that ask while a stale reading is read again wait for the new answer: a withdrawal reaches every
%% follower at the read that finds it, not one re-check later.
followers_asking_during_a_read_get_its_answer() ->
    Counter = counters:new(1, []),
    {error, not_found} = macula_provider_standing:read({?PROVIDER, ?ORG_KEY},
                                                       counted_read(Counter, {error, not_found}), 100),
    timer:sleep(150),
    Slow = fun() -> ok = counters:add(Counter, 1, 1), timer:sleep(200), {error, withdrawn} end,
    Self = self(),
    _ = [spawn(fun() -> Self ! {read, macula_provider_standing:read({?PROVIDER, ?ORG_KEY}, Slow, 100)} end)
         || _ <- lists:seq(1, 5)],
    Answers = [receive {read, A} -> A after 1_000 -> timeout end || _ <- lists:seq(1, 5)],
    ?assertEqual(lists:duplicate(5, {error, withdrawn}), Answers),
    ?assertEqual(2, counters:get(Counter, 1)).

%% A read that raises is stored as lookup_failed, and the next asker after the re-check reads again.
a_raising_read_answers_lookup_failed() ->
    ?assertEqual({error, lookup_failed},
                 macula_provider_standing:read({?PROVIDER, ?ORG_KEY}, fun() -> error(dht_down) end, 100)),
    timer:sleep(150),
    ?assertEqual(ok, macula_provider_standing:read({?PROVIDER, ?ORG_KEY}, fun() -> ok end, 100)).

another_key_has_its_own_reading() ->
    Counter = counters:new(1, []),
    Read = counted_read(Counter, ok),
    ok = macula_provider_standing:read({?PROVIDER, ?ORG_KEY}, Read, 60_000),
    ok = macula_provider_standing:read({<<3:256>>, ?ORG_KEY}, Read, 60_000),
    ok = macula_provider_standing:read({?PROVIDER, <<4:256>>}, Read, 60_000),
    ?assertEqual(3, counters:get(Counter, 1)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

counted_read(Counter, Answer) ->
    fun() -> ok = counters:add(Counter, 1, 1), timer:sleep(20), Answer end.

wait_down(Pid) ->
    Mon = erlang:monitor(process, Pid),
    receive {'DOWN', Mon, process, Pid, _} -> ok after 1_000 -> ok end.
