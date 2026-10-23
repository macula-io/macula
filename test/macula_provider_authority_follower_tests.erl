%% EUnit tests for how a caller's stream follows its provider's authorization (macula#120). A stream opened against a
%% provider's advertisement holds no longer than that provider's authorization: the follower ends it at the
%% authorization's end, unless a renewed, verified advertisement from the same provider moves the end later before
%% then. It looks again at a third of the time left. A lookup that finds nothing, finds only earlier ends, or fails,
%% never moves the end, so withholding a renewal can only let the stream reach its end. A stream that ends first takes
%% its follower with it. The stream is played by a process that answers its abort and reports it.
-module(macula_provider_authority_follower_tests).

-include_lib("eunit/include/eunit.hrl").

follower_test_() ->
    [{Name, {timeout, 15, Case}}
      || {Name, Case} <- [{"a stream no renewal reaches is ended at its provider's authorization end",
                           fun a_stream_is_ended_at_its_end/0},
                          {"a renewal with a later end keeps the stream past the first end",
                           fun a_later_renewal_keeps_the_stream/0},
                          {"a renewal with an earlier end never brings the end forward",
                           fun an_earlier_end_moves_nothing/0},
                          {"a lookup that fails keeps the end where it was",
                           fun a_failed_lookup_keeps_the_end/0},
                          {"the follower ends with its stream",
                           fun the_follower_ends_with_its_stream/0}]].

a_stream_is_ended_at_its_end() ->
    Stream = stream(),
    Started = now_ms(),
    _ = macula_provider_authority_follower:follow(Stream, Started + 1_500, fun(_Deadline) -> [] end),
    ?assertMatch({aborted, <<"unauthorized">>, _}, aborted_within(3_000)),
    Waited = now_ms() - Started,
    ?assert(Waited >= 1_450 andalso Waited =< 2_500).

a_later_renewal_keeps_the_stream() ->
    Stream = stream(),
    Later = now_ms() + 60_000,
    _ = macula_provider_authority_follower:follow(Stream, now_ms() + 1_500, fun(_Deadline) -> [Later] end),
    ?assertEqual(none, aborted_within(3_000)).

an_earlier_end_moves_nothing() ->
    Stream = stream(),
    Started = now_ms(),
    Earlier = Started + 100,
    _ = macula_provider_authority_follower:follow(Stream, Started + 1_500, fun(_Deadline) -> [Earlier] end),
    ?assertMatch({aborted, _, _}, aborted_within(3_000)),
    ?assert(now_ms() - Started >= 1_450).

a_failed_lookup_keeps_the_end() ->
    Stream = stream(),
    Started = now_ms(),
    _ = macula_provider_authority_follower:follow(Stream, Started + 1_500, fun(_Deadline) -> error(dht_down) end),
    ?assertMatch({aborted, _, _}, aborted_within(3_000)),
    ?assert(now_ms() - Started >= 1_450).

the_follower_ends_with_its_stream() ->
    Stream = stream(),
    Follower = macula_provider_authority_follower:follow(Stream, now_ms() + 60_000, fun(_Deadline) -> [] end),
    Mon = erlang:monitor(process, Follower),
    exit(Stream, kill),
    ?assertMatch({'DOWN', Mon, process, Follower, _}, receive Down -> Down after 1_000 -> none end).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% A stream that answers an abort, as macula_stream does, and tells this process about it.
stream() ->
    Test = self(),
    spawn(fun Stream() ->
              receive
                  {'$gen_call', From, {abort, Code, Message}} ->
                      Test ! {aborted, Code, Message},
                      gen_server:reply(From, ok),
                      Stream()
              end
          end).

aborted_within(Ms) ->
    receive {aborted, _, _} = Aborted -> Aborted
    after Ms -> none
    end.

now_ms() ->
    erlang:system_time(millisecond).
