%% Signing and verification cost per crypto profile, through macula_node_keys:sign/2 and verify/4, one core, for a
%% set of message sizes. Times are per operation in milliseconds, after 50 warm-up runs. Run it through
%% scripts/bench-pq-verify.sh, which compiles it with the two modules it measures.
-module(bench_pq_verify).

-export([main/3]).

-define(WARM_UP_RUNS, 50).
-define(PROFILES, [pq_pure, pq_hybrid]).

main(VerifyIterations, SignIterations, Sizes) ->
    io:format("otp: ~s, crypto lib: ~p~n", [erlang:system_info(otp_release), crypto:info_lib()]),
    io:format("verify iterations: ~B, sign iterations: ~B~n", [VerifyIterations, SignIterations]),
    io:format("~-10s ~9s ~11s ~11s ~11s ~11s~n",
              ["profile", "bytes", "verify_p50", "verify_p90", "verify_mean", "sign_p50"]),
    [measure(Profile, Size, VerifyIterations, SignIterations) || Profile <- ?PROFILES, Size <- Sizes],
    ok.

measure(Profile, Size, VerifyIterations, SignIterations) ->
    {ok, Key} = macula_node_keys:generate(identity, Profile, #{}),
    Public = macula_node_keys:public_key(Key),
    Message = crypto:strong_rand_bytes(Size),
    Signature = macula_node_keys:sign(Message, Key),
    true = macula_node_keys:verify(Message, Signature, Public, Profile),
    VerifyTimes = times(fun() -> true = macula_node_keys:verify(Message, Signature, Public, Profile) end,
                        VerifyIterations),
    SignTimes = times(fun() -> macula_node_keys:sign(Message, Key) end, SignIterations),
    io:format("~-10s ~9B ~11s ~11s ~11s ~11s~n",
              [Profile, Size, ms(percentile(VerifyTimes, 50)), ms(percentile(VerifyTimes, 90)),
               ms(mean(VerifyTimes)), ms(percentile(SignTimes, 50))]).

times(Fun, Iterations) ->
    _ = [Fun() || _ <- lists:seq(1, ?WARM_UP_RUNS)],
    lists:sort([element(1, timer:tc(Fun)) || _ <- lists:seq(1, Iterations)]).

percentile(Sorted, Percent) ->
    lists:nth(max(1, (length(Sorted) * Percent + 99) div 100), Sorted).

mean(Times) ->
    lists:sum(Times) div length(Times).

ms(Microseconds) ->
    lists:flatten(io_lib:format("~.3f", [Microseconds / 1000])).
