%% EUnit tests for the caller-side store pacer (D28, 3.5): 16 MiB at once,
%% refilled at 1 MiB per second, lazily recomputed, so a bulk writer sleeps
%% instead of running into a station's STORE byte allowance.
-module(macula_store_pacer_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MIB, 1024 * 1024).

a_record_within_the_bucket_never_sleeps_test() ->
    Conn = make_ref(),
    ?assertEqual(ok, macula_store_pacer:await(Conn, 1000)),
    #{bucket := Bucket} = macula_store_pacer:stats(Conn),
    ?assert(Bucket =< 16 * ?MIB - 1000).

the_bucket_refills_over_time_test() ->
    Conn = make_ref(),
    ok = macula_store_pacer:await(Conn, 16 * ?MIB),
    #{bucket := Empty} = macula_store_pacer:stats(Conn),
    ?assert(Empty < ?MIB),
    timer:sleep(1100),
    #{bucket := Refilled} = macula_store_pacer:stats(Conn),
    ?assert(Refilled >= Empty + ?MIB).

a_write_past_the_bucket_sleeps_until_it_fits_test() ->
    Conn = make_ref(),
    ok = macula_store_pacer:await(Conn, 16 * ?MIB),
    Start = erlang:monotonic_time(millisecond),
    ok = macula_store_pacer:await(Conn, 2 * ?MIB),
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    ?assert(Elapsed >= 1000).

buckets_are_per_connection_test() ->
    A = make_ref(),
    B = make_ref(),
    ok = macula_store_pacer:await(A, 16 * ?MIB),
    #{bucket := BFull} = macula_store_pacer:stats(B),
    ?assert(BFull =:= 16 * ?MIB).
