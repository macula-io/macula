%% EUnit tests for how a pool wires request admission. A pool runs one macula_request_admission for the requests all
%% its links receive, with its limits from the pool option request_admission, then the macula application environment,
%% then the defaults. Each link holds that admission and its share, the normalized seed of the link, which a respawned
%% link to the same seed keeps. A limit outside its range, or a smaller limit above its larger one, does not start the
%% pool. A pool stops when its admission ends, and its admission ends with the pool. Links dial unreachable seeds, each
%% naming the node_id it expects, so every link starts and stays disconnected.
-module(macula_client_request_admission_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ADMISSION_INDEX, macula_station_link:state_field_index(admission)).
-define(SHARE_INDEX, macula_station_link:state_field_index(share)).
-define(EVENT_MS, 5_000).

request_admission_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
     fun(ok) -> ok end,
     [{spawn, Test}
      || Test <- [fun an_admission_limit_outside_its_range_does_not_start_the_pool/0,
                  fun a_smaller_admission_limit_above_its_larger_one_does_not_start_the_pool/0,
                  fun every_link_holds_the_pools_one_admission_and_its_own_share/0,
                  fun a_respawned_link_keeps_the_share_of_its_seed/0,
                  fun the_limits_come_from_the_option_then_the_environment_then_the_defaults/0,
                  fun a_pool_stops_when_its_admission_ends/0,
                  fun a_pools_admission_ends_with_the_pool/0]]}.

%% Each admission limit is an integer from 1 to its cap. Anything else, an atom included, does not start the pool.
an_admission_limit_outside_its_range_does_not_start_the_pool() ->
    [?assertEqual({error, {invalid_admission_limit, Key, Value}},
                  macula_client:connect([seed(1)], #{request_admission => #{Key => Value}}))
     || {Key, Cap} <- [{caller_quota, 65536}, {share, 65536}, {reply_bytes, 16777216}, {reply_bytes_total, 1073741824}],
        Value <- [many, 0, -1, Cap + 1]].

%% A quota per caller above the share, or stored reply bytes per caller above the total, would never bind as the design
%% means, so neither starts the pool. Equal limits do.
a_smaller_admission_limit_above_its_larger_one_does_not_start_the_pool() ->
    ?assertEqual({error, {admission_limit_above, caller_quota, share}},
                 macula_client:connect([seed(1)], #{request_admission => #{caller_quota => 2048, share => 1024}})),
    ?assertEqual({error, {admission_limit_above, reply_bytes, reply_bytes_total}},
                 macula_client:connect([seed(1)], #{request_admission => #{reply_bytes => 2048,
                                                                           reply_bytes_total => 1024}})),
    {ok, Pool} = macula_client:connect([seed(1)], #{request_admission => #{caller_quota => 1024, share => 1024,
                                                                          reply_bytes => 1024,
                                                                          reply_bytes_total => 1024}}),
    ok = macula_client:close(Pool).

%% Every link a pool starts holds the pool's one admission, and the share of its own seed.
every_link_holds_the_pools_one_admission_and_its_own_share() ->
    {ok, Pool} = macula_client:connect([seed(1), seed(2)], #{}),
    Held = [held(Link) || #{pid := Link} <- links(Pool)],
    [Admission] = lists:usort([A || {A, _Share} <- Held]),
    ?assert(is_process_alive(Admission)),
    ?assertEqual(2, length(lists:usort([Share || {_A, Share} <- Held]))),
    ok = macula_client:close(Pool).

%% A link that ends is started again for its seed, with the admission and the share it had.
a_respawned_link_keeps_the_share_of_its_seed() ->
    {ok, Pool} = macula_client:connect([seed(1)], #{}),
    [#{pid := Link}] = links(Pool),
    Before = held(Link),
    exit(Link, kill),
    ?assertEqual(Before, held(respawned(Pool, Link, 50))),
    ok = macula_client:close(Pool).

%% A share of 1 refuses a second caller's request on one share. The pool option sets it, and without the option the
%% macula application environment does, key by key. Without either, a caller's 257th request is refused over the
%% default quota of 256.
the_limits_come_from_the_option_then_the_environment_then_the_defaults() ->
    ?assertEqual({new, {refused, share_full}},
                 two_callers_on_one_share(#{request_admission => #{share => 1, caller_quota => 1}})),
    ok = application:set_env(macula, request_admission, #{share => 1, caller_quota => 1}),
    try
        ?assertEqual({new, {refused, share_full}}, two_callers_on_one_share(#{})),
        ?assertEqual({new, new}, two_callers_on_one_share(#{request_admission => #{share => 1024, caller_quota => 256}}))
    after
        ok = application:unset_env(macula, request_admission)
    end,
    ?assertEqual({lists:duplicate(256, new), {refused, caller_quota}}, one_caller_past_its_quota(#{})).

%% A pool whose admission ends stops, rather than go on serving without the requests it has seen.
a_pool_stops_when_its_admission_ends() ->
    process_flag(trap_exit, true),
    {ok, Pool} = macula_client:connect([seed(1)], #{}),
    Mon = erlang:monitor(process, Pool),
    [#{pid := Link}] = links(Pool),
    {Admission, _Share} = held(Link),
    exit(Admission, kill),
    ?assertEqual({shutdown, {admission_down, killed}},
                 receive {'DOWN', Mon, process, Pool, Reason} -> Reason after ?EVENT_MS -> still_running end).

%% Closing a pool ends its admission.
a_pools_admission_ends_with_the_pool() ->
    {ok, Pool} = macula_client:connect([seed(1)], #{}),
    [#{pid := Link}] = links(Pool),
    {Admission, _Share} = held(Link),
    Mon = erlang:monitor(process, Admission),
    ok = macula_client:close(Pool),
    ?assertEqual(ended, receive {'DOWN', Mon, process, Admission, _Reason} -> ended after ?EVENT_MS -> still_running end).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

seed(N) ->
    #{host => <<"127.0.0.1">>, port => N, expected_node_id => <<N:256>>}.

links(Pool) ->
    {ok, Links} = macula_client:links(Pool),
    Links.

%% The admission and the share a link holds.
held(Link) ->
    State = sys:get_state(Link),
    {element(?ADMISSION_INDEX, State), element(?SHARE_INDEX, State)}.

%% The link a pool started again after Old ended.
respawned(_Pool, _Old, 0) ->
    erlang:error(link_not_respawned);
respawned(Pool, Old, Tries) ->
    started_again([Link || #{pid := Link} <- links(Pool), Link =/= Old], Pool, Old, Tries).

started_again([Link], _Pool, _Old, _Tries) ->
    Link;
started_again([], Pool, Old, Tries) ->
    timer:sleep(100),
    respawned(Pool, Old, Tries - 1).

%% What the pool's admission answers two callers' requests on one link's share.
two_callers_on_one_share(Opts) ->
    {ok, Pool} = macula_client:connect([seed(1)], Opts),
    [#{pid := Link}] = links(Pool),
    {Admission, Share} = held(Link),
    Now = erlang:system_time(millisecond),
    Verdicts = {macula_request_admission:admit(Admission, request(<<1:256>>, 1, Now), Share, Now),
                macula_request_admission:admit(Admission, request(<<2:256>>, 2, Now), Share, Now)},
    ok = macula_client:close(Pool),
    Verdicts.

%% What the pool's admission answers one caller's 256 requests, and its 257th.
one_caller_past_its_quota(Opts) ->
    {ok, Pool} = macula_client:connect([seed(1)], Opts),
    [#{pid := Link}] = links(Pool),
    {Admission, Share} = held(Link),
    Now = erlang:system_time(millisecond),
    Admitted = [macula_request_admission:admit(Admission, request(<<1:256>>, I, Now), Share, Now)
                || I <- lists:seq(1, 256)],
    Over = macula_request_admission:admit(Admission, request(<<1:256>>, 257, Now), Share, Now),
    ok = macula_client:close(Pool),
    {Admitted, Over}.

request(Caller, I, Now) ->
    #{caller => Caller, request_id => <<I:128>>, request_hash => crypto:hash(sha384, <<I:64>>),
      deadline => Now + 30_000}.
