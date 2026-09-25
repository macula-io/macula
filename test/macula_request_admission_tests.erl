%% EUnit tests for `macula_request_admission'. A provider admits each verified request once: it keeps (caller,
%% request_id) until the request's deadline plus 5 minutes, hands a copy with the same request hash the stored reply,
%% and refuses a copy with another hash (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, Requests). The seen entries are
%% bounded per caller, per share and overall, and a full bound refuses a request rather than evicting an entry.
-module(macula_request_admission_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NOW, 1789000000000).
-define(MINUTE, 60000).
-define(LIMITS, #{caller_quota => 2, share => 3, cap => 5, reply_bytes => 100, reply_bytes_total => 300}).

a_first_request_is_new_test() ->
    with_admission(fun(A) ->
        ?assertEqual(new, admit(A, request(1, 1), link_a))
    end).

%% A copy that arrives while the work runs waits for the reply, so the handler runs once.
a_copy_of_a_request_waits_for_its_reply_test() ->
    with_admission(fun(A) ->
        Request = request(1, 1),
        new = admit(A, Request, link_a),
        ?assertEqual({copy, pending}, admit(A, Request, link_b))
    end).

a_copy_after_the_reply_gets_the_stored_reply_test() ->
    with_admission(fun(A) ->
        Request = request(1, 1),
        new = admit(A, Request, link_a),
        ?assertEqual(kept, store(A, Request, reply, 10)),
        ?assertEqual({copy, {reply, reply}}, admit(A, Request, link_b))
    end).

another_hash_under_a_seen_request_id_is_refused_test() ->
    with_admission(fun(A) ->
        new = admit(A, request(1, 1), link_a),
        ?assertEqual({refused, request_id_reused},
                     admit(A, (request(1, 1))#{request_hash => hash(other)}, link_b))
    end).

the_same_request_id_from_another_caller_is_new_test() ->
    with_admission(fun(A) ->
        new = admit(A, request(1, 1), link_a),
        ?assertEqual(new, admit(A, request(2, 1), link_a))
    end).

%% The deadline must lie inside the provider's clock minus 5 minutes and its clock plus 10 minutes.
a_deadline_more_than_5_minutes_past_is_expired_test() ->
    with_admission(fun(A) ->
        ?assertEqual(new, admit(A, (request(1, 1))#{deadline => ?NOW - 5 * ?MINUTE}, link_a)),
        ?assertEqual({refused, {expired, 1}},
                     admit(A, (request(1, 2))#{deadline => ?NOW - 5 * ?MINUTE - 1}, link_a))
    end).

a_deadline_more_than_10_minutes_ahead_is_not_yet_valid_test() ->
    with_admission(fun(A) ->
        ?assertEqual(new, admit(A, (request(1, 1))#{deadline => ?NOW + 10 * ?MINUTE}, link_a)),
        ?assertEqual({refused, {not_yet_valid, 1}},
                     admit(A, (request(1, 2))#{deadline => ?NOW + 10 * ?MINUTE + 1}, link_a))
    end).

a_caller_over_its_quota_is_refused_while_another_caller_on_the_connection_is_accepted_test() ->
    with_admission(fun(A) ->
        [new = admit(A, request(1, N), link_a) || N <- [1, 2]],
        ?assertEqual({refused, caller_quota}, admit(A, request(1, 3), link_a)),
        ?assertEqual(new, admit(A, request(2, 1), link_a))
    end).

a_connection_filling_its_share_leaves_another_connection_accepted_test() ->
    with_admission(fun(A) ->
        [new = admit(A, request(C, 1), link_a) || C <- [1, 2, 3]],
        ?assertEqual({refused, share_full}, admit(A, request(4, 1), link_a)),
        ?assertEqual(new, admit(A, request(4, 1), link_b))
    end).

%% The overall cap bounds the set even while no share is full.
a_full_set_refuses_on_every_connection_test() ->
    with_admission(fun(A) ->
        [new = admit(A, request(C, 1), Share)
         || {C, Share} <- [{1, link_a}, {2, link_a}, {3, link_b}, {4, link_b}, {5, link_c}]],
        ?assertEqual({refused, admission_full}, admit(A, request(6, 1), link_c))
    end).

%% A refused request takes nothing: not the caller's quota, not the share, not a place in the set.
a_refused_request_takes_no_place_test() ->
    with_admission(fun(A) ->
        [new = admit(A, request(C, 1), link_a) || C <- [1, 2, 3]],
        {refused, share_full} = admit(A, request(4, 1), link_a),
        {refused, share_full} = admit(A, request(4, 2), link_a),
        ?assertEqual(new, admit(A, request(4, 3), link_b)),
        ?assertEqual(new, admit(A, request(4, 4), link_b))
    end).

%% Under a controlled clock, an entry keeps its place through its deadline plus 5 minutes and frees it after.
an_entry_past_its_deadline_plus_5_minutes_frees_its_place_test() ->
    with_admission(fun(A) ->
        Deadline = ?NOW + ?MINUTE,
        Held = [(request(C, 1))#{deadline => Deadline} || C <- [1, 2, 3, 4, 5]],
        [new = admit(A, R, Share) || {R, Share} <- lists:zip(Held, [link_a, link_a, link_a, link_b, link_b])],
        Last = Deadline + 5 * ?MINUTE,
        ?assertEqual(0, macula_request_admission:sweep(A, Last)),
        ?assertEqual({refused, admission_full},
                     macula_request_admission:admit(A, (request(6, 1))#{deadline => Last}, link_c, Last)),
        After = Last + 1,
        ?assertEqual(5, macula_request_admission:sweep(A, After)),
        Again = [(request(C, 2))#{deadline => After + ?MINUTE} || C <- [1, 1, 2, 2, 3]],
        ?assertEqual([new, new, new, new, new],
                     [macula_request_admission:admit(A, (R)#{request_id => <<I:128>>}, Share, After)
                      || {I, R, Share} <- lists:zip3([11, 12, 13, 14, 15], Again,
                                                     [link_a, link_a, link_a, link_b, link_b])])
    end).

%% Expiry is judged when admission runs: an entry past its deadline plus 5 minutes that no sweep has removed yet does
%% not keep its request_id.
an_expired_entry_not_yet_swept_does_not_hold_its_request_id_test() ->
    with_admission(fun(A) ->
        Deadline = ?NOW + ?MINUTE,
        new = admit(A, (request(1, 1))#{deadline => Deadline}, link_a),
        After = Deadline + 5 * ?MINUTE + 1,
        Reused = (request(1, 1))#{request_hash => hash(reused), deadline => After + ?MINUTE},
        ?assertEqual(new, macula_request_admission:admit(A, Reused, link_a, After))
    end).

%% A full bound is judged on the entries alive when admission runs: before refusing, the admission drops the entries
%% past their deadline plus 5 minutes, so a caller whose quota holds only expired entries is admitted without waiting
%% for a sweep (macula#37: a station's liveness pings held a provider's quota for a caller full for good).
a_caller_whose_quota_holds_only_expired_entries_is_admitted_without_a_sweep_test() ->
    with_admission(fun(A) ->
        Deadline = ?NOW + ?MINUTE,
        [new = admit(A, (request(1, N))#{deadline => Deadline}, link_a) || N <- [1, 2]],
        After = Deadline + 5 * ?MINUTE + 1,
        ?assertEqual(new, macula_request_admission:admit(A, (request(1, 3))#{deadline => After}, link_a, After)),
        ?assertEqual(new, macula_request_admission:admit(A, (request(1, 4))#{deadline => After}, link_a, After)),
        ?assertEqual({refused, caller_quota},
                     macula_request_admission:admit(A, (request(1, 5))#{deadline => After}, link_a, After))
    end).

%% Likewise a full set: its expired entries leave it, and every caller holding one gets its place back.
a_set_full_of_expired_entries_admits_without_a_sweep_test() ->
    with_admission(fun(A) ->
        Deadline = ?NOW + ?MINUTE,
        [new = admit(A, (request(C, 1))#{deadline => Deadline}, Share)
         || {C, Share} <- [{1, link_a}, {2, link_a}, {3, link_b}, {4, link_b}, {5, link_c}]],
        After = Deadline + 5 * ?MINUTE + 1,
        ?assertEqual(new, macula_request_admission:admit(A, (request(6, 1))#{deadline => After}, link_c, After)),
        ?assertEqual(1, macula_request_admission:sweep(A, After + 6 * ?MINUTE))
    end).

%% Stored reply bytes are bounded per caller: a reply past the bound is not kept, and a copy of its request is then
%% refused.
a_reply_past_the_callers_byte_bound_is_not_kept_test() ->
    with_admission(fun(A) ->
        [R1, R2] = [request(1, 1), request(1, 2)],
        new = admit(A, R1, link_a),
        new = admit(A, R2, link_a),
        ?assertEqual(kept, store(A, R1, reply_one, 60)),
        ?assertEqual(not_kept, store(A, R2, reply_two, 41)),
        ?assertEqual({refused, reply_not_kept}, admit(A, R2, link_b)),
        ?assertEqual({copy, {reply, reply_one}}, admit(A, R1, link_b))
    end).

another_callers_reply_bytes_are_their_own_test() ->
    with_admission(fun(A) ->
        [R1, R2] = [request(1, 1), request(2, 1)],
        new = admit(A, R1, link_a),
        new = admit(A, R2, link_a),
        ?assertEqual(kept, store(A, R1, reply_one, 100)),
        ?assertEqual(kept, store(A, R2, reply_two, 100))
    end).

a_reply_for_a_request_no_longer_held_is_gone_test() ->
    with_admission(fun(A) ->
        Request = request(1, 1),
        new = admit(A, Request, link_a),
        1 = macula_request_admission:sweep(A, ?NOW + ?MINUTE + 5 * ?MINUTE + 1),
        ?assertEqual(gone, store(A, Request, reply, 10))
    end).

removing_an_entry_releases_its_reply_bytes_test() ->
    with_admission(fun(A) ->
        new = admit(A, request(1, 1), link_a),
        kept = store(A, request(1, 1), reply_one, 100),
        After = ?NOW + ?MINUTE + 5 * ?MINUTE + 1,
        1 = macula_request_admission:sweep(A, After),
        Next = (request(1, 2))#{deadline => After + ?MINUTE},
        new = macula_request_admission:admit(A, Next, link_a, After),
        ?assertEqual(kept, store(A, Next, reply_two, 100))
    end).

%% The stored reply bytes of all callers together are bounded too, since callers are cheap to make: a reply within its
%% caller's bound but past the total is not kept, a copy of its request is then refused, and removing entries frees
%% their bytes for everyone.
a_reply_past_the_total_byte_bound_is_not_kept_test() ->
    {ok, A} = macula_request_admission:start_link(maps:put(reply_bytes_total, 150, ?LIMITS)),
    try
        [R1, R2] = [request(1, 1), request(2, 1)],
        new = admit(A, R1, link_a),
        new = admit(A, R2, link_a),
        kept = store(A, R1, reply_one, 100),
        ?assertEqual(not_kept, store(A, R2, reply_two, 51)),
        ?assertEqual({refused, reply_not_kept}, admit(A, R2, link_b)),
        After = ?NOW + ?MINUTE + 5 * ?MINUTE + 1,
        2 = macula_request_admission:sweep(A, After),
        Next = (request(2, 2))#{deadline => After + ?MINUTE},
        new = macula_request_admission:admit(A, Next, link_a, After),
        ?assertEqual(kept, store(A, Next, reply_three, 100))
    after
        macula_request_admission:stop(A)
    end.

%% Many callers, each under its own cap, fill the total: a fresh caller's request is still admitted, its reply is not
%% stored, a copy of it is refused, and every reply not stored is counted.
many_callers_under_their_caps_fill_the_total_and_new_requests_are_still_admitted_test() ->
    {ok, A} = macula_request_admission:start_link(maps:merge(?LIMITS, #{cap => 100, share => 100,
                                                                        reply_bytes_total => 250})),
    try
        Stored = [begin
                      R = request(C, 1),
                      new = admit(A, R, link_a),
                      store(A, R, reply, 100)
                  end || C <- lists:seq(1, 4)],
        ?assertEqual([kept, kept, not_kept, not_kept], Stored),
        Fresh = request(99, 1),
        ?assertEqual(new, admit(A, Fresh, link_b)),
        ?assertEqual(not_kept, store(A, Fresh, reply, 100)),
        ?assertEqual({refused, reply_not_kept}, admit(A, Fresh, link_c)),
        ?assertMatch(#{reply_not_stored := 3, reply_not_kept := 1}, macula_request_admission:refusals(A))
    after
        macula_request_admission:stop(A)
    end.

%% A request is recorded before its handler runs: with the total full, a copy that arrives while the handler runs waits
%% for its reply, and a copy after it finished is refused; neither is admitted as new.
a_copy_during_execution_is_never_admitted_again_when_the_total_is_full_test() ->
    {ok, A} = macula_request_admission:start_link(maps:put(reply_bytes_total, 100, ?LIMITS)),
    try
        Filler = request(1, 1),
        new = admit(A, Filler, link_a),
        kept = store(A, Filler, reply, 100),
        Running = request(2, 1),
        ?assertEqual(new, admit(A, Running, link_a)),
        ?assertEqual({copy, pending}, admit(A, Running, link_b)),
        ?assertEqual(not_kept, store(A, Running, reply, 10)),
        ?assertEqual({refused, reply_not_kept}, admit(A, Running, link_c))
    after
        macula_request_admission:stop(A)
    end.

%% Refusals are counted by kind; a deadline refusal counts under its kind, whatever its milliseconds.
refusals_are_counted_by_kind_test() ->
    with_admission(fun(A) ->
        [new = admit(A, request(1, N), link_a) || N <- [1, 2]],
        {refused, caller_quota} = admit(A, request(1, 3), link_a),
        {refused, request_id_reused} = admit(A, (request(1, 1))#{request_hash => hash(other)}, link_b),
        {refused, {expired, _}} = admit(A, (request(2, 1))#{deadline => ?NOW - 6 * ?MINUTE}, link_a),
        {refused, {expired, _}} = admit(A, (request(2, 2))#{deadline => ?NOW - 7 * ?MINUTE}, link_a),
        ?assertEqual(#{caller_quota => 1, request_id_reused => 1, expired => 2},
                     macula_request_admission:refusals(A))
    end).

%% A refusal names its caller and procedure, so a caller_quota warning can be traced to the node filling its quota
%% and what it is calling (macula#34: a fleet node logged caller_quota once a minute, naming no one).
refusals_name_their_caller_and_procedure_test() ->
    with_admission(fun(A) ->
        [new = admit(A, (request(1, N))#{procedure => <<"acme/count_v1">>}, link_a) || N <- [1, 2]],
        {refused, caller_quota} = admit(A, (request(1, 3))#{procedure => <<"acme/count_v1">>}, link_a),
        Prefix = binary:encode_hex(binary:part(<<1:256>>, 0, 8), lowercase),
        ?assertEqual(#{caller_quota => [{{Prefix, <<"acme/count_v1">>}, 1}]},
                     macula_request_admission:refusal_sources(A))
    end).

the_refusal_line_names_the_sources_test() ->
    ?assertEqual("[macula_request_admission] 3 refused: caller_quota, from 00000000000000aa on acme/count_v1 (2), "
                 "00000000000000bb on acme/other_v1 (1)",
                 lists:flatten(macula_request_admission:refusal_line(
                   3, caller_quota, [{{<<"00000000000000aa">>, <<"acme/count_v1">>}, 2},
                                     {{<<"00000000000000bb">>, <<"acme/other_v1">>}, 1}]))).

%% Copies racing on several connections admit the request once, and the losers leave no place taken behind.
racing_copies_admit_a_request_once_test() ->
    with_admission(fun(A) ->
        Request = request(1, 1),
        Test = self(),
        [spawn(fun() -> Test ! {admitted, admit(A, Request, Share)} end)
         || Share <- [link_a, link_b, link_c, link_a, link_b, link_c]],
        Results = [receive {admitted, Result} -> Result after 1000 -> timeout end || _ <- lists:seq(1, 6)],
        ?assertEqual(1, length([new || new <- Results])),
        ?assertEqual(5, length([copy || {copy, pending} <- Results])),
        ?assertEqual(new, admit(A, request(1, 2), link_a)),
        ?assertEqual({refused, caller_quota}, admit(A, request(1, 3), link_a))
    end).

%% A caller and a request hash are fixed-size ids, never a key or a label.
a_request_with_a_wrong_sized_id_is_refused_test() ->
    with_admission(fun(A) ->
        ?assertError(function_clause, admit(A, (request(1, 1))#{caller => <<1:384>>}, link_a)),
        ?assertError(function_clause, admit(A, (request(1, 1))#{request_hash => <<1:256>>}, link_a))
    end).

%%---------------------------------------------------------------------
%% Helpers
%%---------------------------------------------------------------------

with_admission(Test) ->
    {ok, Admission} = macula_request_admission:start_link(?LIMITS),
    try Test(Admission) after macula_request_admission:stop(Admission) end.

admit(Admission, Request, Share) ->
    macula_request_admission:admit(Admission, Request, Share, ?NOW).

store(Admission, Request, Reply, Bytes) ->
    macula_request_admission:store_reply(Admission, Request, Reply, Bytes, ?NOW).

request(Caller, RequestId) ->
    #{caller => <<Caller:256>>, request_id => <<RequestId:128>>, request_hash => hash({Caller, RequestId}),
      deadline => ?NOW + ?MINUTE}.

hash(Term) ->
    crypto:hash(sha384, term_to_binary(Term)).
