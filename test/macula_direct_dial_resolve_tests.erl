%%% @doc Direct-dial resolution in `macula_direct_dial': every advertisement
%%% that passes the trust check is a candidate; one whose endpoint can't be
%%% resolved or whose dial doesn't connect is passed over for the next before
%%% anything is sent; resolution asks the DHT again until the call's deadline,
%%% backing off to one second; a candidate that failed is tried again only on
%%% a changed record; a content fetch that fails moves on to the next provider.
%%% The record part: a provider's advertisement is signed with its node
%%% identity key and put as its wire form, a station is dialled only through an
%%% endpoint record the station itself signed, and an org namespaced procedure
%%% needs a provider authorization.
%%%
%%% The DHT, the dials, the transfers and the realm keys the pool pins are
%%% faked with meck on the `macula' facade, `macula_client' and
%%% `macula_content_transfer'. The records
%%% themselves are real: signed with node keys in the node's crypto profile and
%%% handed over verified, as the facade hands them over.
-module(macula_direct_dial_resolve_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_download).
-export([init/1, handle_downloaded/2]).

-define(STATE, macula_direct_dial_resolve_tests_state).
-define(REALM, <<16#11:256>>).
-define(OTHER_REALM, <<16#22:256>>).
-define(PROC, <<"echo_v1">>).
-define(ORG, <<"resolve-tests">>).
-define(ORG_PROC, <<"resolve-tests/echo_v1">>).
-define(HOUR_MS, 3_600_000).
-define(DAY_MS, 86_400_000).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup() ->
    ?STATE = ets:new(?STATE, [named_table, public, set]),
    ets:insert(?STATE, {visits, []}),
    meck:new(macula, [passthrough, non_strict]),
    meck:new(macula_client, [passthrough, non_strict]),
    meck:new(macula_content_transfer, [passthrough]),
    %% A pool pins no realm key unless a test pins one (pin_realm_key/2).
    meck:expect(macula_client, realm_key, fun(_Pool, _Realm) -> none end),
    %% No head start unless a test plants one (remember_station/3), and what
    %% the code remembers is recorded rather than dropped (remembered/1).
    meck:expect(macula_client, resolved_candidate,
                fun(_Pool, Realm, Procedure) -> planted_head_start(Realm, Procedure) end),
    meck:expect(macula_client, remember_resolved,
                fun(_Pool, Realm, Procedure, Candidate, TtlMs) ->
                        ets:insert(?STATE, {{remembered, Realm, Procedure}, Candidate, TtlMs}),
                        ok
                end),
    meck:expect(macula, find_records, fun(_Pool, Key) -> find_records(Key) end),
    meck:expect(macula, find_records,
                fun(_Pool, Key, TimeoutMs) -> find_records(Key, TimeoutMs) end),
    meck:expect(macula, find_record, fun(_Pool, Key) -> find_record(Key) end),
    meck:expect(macula, find_record,
                fun(_Pool, Key, TimeoutMs) -> find_record(Key, TimeoutMs) end),
    meck:expect(macula, call_station,
                fun(_Pool, DialUrl, _Provider, _Realm, _Proc, _Payload, _TimeoutMs, _Opts) ->
                        visit(DialUrl)
                end),
    meck:expect(macula, call_stream_station,
                fun(_Pool, DialUrl, Provider, _Realm, _Proc, _Args, _Opts) ->
                        ets:insert(?STATE, {stream_provider, Provider}),
                        visit(DialUrl)
                end),
    meck:expect(macula, get_content_station,
                fun(_Pool, Endpoint, _Mcid, _TimeoutMs, _Opts) -> visit(Endpoint) end),
    meck:expect(macula, put_content_station,
                fun(_Pool, DialUrl, _Bytes, _TimeoutMs, _Opts) -> visit(DialUrl) end),
    meck:expect(macula_content_transfer, start_get_station,
                fun(_Pool, Endpoint, _Mcid, _TimeoutMs, _Opts) ->
                        {ok, {fake_transfer, Endpoint}}
                end),
    meck:expect(macula_content_transfer, await,
                fun({fake_transfer, Endpoint}, _Timeout) -> visit(Endpoint) end),
    meck:expect(macula_content_transfer, await,
                fun({fake_transfer, Endpoint}) -> visit(Endpoint) end),
    meck:expect(macula_content_transfer, cancel, fun(_Transfer) -> ok end),
    meck:expect(macula, publish, fun(_Pool, _Realm, _Topic, _Payload) -> ok end),
    meck:expect(macula, publish, fun(_Pool, _Realm, _Topic, _Payload, _Opts) -> ok end),
    ok.

teardown(_) ->
    meck:unload([macula, macula_client, macula_content_transfer]),
    ets:delete(?STATE).

resolve_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{timeout, 30, fun call_tries_the_next_advertisement_when_a_station_has_no_endpoint/0},
      {timeout, 30, fun call_retries_when_no_advertisement_qualifies/0},
      {timeout, 30, fun call_tries_the_next_station_when_a_dial_fails/0},
      {timeout, 30, fun call_never_sends_the_request_twice/0},
      {timeout, 30, fun call_tries_the_next_station_when_a_dial_is_refused/0},
      {timeout, 30, fun call_stops_at_a_request_the_wire_cannot_carry/0},
      {timeout, 30, fun call_timeout_bounds_resolution/0},
      {timeout, 30, fun call_timeout_bounds_the_endpoint_lookup/0},
      {timeout, 30, fun call_answers_from_a_remembered_station_without_asking_the_dht/0},
      {timeout, 30,
       fun call_falls_through_to_the_dht_when_the_remembered_station_does_not_connect/0},
      {timeout, 30, fun call_tries_a_remembered_station_again_when_the_dht_names_it/0},
      {timeout, 30, fun call_with_a_dead_head_start_and_no_advertisement_ends_at_its_deadline/0},
      {timeout, 30,
       fun call_stream_answers_from_a_remembered_station_without_asking_the_dht/0},
      {timeout, 30, fun call_remembers_the_station_that_answered/0},
      {timeout, 30, fun call_remembers_for_the_advertisements_own_remaining_lifetime/0},
      {timeout, 30, fun call_does_not_remember_a_station_whose_call_failed/0},
      {timeout, 30, fun call_does_not_remember_a_head_start_that_answered/0},
      {timeout, 30, fun call_stream_tries_the_next_station_when_a_dial_fails/0},
      {timeout, 30, fun call_stream_never_opens_the_stream_twice/0},
      {timeout, 30, fun call_stream_names_the_provider_its_advertisement_names/0},
      {timeout, 30, fun get_content_retries_when_no_provider_qualifies/0},
      {timeout, 30, fun get_content_tries_the_next_provider_after_a_failed_fetch/0},
      {timeout, 30, fun get_content_timeout_bounds_resolution/0},
      {timeout, 30, fun put_content_timeout_bounds_the_endpoint_lookup/0},
      {timeout, 30, fun a_published_advertisement_is_signed_by_the_node_identity_and_put_as_its_wire_form/0},
      {timeout, 30, fun a_call_dials_the_station_a_trusted_advertisement_names_pinned_to_its_node_id/0},
      {timeout, 30, fun an_endpoint_signed_by_another_node_is_not_dialed/0},
      {timeout, 30, fun an_org_namespaced_procedure_without_an_authorization_resolves_to_nothing/0},
      {timeout, 30, fun call_with_authorization_tries_the_next_advertisement_when_a_station_has_no_endpoint/0},
      {timeout, 30, fun call_with_authorization_asks_again_until_its_deadline/0},
      {timeout, 30, fun a_realm_key_pinned_for_another_realm_authorizes_nothing_in_this_one/0},
      {timeout, 30, fun call_dials_a_refusing_station_once_per_endpoint_version/0},
      {timeout, 30, fun call_tries_an_advertisement_that_appears_on_a_later_pass/0},
      {timeout, 30, fun resolution_backs_off_between_passes/0},
      {timeout, 30, fun get_content_fetches_from_a_failing_provider_once_per_announcement/0},
      {timeout, 30, fun call_picks_up_an_endpoint_record_that_changes_mid_deadline/0},
      {timeout, 60, fun download_direct_tries_the_next_provider_after_a_failed_fetch/0},
      {timeout, 60, fun download_direct_retries_when_no_provider_qualifies/0},
      {timeout, 30, fun resolve_content_provider_returns_the_first_qualifying_provider/0},
      {timeout, 30, fun resolve_content_provider_reports_content_not_announced/0},
      {timeout, 30, fun call_reports_the_last_candidate_failure_when_a_later_pass_finds_none/0},
      {timeout, 30, fun call_reports_the_last_candidate_failure_when_a_later_lookup_fails/0},
      {timeout, 30, fun get_content_reports_the_last_candidate_failure_when_a_later_lookup_fails/0},
      {timeout, 30, fun call_reports_not_advertised_when_a_lookup_answered_before_later_ones_failed/0},
      {timeout, 30, fun get_content_reports_not_announced_when_a_lookup_answered_before_later_ones_failed/0},
      {timeout, 30, fun call_reports_a_failed_lookup_at_its_deadline_when_no_candidate_was_tried/0},
      {timeout, 30, fun get_content_reports_a_failed_lookup_at_its_deadline_when_no_provider_was_tried/0},
      {timeout, 30, fun call_reports_a_timeout_when_no_lookup_was_answered_in_time/0},
      {timeout, 30, fun get_content_reports_a_timeout_when_no_lookup_was_answered_in_time/0},
      {timeout, 30, fun call_keeps_a_lookup_error_when_a_later_lookup_is_cut_off_by_the_deadline/0},
      {timeout, 30, fun call_retries_after_a_lookup_fails/0},
      {timeout, 30, fun get_content_retries_after_a_lookup_fails/0},
      {timeout, 30, fun put_content_reports_no_station_endpoint_when_a_lookup_answered_not_found/0},
      {timeout, 30, fun put_content_retries_an_endpoint_lookup_that_fails/0},
      {timeout, 30, fun put_content_reports_a_failed_endpoint_lookup_when_every_lookup_failed/0},
      {timeout, 30, fun put_content_reports_a_timeout_when_no_endpoint_lookup_was_answered_in_time/0},
      {timeout, 30, fun put_content_keeps_a_lookup_error_when_a_later_endpoint_lookup_is_cut_off_by_the_deadline/0},
      {timeout, 30, fun call_reports_a_timeout_when_no_endpoint_lookup_was_answered_in_time/0},
      {timeout, 30, fun put_content_asks_again_past_a_malformed_endpoint_record/0},
      {timeout, 30, fun put_content_reports_a_malformed_endpoint_record_at_its_deadline/0},
      {timeout, 30, fun put_content_asks_again_past_an_expired_endpoint_record/0},
      {timeout, 30, fun put_content_reports_an_expired_endpoint_record_at_its_deadline/0},
      {timeout, 30, fun resolve_station_endpoint_tells_an_expired_record_from_an_absent_one/0},
      {timeout, 30, fun put_content_ends_the_lookup_at_a_lifetime_refusal/0},
      {timeout, 30, fun put_content_ends_the_lookup_at_a_reversed_lifetime/0},
      {timeout, 30, fun put_content_ends_the_lookup_at_an_endpoint_record_that_does_not_verify/0},
      {timeout, 30, fun a_call_with_a_removed_trust_option_is_refused_before_any_lookup/0},
      {timeout, 30, fun a_stream_with_a_removed_trust_option_is_refused_before_any_lookup/0},
      {timeout, 30, fun a_call_passing_realm_trust_is_refused_before_any_lookup/0},
      {timeout, 30, fun a_stream_passing_realm_trust_is_refused_before_any_lookup/0},
      {timeout, 30, fun an_advertisement_with_a_removed_trust_option_is_not_published/0},
      {timeout, 30, fun a_stream_dial_timeout_outside_its_bounds_is_refused_in_the_caller/0},
      {timeout, 30, fun a_dial_io_with_an_unknown_function_is_refused_in_the_caller/0},
      {timeout, 30, fun a_dial_io_function_of_the_wrong_arity_is_refused_in_the_caller/0},
      {timeout, 30, fun a_dial_io_without_a_function_the_call_uses_is_refused_in_the_caller/0}]}.

%%%===================================================================
%%% Dial I/O
%%%===================================================================

%% A dial_io carrying a function direct dial does not take is refused, in the
%% caller, before anything is looked up.
a_dial_io_with_an_unknown_function_is_refused_in_the_caller() ->
    Io = (dial_io())#{publish => fun(_Pool, _Realm, _Topic, _Payload) -> ok end},
    ?assertError(function_clause,
                 macula_direct_dial:call(self(), ?REALM, ?PROC, <<"hi">>, 1000,
                                         #{dial_io => Io})),
    ?assertEqual(0, lookups(procedure_key())).

%% A function at another arity than its key takes is refused, in the caller,
%% before anything is looked up. The wrong arity is on a function this call
%% does not run, so the lookup it does run would be counted.
a_dial_io_function_of_the_wrong_arity_is_refused_in_the_caller() ->
    S = station(<<"s.test">>),
    set_endpoint_replies(S, [not_found]),
    Io = (dial_io())#{cancel => fun(_Transfer, _TimeoutMs) -> ok end},
    ?assertError(function_clause,
                 macula_direct_dial:resolve_station_endpoint(self(), maps:get(id, S), 1000,
                                                             #{dial_io => Io})),
    ?assertEqual(0, endpoint_lookups(S)).

%% A dial_io without a function the call runs on is refused, in the caller,
%% before the pool is asked for its links.
a_dial_io_without_a_function_the_call_uses_is_refused_in_the_caller() ->
    Links = fun(_Pool) -> _ = visit(links), {ok, []} end,
    Io = maps:remove(put_record, (dial_io())#{links => Links}),
    ?assertError(function_clause,
                 macula_direct_dial:publish_advertisement(self(), ?REALM, ?PROC,
                                                          node_key(identity),
                                                          #{dial_io => Io})),
    ?assertEqual([], visits()).

%% The DHT, the dials and the transfers direct dial runs on here: fakes that
%% count on the same ETS state as the meck ones.
dial_io() ->
    #{links => fun(_Pool) -> {ok, []} end,
      put_record => fun(_Pool, _Record) -> ok end,
      find_records => fun(_Pool, Key, TimeoutMs) -> find_records(Key, TimeoutMs) end,
      find_record => fun(_Pool, Key, TimeoutMs) -> find_record(Key, TimeoutMs) end,
      call_station =>
          fun(_Pool, DialUrl, _Provider, _Realm, _Proc, _Payload, _TimeoutMs, _Opts) ->
                  visit(DialUrl)
          end,
      call_stream_station =>
          fun(_Pool, DialUrl, _Provider, _Realm, _Proc, _Args, _Opts) -> visit(DialUrl) end,
      put_content_station =>
          fun(_Pool, DialUrl, _Bytes, _TimeoutMs, _Opts) -> visit(DialUrl) end,
      start_get_station =>
          fun(_Pool, Endpoint, _Mcid, _TimeoutMs, _Opts) -> {ok, {fake_transfer, Endpoint}} end,
      await => fun({fake_transfer, Endpoint}, _Timeout) -> visit(Endpoint) end,
      cancel => fun(_Transfer) -> ok end,
      %% No head start by default: every existing case here measures what
      %% resolution does when it has to ask the DHT, and a remembered
      %% candidate would skip the lookups they count.
      resolved_candidate => fun(_Pool, _Realm, _Proc) -> none end,
      remember_resolved => fun(_Pool, _Realm, _Proc, _Candidate, _TtlMs) -> ok end}.

%%%===================================================================
%%% Calls
%%%===================================================================

%% A station whose endpoint record can't be found is passed over for the next
%% advertisement's station.
call_tries_the_next_advertisement_when_a_station_has_no_endpoint() ->
    A = station(<<"a.test">>), B = station(<<"b.test">>),
    set_replies(procedure_key(), [[advertisement(A), advertisement(B)]]),
    set_endpoint(B, endpoint_record(B)),
    set_answer(dial_url(B), {ok, <<"from b">>}),
    ?assertEqual({ok, <<"from b">>}, call(3000)),
    ?assertEqual([dial_url(B)], visits()).

%% When records come back but none qualifies, here one for another procedure,
%% resolution asks again.
call_retries_when_no_advertisement_qualifies() ->
    Other = station(<<"other.test">>), Fresh = station(<<"fresh.test">>),
    set_replies(procedure_key(), [[advertisement(Other, <<"other_v1">>)], [advertisement(Fresh)]]),
    set_endpoint(Fresh, endpoint_record(Fresh)),
    set_answer(dial_url(Fresh), {ok, <<"from fresh">>}),
    ?assertEqual({ok, <<"from fresh">>}, call(3000)),
    ?assertEqual([dial_url(Fresh)], visits()).

%% A station whose link doesn't connect is passed over for the next one.
call_tries_the_next_station_when_a_dial_fails() ->
    A = station(<<"a.test">>), B = station(<<"b.test">>),
    set_replies(procedure_key(), [[advertisement(A), advertisement(B)]]),
    set_endpoint(A, endpoint_record(A)),
    set_endpoint(B, endpoint_record(B)),
    set_answer(dial_url(A), {error, not_connected}),
    set_answer(dial_url(B), {ok, <<"from b">>}),
    ?assertEqual({ok, <<"from b">>}, call(3000)),
    ?assertEqual([dial_url(A), dial_url(B)], visits()).

%% A DIAL THIS POOL REFUSED SENT NOTHING, SO THE NEXT CANDIDATE IS TRIED.
%%
%% `macula_client:refused_dial/2' answers before any worker starts: the pool is
%% at its direct-link cap, has spent its new-peer budget, or cannot dial that
%% seed at all. Nothing was built, let alone sent. And the refusal is about
%% THIS candidate rather than about the request, because another candidate may
%% need no new link at all -- the pool may already hold a live one to it.
call_tries_the_next_station_when_a_dial_is_refused() ->
    A = station(<<"a.test">>), B = station(<<"b.test">>),
    set_replies(procedure_key(), [[advertisement(A), advertisement(B)]]),
    set_endpoint(A, endpoint_record(A)),
    set_endpoint(B, endpoint_record(B)),
    set_answer(dial_url(A), {error, {dial_refused, too_many_direct_links}}),
    set_answer(dial_url(B), {ok, <<"from b">>}),
    ?assertEqual({ok, <<"from b">>}, call(3000)),
    ?assertEqual([dial_url(A), dial_url(B)], visits()).

%% THE OTHER HALF, AND THE ONE THAT KEEPS THE FIRST FROM BEING A LICENCE TO
%% RETRY EVERYTHING. A payload the wire cannot carry is refused identically by
%% every station, so resolution must stop at the first rather than spend its
%% deadline collecting the same answer.
call_stops_at_a_request_the_wire_cannot_carry() ->
    A = station(<<"a.test">>), B = station(<<"b.test">>),
    TooBig = {error, {refused, {unsupported_payload_type, payload_too_large, []}}},
    set_replies(procedure_key(), [[advertisement(A), advertisement(B)]]),
    set_endpoint(A, endpoint_record(A)),
    set_endpoint(B, endpoint_record(B)),
    set_answer(dial_url(A), TooBig),
    set_answer(dial_url(B), {ok, <<"never reached">>}),
    ?assertEqual(TooBig, call(3000)),
    ?assertEqual([dial_url(A)], visits()).

%% Once the CALL has gone out, its outcome stands: no other station receives it.
call_never_sends_the_request_twice() ->
    A = station(<<"a.test">>), B = station(<<"b.test">>),
    set_replies(procedure_key(), [[advertisement(A), advertisement(B)]]),
    set_endpoint(A, endpoint_record(A)),
    set_endpoint(B, endpoint_record(B)),
    set_answer(dial_url(A), {error, timeout}),
    set_answer(dial_url(B), {ok, <<"from b">>}),
    ?assertEqual({error, timeout}, call(3000)),
    ?assertEqual([dial_url(A)], visits()).

%% The timeout bounds resolution, not only the dial and the request.
call_timeout_bounds_resolution() ->
    {Elapsed, Result} = timed(fun() -> call(300) end),
    ?assertMatch({error, {unresolved, _}}, Result),
    ?assert(Elapsed < 1000).

%% The timeout bounds the lookup of a station's endpoint.
call_timeout_bounds_the_endpoint_lookup() ->
    A = station(<<"a.test">>),
    set_replies(procedure_key(), [[advertisement(A)]]),
    {Elapsed, Result} = timed(fun() -> call(300) end),
    ?assertMatch({error, {unresolved, _}}, Result),
    ?assert(Elapsed < 1000).

%%%===================================================================
%%% Head start
%%%===================================================================

%% The whole point: a remembered station is called with NO DHT lookup at all,
%% neither the advertisement's nor the station's endpoint. Nothing is planted
%% for either key here, so a call that reached the DHT could not have
%% answered.
call_answers_from_a_remembered_station_without_asking_the_dht() ->
    A = station(<<"a.test">>),
    remember_station(?PROC, A, provider_id()),
    set_answer(dial_url(A), {ok, <<"from a">>}),
    ?assertEqual({ok, <<"from a">>}, call(3000)),
    ?assertEqual([dial_url(A)], visits()),
    ?assertEqual(0, lookups(procedure_key())),
    ?assertEqual(0, endpoint_lookups(A)).

%% A HEAD START, NEVER A SUBSTITUTE. A remembered station whose link is gone
%% is passed over exactly as a DHT-resolved one is, and resolution carries on
%% into the DHT, which answers. This is the property the whole shape rests on:
%% the cache may not turn a call that would have succeeded into a failure.
call_falls_through_to_the_dht_when_the_remembered_station_does_not_connect() ->
    A = station(<<"a.test">>), B = station(<<"b.test">>),
    remember_station(?PROC, A, provider_id()),
    set_answer(dial_url(A), {error, not_connected}),
    set_replies(procedure_key(), [[advertisement(B)]]),
    set_endpoint(B, endpoint_record(B)),
    set_answer(dial_url(B), {ok, <<"from b">>}),
    ?assertEqual({ok, <<"from b">>}, call(3000)),
    ?assertEqual([dial_url(A), dial_url(B)], visits()),
    ?assertEqual(1, lookups(procedure_key())).

%% The head start is one candidate among the rest, not a separate mechanism:
%% when it fails and the DHT names the SAME station, that station is tried
%% again rather than suppressed as already-failed. A head start does no
%% endpoint lookup, so the first real endpoint record it sees is a change.
call_tries_a_remembered_station_again_when_the_dht_names_it() ->
    A = station(<<"a.test">>),
    Advertisement = advertisement(A),
    #{key_id := Provider} = Advertisement,
    remember_station(?PROC, A, Provider),
    set_replies(procedure_key(), [[Advertisement]]),
    set_endpoint(A, endpoint_record(A)),
    set_answers(dial_url(A), [{error, not_connected}, {ok, <<"from a">>}]),
    ?assertEqual({ok, <<"from a">>}, call(3000)),
    ?assertEqual([dial_url(A), dial_url(A)], visits()).

%% A remembered station that never connects and a DHT that never answers must
%% still end at the deadline rather than looping on the head start.
call_with_a_dead_head_start_and_no_advertisement_ends_at_its_deadline() ->
    A = station(<<"a.test">>),
    remember_station(?PROC, A, provider_id()),
    set_answer(dial_url(A), {error, not_connected}),
    {Elapsed, Result} = timed(fun() -> call(300) end),
    ?assertMatch({error, _}, Result),
    ?assert(Elapsed < 1000).

%% A stream resolves through the same machinery, so it gets the same head
%% start. An advertisement does not distinguish RPC from streaming.
call_stream_answers_from_a_remembered_station_without_asking_the_dht() ->
    A = station(<<"a.test">>),
    remember_station(?PROC, A, provider_id()),
    set_answer(dial_url(A), {ok, fake_stream}),
    ?assertEqual({ok, fake_stream}, call_stream(3000)),
    ?assertEqual(0, lookups(procedure_key())).

%%%===================================================================
%%% Remembering
%%%===================================================================

%% A DHT-resolved call that ANSWERED is remembered, as the candidate without
%% the bookkeeping the loop carried, and with a lifetime to hold it for.
call_remembers_the_station_that_answered() ->
    A = station(<<"a.test">>),
    Advertisement = advertisement(A),
    #{key_id := Provider, version := Version} = Advertisement,
    set_replies(procedure_key(), [[Advertisement]]),
    set_endpoint(A, endpoint_record(A)),
    set_answer(dial_url(A), {ok, <<"from a">>}),
    ?assertEqual({ok, <<"from a">>}, call(3000)),
    {Candidate, TtlMs} = remembered(?PROC),
    ?assertEqual(#{provider => Provider, version => Version,
                   station => maps:get(id, A)}, Candidate),
    ?assert(TtlMs > 0).

%% The lifetime is the advertisement's OWN remaining lifetime, relative to
%% now. It is the minimum of the three freshness surfaces by an invariant
%% macula_record enforces (see reusable_for/2), so it is read off the record
%% rather than re-derived.
%%
%% ⚠ THE ADVERTISEMENT IS BUILT WITH A DELIBERATELY NON-DEFAULT LIFETIME, and
%% that is the whole strength of this test. A procedure_advertisement's
%% default ttl is its type maximum, five minutes, so an earlier version of
%% this case using a default advertisement could not tell "read off the
%% record" from "hard-coded 300000": a mutation replacing reusable_for/2 with
%% that constant left it GREEN. A lifetime no constant would guess is what
%% makes the assertion mean what its name says.
call_remembers_for_the_advertisements_own_remaining_lifetime() ->
    A = station(<<"a.test">>),
    #{expires_at := ExpiresAt} = Advertisement = advertisement_living(A, 37_000),
    set_replies(procedure_key(), [[Advertisement]]),
    set_endpoint(A, endpoint_record(A)),
    set_answer(dial_url(A), {ok, <<"from a">>}),
    ?assertEqual({ok, <<"from a">>}, call(3000)),
    {_Candidate, TtlMs} = remembered(?PROC),
    Remaining = ExpiresAt - erlang:system_time(millisecond),
    ?assert(abs(Remaining - TtlMs) < 2_000),
    ?assert(TtlMs > 30_000 andalso TtlMs =< 37_000).

%% A CALL that went out and came back an error proves a route to the station,
%% not that the station still serves the procedure. A station answering
%% unknown_next_peer is precisely the one not to try first next time.
call_does_not_remember_a_station_whose_call_failed() ->
    A = station(<<"a.test">>),
    set_replies(procedure_key(), [[advertisement(A)]]),
    set_endpoint(A, endpoint_record(A)),
    set_answer(dial_url(A), {error, {call_error, unknown_next_peer, undefined}}),
    ?assertMatch({error, _}, call(3000)),
    ?assertEqual(none, remembered(?PROC)).

%% A HEAD START THAT ANSWERS DOES NOT REFRESH ITS OWN HORIZON. Answering is
%% no fresh evidence about the advertisement, and refreshing on a hit would
%% let one remembered station live for as long as it kept answering while the
%% DHT was never asked again.
call_does_not_remember_a_head_start_that_answered() ->
    A = station(<<"a.test">>),
    remember_station(?PROC, A, provider_id()),
    set_answer(dial_url(A), {ok, <<"from a">>}),
    ?assertEqual({ok, <<"from a">>}, call(3000)),
    ?assertEqual(none, remembered(?PROC)).

%%%===================================================================
%%% Streams
%%%===================================================================

%% A station whose link doesn't connect is passed over when opening a stream.
call_stream_tries_the_next_station_when_a_dial_fails() ->
    A = station(<<"a.test">>), B = station(<<"b.test">>),
    set_replies(procedure_key(), [[advertisement(A), advertisement(B)]]),
    set_endpoint(A, endpoint_record(A)),
    set_endpoint(B, endpoint_record(B)),
    set_answer(dial_url(A), {error, not_connected}),
    set_answer(dial_url(B), {ok, fake_stream}),
    ?assertEqual({ok, fake_stream}, call_stream(3000)),
    ?assertEqual([dial_url(A), dial_url(B)], visits()).

%% A stream whose opening frame may have gone out is not opened again elsewhere.
call_stream_never_opens_the_stream_twice() ->
    A = station(<<"a.test">>), B = station(<<"b.test">>),
    set_replies(procedure_key(), [[advertisement(A), advertisement(B)]]),
    set_endpoint(A, endpoint_record(A)),
    set_endpoint(B, endpoint_record(B)),
    set_answer(dial_url(A), {error, refused}),
    set_answer(dial_url(B), {ok, fake_stream}),
    ?assertEqual({error, refused}, call_stream(3000)),
    ?assertEqual([dial_url(A)], visits()).

%% A stream opens at a resolved station naming, as its target, the provider the
%% advertisement it was resolved from names.
call_stream_names_the_provider_its_advertisement_names() ->
    A = station(<<"a.test">>),
    Advertisement = advertisement(A),
    set_replies(procedure_key(), [[Advertisement]]),
    set_endpoint(A, endpoint_record(A)),
    set_answer(dial_url(A), {ok, fake_stream}),
    ?assertEqual({ok, fake_stream}, call_stream(3000)),
    #{advertiser_node := Provider} = macula_record:read_procedure_advertisement(Advertisement),
    ?assertEqual([{stream_provider, Provider}], ets:lookup(?STATE, stream_provider)).

%%%===================================================================
%%% Content
%%%===================================================================

%% When no provider has announced the content yet, resolution asks again.
get_content_retries_when_no_provider_qualifies() ->
    P = station(<<"p.test">>),
    Mcid = mcid(),
    set_replies(macula_record:content_key(Mcid), [[], [announcement(P, Mcid)]]),
    set_answer(dial_url(P), {ok, <<"content">>}),
    ?assertEqual({ok, <<"content">>},
                 macula_direct_dial:get_content(self(), Mcid, 3000)).

%% A provider whose fetch fails is passed over for the next: a fetch is
%% verified against its MCID, so trying another provider is safe.
get_content_tries_the_next_provider_after_a_failed_fetch() ->
    A = station(<<"a.test">>), B = station(<<"b.test">>),
    Mcid = mcid(),
    set_replies(macula_record:content_key(Mcid),
                [[announcement(A, Mcid), announcement(B, Mcid)]]),
    set_answer(dial_url(A), {error, hash_mismatch}),
    set_answer(dial_url(B), {ok, <<"content">>}),
    ?assertEqual({ok, <<"content">>},
                 macula_direct_dial:get_content(self(), Mcid, 3000)),
    ?assertEqual([dial_url(A), dial_url(B)], visits()).

%% The timeout bounds the search for a provider.
get_content_timeout_bounds_resolution() ->
    {Elapsed, Result} =
        timed(fun() -> macula_direct_dial:get_content(self(), mcid(), 300) end),
    ?assertMatch({error, {unresolved, _}}, Result),
    ?assert(Elapsed < 1000).

%% The timeout bounds the lookup of the station a put goes to.
put_content_timeout_bounds_the_endpoint_lookup() ->
    S = station(<<"s.test">>),
    {Elapsed, Result} = timed(fun() -> put_at_station(S, 300) end),
    ?assertMatch({error, {unresolved, _}}, Result),
    ?assert(Elapsed < 1000).

%%%===================================================================
%%% Records and trust
%%%===================================================================

%% A provider's advertisement is signed with its node identity key, names the
%% pool's connected station, and is put as its wire form.
a_published_advertisement_is_signed_by_the_node_identity_and_put_as_its_wire_form() ->
    Provider = node_key(identity),
    Station = station(<<"s.test">>),
    Test = self(),
    StationId = maps:get(id, Station),
    meck:expect(macula, links, fun(_Pool) -> {ok, [#{connected => true, node_id => StationId}]} end),
    meck:expect(macula_client, call_linked_station,
                fun(_Pool, _Realm, Procedure, Payload, _TimeoutMs) ->
                        Test ! {called, Procedure, Payload},
                        {ok, ok}
                end),
    ?assertEqual(ok, macula_direct_dial:publish_advertisement(self(), ?REALM, ?PROC, Provider)),
    {<<"_dht.put_record">>, Wire} = receive {called, P, W} -> {P, W} after 1000 -> erlang:error(not_put) end,
    {ok, Verified} = macula_record:verify(Wire, profile()),
    ?assertEqual(#{realm_id => ?REALM, procedure => ?PROC, advertiser_node => macula_node_keys:key_id(Provider),
                   serving_station => StationId, authorization => undefined},
                 macula_record:read_procedure_advertisement(Verified)).

%% verify_cert_chain turned a check on in 10.x and is gone. A call that still
%% passes it is refused by name before anything is looked up or dialed, instead
%% of resolving without the check it asked for; the realm trust the pool pins
%% replaces it.
a_call_with_a_removed_trust_option_is_refused_before_any_lookup() ->
    ?assertEqual({error, {removed_option, verify_cert_chain}},
                 macula_direct_dial:call(pool, ?REALM, ?PROC, #{}, 1000,
                                         #{verify_cert_chain => {<<"pem">>, ?ORG}})),
    ?assertEqual(0, lookups_and_dials()).

a_stream_with_a_removed_trust_option_is_refused_before_any_lookup() ->
    ?assertEqual({error, {removed_option, verify_cert_chain}},
                 macula_direct_dial:call_stream(pool, ?REALM, ?PROC, #{}, #{dial_timeout_ms => 1000},
                                                #{verify_cert_chain => {<<"pem">>, ?ORG}})),
    ?assertEqual(0, lookups_and_dials()).

%% A realm key never arrives with a request: the pool pins each realm's key
%% when it starts. A call or stream that still passes realm_trust is refused by
%% name before anything is looked up or dialed.
a_call_passing_realm_trust_is_refused_before_any_lookup() ->
    ?assertEqual({error, {removed_option, realm_trust}},
                 macula_direct_dial:call(pool, ?REALM, ?PROC, #{}, 1000, #{realm_trust => #{?REALM => <<"key">>}})),
    ?assertEqual(0, lookups_and_dials()).

a_stream_passing_realm_trust_is_refused_before_any_lookup() ->
    ?assertEqual({error, {removed_option, realm_trust}},
                 macula_direct_dial:call_stream(pool, ?REALM, ?PROC, #{}, #{dial_timeout_ms => 1000},
                                                #{realm_trust => #{?REALM => <<"key">>}})),
    ?assertEqual(0, lookups_and_dials()).

%% cert_chain carried a provider's certificate chain in 10.x and is gone. An
%% advertisement that still passes it is refused by name before the pool's
%% links are read or anything is put; authorization replaces it.
an_advertisement_with_a_removed_trust_option_is_not_published() ->
    Test = self(),
    meck:expect(macula, links, fun(_Pool) -> Test ! links_read, {ok, []} end),
    meck:expect(macula_client, call_linked_station,
                fun(_Pool, _Realm, _Procedure, _Payload, _TimeoutMs) -> Test ! put, {ok, ok} end),
    ?assertEqual({error, {removed_option, cert_chain}},
                 macula_direct_dial:publish_advertisement(self(), ?REALM, ?PROC, node_key(identity),
                                                          #{cert_chain => <<"pem">>})),
    ?assertEqual(none, receive links_read -> links_read; put -> put after 0 -> none end).

%% A stream's dial timeout is a positive number of milliseconds up to ten minutes, as a call's timeout is; anything else
%% is refused in the caller before anything is looked up.
a_stream_dial_timeout_outside_its_bounds_is_refused_in_the_caller() ->
    [?assertError(function_clause,
                  macula_direct_dial:call_stream(pool, ?REALM, ?PROC, #{}, #{dial_timeout_ms => Timeout}, #{}))
     || Timeout <- [infinity, 0, 600_001]],
    ?assertEqual(0, lookups_and_dials()).

lookups_and_dials() ->
    lists:sum([meck:num_calls(macula, Fun, '_')
               || Fun <- [find_records, find_record, call_station, call_stream_station]]).

%% The dial goes to the station a trusted advertisement names, pinned to that
%% station's node_id, and the CALL targets the provider that signed the
%% advertisement.
a_call_dials_the_station_a_trusted_advertisement_names_pinned_to_its_node_id() ->
    A = station(<<"a.test">>),
    Test = self(),
    #{key_id := Provider} = Advertisement = advertisement(A),
    set_replies(procedure_key(), [[Advertisement]]),
    set_endpoint(A, endpoint_record(A)),
    meck:expect(macula, call_station,
                fun(_Pool, DialUrl, Target, _Realm, _Proc, _Payload, _TimeoutMs, Opts) ->
                        Test ! {dialed, DialUrl, Target, Opts},
                        {ok, answered}
                end),
    ?assertEqual({ok, answered}, call(3000)),
    AId = maps:get(id, A),
    ?assertMatch({<<"quic://[a.test]:4433">>, Provider, #{expected_node_id := AId}},
                 receive {dialed, Url, Target, Opts} -> {Url, Target, Opts} after 0 -> none end).

%% An endpoint record under the station's key but signed by another node is
%% never dialled.
an_endpoint_signed_by_another_node_is_not_dialed() ->
    A = station(<<"a.test">>), Other = station(<<"other.test">>),
    set_replies(procedure_key(), [[advertisement(A)]]),
    set_endpoint(A, endpoint_record_signed_by(A, Other)),
    ?assertEqual({error, {unresolved, station_endpoint_signer_mismatch}}, call(1000)),
    ?assertEqual([], visits()).

%% An advertisement for an org namespaced procedure that carries no provider
%% authorization is no candidate.
an_org_namespaced_procedure_without_an_authorization_resolves_to_nothing() ->
    A = station(<<"a.test">>),
    set_replies(org_procedure_key(), [[advertisement(A, ?ORG_PROC)]]),
    set_endpoint(A, endpoint_record(A)),
    ?assertEqual({error, {unresolved, no_trusted_advertisement}}, call(500, ?ORG_PROC, #{})),
    ?assertEqual([], visits()).

%% With provider authorizations checked against the realm key the pool pins,
%% a station without an endpoint is passed over just the same.
call_with_authorization_tries_the_next_advertisement_when_a_station_has_no_endpoint() ->
    Authority = authority(),
    A = station(<<"a.test">>), B = station(<<"b.test">>),
    set_replies(org_procedure_key(), [[authorized_advertisement(A, Authority, ?ORG),
                                       authorized_advertisement(B, Authority, ?ORG)]]),
    set_endpoint(B, endpoint_record(B)),
    set_answer(dial_url(B), {ok, <<"from b">>}),
    pin_realm_key(?REALM, Authority),
    ?assertEqual({ok, <<"from b">>}, call(3000, ?ORG_PROC, #{})),
    ?assertEqual([dial_url(B)], visits()).

%% An advertisement authorized for another org doesn't qualify, and resolution
%% keeps asking until the deadline instead of giving up at once.
call_with_authorization_asks_again_until_its_deadline() ->
    Authority = authority(),
    A = station(<<"a.test">>),
    set_replies(org_procedure_key(), [[authorized_advertisement(A, Authority, <<"another-org">>)]]),
    set_endpoint(A, endpoint_record(A)),
    pin_realm_key(?REALM, Authority),
    {Elapsed, Result} = timed(fun() -> call(300, ?ORG_PROC, #{}) end),
    ?assertEqual({error, {unresolved, no_trusted_advertisement}}, Result),
    ?assert(Elapsed < 1000),
    ?assert(lookups(org_procedure_key()) >= 2),
    ?assertEqual([], visits()).

%% A realm key authorizes its own realm's advertisements only. With the
%% authority's key pinned for another realm id, an advertisement for ?REALM
%% whose org directory that key signed is no candidate; pinned for ?REALM, the
%% same advertisement is.
a_realm_key_pinned_for_another_realm_authorizes_nothing_in_this_one() ->
    Authority = authority(),
    A = station(<<"a.test">>),
    set_replies(org_procedure_key(), [[authorized_advertisement(A, Authority, ?ORG)]]),
    set_endpoint(A, endpoint_record(A)),
    set_answer(dial_url(A), {ok, <<"from a">>}),
    pin_realm_key(?OTHER_REALM, Authority),
    ?assertEqual({error, {unresolved, no_trusted_advertisement}}, call(300, ?ORG_PROC, #{})),
    ?assertEqual([], visits()),
    pin_realm_key(?REALM, Authority),
    ?assertEqual({ok, <<"from a">>}, call(3000, ?ORG_PROC, #{})),
    ?assertEqual([dial_url(A)], visits()).

%%%===================================================================
%%% A failed candidate is tried again only on a changed record
%%%===================================================================

%% A station that refuses at once is dialled once per version of its endpoint
%% record, not again on every pass over the DHT.
call_dials_a_refusing_station_once_per_endpoint_version() ->
    A = station(<<"a.test">>),
    set_replies(procedure_key(), [[advertisement(A)]]),
    set_endpoint(A, endpoint_record(A)),
    Republished = endpoint_record(A),
    {ok, _} = timer:apply_after(1500, ets, insert,
                                [?STATE, {endpoint_entry(A), Republished}]),
    set_answer(dial_url(A), {error, not_connected}),
    ?assertEqual({error, not_connected}, call(3000)),
    ?assertEqual([dial_url(A), dial_url(A)], visits()).

%% An advertisement that appears on a later pass is tried, while the station
%% that already refused, with nothing changed, is not dialled again.
call_tries_an_advertisement_that_appears_on_a_later_pass() ->
    A = station(<<"a.test">>), B = station(<<"b.test">>),
    AdA = advertisement(A),
    set_replies(procedure_key(), [[AdA], [AdA, advertisement(B)]]),
    set_endpoint(A, endpoint_record(A)),
    set_endpoint(B, endpoint_record(B)),
    set_answer(dial_url(A), {error, not_connected}),
    set_answer(dial_url(B), {ok, <<"from b">>}),
    ?assertEqual({ok, <<"from b">>}, call(3000)),
    ?assertEqual([dial_url(A), dial_url(B)], visits()).

%% Passes over the DHT back off, doubling from 100 ms to at most 1 s: lookups
%% at about 0, 0.1, 0.3, 0.7, 1.5 and 2.5 s in a 3 s deadline.
resolution_backs_off_between_passes() ->
    _ = call(3000),
    Lookups = lookups(procedure_key()),
    ?assert(Lookups >= 5 andalso Lookups =< 8).

%% A provider whose fetch fails is fetched from again only when its
%% announcement changes.
get_content_fetches_from_a_failing_provider_once_per_announcement() ->
    A = station(<<"a.test">>),
    Mcid = mcid(),
    set_replies(macula_record:content_key(Mcid), [[announcement(A, Mcid)]]),
    set_answer(dial_url(A), {error, hash_mismatch}),
    ?assertEqual({error, hash_mismatch},
                 macula_direct_dial:get_content(self(), Mcid, 2000)),
    ?assertEqual([dial_url(A)], visits()).

%% A station whose endpoint record changes partway through the deadline is
%% reached at the new endpoint.
call_picks_up_an_endpoint_record_that_changes_mid_deadline() ->
    Old = station(<<"a-old.test">>),
    Moved = Old#{host => <<"a.test">>},
    set_replies(procedure_key(), [[advertisement(Old)]]),
    set_endpoint(Old, endpoint_record(Old)),
    Republished = endpoint_record(Moved),
    {ok, _} = timer:apply_after(500, ets, insert,
                                [?STATE, {endpoint_entry(Moved), Republished}]),
    set_answer(dial_url(Old), {error, not_connected}),
    set_answer(dial_url(Moved), {ok, <<"from a">>}),
    ?assertEqual({ok, <<"from a">>}, call(3000)),
    ?assertEqual([dial_url(Old), dial_url(Moved)], visits()).

%%%===================================================================
%%% Direct downloads (macula_download)
%%%===================================================================

%% A direct download whose provider's fetch fails moves on to the next provider.
download_direct_tries_the_next_provider_after_a_failed_fetch() ->
    process_flag(trap_exit, true),
    A = station(<<"a.test">>), B = station(<<"b.test">>),
    Mcid = mcid(),
    set_replies(macula_record:content_key(Mcid),
                [[announcement(A, Mcid), announcement(B, Mcid)]]),
    set_answer(dial_url(A), {error, hash_mismatch}),
    set_answer(dial_url(B), {ok, <<"content">>}),
    {ok, _Download} = macula_download:start_link_direct(?MODULE, self(), ?REALM, Mcid, self()),
    ?assertEqual({downloaded, {ok, <<"content">>}}, downloaded()),
    ?assertEqual([dial_url(A), dial_url(B)], visits()).

%% A direct download asks again when no provider has announced the content yet.
download_direct_retries_when_no_provider_qualifies() ->
    process_flag(trap_exit, true),
    P = station(<<"p.test">>),
    Mcid = mcid(),
    set_replies(macula_record:content_key(Mcid), [[], [announcement(P, Mcid)]]),
    set_answer(dial_url(P), {ok, <<"content">>}),
    {ok, _Download} = macula_download:start_link_direct(?MODULE, self(), ?REALM, Mcid, self()),
    ?assertEqual({downloaded, {ok, <<"content">>}}, downloaded()).

%% resolve_content_provider/2 returns the first provider whose announcement
%% qualifies, asking again until one does.
resolve_content_provider_returns_the_first_qualifying_provider() ->
    P = station(<<"p.test">>),
    Mcid = mcid(),
    set_replies(macula_record:content_key(Mcid), [[], [announcement(P, Mcid)]]),
    Node = maps:get(id, P),
    Endpoint = dial_url(P),
    ?assertMatch({ok, #{announcer_node := Node, endpoint := Endpoint}},
                 macula_direct_dial:resolve_content_provider(self(), Mcid)).

%% When no provider qualifies within its 10 seconds, resolve_content_provider/2
%% reports the content as not announced.
resolve_content_provider_reports_content_not_announced() ->
    Mcid = mcid(),
    set_replies(macula_record:content_key(Mcid), [[]]),
    ?assertEqual({error, content_not_announced},
                 macula_direct_dial:resolve_content_provider(self(), Mcid)).

%% Once a candidate has failed before sending, a later pass that finds no
%% candidate doesn't take its place: the call reports the refused dial.
call_reports_the_last_candidate_failure_when_a_later_pass_finds_none() ->
    A = station(<<"a.test">>),
    set_replies(procedure_key(), [[advertisement(A)], []]),
    set_endpoint(A, endpoint_record(A)),
    set_answer(dial_url(A), {error, not_connected}),
    ?assertEqual({error, not_connected}, call(1000)).

%% Nor does a later lookup that fails outright.
call_reports_the_last_candidate_failure_when_a_later_lookup_fails() ->
    A = station(<<"a.test">>),
    set_replies(procedure_key(), [[advertisement(A)], {error, connection_lost}]),
    set_endpoint(A, endpoint_record(A)),
    set_answer(dial_url(A), {error, not_connected}),
    ?assertEqual({error, not_connected}, call(1000)).

%% get_content reports a provider's failed fetch in the same way when a later
%% content lookup fails.
get_content_reports_the_last_candidate_failure_when_a_later_lookup_fails() ->
    A = station(<<"a.test">>),
    Mcid = mcid(),
    set_replies(macula_record:content_key(Mcid),
                [[announcement(A, Mcid)], {error, connection_lost}]),
    set_answer(dial_url(A), {error, not_connected}),
    ?assertEqual({error, not_connected},
                 macula_direct_dial:get_content(self(), Mcid, 1000)).

%% At the deadline a call reports, in this order: the last candidate failure,
%% why an answered lookup found nothing qualifying, a failed lookup's own
%% error, or a timeout.

%% A lookup that answered decides the reason over later lookups that failed.
call_reports_not_advertised_when_a_lookup_answered_before_later_ones_failed() ->
    set_replies(procedure_key(), [[], {error, connection_lost}]),
    ?assertEqual({error, {unresolved, procedure_not_advertised}}, call(1000)).

get_content_reports_not_announced_when_a_lookup_answered_before_later_ones_failed() ->
    Mcid = mcid(),
    set_replies(macula_record:content_key(Mcid), [[], {error, connection_lost}]),
    ?assertEqual({error, {unresolved, content_not_announced}},
                 macula_direct_dial:get_content(self(), Mcid, 1000)).

%% When every lookup failed, the lookup's own error is the reason.
call_reports_a_failed_lookup_at_its_deadline_when_no_candidate_was_tried() ->
    set_replies(procedure_key(), [{error, connection_lost}]),
    ?assertEqual({error, {unresolved, connection_lost}}, call(1000)).

get_content_reports_a_failed_lookup_at_its_deadline_when_no_provider_was_tried() ->
    Mcid = mcid(),
    set_replies(macula_record:content_key(Mcid), [{error, connection_lost}]),
    ?assertEqual({error, {unresolved, connection_lost}},
                 macula_direct_dial:get_content(self(), Mcid, 1000)).

%% With no answer and no error before the deadline, the reason is a timeout.
call_reports_a_timeout_when_no_lookup_was_answered_in_time() ->
    set_replies(procedure_key(), [silent]),
    ?assertEqual({error, {unresolved, timeout}}, call(500)).

get_content_reports_a_timeout_when_no_lookup_was_answered_in_time() ->
    Mcid = mcid(),
    set_replies(macula_record:content_key(Mcid), [silent]),
    ?assertEqual({error, {unresolved, timeout}},
                 macula_direct_dial:get_content(self(), Mcid, 500)).

%% A lookup the deadline cuts off records nothing, so an earlier lookup's
%% error stands.
call_keeps_a_lookup_error_when_a_later_lookup_is_cut_off_by_the_deadline() ->
    set_replies(procedure_key(), [{error, connection_lost}, silent]),
    ?assertEqual({error, {unresolved, connection_lost}}, call(1000)).

%% A failed lookup is an empty pass: the call asks again and can still succeed.
call_retries_after_a_lookup_fails() ->
    A = station(<<"a.test">>),
    set_replies(procedure_key(), [{error, connection_lost}, [advertisement(A)]]),
    set_endpoint(A, endpoint_record(A)),
    set_answer(dial_url(A), {ok, <<"from a">>}),
    ?assertEqual({ok, <<"from a">>}, call(2000)).

get_content_retries_after_a_lookup_fails() ->
    P = station(<<"p.test">>),
    Mcid = mcid(),
    set_replies(macula_record:content_key(Mcid), [{error, connection_lost}, [announcement(P, Mcid)]]),
    set_answer(dial_url(P), {ok, <<"content">>}),
    ?assertEqual({ok, <<"content">>}, macula_direct_dial:get_content(self(), Mcid, 2000)).

%% Within one station endpoint lookup, retries included, the result is: the
%% endpoint not found when a lookup answered so, else a failed lookup's
%% error, else a timeout.
put_content_reports_no_station_endpoint_when_a_lookup_answered_not_found() ->
    S = station(<<"s.test">>),
    set_endpoint_replies(S, [not_found, {error, connection_lost}]),
    ?assertEqual({error, {unresolved, station_endpoint_not_found}}, put_at_station(S, 1000)).

put_content_retries_an_endpoint_lookup_that_fails() ->
    S = station(<<"s.test">>),
    set_endpoint_replies(S, [{error, connection_lost}, endpoint_record(S)]),
    set_answer(dial_url(S), {ok, <<"mcid">>}),
    ?assertEqual({ok, <<"mcid">>}, put_at_station(S, 2000)).

put_content_reports_a_failed_endpoint_lookup_when_every_lookup_failed() ->
    S = station(<<"s.test">>),
    set_endpoint_replies(S, [{error, connection_lost}]),
    ?assertEqual({error, {unresolved, connection_lost}}, put_at_station(S, 1000)),
    ?assert(endpoint_lookups(S) > 1).

put_content_reports_a_timeout_when_no_endpoint_lookup_was_answered_in_time() ->
    S = station(<<"s.test">>),
    set_endpoint_replies(S, [silent]),
    ?assertEqual({error, {unresolved, timeout}}, put_at_station(S, 500)).

put_content_keeps_a_lookup_error_when_a_later_endpoint_lookup_is_cut_off_by_the_deadline() ->
    S = station(<<"s.test">>),
    set_endpoint_replies(S, [{error, connection_lost}, silent]),
    ?assertEqual({error, {unresolved, connection_lost}}, put_at_station(S, 1000)).

%% A candidate whose endpoint lookup never answers in time fails with a
%% timeout, and so does the call when it was the last candidate tried.
call_reports_a_timeout_when_no_endpoint_lookup_was_answered_in_time() ->
    A = station(<<"a.test">>),
    set_replies(procedure_key(), [[advertisement(A)]]),
    set_endpoint_replies(A, [silent]),
    ?assertEqual({error, {unresolved, timeout}}, call(500)).

%% A record that verifies but names no dialable endpoint is asked about
%% again, as an absent one is.
put_content_asks_again_past_a_malformed_endpoint_record() ->
    S = station(<<"s.test">>),
    set_endpoint_replies(S, [malformed_endpoint_record(S), endpoint_record(S)]),
    set_answer(dial_url(S), {ok, <<"mcid">>}),
    ?assertEqual({ok, <<"mcid">>}, put_at_station(S, 2000)).

%% When every lookup found only a malformed record, that is the reason.
put_content_reports_a_malformed_endpoint_record_at_its_deadline() ->
    S = station(<<"s.test">>),
    set_endpoint_replies(S, [malformed_endpoint_record(S)]),
    ?assertEqual({error, {unresolved, malformed_station_endpoint}}, put_at_station(S, 1000)),
    ?assert(endpoint_lookups(S) > 1).

%% The facade refuses an expired endpoint record as `expired'; that is asked
%% about again, as an absent record is.
put_content_asks_again_past_an_expired_endpoint_record() ->
    S = station(<<"s.test">>),
    set_endpoint_replies(S, [{error, expired}, endpoint_record(S)]),
    set_answer(dial_url(S), {ok, <<"mcid">>}),
    ?assertEqual({ok, <<"mcid">>}, put_at_station(S, 2000)).

%% An endpoint record that stays expired to the deadline is reported AS
%% expired, not as an absent one. The two mean different things to an
%% operator: absent is "this station never published an endpoint", expired is
%% "this station published one and our own clock check refused it as stale".
%% It is asked about again throughout, as an absent record is.
put_content_reports_an_expired_endpoint_record_at_its_deadline() ->
    S = station(<<"s.test">>),
    set_endpoint_replies(S, [{error, expired}]),
    ?assertEqual({error, {unresolved, station_endpoint_expired}}, put_at_station(S, 1000)),
    ?assert(endpoint_lookups(S) > 1).

%% The two conditions reach the public facade as two different atoms. This is
%% the surface an operator reads, and reading "not found" for a record the
%% station served is what sends them looking in the wrong place.
resolve_station_endpoint_tells_an_expired_record_from_an_absent_one() ->
    Expired = station(<<"expired.test">>),
    Absent = station(<<"absent.test">>),
    set_endpoint_replies(Expired, [{error, expired}]),
    set_endpoint_replies(Absent, [not_found]),
    ?assertEqual({error, station_endpoint_expired}, resolve_endpoint(Expired, 1000)),
    ?assertEqual({error, station_endpoint_not_found}, resolve_endpoint(Absent, 1000)).

%% A lifetime refusal is permanent: the record's own created_at and
%% expires_at are outside what its type allows, and asking the same station
%% again cannot change that. It ends the lookup rather than being retried to
%% the deadline like a transport failure.
put_content_ends_the_lookup_at_a_lifetime_refusal() ->
    S = station(<<"s.test">>),
    set_endpoint_replies(S, [{error, lifetime_too_long}, endpoint_record(S)]),
    ?assertEqual({error, {unresolved, lifetime_too_long}}, put_at_station(S, 1000)),
    ?assertEqual(1, endpoint_lookups(S)),
    ?assertEqual([], visits()).

put_content_ends_the_lookup_at_a_reversed_lifetime() ->
    S = station(<<"s.test">>),
    set_endpoint_replies(S, [{error, lifetime_reversed}, endpoint_record(S)]),
    ?assertEqual({error, {unresolved, lifetime_reversed}}, put_at_station(S, 1000)),
    ?assertEqual(1, endpoint_lookups(S)),
    ?assertEqual([], visits()).

%% A record the facade refuses for any other reason is a record that does not
%% verify, and ends the lookup with that reason.
put_content_ends_the_lookup_at_an_endpoint_record_that_does_not_verify() ->
    S = station(<<"s.test">>),
    set_endpoint_replies(S, [{error, signature_invalid}, endpoint_record(S)]),
    ?assertEqual({error, {unresolved, signature_invalid}}, put_at_station(S, 1000)),
    ?assertEqual(1, endpoint_lookups(S)),
    ?assertEqual([], visits()).

put_at_station(#{id := Id}, TimeoutMs) ->
    macula_direct_dial:put_content(self(), Id, <<"bytes">>, TimeoutMs).

resolve_endpoint(#{id := Id}, TimeoutMs) ->
    macula_direct_dial:resolve_station_endpoint(self(), Id, TimeoutMs).

%% macula_download callbacks: hand the outcome back to the test process.
init(Parent) -> {ok, Parent}.

handle_downloaded(Result, Parent) ->
    Parent ! {downloaded, Result},
    {stop, normal, Parent}.

downloaded() ->
    receive
        {downloaded, Result} -> {downloaded, Result}
    after 40_000 -> no_download_result
    end.

%%%===================================================================
%%% Helpers: calls and fakes
%%%===================================================================

call(TimeoutMs) -> call(TimeoutMs, ?PROC, #{}).

call(TimeoutMs, Procedure, Opts) ->
    macula_direct_dial:call(self(), ?REALM, Procedure, <<"hi">>, TimeoutMs, Opts).

call_stream(DialTimeoutMs) ->
    macula_direct_dial:call_stream(self(), ?REALM, ?PROC, <<"args">>,
                                   #{dial_timeout_ms => DialTimeoutMs}, #{}).

timed(Fun) ->
    Start = erlang:monotonic_time(millisecond),
    Result = Fun(),
    {erlang:monotonic_time(millisecond) - Start, Result}.

find_records(Key) -> find_records(Key, 0).

%% Each lookup answers with the next of a key's replies, the last one
%% repeating: a list of records, `{error, Reason}' for a lookup that fails, or
%% `silent' for one that never answers and so runs out its timeout.
find_records(Key, TimeoutMs) ->
    Asked = ets:update_counter(?STATE, {asked, Key}, 1, {{asked, Key}, 0}) - 1,
    lookup_reply(reply_at(ets:lookup(?STATE, {replies, Key}), Asked), TimeoutMs).

lookup_reply(silent, TimeoutMs) ->
    timer:sleep(TimeoutMs),
    {error, timeout};
lookup_reply({error, _} = Failed, _TimeoutMs) -> Failed;
lookup_reply(Records, _TimeoutMs) -> {ok, Records}.

reply_at([], _Asked) -> [];
reply_at([{_, Replies}], Asked) -> lists:nth(min(Asked + 1, length(Replies)), Replies).

find_record(Key) -> find_record(Key, 0).

%% A station's endpoint lookups answer from its scripted replies when it has
%% them (set_endpoint_replies/2), the last one repeating: a record, `not_found',
%% `{error, Reason}' for a lookup that fails or a record the facade refused, or
%% `silent' for one that never answers. Otherwise they answer from its one
%% record, if any.
find_record(Key, TimeoutMs) ->
    endpoint_reply(ets:lookup(?STATE, {endpoint_replies, Key}), Key, TimeoutMs).

endpoint_reply([{_, Replies}], Key, TimeoutMs) ->
    Asked = ets:update_counter(?STATE, {endpoint_asked, Key}, 1,
                               {{endpoint_asked, Key}, 0}) - 1,
    endpoint_lookup_reply(lists:nth(min(Asked + 1, length(Replies)), Replies), TimeoutMs);
endpoint_reply([], Key, _TimeoutMs) ->
    endpoint_found(ets:lookup(?STATE, {endpoint, Key})).

endpoint_lookup_reply(not_found, _TimeoutMs) -> {error, not_found};
endpoint_lookup_reply(silent, TimeoutMs) ->
    timer:sleep(TimeoutMs),
    {error, timeout};
endpoint_lookup_reply({error, _} = Failed, _TimeoutMs) -> Failed;
endpoint_lookup_reply(Record, _TimeoutMs) -> {ok, Record}.

set_endpoint_replies(#{id := Id}, Replies) ->
    ets:insert(?STATE, {{endpoint_replies, macula_record:station_endpoint_key(Id)}, Replies}).

endpoint_lookups(#{id := Id}) ->
    lookup_count(ets:lookup(?STATE, {endpoint_asked, macula_record:station_endpoint_key(Id)})).

endpoint_found([{_, Record}]) -> {ok, Record};
endpoint_found([]) -> {error, not_found}.

visit(Target) ->
    [{visits, Seen}] = ets:lookup(?STATE, visits),
    ets:insert(?STATE, {visits, Seen ++ [Target]}),
    answer(ets:lookup(?STATE, {answer, Target}), Target).

%% A sequence answers each dial in turn and then keeps answering its last,
%% for a case that needs the SAME target to answer differently the second
%% time it is dialled.
answer([{_, {sequence, [Next | Rest]}}], Target) ->
    ets:insert(?STATE, {{answer, Target}, sequence_or_last(Rest, Next)}),
    Next;
answer([{_, Answer}], _Target) -> Answer;
answer([], _Target) -> {error, not_connected}.

sequence_or_last([], Last)     -> Last;
sequence_or_last(Rest, _Last)  -> {sequence, Rest}.

visits() ->
    [{visits, Seen}] = ets:lookup(?STATE, visits),
    Seen.

lookups(Key) ->
    lookup_count(ets:lookup(?STATE, {asked, Key})).

lookup_count([{_, N}]) -> N;
lookup_count([]) -> 0.

set_replies(Key, Replies) -> ets:insert(?STATE, {{replies, Key}, Replies}).

set_endpoint(Station, Record) -> ets:insert(?STATE, {endpoint_entry(Station), Record}).

endpoint_entry(#{id := Id}) -> {endpoint, macula_record:station_endpoint_key(Id)}.

set_answer(Target, Answer) -> ets:insert(?STATE, {{answer, Target}, Answer}).

set_answers(Target, [_ | _] = Answers) ->
    ets:insert(?STATE, {{answer, Target}, {sequence, Answers}}).

%% A provider node id for a planted head start. The head-start path reads no
%% record, so nothing verifies this: it only has to be a node id.
provider_id() -> <<16#5A:256>>.

%% Plant the head start the pool would hand back: the candidate it remembered
%% and the seed its own live link to that station is keyed by.
remember_station(Procedure, #{id := Id} = Station, Provider) ->
    ets:insert(?STATE, {{head_start, ?REALM, Procedure},
                        #{provider => Provider, version => <<9:128>>, station => Id},
                        dial_url(Station)}).

planted_head_start(Realm, Procedure) ->
    planted(ets:lookup(?STATE, {head_start, Realm, Procedure})).

planted([{_Key, Candidate, Seed}]) -> {ok, Candidate, Seed};
planted([])                        -> none.

%% What the code remembered for a procedure, as {Candidate, TtlMs}, or none.
remembered(Procedure) ->
    recalled(ets:lookup(?STATE, {remembered, ?REALM, Procedure})).

recalled([{_Key, Candidate, TtlMs}]) -> {Candidate, TtlMs};
recalled([])                         -> none.

%%%===================================================================
%%% Helpers: keys and records
%%%===================================================================

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

node_key(Purpose) ->
    {ok, Key} = macula_node_keys:generate(Purpose, profile()),
    Key.

%% A record as the facade hands it over: verified under the node's profile.
verified(Signed) ->
    {ok, Verified} = macula_record:verify(macula_record:encode(Signed), profile()),
    Verified.

station(Host) ->
    Key = node_key(identity),
    #{key => Key, id => macula_node_keys:key_id(Key), host => Host}.

dial_url(#{host := Host}) -> <<"quic://[", Host/binary, "]:4433">>.

endpoint_record(Station) ->
    endpoint_record_signed_by(Station, Station).

%% A station_endpoint record naming Station's host, signed by Signer's key.
endpoint_record_signed_by(#{host := Host}, #{key := SignerKey}) ->
    verified(macula_record:sign(macula_record:station_endpoint(4433, #{host_advertised => [Host]}), SignerKey)).

%% Station's own signed station_endpoint record, naming no host.
malformed_endpoint_record(#{key := Key}) ->
    verified(macula_record:sign(macula_record:station_endpoint(4433, #{}), Key)).

procedure_key() -> macula_record:procedure_key(?REALM, ?PROC).

org_procedure_key() -> macula_record:procedure_key(?REALM, ?ORG_PROC).

advertisement(Station) -> advertisement(Station, ?PROC).

%% An advertisement for ?PROC with a lifetime of its own rather than the
%% type's default, so a test can tell a lifetime READ from the record apart
%% from one that happens to equal the default.
advertisement_living(#{id := StationId}, TtlMs) ->
    Provider = node_key(identity),
    verified(macula_record:sign(
               macula_record:procedure_advertisement(macula_node_keys:key_id(Provider), ?REALM,
                                                     ?PROC, StationId, #{ttl_ms => TtlMs}),
               Provider)).

%% A signed procedure_advertisement for Procedure from a fresh provider naming
%% Station.
advertisement(#{id := StationId}, Procedure) ->
    Provider = node_key(identity),
    verified(macula_record:sign(
               macula_record:procedure_advertisement(macula_node_keys:key_id(Provider), ?REALM, Procedure,
                                                     StationId),
               Provider)).

mcid() -> <<2, 16#55, (crypto:strong_rand_bytes(48))/binary>>.

announcement(#{key := Key, id := Id} = Station, Mcid) ->
    verified(macula_record:sign(macula_record:content_announcement(Id, Mcid, dial_url(Station)), Key)).

%% The realm key and the org key an authorization chains to.
authority() ->
    #{realm => node_key(realm), org => node_key(org)}.

%% The pool pins the authority's realm key for RealmId alone.
pin_realm_key(RealmId, #{realm := Realm}) ->
    Key = macula_node_keys:public_key(Realm),
    meck:expect(macula_client, realm_key, fun(_Pool, Id) when Id =:= RealmId -> {ok, Key};
                                             (_Pool, _Other) -> none
                                          end).

%% An advertisement for ?ORG_PROC from a fresh provider naming Station, carrying
%% the realm-signed org directory that names OrgName and the org-signed
%% delegation to that provider.
authorized_advertisement(#{id := StationId}, #{realm := Realm, org := Org}, OrgName) ->
    Provider = node_key(identity),
    ProviderId = macula_node_keys:key_id(Provider),
    OrgId = macula_node_keys:key_id(Org),
    OrgDirectory = macula_record:sign(macula_record:org_directory(?REALM, OrgName, OrgId), Realm),
    Delegation = macula_record:sign(macula_record:procedure_delegation(OrgId, ProviderId, #{ttl_ms => 6 * ?HOUR_MS}),
                                    Org),
    Authorization = #{org_directory => macula_record:encode(OrgDirectory),
                      procedure_delegation => macula_record:encode(Delegation)},
    verified(macula_record:sign(
               macula_record:procedure_advertisement(ProviderId, ?REALM, ?ORG_PROC, StationId,
                                                     #{authorization => Authorization, ttl_ms => 300_000}),
               Provider)).
