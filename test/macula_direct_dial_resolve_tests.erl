%%% @doc Direct-dial resolution in `macula_direct_dial': every advertisement
%%% that verifies is a candidate; one whose endpoint can't be resolved or
%%% whose dial doesn't connect is passed over for the next before anything is
%%% sent; resolution asks the DHT again until the call's deadline, backing off
%%% to one second; a candidate that failed is tried again only on a changed
%%% record; a content fetch that fails moves on to the next provider.
%%%
%%% The DHT, the dials and the transfers are faked with meck on the `macula'
%%% facade and `macula_content_transfer'; the records themselves are real and
%%% signed.
-module(macula_direct_dial_resolve_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

-behaviour(macula_download).
-export([init/1, handle_downloaded/2]).

-define(STATE, macula_direct_dial_resolve_tests_state).
-define(REALM, <<0:256>>).
-define(ORG, <<"resolve-tests">>).
-define(PROC, <<"resolve-tests/echo_v1">>).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup() ->
    ?STATE = ets:new(?STATE, [named_table, public, set]),
    ets:insert(?STATE, {visits, []}),
    meck:new(macula, [passthrough, non_strict]),
    meck:new(macula_content_transfer, [passthrough]),
    meck:expect(macula, find_records, fun(_Pool, Key) -> find_records(Key) end),
    meck:expect(macula, find_records,
                fun(_Pool, Key, TimeoutMs) -> find_records(Key, TimeoutMs) end),
    meck:expect(macula, find_record, fun(_Pool, Key) -> find_record(Key) end),
    meck:expect(macula, find_record,
                fun(_Pool, Key, TimeoutMs) -> find_record(Key, TimeoutMs) end),
    meck:expect(macula, find_content_providers,
                fun(_Pool, Mcid) -> {ok, providers(Mcid)} end),
    meck:expect(macula, call_station,
                fun(_Pool, DialUrl, _Realm, _Proc, _Payload, _TimeoutMs, _Opts) ->
                        visit(DialUrl)
                end),
    meck:expect(macula, call_stream_station,
                fun(_Pool, DialUrl, _Realm, _Proc, _Args, _Opts) -> visit(DialUrl) end),
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
    ok.

teardown(_) ->
    meck:unload([macula, macula_content_transfer]),
    ets:delete(?STATE).

resolve_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{timeout, 30, fun call_tries_the_next_advertisement_when_a_station_has_no_endpoint/0},
      {timeout, 30, fun call_retries_when_no_advertisement_qualifies/0},
      {timeout, 30, fun call_tries_the_next_station_when_a_dial_fails/0},
      {timeout, 30, fun call_never_sends_the_request_twice/0},
      {timeout, 30, fun call_timeout_bounds_resolution/0},
      {timeout, 30, fun call_timeout_bounds_the_endpoint_lookup/0},
      {timeout, 30, fun call_stream_tries_the_next_station_when_a_dial_fails/0},
      {timeout, 30, fun call_stream_never_opens_the_stream_twice/0},
      {timeout, 30, fun get_content_retries_when_no_provider_qualifies/0},
      {timeout, 30, fun get_content_tries_the_next_provider_after_a_failed_fetch/0},
      {timeout, 30, fun get_content_timeout_bounds_resolution/0},
      {timeout, 30, fun put_content_timeout_bounds_the_endpoint_lookup/0},
      {timeout, 30, fun call_with_cert_chain_tries_the_next_advertisement_when_a_station_has_no_endpoint/0},
      {timeout, 30, fun call_with_cert_chain_asks_again_until_its_deadline/0},
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
      {timeout, 30, fun call_reports_a_timeout_when_no_endpoint_lookup_was_answered_in_time/0}]}.

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

%% When records come back but none qualifies, resolution asks again.
call_retries_when_no_advertisement_qualifies() ->
    Stale = station(<<"stale.test">>), Fresh = station(<<"fresh.test">>),
    set_replies(procedure_key(), [[expired(advertisement(Stale))], [advertisement(Fresh)]]),
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
    {Elapsed, Result} =
        timed(fun() ->
                      macula_direct_dial:put_content(self(), maps:get(key, S),
                                                     <<"bytes">>, 300)
              end),
    ?assertMatch({error, {unresolved, _}}, Result),
    ?assert(Elapsed < 1000).

%%%===================================================================
%%% Cert chains
%%%===================================================================

%% With cert-chain verification, a station without an endpoint is passed over
%% just the same.
call_with_cert_chain_tries_the_next_advertisement_when_a_station_has_no_endpoint() ->
    Ca = realm_ca(),
    A = station(<<"a.test">>), B = station(<<"b.test">>),
    set_replies(procedure_key(), [[authorized_advertisement(A, Ca, ?ORG),
                                   authorized_advertisement(B, Ca, ?ORG)]]),
    set_endpoint(B, endpoint_record(B)),
    set_answer(dial_url(B), {ok, <<"from b">>}),
    ?assertEqual({ok, <<"from b">>},
                 call(3000, #{verify_cert_chain => {realm_ca_pem(Ca), ?ORG}})),
    ?assertEqual([dial_url(B)], visits()).

%% An advertisement authorized for another org doesn't qualify, and resolution
%% keeps asking until the deadline instead of giving up at once.
call_with_cert_chain_asks_again_until_its_deadline() ->
    Ca = realm_ca(),
    A = station(<<"a.test">>),
    set_replies(procedure_key(), [[authorized_advertisement(A, Ca, <<"another-org">>)]]),
    set_endpoint(A, endpoint_record(A)),
    {Elapsed, Result} =
        timed(fun() -> call(300, #{verify_cert_chain => {realm_ca_pem(Ca), ?ORG}}) end),
    ?assertEqual({error, {unresolved, no_trusted_advertisement}}, Result),
    ?assert(Elapsed < 1000),
    ?assert(lookups(procedure_key()) >= 2),
    ?assertEqual([], visits()).

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

%% resolve_content_provider/2 stays for 10.x callers: it returns the first
%% provider whose announcement qualifies, asking again until one does.
resolve_content_provider_returns_the_first_qualifying_provider() ->
    P = station(<<"p.test">>),
    Mcid = mcid(),
    set_replies(macula_record:content_key(Mcid), [[], [announcement(P, Mcid)]]),
    Node = maps:get(key, P),
    Endpoint = dial_url(P),
    ?assertMatch({ok, #{announcer_node := Node, endpoint := Endpoint}},
                 macula_direct_dial:resolve_content_provider(self(), Mcid)).

%% When no provider qualifies within its 10 seconds, resolve_content_provider/2
%% reports the content as not announced, as it did before.
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

put_at_station(#{key := Key}, TimeoutMs) ->
    macula_direct_dial:put_content(self(), Key, <<"bytes">>, TimeoutMs).

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

call(TimeoutMs) -> call(TimeoutMs, #{}).

call(TimeoutMs, Opts) ->
    macula_direct_dial:call(self(), ?REALM, ?PROC, <<"hi">>, TimeoutMs, Opts).

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
%% `{error, Reason}' for a lookup that fails, or `silent' for one that never
%% answers. Otherwise they answer from its one record, if any.
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

set_endpoint_replies(#{key := Key}, Replies) ->
    ets:insert(?STATE, {{endpoint_replies, macula_record:station_endpoint_key(Key)}, Replies}).

endpoint_lookups(#{key := Key}) ->
    lookup_count(ets:lookup(?STATE, {endpoint_asked, macula_record:station_endpoint_key(Key)})).

endpoint_found([{_, Record}]) -> {ok, Record};
endpoint_found([]) -> {error, not_found}.

%% The decoded providers find_content_providers/2 would return for Mcid.
providers(Mcid) ->
    {ok, Records} = find_records(macula_record:content_key(Mcid)),
    [Provider || {true, Provider} <- [macula:decode_provider(R) || R <- Records]].

visit(Target) ->
    [{visits, Seen}] = ets:lookup(?STATE, visits),
    ets:insert(?STATE, {visits, Seen ++ [Target]}),
    answer(ets:lookup(?STATE, {answer, Target})).

answer([{_, Answer}]) -> Answer;
answer([]) -> {error, not_connected}.

visits() ->
    [{visits, Seen}] = ets:lookup(?STATE, visits),
    Seen.

lookups(Key) ->
    lookup_count(ets:lookup(?STATE, {asked, Key})).

lookup_count([{_, N}]) -> N;
lookup_count([]) -> 0.

set_replies(Key, Replies) -> ets:insert(?STATE, {{replies, Key}, Replies}).

set_endpoint(Station, Record) -> ets:insert(?STATE, {endpoint_entry(Station), Record}).

endpoint_entry(#{key := Key}) -> {endpoint, macula_record:station_endpoint_key(Key)}.

set_answer(Target, Answer) -> ets:insert(?STATE, {{answer, Target}, Answer}).

%%%===================================================================
%%% Helpers: records
%%%===================================================================

station(Host) ->
    Kp = macula_identity:generate(),
    #{kp => Kp, key => macula_identity:public(Kp), host => Host}.

dial_url(#{host := Host}) -> <<"quic://[", Host/binary, "]:4433">>.

endpoint_record(#{kp := Kp, key := Key, host := Host}) ->
    macula_record:sign(macula_record:station_endpoint(Key, 4433, #{host_advertised => [Host]}), Kp).

procedure_uri() ->
    <<(binary:encode_hex(?REALM, uppercase))/binary, "/", ?PROC/binary>>.

procedure_key() -> macula_record:procedure_key(procedure_uri()).

%% A signed procedure_advertisement from a fresh provider naming Station.
advertisement(#{key := Station}) ->
    Provider = macula_identity:generate(),
    macula_record:sign(
      macula_record:procedure_advertisement(macula_identity:public(Provider),
                                            procedure_uri(), Station),
      Provider).

%% Record with its validity ended, re-signed by the same provider is not
%% possible without its key, so expire before signing instead.
expired(#{key := _} = Signed) ->
    Provider = macula_identity:generate(),
    Now = erlang:system_time(millisecond),
    Unsigned = maps:remove(signature, Signed),
    macula_record:sign(Unsigned#{key => macula_identity:public(Provider),
                                 payload => (maps:get(payload, Unsigned))#{
                                              {text, <<"advertiser_node">>} =>
                                                  macula_identity:public(Provider)},
                                 created_at => Now - 7_200_000,
                                 expires_at => Now - 3_600_000},
                       Provider).

mcid() -> <<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>.

announcement(#{kp := Kp, key := Key} = Station, Mcid) ->
    macula_record:sign(macula_record:content_announcement(Key, Mcid, dial_url(Station)), Kp).

authorized_advertisement(#{key := Station}, Ca, Org) ->
    Provider = macula_identity:generate(),
    ProviderKey = macula_identity:public(Provider),
    ChainPem = issue_leaf(Ca, ProviderKey, Org),
    macula_record:sign(
      macula_record:procedure_advertisement(ProviderKey, procedure_uri(), Station,
                                            #{cert_chain => ChainPem}),
      Provider).

%%%===================================================================
%%% Helpers: a minimal in-process realm CA (OTP public_key), as in
%%% macula_record_cert_chain_tests
%%%===================================================================

realm_ca() ->
    {Pub, Priv} = ca_key(),
    Subject = subject(<<"io.macula">>, <<"io.macula">>),
    #{subject => Subject, key => Priv,
      der => sign_cert(Subject, ed_spki(Pub), Subject, Priv, true)}.

realm_ca_pem(#{der := Der}) -> pem([Der]).

%% realm CA -> org CA (O=Org) -> Ed25519 leaf binding LeafPub; returns the
%% leaf-first [leaf, org CA] bundle an advertiser embeds.
issue_leaf(#{subject := RealmSubject, key := RealmKey}, LeafPub, Org) ->
    {OrgPub, OrgPriv} = ca_key(),
    OrgSubject = subject(<<"io.macula.", Org/binary>>, Org),
    LeafSubject = subject(<<"mri:app:io.macula/", Org/binary, "/svc">>, Org),
    OrgDer = sign_cert(OrgSubject, ed_spki(OrgPub), RealmSubject, RealmKey, true),
    LeafDer = sign_cert(LeafSubject, ed_spki(LeafPub), OrgSubject, OrgPriv, false),
    pem([LeafDer, OrgDer]).

ca_key() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {Pub, #'ECPrivateKey'{version = 1, privateKey = Priv,
                          parameters = {namedCurve, ?'id-Ed25519'},
                          publicKey = Pub}}.

ed_spki(Pub) ->
    #'OTPSubjectPublicKeyInfo'{
       algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-Ed25519',
                                         parameters = asn1_NOVALUE},
       subjectPublicKey = #'ECPoint'{point = Pub}}.

subject(CN, O) ->
    {rdnSequence,
     [[#'AttributeTypeAndValue'{type = {2, 5, 4, 3}, value = {utf8String, CN}}],
      [#'AttributeTypeAndValue'{type = {2, 5, 4, 10}, value = {utf8String, O}}]]}.

sign_cert(Subject, Spki, IssuerSubject, IssuerKey, IsCA) ->
    TBS = #'OTPTBSCertificate'{
             version = v3,
             serialNumber = rand:uniform(1 bsl 60),
             signature = #'SignatureAlgorithm'{algorithm = ?'id-Ed25519',
                                               parameters = asn1_NOVALUE},
             issuer = IssuerSubject,
             validity = #'Validity'{notBefore = {utcTime, "230101000000Z"},
                                    notAfter  = {utcTime, "330101000000Z"}},
             subject = Subject,
             subjectPublicKeyInfo = Spki,
             extensions = [#'Extension'{extnID = ?'id-ce-basicConstraints',
                                        critical = true,
                                        extnValue = #'BasicConstraints'{
                                                       cA = IsCA,
                                                       pathLenConstraint = asn1_NOVALUE}}]},
    public_key:pkix_sign(TBS, IssuerKey).

pem(Ders) ->
    public_key:pem_encode([{'Certificate', D, not_encrypted} || D <- Ders]).
