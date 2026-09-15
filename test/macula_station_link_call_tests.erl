%% EUnit tests for an outbound unary call on `macula_station_link'. A call names its target: the station the link is
%% connected to, or a provider's node_id. The link signs it as a request with its node identity key and waits for the
%% reply that verifies against that request. A RESULT answers the caller with its payload; a provider's error reaches
%% the caller as binaries only, so nothing a provider sends takes a shape the link builds itself; a relay error from the
%% station is final. A call the link refuses before sending is not sent, and every pending call ends: on its reply, its
%% timeout, or the link's close.
%%
%% The test process stands in for the peering connection, and each test runs in a process of its own.
-module(macula_station_link_call_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).
-define(PROCEDURE, <<"acme.count_v1">>).
-define(TEN_MINUTES_MS, 600_000).

%% The link's state fields these tests read or set, looked up by name in the state record.
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(NODE_IDENTITY_INDEX, macula_station_link:state_field_index(node_identity)).

%% A call to `station' is a request to the node_id of the station the link is connected to, signed with the link's node
%% identity key, carrying the caller's deadline.
a_call_to_the_station_is_a_request_to_its_node_id_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, StationKey, Profile} = start_link_to_station(),
         Before = erlang:system_time(millisecond),
         _ = call_async(Pid, station, ?PROCEDURE, #{}, 2_000),
         #{deadline := Deadline} = Request = sent_request(Profile),
         Station = macula_node_keys:key_id(StationKey),
         ?assertMatch(#{target := Station, realm := ?REALM, procedure := ?PROCEDURE, request_id := <<_:128>>}, Request),
         ?assertEqual(macula_node_keys:public_key(link_key(Pid)), maps:get(key, Request)),
         ?assert(Deadline >= Before + 2_000 andalso Deadline < Before + 2_500),
         macula_station_link:stop(Pid)
     end}}.

%% A call to a provider's node_id is a request to that node_id.
a_call_to_a_provider_targets_its_node_id_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, _StationKey, Profile} = start_link_to_station(),
         Provider = macula_node_keys:key_id(new_key(Profile)),
         _ = call_async(Pid, Provider, ?PROCEDURE, #{}, 2_000),
         ?assertMatch(#{target := Provider}, sent_request(Profile)),
         macula_station_link:stop(Pid)
     end}}.

%% The link's DHT helpers call the station itself.
the_dht_helpers_call_the_station_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, StationKey, Profile} = start_link_to_station(),
         Test = self(),
         _ = spawn(fun() -> Test ! {found, macula_station_link:find_record(Pid, <<1:256>>, 1_000)} end),
         Station = macula_node_keys:key_id(StationKey),
         ?assertMatch(#{target := Station, procedure := <<"_dht.find_record">>}, sent_request(Profile)),
         macula_station_link:stop(Pid)
     end}}.

%% A RESULT that verifies against the request answers the caller with its payload.
a_verified_result_answers_the_caller_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, _StationKey, Profile} = start_link_to_station(),
         ProviderKey = new_key(Profile),
         Ref = call_async(Pid, macula_node_keys:key_id(ProviderKey), ?PROCEDURE, #{}, 2_000),
         Request = sent_request(Profile),
         deliver(Pid, macula_frame:result(#{request => Request, payload => <<"forty-two">>}, ProviderKey)),
         ?assertEqual({ok, <<"forty-two">>}, answer(Ref)),
         macula_station_link:stop(Pid)
     end}}.

%% A provider error with the handler_error code reaches the caller as its detail text; any other provider error as
%% {call_error, Code, Detail}, with Detail undefined when the error carries none.
a_provider_error_reaches_the_caller_as_binaries_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, _StationKey, Profile} = start_link_to_station(),
         ProviderKey = new_key(Profile),
         Provider = macula_node_keys:key_id(ProviderKey),
         ?assertEqual({error, <<"no such order">>},
                      provider_error_answer(Pid, Profile, Provider, ProviderKey,
                                            #{code => <<"handler_error">>, detail => <<"no such order">>})),
         ?assertEqual({error, {call_error, <<"unauthorized">>, <<"token expired">>}},
                      provider_error_answer(Pid, Profile, Provider, ProviderKey,
                                            #{code => <<"unauthorized">>, detail => <<"token expired">>})),
         ?assertEqual({error, {call_error, <<"overloaded">>, undefined}},
                      provider_error_answer(Pid, Profile, Provider, ProviderKey, #{code => <<"overloaded">>})),
         macula_station_link:stop(Pid)
     end}}.

%% A provider error whose code or detail spells a reason the link reports for a call it never sent still reaches the
%% caller as text, and not_sent/1 says the call went out.
provider_text_that_spells_a_not_sent_reason_is_still_delivered_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {Pid, _StationKey, Profile} = start_link_to_station(),
         ProviderKey = new_key(Profile),
         Provider = macula_node_keys:key_id(ProviderKey),
         [begin
              Coded = provider_error_answer(Pid, Profile, Provider, ProviderKey, #{code => Text, detail => Text}),
              ?assertEqual({error, {call_error, Text, Text}}, Coded),
              ?assertNot(macula_station_link:not_sent(Coded)),
              Detailed = provider_error_answer(Pid, Profile, Provider, ProviderKey,
                                               #{code => <<"handler_error">>, detail => Text}),
              ?assertEqual({error, Text}, Detailed),
              ?assertNot(macula_station_link:not_sent(Detailed))
          end || Text <- [<<"not_connected">>, <<"noproc">>, <<"refused">>, <<"unknown_next_peer">>, <<"timeout">>]],
         macula_station_link:stop(Pid)
     end}}.

%% A relay error from the connected station ends the call with unknown_next_peer, and the call counts as sent: the
%% caller cannot check that the provider never received it.
a_relay_error_from_the_station_is_final_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, StationKey, Profile} = start_link_to_station(),
         Ref = call_async(Pid, macula_node_keys:key_id(new_key(Profile)), ?PROCEDURE, #{}, 2_000),
         Request = sent_request(Profile),
         deliver(Pid, macula_frame:relay_error(#{frame_type => error, request => Request, code => unknown_next_peer},
                                               StationKey)),
         Answer = answer(Ref),
         ?assertEqual({error, {call_error, unknown_next_peer, undefined}}, Answer),
         ?assertNot(macula_station_link:not_sent(Answer)),
         macula_station_link:stop(Pid)
     end}}.

%% A reply that does not verify as the answer leaves the call pending and is counted; the genuine reply that follows
%% answers the caller.
a_reply_that_does_not_verify_leaves_the_call_pending_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, _StationKey, Profile} = start_link_to_station(),
         ProviderKey = new_key(Profile),
         Ref = call_async(Pid, macula_node_keys:key_id(ProviderKey), ?PROCEDURE, #{}, 2_000),
         Request = sent_request(Profile),
         deliver(Pid, macula_frame:result(#{request => Request, payload => <<"forged">>}, new_key(Profile))),
         ?assertEqual(none, answer_within(Ref, 200)),
         deliver(Pid, macula_frame:result(#{request => Request, payload => <<"genuine">>}, ProviderKey)),
         ?assertEqual({ok, <<"genuine">>}, answer(Ref)),
         ?assertEqual(#{not_the_target => 1}, refused_replies(Pid)),
         macula_station_link:stop(Pid)
     end}}.

%% A payload the wire cannot carry is refused before anything is built or sent, and the call counts as not sent.
an_unsendable_payload_is_refused_and_not_sent_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, _StationKey, _Profile} = start_link_to_station(),
         Refused = macula_station_link:call(Pid, station, ?REALM, ?PROCEDURE, #{pid => self()}, 1_000),
         ?assertMatch({error, {refused, _}}, Refused),
         ?assert(macula_station_link:not_sent(Refused)),
         ?assertEqual(none, sent_frame_within(100)),
         macula_station_link:stop(Pid)
     end}}.

%% A call to the station on a link that has not connected is refused before anything is built, as not sent.
a_station_call_on_an_unconnected_link_is_refused_before_building_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         Pid = start_link_with_peer(),
         Refused = macula_station_link:call(Pid, station, ?REALM, ?PROCEDURE, #{}, 1_000),
         ?assertEqual({error, not_connected}, Refused),
         ?assert(macula_station_link:not_sent(Refused)),
         ?assertEqual(none, sent_frame_within(100)),
         macula_station_link:stop(Pid)
     end}}.

%% A timeout is a positive number of milliseconds up to ten minutes, the provider's deadline window; anything else is
%% refused in the caller.
a_timeout_outside_its_bounds_is_refused_in_the_caller_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, _StationKey, _Profile} = start_link_to_station(),
         [?assertError(function_clause, macula_station_link:call(Pid, station, ?REALM, ?PROCEDURE, #{}, Timeout))
          || Timeout <- [0, ?TEN_MINUTES_MS + 1, infinity]],
         ?assertEqual(none, sent_frame_within(100)),
         macula_station_link:stop(Pid)
     end}}.

%% A call the link reaches only after its caller's deadline, because the link was busy for longer than that, is not
%% sent: the caller was already told it timed out.
a_call_the_link_reaches_after_its_deadline_is_not_sent_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, _StationKey, _Profile} = start_link_to_station(),
         ok = sys:suspend(Pid),
         ?assertEqual({error, timeout}, macula_station_link:call(Pid, station, ?REALM, ?PROCEDURE, #{}, 100)),
         ok = sys:resume(Pid),
         ?assertEqual(none, sent_frame_within(300)),
         macula_station_link:stop(Pid)
     end}}.

%% A call nobody answers times out, and the link keeps nothing of it; a reply that arrives afterwards counts as a reply
%% for a request the link does not hold.
a_call_nobody_answers_times_out_and_is_forgotten_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, _StationKey, Profile} = start_link_to_station(),
         ProviderKey = new_key(Profile),
         Ref = call_async(Pid, macula_node_keys:key_id(ProviderKey), ?PROCEDURE, #{}, 200),
         Request = sent_request(Profile),
         ?assertEqual({error, timeout}, answer(Ref)),
         ?assertEqual(0, map_size(element(macula_station_link:state_field_index(pending), sys:get_state(Pid)))),
         deliver(Pid, macula_frame:result(#{request => Request, payload => <<"late">>}, ProviderKey)),
         ?assertEqual(#{unknown_request => 1}, refused_replies(Pid)),
         macula_station_link:stop(Pid)
     end}}.

%% When the link's connection closes, every pending caller is answered at once, and the calls count as sent.
closing_the_link_answers_every_pending_caller_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, _StationKey, Profile} = start_link_to_station(),
         Provider = macula_node_keys:key_id(new_key(Profile)),
         Refs = [call_async(Pid, Provider, ?PROCEDURE, #{}, 4_000) || _ <- [1, 2]],
         _ = [sent_request(Profile) || _ <- Refs],
         Pid ! {macula_peering, disconnected, self(), peer_closed},
         Answers = [answer(Ref) || Ref <- Refs],
         ?assertEqual([{error, {disconnected, peer_closed}}, {error, {disconnected, peer_closed}}], Answers),
         ?assertEqual([false, false], [macula_station_link:not_sent(Answer) || Answer <- Answers])
     end}}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% Start options with the keys a link starts with: a node identity key in the node's profile, an issuer of its own for
%% that key, owned by the calling process, and the node_id its seed expects.
with_link_keys(Opts) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    Opts#{node_identity => fun() -> Key end, issuer => Issuer, expected_node_id => <<1:256>>}.

new_key(Profile) ->
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    Key.

%% A link whose peering connection is this process, not yet connected.
start_link_with_peer() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
        seed     => #{host => <<"127.0.0.1">>, port => 1},
        connect_timeout_ms => 2000
    })),
    FakePeer = self(),
    _ = sys:replace_state(Pid, fun(S) -> setelement(?PEER_PID_INDEX, S, FakePeer) end),
    Pid.

%% A link that believes it is connected to a station holding an identity key of the node's profile, with this process as
%% its peer. Returns the link, the station's key and the profile.
start_link_to_station() ->
    {ok, Profile} = begin _ = application:ensure_all_started(macula), macula_crypto_profile:configured() end,
    StationKey = new_key(Profile),
    Pid = start_link_with_peer(),
    Station = macula_node_keys:key_id(StationKey),
    _ = sys:replace_state(Pid, fun(S) -> setelement(?PEER_NODE_ID_INDEX, S, Station) end),
    {Pid, StationKey, Profile}.

link_key(Pid) ->
    element(?NODE_IDENTITY_INDEX, sys:get_state(Pid)).

%% Calls from a process of its own, which hands the answer back to this process under the returned reference.
call_async(Pid, Target, Procedure, Payload, TimeoutMs) ->
    Test = self(),
    Ref = make_ref(),
    _ = spawn(fun() ->
                  Test ! {Ref, macula_station_link:call(Pid, Target, ?REALM, Procedure, Payload, TimeoutMs)}
              end),
    Ref.

answer(Ref) ->
    receive {Ref, Answer} -> Answer
    after 3_000 -> erlang:error(no_answer)
    end.

answer_within(Ref, Ms) ->
    receive {Ref, Answer} -> Answer
    after Ms -> none
    end.

%% The request the link sends next, as its receiver verifies it.
sent_request(Profile) ->
    receive
        {'$gen_cast', {send_frame, #{frame_type := call} = Frame}} ->
            {ok, Request} = macula_frame:verify_request(received(Frame), Profile),
            Request
    after 1_000 ->
        erlang:error(no_request_sent)
    end.

%% The answer to one call whose provider replies with an ERROR of `Fields'.
provider_error_answer(Pid, Profile, Provider, ProviderKey, Fields) ->
    Ref = call_async(Pid, Provider, ?PROCEDURE, #{}, 2_000),
    Request = sent_request(Profile),
    deliver(Pid, macula_frame:provider_error(Fields#{request => Request}, ProviderKey)),
    answer(Ref).

%% Hands a frame to the link as its connection delivers one: encoded, then decoded.
deliver(Pid, Frame) ->
    Pid ! {macula_peering, frame, self(), received(Frame)}.

received(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

%% The first frame the link sent to its peer within `Ms', or none.
sent_frame_within(Ms) ->
    receive
        {'$gen_cast', {send_frame, Frame}} -> {sent, Frame}
    after Ms ->
        none
    end.

%% The replies the link refused, counted by reason.
refused_replies(Pid) ->
    macula_refusal_report:counts(element(macula_station_link:state_field_index(refused_replies), sys:get_state(Pid))).
