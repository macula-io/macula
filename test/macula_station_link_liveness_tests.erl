%% EUnit tests for the liveness probe of `macula_station_link': a probe goes out on each tick, a verified reply to it
%% from the connected station clears it, and a link whose probes go unanswered closes its peer on the third tick. A reply
%% for another request, or one that names the probe but does not verify as its answer, clears nothing and is counted.
%%
%% The tests drive the `liveness_tick' info message directly rather than waiting the probe interval, and stand in for
%% the peering connection: the test process is the link's peer, and a station key of the node's profile signs the
%% replies. Each test runs in a process of its own, so a probe or a close that an earlier test's link sent is never read
%% as this test's.
-module(macula_station_link_liveness_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).

%% The link's state fields these tests read or set, looked up by name in the state record, so a field added to the
%% record cannot shift them.
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(NODE_IDENTITY_INDEX, macula_station_link:state_field_index(node_identity)).

%% The first tick after connect sends a probe: a request for `_macula.ping' to the connected station, signed with the
%% link's node identity key, that passes the send check. The link keeps running.
liveness_tick_emits_probe_call_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, StationKey, Profile} = start_link_to_station(),
         Pid ! liveness_tick,
         Probe = receive
             {'$gen_cast', {send_frame, _, #{frame_type := call} = Frame}} -> Frame
         after 1_000 ->
             erlang:error(no_probe_call_emitted)
         end,
         ?assertEqual(ok, macula_frame:check_frame(Probe)),
         Key = element(?NODE_IDENTITY_INDEX, sys:get_state(Pid)),
         {ok, Request} = macula_frame:verify_request(Probe, Profile),
         Station = macula_node_keys:key_id(StationKey),
         ?assertMatch(#{procedure := <<"_macula.ping">>, target := Station, request_id := <<_:128>>}, Request),
         ?assertEqual(macula_node_keys:public_key(Key), maps:get(key, Request)),
         ?assert(is_process_alive(Pid)),
         macula_station_link:stop(Pid)
     end}}.

%% A probe answered by a verified reply from the connected station, a RESULT or a relay ERROR, clears the outstanding
%% slot before the next tick, and no answer is refused, so a link whose station answers every probe stays up past three
%% ticks. Stations answer `_macula.ping' with a relay ERROR, so that answer is checked on its own tick.
liveness_probe_reply_clears_outstanding_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {Pid, StationKey, Profile} = start_link_to_station(),
         [begin
              answer_next_probe(Pid, Profile, Answer, StationKey),
              ?assertEqual(undefined, outstanding_probe(Pid))
          end || Answer <- [fun station_result/2, fun station_relay_error/2, fun station_result/2]],
         Pid ! liveness_tick,
         _ = sent_probe(Profile),
         ?assertNot(liveness_close_within(300)),
         ?assertEqual(#{}, refused_replies(Pid)),
         ?assert(is_process_alive(Pid)),
         macula_station_link:stop(Pid)
     end}}.

%% A probe nobody answers counts a miss on the next tick, and the link closes its peer on the third unanswered tick.
liveness_consecutive_misses_close_peer_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, _StationKey, Profile} = start_link_to_station(),
         Pid ! liveness_tick,
         _ = sent_probe(Profile),
         Pid ! liveness_tick,
         _ = sent_probe(Profile),
         Pid ! liveness_tick,
         ?assert(liveness_close_within(1_000)),
         macula_station_link:stop(Pid)
     end}}.

%% A verified reply for a request the link is not waiting on clears nothing: it is counted as unknown_request, and the
%% link still closes on the third unanswered tick.
a_reply_for_another_request_clears_no_probe_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         {Pid, StationKey, Profile} = start_link_to_station(),
         Other = link_request(Pid, Profile, #{request_id => crypto:strong_rand_bytes(16),
                                              target => macula_node_keys:key_id(StationKey)}),
         Pid ! liveness_tick,
         _ = sent_probe(Profile),
         Pid ! {macula_peering, frame, self(), received(station_result(Other, StationKey))},
         Pid ! liveness_tick,
         _ = sent_probe(Profile),
         Pid ! {macula_peering, frame, self(), received(station_result(Other, StationKey))},
         Pid ! liveness_tick,
         ?assert(liveness_close_within(1_000)),
         ?assertEqual(#{unknown_request => 2}, refused_replies(Pid)),
         macula_station_link:stop(Pid)
     end}}.

%% A reply that names the probe's request but does not verify as its answer clears nothing, however it fails: signed by
%% a key other than the probe's target, carrying another request's hash, or a relay error reported by a station other
%% than the connected one. Each is counted by its refusal, and the link still closes on the third unanswered tick.
a_reply_naming_the_probe_that_does_not_verify_clears_nothing_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {Pid, StationKey, Profile} = start_link_to_station(),
         {ok, OtherKey} = macula_node_keys:generate(identity, Profile),
         Pid ! liveness_tick,
         #{request_id := RequestId} = Probe = sent_probe(Profile),
         SameIdOtherRequest = link_request(Pid, Profile, #{request_id => RequestId,
                                                           target => macula_node_keys:key_id(StationKey),
                                                           payload => #{other => 1}}),
         [Pid ! {macula_peering, frame, self(), received(Forged)}
          || Forged <- [station_result(Probe, OtherKey),
                        station_result(SameIdOtherRequest, StationKey),
                        station_relay_error(Probe, OtherKey)]],
         Pid ! liveness_tick,
         _ = sent_probe(Profile),
         Pid ! liveness_tick,
         ?assert(liveness_close_within(1_000)),
         ?assertEqual(#{not_the_target => 1, request_mismatch => 1, not_the_connection => 1}, refused_replies(Pid)),
         macula_station_link:stop(Pid)
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
    %% A link also starts with a request admission and its share in it.
    {ok, Admission} = macula_request_admission:start_link(#{caller_quota => 256, share => 1024, cap => 46080,
                                                             reply_bytes => 262144, reply_bytes_total => 16777216}),
    Opts#{node_identity => fun() -> Key end, issuer => Issuer, admission => Admission,
          share => {seed, {<<"127.0.0.1">>, 1}}, expected_node_id => <<1:256>>}.

%% A link that believes it is connected to a station holding an identity key of the node's profile, with this process as
%% its peer. Returns the link, the station's key and the profile.
start_link_to_station() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, StationKey} = macula_node_keys:generate(identity, Profile),
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
        seed     => #{host => <<"127.0.0.1">>, port => 1},
        connect_timeout_ms => 2000
    })),
    FakePeer = self(),
    Station = macula_node_keys:key_id(StationKey),
    _ = sys:replace_state(Pid, fun(S) ->
        S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
        setelement(?PEER_NODE_ID_INDEX, S2, Station)
    end),
    {Pid, StationKey, Profile}.

%% Drives one tick, reads the probe it sends, and answers it with `Answer' signed by `Key'.
answer_next_probe(Pid, Profile, Answer, Key) ->
    Pid ! liveness_tick,
    Pid ! {macula_peering, frame, self(), received(Answer(sent_probe(Profile), Key))}.

%% The request of the probe the link sends next, as its receiver verifies it.
sent_probe(Profile) ->
    receive
        {'$gen_cast', {send_frame, _, #{frame_type := call} = Probe}} ->
            {ok, #{procedure := <<"_macula.ping">>} = Request} = macula_frame:verify_request(received(Probe), Profile),
            Request
    after 1_000 ->
        erlang:error(no_probe_call_emitted)
    end.

%% A request of the link's own, signed with its node identity key, with the fields `Fields' names.
link_request(Pid, Profile, Fields) ->
    Key = element(?NODE_IDENTITY_INDEX, sys:get_state(Pid)),
    Spec = maps:merge(#{realm => ?REALM, procedure => <<"_macula.ping">>,
                        deadline => erlang:system_time(millisecond) + 60_000, payload => #{}}, Fields),
    {ok, Request} = macula_frame:verify_request(received(macula_frame:call(Spec, Key)), Profile),
    Request.

station_result(Request, Key) ->
    macula_frame:result(#{request => Request, payload => #{}}, Key).

station_relay_error(Request, Key) ->
    macula_frame:relay_error(#{frame_type => error, request => Request, code => unknown_next_peer}, Key).

%% A frame as a connection delivers it: encoded, then decoded.
received(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

%% Whether the link asks its peer to close for lost liveness within `Ms'.
liveness_close_within(Ms) ->
    receive
        {'$gen_statem', _, {close, app_liveness_lost}} -> true;
        {'$gen_cast', {close, app_liveness_lost}} -> true
    after Ms ->
        false
    end.

%% The probe the link is waiting on an answer to, or undefined.
outstanding_probe(Pid) ->
    element(macula_station_link:state_field_index(liveness_outstanding), sys:get_state(Pid)).

%% The replies the link refused, counted by reason.
refused_replies(Pid) ->
    macula_refusal_report:counts(element(macula_station_link:state_field_index(refused_replies), sys:get_state(Pid))).
