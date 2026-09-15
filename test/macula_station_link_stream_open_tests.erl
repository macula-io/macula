%% EUnit tests for how a station link serves a STREAM_OPEN a peer opens on a dedicated stream. The open is verified,
%% addressed to this node, and admitted once per (caller, request_id) before any policy or handler. A refusal of a
%% verified open is a provider STREAM_ERROR signed under that open; an open that does not verify, names another node,
%% is too long, or is not an open at all, closes its stream with nothing written. A server session is started with the
%% link's key as a closure, the verified open, the peering connection and the profile, under an attach id the link
%% chooses, and its frames are routed by the QUIC stream they arrive on. A dedicated stream carries one session: a
%% refusal or a close takes no frame behind it, and a stream that brings no session in time closes. The test process
%% is the link's peering connection, and the link's open_stream, send_on_stream and close_stream options tell it what
%% the link does to dedicated streams, so no shared module is replaced.
%%
%% Vulcan's condition 1 on the admission wiring: the STREAM_OPEN half is here
%% (an_open_the_admission_cannot_judge_is_refused_and_the_link_stays_up); the CALL half lands with P2.
-module(macula_station_link_stream_open_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<9:256>>).
-define(PROCEDURE, <<"acme/count_v1">>).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(ADMISSION_INDEX, macula_station_link:state_field_index(admission)).
-define(EVENT_MS, 1_000).

stream_open_test_() ->
    [{case_name(Case), {timeout, 15, {spawn, fun() -> process_flag(trap_exit, true), Case() end}}}
     || Case <- [fun an_open_that_does_not_verify_closes_its_stream_and_writes_nothing/0,
                 fun an_open_signed_by_another_key_is_not_served_and_a_genuine_one_is/0,
                 fun an_open_for_another_node_closes_with_not_the_target_and_admits_nothing/0,
                 fun a_verified_open_runs_its_handler_and_the_session_signs_its_first_frame/0,
                 fun a_verified_open_for_an_unadvertised_procedure_is_refused_not_found/0,
                 fun a_copy_of_an_admitted_open_on_another_stream_is_refused_and_starts_no_second_handler/0,
                 fun an_open_whose_deadline_is_outside_the_window_is_refused_before_its_policy/0,
                 fun an_open_the_admission_cannot_judge_is_refused_and_the_link_stays_up/0,
                 fun opens_from_two_callers_with_one_request_id_reach_only_their_own_handlers/0,
                 fun a_caller_frame_with_a_failing_signature_is_charged_once_and_reaches_no_handler/0,
                 fun a_caller_chunk_the_mode_forbids_is_refused_and_reaches_no_reader/0,
                 fun a_long_chunk_behind_the_open_reaches_its_session/0,
                 fun an_open_missing_a_required_field_ends_its_stream_and_the_next_open_is_served/0,
                 fun a_new_stream_whose_bytes_do_not_decode_is_ended/0,
                 fun a_second_open_on_a_stream_that_carries_a_session_is_refused/0,
                 fun an_open_on_a_callers_stream_is_refused/0,
                 fun a_refused_stream_takes_no_frame_behind_its_refusal/0,
                 fun a_closed_stream_takes_no_frame_behind_its_close/0,
                 fun a_silent_opened_stream_closes_at_its_deadline/0,
                 fun a_first_frame_past_the_open_limit_closes_its_stream/0]]
    ++ [{"a session frame missing its signed object ends its stream: " ++ atom_to_list(Type),
         {timeout, 15, {spawn, fun() -> process_flag(trap_exit, true), session_frame_missing_its_object(Frame) end}}}
        || {Type, Frame} <- [{stream_data, #{frame_type => stream_data, seq => 0, encoding => raw, body => <<"x">>}},
                             {stream_end, #{frame_type => stream_end, role => both}},
                             {stream_error, #{frame_type => stream_error, code => <<"error">>, message => <<"stop">>}},
                             {stream_reply, #{frame_type => stream_reply, payload => <<"x">>}}]]
    ++ [{"a refused open closes its stream: " ++ atom_to_list(Refusal),
         {timeout, 15, {spawn, fun() -> process_flag(trap_exit, true), refusal_closes_its_stream(Refusal) end}}}
        || Refusal <- [not_found, unauthorized, too_many_sessions]]
    ++ [{"a stream that brings no session closes: " ++ atom_to_list(First),
         {timeout, 15, {spawn, fun() -> process_flag(trap_exit, true),
                                        stream_that_brings_no_session_closes(First) end}}}
        || First <- [unverified_open, stream_end]]
    ++ [{"a first read that ends badly ends its session: " ++ Name,
         {timeout, 15, {spawn, fun() -> process_flag(trap_exit, true), first_read_that_ends_badly(After) end}}}
        || {Name, After} <- [{"bytes that do not decode follow the open", <<4:32, "junk">>},
                             {"a frame missing a required field follows the open",
                              macula_frame:encode(#{frame_type => stream_end, role => both})}]].

%%------------------------------------------------------------------
%% Cases
%%------------------------------------------------------------------

%% A STREAM_OPEN whose signature does not verify runs no handler, gets nothing back, and its stream closes.
an_open_that_does_not_verify_closes_its_stream_and_writes_nothing() ->
    #{link := Link} = World = link_serving(),
    Stream = make_ref(),
    open_on(Link, Stream, flipped(open_frame(World, caller_key(), #{}))),
    ?assertEqual(closed, closed_within(Stream, ?EVENT_MS)),
    ?assertEqual(none, written_within(Stream, 200)),
    ?assertEqual(none, handler_ran_within(200)),
    stop(World).

%% An open whose request names its caller's key but is signed by another key is not served and gets nothing back;
%% a genuinely signed open that follows is served.
an_open_signed_by_another_key_is_not_served_and_a_genuine_one_is() ->
    #{link := Link} = World = link_serving(),
    Caller = caller_key(),
    Forged = make_ref(),
    open_on(Link, Forged, resigned(open_frame(World, Caller, #{payload => #{tag => 1}}), caller_key())),
    open_on(Link, make_ref(), open_frame(World, Caller, #{payload => #{tag => 2}})),
    ?assertEqual({handler_ran, #{{text, <<"tag">>} => 2}}, handler_ran_within(?EVENT_MS)),
    ?assertEqual(none, handler_ran_within(300)),
    ?assertEqual(none, written_within(Forged, 0)),
    stop(World).

%% A verified open naming another node's id closes its stream as not_the_target: the link signs and writes nothing,
%% takes no admission entry and runs no handler, and a correctly addressed open of the same request is still admitted.
an_open_for_another_node_closes_with_not_the_target_and_admits_nothing() ->
    #{link := Link} = World = link_serving(),
    Caller = caller_key(),
    RequestId = crypto:strong_rand_bytes(16),
    Stream = make_ref(),
    open_on(Link, Stream, open_frame(World, Caller, #{request_id => RequestId, target => <<7:256>>})),
    ?assertEqual({not_the_target, false}, reported_within(?EVENT_MS)),
    ?assertEqual(closed, closed_within(Stream, ?EVENT_MS)),
    ?assertEqual(none, written_within(Stream, 200)),
    ?assertEqual(none, handler_ran_within(200)),
    open_on(Link, make_ref(), open_frame(World, Caller, #{request_id => RequestId})),
    ?assertMatch({handler_ran, _Args}, handler_ran_within(?EVENT_MS)),
    stop(World).

%% A verified open of an advertised procedure runs its handler with the payload. The server session starts with the
%% link's key as a closure and signs its first frame: the chunk the handler sends verifies as the provider's seq 0
%% under the open.
a_verified_open_runs_its_handler_and_the_session_signs_its_first_frame() ->
    #{link := Link} = World = link_serving(fun(Stream, Args) -> ok = macula_stream:send(Stream, <<"one">>), Args end),
    Stream = make_ref(),
    Frame = open_frame(World, caller_key(), #{payload => #{n => 1}}),
    open_on(Link, Stream, Frame),
    ?assertEqual({handler_ran, #{{text, <<"n">>} => 1}}, handler_ran_within(?EVENT_MS)),
    {ok, Written} = written_within(Stream, ?EVENT_MS),
    ?assertMatch({ok, #{frame_type := stream_data, seq := 0, body := <<"one">>}, _State},
                 macula_frame:verify_provider_stream(Written, macula_frame:open_stream(verified(Frame)), profile())),
    stop(World).

%% A verified open for a procedure this link does not advertise gets a verified not_found on its own stream.
a_verified_open_for_an_unadvertised_procedure_is_refused_not_found() ->
    #{link := Link} = World = link_serving(),
    Stream = make_ref(),
    Frame = open_frame(World, caller_key(), #{procedure => <<"unknown.proc">>}),
    open_on(Link, Stream, Frame),
    {ok, Refusal} = written_within(Stream, ?EVENT_MS),
    ?assertEqual({ok, <<"not_found">>}, refusal_code(Frame, Refusal)),
    stop(World).

%% The same verified open bytes on a second QUIC stream are a copy of an admitted request: the second stream gets a
%% provider STREAM_ERROR under the open, and no second session or handler starts.
a_copy_of_an_admitted_open_on_another_stream_is_refused_and_starts_no_second_handler() ->
    #{link := Link} = World = link_serving(),
    Frame = open_frame(World, caller_key(), #{}),
    open_on(Link, make_ref(), Frame),
    ?assertMatch({handler_ran, _}, handler_ran_within(?EVENT_MS)),
    Second = make_ref(),
    open_on(Link, Second, Frame),
    {ok, Refusal} = written_within(Second, ?EVENT_MS),
    ?assertMatch({ok, _Code}, refusal_code(Frame, Refusal)),
    ?assertEqual(none, handler_ran_within(200)),
    stop(World).

%% An open whose deadline lies more than 10 minutes ahead of the provider's clock is refused by admission, before the
%% procedure's policy is asked, and runs no handler.
an_open_whose_deadline_is_outside_the_window_is_refused_before_its_policy() ->
    #{link := Link} = World = link_serving(),
    Stream = make_ref(),
    Frame = open_frame(World, caller_key(), #{deadline => erlang:system_time(millisecond) + 11 * 60_000}),
    open_on(Link, Stream, Frame),
    {ok, Refusal} = written_within(Stream, ?EVENT_MS),
    ?assertEqual({ok, <<"not_yet_valid">>}, refusal_code(Frame, Refusal)),
    ?assertEqual(none, handler_ran_within(200)),
    stop(World).

%% When the admission cannot answer, the open is refused, never admitted, and the link stays up until the pool's own
%% stop ends it (Vulcan's condition 1 on the wiring).
an_open_the_admission_cannot_judge_is_refused_and_the_link_stays_up() ->
    #{link := Link} = World = link_serving(),
    Admission = element(?ADMISSION_INDEX, sys:get_state(Link)),
    true = unlink(Admission),
    exit(Admission, kill),
    Stream = make_ref(),
    Frame = open_frame(World, caller_key(), #{}),
    open_on(Link, Stream, Frame),
    {ok, Refusal} = written_within(Stream, ?EVENT_MS),
    ?assertMatch({ok, _Code}, refusal_code(Frame, Refusal)),
    ?assertEqual(none, handler_ran_within(200)),
    ?assert(is_process_alive(Link)),
    stop(World).

%% Callers choose request ids, so two callers' opens can carry the same one. Each reaches only its own handler, under
%% an attach id the link chooses.
opens_from_two_callers_with_one_request_id_reach_only_their_own_handlers() ->
    #{link := Link} = World = link_serving(fun(_Stream, Args) -> Args end),
    RequestId = crypto:strong_rand_bytes(16),
    open_on(Link, make_ref(), open_frame(World, caller_key(), #{request_id => RequestId, payload => #{who => 1}})),
    open_on(Link, make_ref(), open_frame(World, caller_key(), #{request_id => RequestId, payload => #{who => 2}})),
    Ran = lists:sort([handler_ran_within(?EVENT_MS), handler_ran_within(?EVENT_MS)]),
    ?assertEqual([{handler_ran, #{{text, <<"who">>} => 1}}, {handler_ran, #{{text, <<"who">>} => 2}}], Ran),
    stop(World).

%% A caller STREAM_DATA whose signature fails reaches no handler reader, and the connection is told once, charged.
a_caller_frame_with_a_failing_signature_is_charged_once_and_reaches_no_handler() ->
    Test = self(),
    #{link := Link} = World = link_serving(fun(Stream, _Args) -> Test ! {read, macula_stream:recv(Stream, 500)} end,
                                           bidi),
    Caller = caller_key(),
    Stream = make_ref(),
    Frame = open_frame(World, Caller, #{mode => bidi}),
    open_on(Link, Stream, Frame),
    #{caller_stream := Signed} = Data =
        macula_frame:caller_stream(#{frame_type => stream_data, seq => 0, encoding => raw, body => <<"x">>}, Caller,
                                   verified(Frame)),
    on_stream(Link, Stream, Data#{caller_stream := Signed#{signature := flip(maps:get(signature, Signed))}}),
    ?assertEqual({signature_invalid, true}, reported_within(?EVENT_MS)),
    ?assertEqual({read, {error, timeout}}, receive {read, _} = Read -> Read after 2_000 -> none end),
    stop(World).

%% In a server_stream the caller sends no data: a caller STREAM_DATA there is malformed_frame, refused and charged, and
%% reaches no reader.
a_caller_chunk_the_mode_forbids_is_refused_and_reaches_no_reader() ->
    Test = self(),
    #{link := Link} = World = link_serving(fun(Stream, _Args) -> Test ! {read, macula_stream:recv(Stream, 500)} end),
    Caller = caller_key(),
    Stream = make_ref(),
    Frame = open_frame(World, Caller, #{}),
    open_on(Link, Stream, Frame),
    on_stream(Link, Stream, forbidden_caller_chunk(Caller, verified(Frame))),
    ?assertEqual({malformed_frame, true}, reported_within(?EVENT_MS)),
    ?assertEqual({read, {error, timeout}}, receive {read, _} = Read -> Read after 2_000 -> none end),
    stop(World).

%% Only the first frame on a stream the peer opened has the open limit: a verified chunk longer than the limit that
%% comes right behind a STREAM_OPEN within it, in the same read, reaches the session, and the stream stays open.
a_long_chunk_behind_the_open_reaches_its_session() ->
    with_env(max_stream_open_bytes, 16_384, fun() ->
        Test = self(),
        #{link := Link} = World = link_serving(fun(Stream, _Args) ->
                                                   Test ! {read, macula_stream:recv(Stream, ?EVENT_MS)},
                                                   timer:sleep(500)
                                               end, bidi),
        Caller = caller_key(),
        Stream = make_ref(),
        Frame = open_frame(World, Caller, #{mode => bidi}),
        Body = binary:copy(<<7>>, 20_000),
        Chunk = wire(macula_frame:caller_stream(#{frame_type => stream_data, seq => 0, encoding => raw, body => Body},
                                                Caller, verified(Frame))),
        Link ! {macula_peering, new_dedicated_stream, self(), Stream},
        Link ! {quic, <<(macula_frame:encode(Frame))/binary, (macula_frame:encode(Chunk))/binary>>, Stream, undefined},
        ?assertEqual({read, {chunk, Body}}, receive {read, _} = Read -> Read after ?EVENT_MS -> none end),
        ?assertEqual(open, closed_within(Stream, 100)),
        stop(World)
    end).

%% An open missing a field its type requires never reaches a handler: its stream ends and its buffer goes, and the next
%% valid open is served.
an_open_missing_a_required_field_ends_its_stream_and_the_next_open_is_served() ->
    #{link := Link} = World = link_serving(),
    Bad = make_ref(),
    open_on(Link, Bad, maps:remove(request, open_frame(World, caller_key(), #{}))),
    ?assertEqual(closed, closed_within(Bad, ?EVENT_MS)),
    ?assertNot(has_stream_buffer(Link, Bad)),
    open_on(Link, make_ref(), open_frame(World, caller_key(), #{})),
    ?assertMatch({handler_ran, _}, handler_ran_within(?EVENT_MS)),
    stop(World).

%% A session frame without its signed object ends the session on that stream and frees its buffer; the link keeps
%% serving.
session_frame_missing_its_object(Frame) ->
    Test = self(),
    #{link := Link} = World = link_serving(fun(Stream, _Args) -> Test ! {serving, Stream}, timer:sleep(2_000) end,
                                           bidi),
    Stream = make_ref(),
    open_on(Link, Stream, open_frame(World, caller_key(), #{mode => bidi})),
    receive {serving, _} -> ok after ?EVENT_MS -> erlang:error(session_not_opened) end,
    on_stream(Link, Stream, Frame),
    ?assertEqual(closed, closed_within(Stream, ?EVENT_MS)),
    ?assertNot(has_stream_buffer(Link, Stream)),
    open_on(Link, make_ref(), open_frame(World, caller_key(), #{mode => bidi})),
    ?assertMatch({serving, _}, receive {serving, _} = Serving -> Serving after ?EVENT_MS -> none end),
    stop(World).

%% A new dedicated stream whose first bytes do not decode is ended, with nothing written.
a_new_stream_whose_bytes_do_not_decode_is_ended() ->
    #{link := Link} = World = link_serving(),
    Stream = make_ref(),
    Link ! {macula_peering, new_dedicated_stream, self(), Stream},
    Link ! {quic, <<4:32, "junk">>, Stream, undefined},
    ?assertEqual(closed, closed_within(Stream, ?EVENT_MS)),
    ?assertEqual(none, written_within(Stream, 100)),
    ?assertNot(has_stream_buffer(Link, Stream)),
    stop(World).

%% A dedicated stream carries one session. A second verified open on a stream that already carries one is refused under
%% that second open, starts no handler, and leaves the first session serving.
a_second_open_on_a_stream_that_carries_a_session_is_refused() ->
    Test = self(),
    #{link := Link} = World = link_serving(fun(Stream, Args) -> Test ! {serving, Stream}, timer:sleep(2_000), Args end,
                                           bidi),
    Caller = caller_key(),
    Stream = make_ref(),
    open_on(Link, Stream, open_frame(World, Caller, #{mode => bidi})),
    receive {serving, _} -> ok after ?EVENT_MS -> erlang:error(session_not_opened) end,
    Second = open_frame(World, Caller, #{mode => bidi}),
    on_stream(Link, Stream, Second),
    {ok, Refusal} = written_within(Stream, ?EVENT_MS),
    ?assertEqual({ok, <<"refused">>}, refusal_code(Second, Refusal)),
    ?assertEqual(none, receive {serving, _} = Serving -> Serving after 200 -> none end),
    ?assertEqual(open, closed_within(Stream, 0)),
    ?assert(has_stream_buffer(Link, Stream)),
    stop(World).

%% A verified open on a stream this link opened as a caller is refused too: it starts no handler, and the caller's
%% session keeps the stream.
an_open_on_a_callers_stream_is_refused() ->
    #{link := Link} = World = link_serving(),
    {ok, _Client} = macula_station_link:call_stream(Link, macula_node_keys:key_id(caller_key()), ?REALM,
                                                    <<"foo.elsewhere">>, #{}, #{}),
    Stream = receive {opened, Opened} -> Opened after ?EVENT_MS -> erlang:error(no_stream_opened) end,
    {ok, _ClientOpen} = written_within(Stream, ?EVENT_MS),
    Open = open_frame(World, caller_key(), #{}),
    on_stream(Link, Stream, Open),
    {ok, Refusal} = written_within(Stream, ?EVENT_MS),
    ?assertEqual({ok, <<"refused">>}, refusal_code(Open, Refusal)),
    ?assertEqual(none, handler_ran_within(200)),
    ?assert(has_stream_buffer(Link, Stream)),
    stop(World).

%% A stream closed by a refusal takes no frame after the refused open, even one that came in the same read: an open
%% right behind it starts no handler.
a_refused_stream_takes_no_frame_behind_its_refusal() ->
    #{link := Link} = World = link_serving(),
    Refused = open_frame(World, caller_key(), #{procedure => <<"unknown.proc">>}),
    Behind = open_frame(World, caller_key(), #{}),
    Stream = make_ref(),
    Link ! {macula_peering, new_dedicated_stream, self(), Stream},
    Link ! {quic, <<(macula_frame:encode(Refused))/binary, (macula_frame:encode(Behind))/binary>>, Stream, undefined},
    {ok, Refusal} = written_within(Stream, ?EVENT_MS),
    ?assertEqual({ok, <<"not_found">>}, refusal_code(Refused, Refusal)),
    ?assertEqual(none, handler_ran_within(200)),
    ?assertNot(has_stream_buffer(Link, Stream)),
    stop(World).

%% A frame that ends an established session, as a caller's verified STREAM_ERROR does, closes its stream for the frames
%% behind it in the same read too: an open right behind it starts no handler.
a_closed_stream_takes_no_frame_behind_its_close() ->
    Test = self(),
    #{link := Link} = World = link_serving(fun(Stream, _Args) ->
                                               Test ! {serving, Stream},
                                               receive {macula_stream, ended, Stream, How} -> Test ! {ended, How} end
                                           end, bidi),
    Caller = caller_key(),
    Stream = make_ref(),
    Open = open_frame(World, Caller, #{mode => bidi}),
    open_on(Link, Stream, Open),
    receive {serving, _} -> ok after ?EVENT_MS -> erlang:error(session_not_opened) end,
    Close = wire(macula_frame:caller_stream(#{frame_type => stream_error, seq => 0, code => <<"error">>,
                                              message => <<"stop">>}, Caller, verified(Open))),
    Behind = open_frame(World, Caller, #{mode => bidi}),
    Link ! {quic, <<(macula_frame:encode(Close))/binary, (macula_frame:encode(Behind))/binary>>, Stream, undefined},
    ?assertMatch({ended, {error, _}}, receive {ended, _} = Ended -> Ended after ?EVENT_MS -> none end),
    ?assertEqual(none, receive {serving, _} = Serving -> Serving after 200 -> none end),
    ?assertNot(has_stream_buffer(Link, Stream)),
    stop(World).

%% A dedicated stream the peer opened that brings no whole frame within dedicated_stream_open_timeout_ms closes and
%% leaves no buffer, while a stream that brought a served open outlives that deadline.
a_silent_opened_stream_closes_at_its_deadline() ->
    with_env(dedicated_stream_open_timeout_ms, 200, fun() ->
        Test = self(),
        #{link := Link} = World = link_serving(fun(Stream, _Args) -> Test ! {serving, Stream}, timer:sleep(2_000) end,
                                               bidi),
        Served = make_ref(),
        open_on(Link, Served, open_frame(World, caller_key(), #{mode => bidi})),
        receive {serving, _} -> ok after ?EVENT_MS -> erlang:error(session_not_opened) end,
        Silent = make_ref(),
        Link ! {macula_peering, new_dedicated_stream, self(), Silent},
        Link ! {quic, <<0, 0, 0>>, Silent, undefined},
        ?assert(has_stream_buffer(Link, Silent)),
        ?assertEqual(closed, closed_within(Silent, ?EVENT_MS)),
        ?assertNot(has_stream_buffer(Link, Silent)),
        ?assertEqual(open, closed_within(Served, 300)),
        ?assert(has_stream_buffer(Link, Served)),
        stop(World)
    end).

%% A dedicated stream the peer opens may start with an open of at most max_stream_open_bytes. A longer first frame
%% closes the stream as soon as its length arrives, with nothing written and no buffer left, and an open within the
%% limit is still served.
a_first_frame_past_the_open_limit_closes_its_stream() ->
    with_env(max_stream_open_bytes, 16_384, fun() ->
        #{link := Link} = World = link_serving(),
        Long = make_ref(),
        Link ! {macula_peering, new_dedicated_stream, self(), Long},
        Link ! {quic, <<16_385:32/big>>, Long, undefined},
        ?assertEqual(closed, closed_within(Long, ?EVENT_MS)),
        ?assertNot(has_stream_buffer(Link, Long)),
        ?assertEqual(none, written_within(Long, 0)),
        open_on(Link, make_ref(), open_frame(World, caller_key(), #{})),
        ?assertMatch({handler_ran, _}, handler_ran_within(?EVENT_MS)),
        stop(World)
    end).

%% A refusal that ends an open closes its dedicated stream: the link keeps no buffer for it, and bytes the peer sends on
%% it afterwards are not kept either.
refusal_closes_its_stream(Refusal) ->
    with_env(max_served_sessions_per_caller, 1, fun() ->
        #{link := Link} = World = link_serving(fun(_Stream, Args) -> timer:sleep(2_000), Args end),
        {Stream, Open} = refused_open(Refusal, World, caller_key()),
        {ok, Written} = written_within(Stream, ?EVENT_MS),
        ?assertEqual({ok, atom_to_binary(Refusal)}, refusal_code(Open, Written)),
        ?assertEqual(closed, closed_within(Stream, ?EVENT_MS)),
        ?assertNot(has_stream_buffer(Link, Stream)),
        Link ! {quic, <<0, 0, 0, 9, 1, 2, 3>>, Stream, undefined},
        ?assertNot(has_stream_buffer(Link, Stream)),
        stop(World)
    end).

refused_open(not_found, #{link := Link} = World, Caller) ->
    Stream = make_ref(),
    Open = open_frame(World, Caller, #{procedure => <<"unknown.proc">>}),
    open_on(Link, Stream, Open),
    {Stream, Open};
refused_open(unauthorized, #{link := Link} = World, Caller) ->
    Procedure = <<"foo.gated_closed">>,
    Policy = {realm_member_required, macula_identity:public(macula_identity:generate()), <<"member/email-verified">>},
    ok = macula_station_link:advertise_stream(Link, ?REALM, Procedure, server_stream, fun(_Stream, _Args) -> ok end,
                                              Policy),
    Stream = make_ref(),
    Open = open_frame(World, Caller, #{procedure => Procedure}),
    open_on(Link, Stream, Open),
    {Stream, Open};
refused_open(too_many_sessions, #{link := Link} = World, Caller) ->
    Served = make_ref(),
    open_on(Link, Served, open_frame(World, Caller, #{})),
    ?assertEqual(open, closed_within(Served, 300)),
    Stream = make_ref(),
    Open = open_frame(World, Caller, #{}),
    open_on(Link, Stream, Open),
    {Stream, Open}.

%% A dedicated stream the peer opened stays open only once it carries a session. One whose open does not verify, or
%% whose first frame is not an open, closes without a STREAM_ERROR and leaves no buffer.
stream_that_brings_no_session_closes(First) ->
    #{link := Link} = World = link_serving(),
    Stream = make_ref(),
    open_on(Link, Stream, first_frame(First, World)),
    ?assertEqual(closed, closed_within(Stream, ?EVENT_MS)),
    ?assertEqual(none, written_within(Stream, 0)),
    ?assertNot(has_stream_buffer(Link, Stream)),
    stop(World).

first_frame(unverified_open, World) ->
    flipped(open_frame(World, caller_key(), #{}));
first_frame(stream_end, _World) ->
    #{frame_type => stream_end, role => both}.

%% A first read that carries a verified open and then bytes that do not decode, or a frame that fails validation, serves
%% the open and then ends the stream as any dedicated stream's: its session ends, the stream closes, and no buffer is
%% left.
first_read_that_ends_badly(After) ->
    Test = self(),
    #{link := Link} = World = link_serving(fun(Stream, _Args) ->
                                               Test ! {serving, Stream},
                                               receive {macula_stream, ended, Stream, How} -> Test ! {ended, How} end
                                           end, bidi),
    Stream = make_ref(),
    Link ! {macula_peering, new_dedicated_stream, self(), Stream},
    Link ! {quic, <<(macula_frame:encode(open_frame(World, caller_key(), #{mode => bidi})))/binary, After/binary>>,
            Stream, undefined},
    ?assertMatch({serving, _}, receive {serving, _} = Serving -> Serving after ?EVENT_MS -> none end),
    ?assertMatch({ended, _How}, receive {ended, _} = Ended -> Ended after ?EVENT_MS -> none end),
    ?assertEqual(closed, closed_within(Stream, ?EVENT_MS)),
    ?assertNot(has_stream_buffer(Link, Stream)),
    stop(World).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

caller_key() ->
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    Key.

with_env(Key, Value, Fun) ->
    Old = application:get_env(macula, Key),
    ok = application:set_env(macula, Key, Value),
    try Fun() after restore_env(Key, Old) end.

restore_env(Key, undefined) -> application:unset_env(macula, Key);
restore_env(Key, {ok, Value}) -> application:set_env(macula, Key, Value).

%% A link that believes it is connected, with this process as its peering connection, dedicated-stream functions that
%% tell this process what the link opens, writes and closes, and a stream handler advertised for ?PROCEDURE that
%% reports what it returns.
link_serving() ->
    link_serving(fun(_Stream, Args) -> Args end).

link_serving(Handler) ->
    link_serving(Handler, server_stream).

link_serving(Handler, Mode) ->
    {ok, _} = application:ensure_all_started(macula),
    Test = self(),
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    {ok, Admission} = macula_request_admission:start_link(#{caller_quota => 256, share => 1024, cap => 46080,
                                                             reply_bytes => 262144, reply_bytes_total => 16777216}),
    {ok, Link} = macula_station_link:start_link(
                   #{seed => #{host => <<"127.0.0.1">>, port => 1}, expected_node_id => <<1:256>>,
                     node_identity => fun() -> Key end, issuer => Issuer, admission => Admission,
                     share => {seed, {<<"127.0.0.1">>, 1}},
                     connect => fun(_PeeringOpts) -> {error, not_dialed_here} end,
                     open_stream => fun(_Conn) -> Opened = make_ref(), Test ! {opened, Opened}, {ok, Opened} end,
                     send_on_stream => fun(Stream, Bytes) -> Test ! {written, Stream, Bytes}, ok end,
                     close_stream => fun(Stream) -> Test ! {closed, Stream}, ok end}),
    _ = sys:replace_state(Link, fun(S) -> setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Test),
                                                     <<2:256>>) end),
    ok = macula_station_link:advertise_stream(Link, ?REALM, ?PROCEDURE, Mode,
                                              fun(Stream, Args) -> Test ! {handler_ran, Handler(Stream, Args)}, ok end),
    #{link => Link, key => Key, admission => Admission}.

stop(#{link := Link}) ->
    catch macula_station_link:stop(Link),
    ok.

node_id(#{key := Key}) ->
    macula_node_keys:key_id(Key).

%% A STREAM_OPEN from Caller for the advertised procedure, addressed to the link's node unless Overrides says otherwise.
open_frame(World, Caller, Overrides) ->
    Spec = maps:merge(#{request_id => crypto:strong_rand_bytes(16), realm => ?REALM, procedure => ?PROCEDURE,
                        target => node_id(World), deadline => erlang:system_time(millisecond) + 30_000,
                        payload => #{}, mode => server_stream},
                      Overrides),
    wire(macula_frame:stream_open(Spec, Caller)).

%% The request a STREAM_OPEN carries, as the link verifies it.
verified(Frame) ->
    {ok, Open} = macula_frame:verify_request(Frame, profile()),
    Open.

wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

%% A dedicated stream the peer opens, with Frame as its first bytes.
open_on(Link, Stream, Frame) ->
    Link ! {macula_peering, new_dedicated_stream, self(), Stream},
    Link ! {quic, macula_frame:encode(Frame), Stream, undefined}.

on_stream(Link, Stream, Frame) ->
    Link ! {quic, macula_frame:encode(Frame), Stream, undefined}.

flipped(#{request := Signed} = Frame) ->
    Frame#{request := Signed#{signature := flip(maps:get(signature, Signed))}}.

%% The same request, signed by Other while still naming its first caller's key.
resigned(#{request := #{key := CallerKey}} = Frame, Other) ->
    #{request := OtherSigned} = macula_frame:stream_open(maps:without([key, request_hash, caller, frame_type],
                                                                      verified(Frame)), Other),
    Frame#{request := OtherSigned#{key := CallerKey}}.

%% A caller STREAM_DATA signed by Caller for a server_stream Open, where a caller sends no data. caller_stream/3 refuses
%% to build it in that mode, and a caller frame's signed fields name the request and not its mode, so it is signed under
%% the same open read as bidi.
forbidden_caller_chunk(Caller, #{mode := server_stream} = Open) ->
    wire(macula_frame:caller_stream(#{frame_type => stream_data, seq => 0, encoding => raw, body => <<"not yours">>},
                                    Caller, Open#{mode := bidi})).

flip(<<Head:20/binary, Byte, Tail/binary>>) ->
    <<Head/binary, (Byte bxor 1), Tail/binary>>.

%% Whether any map in the link's state has the stream as a key, as a dedicated stream's buffer entry does.
has_stream_buffer(Link, Stream) ->
    lists:any(fun(Field) -> is_map(Field) andalso is_map_key(Stream, Field) end, tuple_to_list(sys:get_state(Link))).

written_within(Stream, Ms) ->
    receive
        {written, Stream, Bytes} ->
            {ok, Frame, <<>>} = macula_frame:decode(Bytes),
            {ok, Frame}
    after Ms ->
        none
    end.

closed_within(Stream, Ms) ->
    receive
        {closed, Stream} -> closed
    after Ms ->
        open
    end.

%% The code of a STREAM_ERROR the link wrote, verified as the provider's first frame under the open it refuses.
refusal_code(OpenFrame, Refusal) ->
    Verified = macula_frame:verify_provider_stream(Refusal, macula_frame:open_stream(verified(OpenFrame)), profile()),
    refused_code(Verified).

refused_code({ok, #{frame_type := stream_error, code := Code}, _State}) -> {ok, Code};
refused_code(Other) -> {not_a_verified_refusal, Other}.

reported_within(Ms) ->
    receive
        {'$gen_cast', {object_refused, Kind, Charged}} -> {Kind, Charged}
    after Ms ->
        none
    end.

handler_ran_within(Ms) ->
    receive
        {handler_ran, Args} -> {handler_ran, Args}
    after Ms ->
        none
    end.
