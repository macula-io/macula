%% EUnit tests for a link-carried macula_stream (identity migration step 3, item 5). The stream signs and numbers its
%% own frames, from 0 across data, end, error and reply, and hands their bytes to its link. It verifies the peer's
%% frames against its STREAM_OPEN before they take effect, and reports a refused frame to its peering connection with
%% whether the refusal is charged. The test process stands in for both the link and the connection.
-module(macula_stream_link_carrier_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SID, <<9:128>>).

carrier_test_() ->
    {setup, fun keys/0, fun cases/1}.

cases(Keys) ->
    [{case_name(Case), fun() -> Case(Keys) end}
     || Case <- [fun a_provider_numbers_its_frames_from_0_across_data_and_reply/1,
                 fun a_caller_numbers_its_frames_from_0_and_sends_nothing_after_its_end/1,
                 fun a_frame_the_side_may_not_send_is_not_sent/1,
                 fun a_verified_provider_frame_reaches_the_reader/1,
                 fun a_frame_with_a_bad_signature_gives_one_charged_report_and_the_stream_carries_on/1,
                 fun an_out_of_order_frame_gives_one_uncharged_report/1,
                 fun a_frame_after_the_peer_ended_gives_one_uncharged_report/1,
                 fun a_frame_for_another_request_gives_one_uncharged_report/1,
                 fun set_error_sends_stream_error_with_the_reason_as_bounded_text/1,
                 fun a_failed_write_ends_the_stream_with_a_transport_failure_and_no_report/1,
                 fun a_control_frame_on_a_stream_rejects_the_connection_and_ends_the_stream_in_pq_pure/1,
                 fun a_control_frame_on_a_stream_rejects_the_connection_and_ends_the_stream_in_pq_hybrid/1,
                 fun the_status_of_a_stream_carries_its_key_with_no_private_half/1]].

%%------------------------------------------------------------------
%% Cases
%%------------------------------------------------------------------

a_provider_numbers_its_frames_from_0_across_data_and_reply(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Stream = stream(server, Provider, Open),
    ok = macula_stream:send(Stream, <<"one">>),
    ok = macula_stream:send(Stream, <<"two">>),
    ok = macula_stream:set_reply(Stream, <<"done">>),
    {Frames, Lasts} = lists:unzip([sent(), sent(), sent()]),
    ?assertEqual([{stream_data, 0}, {stream_data, 1}, {stream_reply, 2}],
                 verified_in_turn(fun macula_frame:verify_provider_stream/3, Frames, Open)),
    ?assertEqual([false, false, true], Lasts),
    gen_server:stop(Stream).

a_caller_numbers_its_frames_from_0_and_sends_nothing_after_its_end(#{caller := Caller} = Keys) ->
    Open = verified_open(Keys, bidi),
    Stream = stream(client, Caller, Open),
    ok = macula_stream:send(Stream, <<"one">>),
    ok = macula_stream:close_send(Stream),
    ok = macula_stream:close(Stream),
    {Frames, Lasts} = lists:unzip([sent(), sent()]),
    ?assertEqual([{stream_data, 0}, {stream_end, 1}],
                 verified_in_turn(fun macula_frame:verify_caller_stream/3, Frames, Open)),
    ?assertEqual([false, false], Lasts),
    ?assertEqual(none, nothing_sent()),
    gen_server:stop(Stream).

a_frame_the_side_may_not_send_is_not_sent(#{caller := Caller} = Keys) ->
    Stream = stream(client, Caller, verified_open(Keys, server_stream)),
    ?assertEqual({error, {send_not_allowed, server_stream}}, macula_stream:send(Stream, <<"data from a caller in a server_stream">>)),
    ?assertEqual({error, not_allowed}, macula_stream:set_reply(Stream, <<"a reply from a caller">>)),
    ?assertEqual(none, nothing_sent()),
    gen_server:stop(Stream).

a_verified_provider_frame_reaches_the_reader(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Stream = stream(client, Caller, Open),
    ok = macula_stream:deliver_frame(Stream, provider_frame(chunk(0, <<"one">>), Provider, Open)),
    ?assertEqual({chunk, <<"one">>}, macula_stream:recv(Stream, 1000)),
    ?assertEqual(none, no_more_reports()),
    gen_server:stop(Stream).

a_frame_with_a_bad_signature_gives_one_charged_report_and_the_stream_carries_on(#{caller := Caller,
                                                                                 provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Stream = stream(client, Caller, Open),
    ok = macula_stream:deliver_frame(Stream, provider_frame(chunk(0, <<"one">>), Provider, Open)),
    #{stream := Signed} = Later = macula_frame:provider_stream(chunk(1, <<"two">>), Provider, Open),
    ok = macula_stream:deliver_frame(Stream, wire(Later#{stream := Signed#{tbs := flip(maps:get(tbs, Signed))}})),
    ?assertEqual({signature_invalid, true}, reported()),
    ?assertEqual(none, no_more_reports()),
    ok = macula_stream:deliver_frame(Stream, wire(Later)),
    ?assertEqual({chunk, <<"one">>}, macula_stream:recv(Stream, 1000)),
    ?assertEqual({chunk, <<"two">>}, macula_stream:recv(Stream, 1000)),
    gen_server:stop(Stream).

an_out_of_order_frame_gives_one_uncharged_report(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Stream = stream(client, Caller, Open),
    ok = macula_stream:deliver_frame(Stream, provider_frame(chunk(0, <<"one">>), Provider, Open)),
    ok = macula_stream:deliver_frame(Stream, provider_frame(chunk(2, <<"three">>), Provider, Open)),
    ?assertEqual({seq_mismatch, false}, reported()),
    ?assertEqual(none, no_more_reports()),
    ok = macula_stream:deliver_frame(Stream, provider_frame(chunk(1, <<"two">>), Provider, Open)),
    ?assertEqual({chunk, <<"one">>}, macula_stream:recv(Stream, 1000)),
    ?assertEqual({chunk, <<"two">>}, macula_stream:recv(Stream, 1000)),
    gen_server:stop(Stream).

a_frame_after_the_peer_ended_gives_one_uncharged_report(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Stream = stream(client, Caller, Open),
    End = #{frame_type => stream_end, seq => 0, role => send},
    ok = macula_stream:deliver_frame(Stream, provider_frame(End, Provider, Open)),
    ok = macula_stream:deliver_frame(Stream, provider_frame(chunk(1, <<"late">>), Provider, Open)),
    ?assertEqual({stream_ended, false}, reported()),
    ?assertEqual(none, no_more_reports()),
    ?assertEqual(eof, macula_stream:recv(Stream, 1000)),
    gen_server:stop(Stream).

a_frame_for_another_request_gives_one_uncharged_report(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Other = verified_request(Caller, (open_spec(Keys, bidi))#{request_id => <<8:128>>}),
    Stream = stream(client, Caller, Open),
    ok = macula_stream:deliver_frame(Stream, provider_frame(chunk(0, <<"one">>), Provider, Other)),
    ?assertEqual({request_mismatch, false}, reported()),
    ?assertEqual(none, no_more_reports()),
    gen_server:stop(Stream).

set_error_sends_stream_error_with_the_reason_as_bounded_text(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Longest = binary:copy(<<"y">>, 256),
    Reasons = [<<"quota exceeded">>, no_capacity, {badarg, 1}, binary:copy(<<"x">>, 257), <<255, 254>>, Longest],
    ?assertEqual([{<<"error">>, <<"quota exceeded">>}, {<<"error">>, <<"no_capacity">>}, {<<"error">>, <<>>},
                  {<<"error">>, <<>>}, {<<"error">>, <<>>}, {<<"error">>, Longest}],
                 [error_sent(Reason, Provider, Open) || Reason <- Reasons]).

a_failed_write_ends_the_stream_with_a_transport_failure_and_no_report(#{provider := Provider} = Keys) ->
    Stream = stream(server, Provider, verified_open(Keys, bidi)),
    Test = self(),
    _Reader = spawn(fun() -> Test ! {read, macula_stream:recv(Stream, 2000)} end),
    ok = await_waiting_reader(Stream),
    Stream ! {stream_write_failed, ?SID, closed},
    ?assertEqual({error, {transport, closed}}, receive {read, Read} -> Read after 1000 -> none end),
    ?assertEqual({error, {transport, closed}}, macula_stream:await_reply(Stream, 100)),
    ?assertEqual({error, send_closed}, macula_stream:send(Stream, <<"after the failure">>)),
    ?assertEqual(none, no_more_reports()),
    ?assertEqual(none, nothing_sent()),
    gen_server:stop(Stream).

the_status_of_a_stream_carries_its_key_with_no_private_half(#{provider := Provider} = Keys) ->
    Stream = stream(server, Provider, verified_open(Keys, bidi)),
    Bytes = term_to_binary(sys:get_status(Stream)),
    gen_server:stop(Stream),
    #{components := Components} = Provider,
    ?assertNotEqual(nomatch, binary:match(Bytes, macula_node_keys:public_key(Provider))),
    ?assertEqual([], [Private || #{private := Private} <- Components, binary:match(Bytes, Private) =/= nomatch]).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

keys() ->
    Generate = fun() -> {ok, Key} = macula_node_keys:generate(identity, pq_pure), Key end,
    #{caller => Generate(), provider => Generate()}.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

open_spec(#{provider := Provider}, Mode) ->
    #{request_id => <<7:128>>, realm => <<1:256>>, procedure => <<"acme/count_v1">>,
      target => macula_node_keys:key_id(Provider), deadline => 1789000600000, payload => #{}, mode => Mode}.

verified_open(#{caller := Caller} = Keys, Mode) ->
    verified_request(Caller, open_spec(Keys, Mode)).

verified_request(Caller, Spec) ->
    {ok, Request} = macula_frame:verify_request(wire(macula_frame:stream_open(Spec, Caller)), pq_pure),
    Request.

%% A link-carried stream on one side of Open, with this process as its link and its connection.
stream(Role, Key, Open) ->
    stream(Role, Key, Open, pq_pure).

stream(Role, Key, #{mode := Mode} = Open, Profile) ->
    {ok, Pid} = macula_stream:start_link(#{id => ?SID, role => Role, mode => Mode, owner => self(), key => Key,
                                            open => Open, conn => self(), profile => Profile}),
    ok = macula_stream:attach_to_link(Pid, self(), ?SID),
    Pid.

chunk(Seq, Body) ->
    #{frame_type => stream_data, seq => Seq, encoding => raw, body => Body}.

provider_frame(Spec, Provider, Open) ->
    wire(macula_frame:provider_stream(Spec, Provider, Open)).

%% The type and number of each frame, verified in turn from a fresh stream state.
verified_in_turn(Verify, Frames, Open) ->
    {Read, _State} = lists:mapfoldl(fun(Frame, State) ->
                                        {ok, #{frame_type := Type, seq := Seq}, Next} = Verify(Frame, State, pq_pure),
                                        {{Type, Seq}, Next}
                                    end, macula_frame:open_stream(Open), Frames),
    Read.

%% The STREAM_ERROR code and message a provider sends for set_error with Reason.
error_sent(Reason, Provider, Open) ->
    Stream = stream(server, Provider, Open),
    ok = macula_stream:set_error(Stream, Reason),
    {Frame, true} = sent(),
    gen_server:stop(Stream),
    {ok, #{frame_type := stream_error, code := Code, message := Message}, _} =
        macula_frame:verify_provider_stream(Frame, macula_frame:open_stream(Open), pq_pure),
    {Code, Message}.

%% The next frame the stream handed to its link, decoded, with whether it was marked last.
sent() ->
    receive
        {'$gen_cast', {send_stream_bytes, ?SID, Bytes, Last}} ->
            {ok, Frame, <<>>} = macula_frame:decode(Bytes),
            {Frame, Last}
    after 1000 ->
        erlang:error(no_frame_sent)
    end.

nothing_sent() ->
    receive
        {'$gen_cast', {send_stream_bytes, ?SID, _Bytes, _Last}} -> sent
    after 200 ->
        none
    end.

%% The next refusal the stream reported to its connection, and whether it is charged.
reported() ->
    receive
        {'$gen_cast', {object_refused, Kind, Charged}} -> {Kind, Charged}
    after 1000 ->
        none
    end.

no_more_reports() ->
    receive
        {'$gen_cast', {object_refused, _Kind, _Charged}} -> reported
    after 200 ->
        none
    end.

await_waiting_reader(Stream) ->
    await_waiting_reader(Stream, 50).

await_waiting_reader(_Stream, 0) ->
    erlang:error(no_waiting_reader);
await_waiting_reader(Stream, Tries) ->
    case macula_stream:info(Stream) of
        #{waiters := 1} -> ok;
        _ -> timer:sleep(20), await_waiting_reader(Stream, Tries - 1)
    end.

%% A frame as a peer receives it: encoded and decoded.
wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

flip(<<Head:20/binary, Byte, Tail/binary>>) ->
    <<Head/binary, (Byte bxor 1), Tail/binary>>.

%% A frame of a type that belongs on the control stream, arriving on a dedicated stream, is the connection peer's doing
%% in either profile: the stream rejects its connection with malformed_frame and ends with that transport failure.
a_control_frame_on_a_stream_rejects_the_connection_and_ends_the_stream_in_pq_pure(Keys) ->
    control_frame_rejected(Keys, pq_pure).

a_control_frame_on_a_stream_rejects_the_connection_and_ends_the_stream_in_pq_hybrid(Keys) ->
    control_frame_rejected(Keys, pq_hybrid).

control_frame_rejected(#{provider := Provider} = Keys, Profile) ->
    Stream = stream(server, Provider, verified_open(Keys, bidi), Profile),
    Test = self(),
    _Reader = spawn(fun() -> Test ! {read, macula_stream:recv(Stream, 2000)} end),
    ok = await_waiting_reader(Stream),
    ok = macula_stream:deliver_frame(Stream, wire(macula_frame:ping(#{nonce => crypto:strong_rand_bytes(16)}))),
    ?assertEqual(malformed_frame, receive {'$gen_cast', {reject, Reason}} -> Reason after 1000 -> none end),
    ?assertEqual({error, {transport, malformed_frame}}, receive {read, Read} -> Read after 1000 -> none end),
    ?assertEqual({error, send_closed}, macula_stream:send(Stream, <<"after the control frame">>)),
    ?assertEqual(none, no_more_reports()),
    gen_server:stop(Stream).
