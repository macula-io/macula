%% EUnit tests for a station link's stream sessions as a caller, and for routing a session's frames by the QUIC stream
%% they arrive on. A link opens a session with a STREAM_OPEN it signs for a named target, and starts the session's
%% stream with its key as a closure, the open, its connection and its profile, so the session signs its own frames. A
%% call before the handshake, a build the frame refuses, and an open past the limit open no QUIC stream. The provider's
%% verified frames reach the session bound to the QUIC stream they arrive on: once a session has ended, a later frame
%% takes no effect, and a frame for another session is refused there. A same-pool request keeps its client and server
%% sessions apart, a failed write ends the session it was for, and a disconnect ends every open session with its
%% reason's name only. The test process is the link's peering connection, and the link's open_stream, send_on_stream
%% and close_stream options tell it what the link does to dedicated streams, so no shared module is replaced.
-module(macula_station_link_stream_session_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<9:256>>).
-define(PROCEDURE, <<"acme/count_v1">>).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(MARKER, <<"a term of the reason that must not reach a reader">>).
-define(EVENT_MS, 1_000).

stream_session_test_() ->
    [{case_name(Case), {spawn, fun() -> process_flag(trap_exit, true), Case() end}}
     || Case <- [fun a_link_started_session_signs_its_open_and_its_first_frame/0,
                 fun a_call_before_the_handshake_is_refused_and_opens_no_stream/0,
                 fun a_refused_build_or_an_open_past_the_limit_opens_no_stream/0,
                 fun an_open_just_within_the_limit_goes_out/0,
                 fun the_open_carries_a_token_only_when_one_is_given/0,
                 fun a_verified_provider_chunk_reaches_the_reader/0,
                 fun a_verified_provider_reply_settles_await_reply/0,
                 fun a_verified_provider_error_reaches_the_reader/0,
                 fun after_a_verified_stream_error_a_later_reply_and_chunk_take_no_effect/0,
                 fun a_frame_for_one_session_on_another_sessions_stream_reaches_neither_owner/0,
                 fun a_same_pool_request_keeps_its_client_and_server_sessions_apart/0,
                 fun a_failed_write_ends_the_session_it_was_for/0,
                 fun a_disconnect_ends_open_sessions/0,
                 fun a_disconnect_tells_open_sessions_its_reasons_name_only/0]].

%%------------------------------------------------------------------
%% Cases
%%------------------------------------------------------------------

%% The STREAM_OPEN a link writes verifies, names the link's node as its caller and the given target, carries the
%% procedure, mode and payload with a 16-byte request id, and the session's first chunk verifies as the caller's seq 0
%% under that open.
a_link_started_session_signs_its_open_and_its_first_frame() ->
    #{link := Link} = World = linked(),
    Provider = key(),
    #{stream := StreamPid, quic := Quic, open := Open} = opened(World, macula_node_keys:key_id(Provider), bidi),
    ?assertMatch(#{procedure := ?PROCEDURE, mode := bidi, payload := #{{text, <<"n">>} := 1}, request_id := <<_:128>>},
                 Open),
    ?assertEqual({node_id(World), macula_node_keys:key_id(Provider)}, {maps:get(caller, Open), maps:get(target, Open)}),
    ok = macula_stream:send(StreamPid, <<"one">>),
    {ok, Written} = written_within(Quic, ?EVENT_MS),
    ?assertMatch({ok, #{frame_type := stream_data, seq := 0, body := <<"one">>}, _State},
                 macula_frame:verify_caller_stream(Written, macula_frame:open_stream(Open), profile())),
    stop(Link).

%% A link whose handshake has not completed refuses a stream call with not_connected, and opens nothing.
a_call_before_the_handshake_is_refused_and_opens_no_stream() ->
    #{link := Link} = linked(unconnected),
    ?assertEqual({error, not_connected},
                 macula_station_link:call_stream(Link, macula_node_keys:key_id(key()), ?REALM, ?PROCEDURE, #{}, #{})),
    ?assertEqual(none, receive {opened, _} = Opened -> Opened after 200 -> none end),
    stop(Link).

%% A procedure the frame's text bound refuses, and an open longer than max_stream_open_bytes, are refused where the
%% call is made: no stream process starts and no dedicated stream is opened.
a_refused_build_or_an_open_past_the_limit_opens_no_stream() ->
    #{link := Link} = linked(),
    Target = macula_node_keys:key_id(key()),
    TooLong = binary:copy(<<"p">>, 513),
    ?assertEqual({error, {refused, {text_too_long, procedure}}},
                 macula_station_link:call_stream(Link, Target, ?REALM, TooLong, #{}, #{})),
    with_open_limit(1024, fun() ->
        ?assertEqual({error, {open_too_large, 1024}},
                     macula_station_link:call_stream(Link, Target, ?REALM, ?PROCEDURE, crypto:strong_rand_bytes(2048),
                                                     #{}))
    end),
    ?assertEqual(none, receive {opened, _} = Opened -> Opened after 200 -> none end),
    stop(Link).

%% The limit applies to the signed STREAM_OPEN as written: an open 10 bytes under it goes out, and one 10 bytes over it
%% is refused with nothing opened.
an_open_just_within_the_limit_goes_out() ->
    #{link := Link} = World = linked(),
    Target = macula_node_keys:key_id(key()),
    #{open_frame := Sized} = opened(World, Target, server_stream, #{pad => <<>>}),
    Limit = byte_size(macula_frame:encode(Sized)) - 4 + 100,
    with_open_limit(Limit, fun() ->
        ?assertMatch({ok, _}, macula_station_link:call_stream(Link, Target, ?REALM, ?PROCEDURE,
                                                              #{pad => binary:copy(<<0>>, 90)}, #{})),
        ?assertMatch(#{frame_type := stream_open}, open_written_within(?EVENT_MS)),
        ?assertMatch({opened, _}, receive {opened, _} = Within -> Within after ?EVENT_MS -> none end),
        ?assertEqual({error, {open_too_large, Limit}},
                     macula_station_link:call_stream(Link, Target, ?REALM, ?PROCEDURE,
                                                     #{pad => binary:copy(<<0>>, 110)}, #{})),
        ?assertEqual(none, receive {opened, _} = Opened -> Opened after 200 -> none end)
    end),
    stop(Link).

%% A caller reaches a gated procedure by passing ucan_token in call_stream's options. The verified open carries it as
%% its token, and an open without one carries no token at all.
the_open_carries_a_token_only_when_one_is_given() ->
    #{link := Link} = World = linked(),
    Target = macula_node_keys:key_id(key()),
    #{open := WithToken} = opened(World, Target, server_stream, #{n => 1}, #{ucan_token => <<"token-bytes">>}),
    #{open := WithoutToken} = opened(World, Target, server_stream, #{n => 1}, #{}),
    ?assertEqual(<<"token-bytes">>, maps:get(token, WithToken, undefined)),
    ?assertNot(is_map_key(token, WithoutToken)),
    stop(Link).

%% The provider's verified chunk on the session's own stream reaches the reader.
a_verified_provider_chunk_reaches_the_reader() ->
    #{link := Link} = World = linked(),
    Provider = key(),
    Target = macula_node_keys:key_id(Provider),
    #{stream := StreamPid, quic := Quic, open := Open} = opened(World, Target, server_stream),
    on_stream(Link, Quic, provider_frame(#{frame_type => stream_data, seq => 0, encoding => raw,
                                           body => <<"chunk-bytes">>}, Provider, Open)),
    ?assertEqual({chunk, <<"chunk-bytes">>}, macula_stream:recv(StreamPid, ?EVENT_MS)),
    stop(Link).

%% The provider's verified STREAM_REPLY settles await_reply with its payload.
a_verified_provider_reply_settles_await_reply() ->
    #{link := Link} = World = linked(),
    Provider = key(),
    #{stream := StreamPid, quic := Quic, open := Open} = opened(World, macula_node_keys:key_id(Provider), bidi),
    on_stream(Link, Quic, provider_frame(#{frame_type => stream_reply, seq => 0, payload => #{count => 12}},
                                         Provider, Open)),
    ?assertEqual({ok, #{{text, <<"count">>} => 12}}, macula_stream:await_reply(StreamPid, ?EVENT_MS)),
    stop(Link).

%% The provider's verified STREAM_ERROR reaches a waiting reader as its code and message.
a_verified_provider_error_reaches_the_reader() ->
    #{link := Link} = World = linked(),
    Provider = key(),
    Target = macula_node_keys:key_id(Provider),
    #{stream := StreamPid, quic := Quic, open := Open} = opened(World, Target, server_stream),
    on_stream(Link, Quic, provider_frame(#{frame_type => stream_error, seq => 0, code => <<"deadline_exceeded">>,
                                           message => <<"server too slow">>}, Provider, Open)),
    ?assertEqual({error, {<<"deadline_exceeded">>, <<"server too slow">>}}, macula_stream:recv(StreamPid, ?EVENT_MS)),
    stop(Link).

%% Once the provider's verified STREAM_ERROR has ended a session, a verified STREAM_REPLY and a STREAM_DATA that follow
%% through the link each give one uncharged stream_ended report and reach no reader: await_reply keeps the error, and
%% recv returns it, never eof.
after_a_verified_stream_error_a_later_reply_and_chunk_take_no_effect() ->
    #{link := Link} = World = linked(),
    Provider = key(),
    #{stream := StreamPid, quic := Quic, open := Open} = opened(World, macula_node_keys:key_id(Provider), bidi),
    on_stream(Link, Quic, provider_frame(#{frame_type => stream_error, seq => 0, code => <<"stop">>,
                                           message => <<"why">>}, Provider, Open)),
    on_stream(Link, Quic, provider_frame(#{frame_type => stream_reply, seq => 1, payload => <<"late">>},
                                         Provider, Open)),
    on_stream(Link, Quic, provider_frame(#{frame_type => stream_data, seq => 2, encoding => raw, body => <<"late">>},
                                         Provider, Open)),
    Reported = [reported_within(?EVENT_MS), reported_within(?EVENT_MS)],
    ?assertEqual([{stream_ended, false}, {stream_ended, false}], Reported),
    ?assertEqual({error, {<<"stop">>, <<"why">>}}, macula_stream:await_reply(StreamPid, ?EVENT_MS)),
    ?assertEqual({error, {<<"stop">>, <<"why">>}}, macula_stream:recv(StreamPid, ?EVENT_MS)),
    stop(Link).

%% A provider frame signed under session A's open, arriving on session B's QUIC stream, reaches only B, whose verifier
%% refuses it as request_mismatch, uncharged. Neither session's reader gets it.
a_frame_for_one_session_on_another_sessions_stream_reaches_neither_owner() ->
    #{link := Link} = World = linked(),
    Provider = key(),
    Target = macula_node_keys:key_id(Provider),
    #{stream := A, open := OpenA} = opened(World, Target, bidi),
    #{stream := B, quic := QuicB} = opened(World, Target, bidi),
    on_stream(Link, QuicB, provider_frame(#{frame_type => stream_data, seq => 0, encoding => raw, body => <<"for a">>},
                                          Provider, OpenA)),
    ?assertEqual({request_mismatch, false}, reported_within(?EVENT_MS)),
    ?assertEqual([{error, timeout}, {error, timeout}], [macula_stream:recv(A, 300), macula_stream:recv(B, 300)]),
    stop(Link).

%% A link that calls a procedure it serves, as a same-pool request does: the STREAM_OPEN it wrote comes back on a
%% dedicated stream the peer opens. The server session writes on the stream its open came in on, and a provider frame on
%% the client's stream reaches the client session only.
a_same_pool_request_keeps_its_client_and_server_sessions_apart() ->
    #{link := Link} = World = linked(),
    Test = self(),
    %% The handler owns the server session's stream, which ends when it returns, so it waits while the test uses it.
    ok = macula_station_link:advertise_stream(Link, ?REALM, ?PROCEDURE, bidi,
                                              fun(Stream, _Args) -> Test ! {serving, Stream}, timer:sleep(2_000) end),
    #{stream := Client, quic := ClientQuic, open_frame := OpenFrame} = opened(World, node_id(World), bidi),
    ServerQuic = make_ref(),
    Link ! {macula_peering, new_dedicated_stream, Test, ServerQuic},
    Link ! {quic, macula_frame:encode(OpenFrame), ServerQuic, undefined},
    Server = receive {serving, S} -> S after ?EVENT_MS -> erlang:error(not_served) end,
    ok = macula_stream:send(Server, <<"from the server session">>),
    {ok, ServerWritten} = written_within(ServerQuic, ?EVENT_MS),
    ?assertEqual(none, written_within(ClientQuic, 200)),
    on_stream(Link, ClientQuic, ServerWritten),
    ?assertEqual({chunk, <<"from the server session">>}, macula_stream:recv(Client, ?EVENT_MS)),
    stop(Link).

%% A write that fails on a session's dedicated stream ends that session with a transport failure: the link reports it
%% under the session's attach id, which is the stream's own id.
a_failed_write_ends_the_session_it_was_for() ->
    #{link := Link} = World = linked(fun(_Stream, _Bytes) -> {error, closed} end),
    #{stream := StreamPid} = opened(World, macula_node_keys:key_id(key()), bidi, #{n => 1}, #{}, open_failing),
    ?assertEqual({error, {transport, closed}}, macula_stream:recv(StreamPid, ?EVENT_MS)),
    stop(Link).

%% A disconnect ends every open session: a reply waiter gets a disconnected error.
a_disconnect_ends_open_sessions() ->
    #{link := Link} = World = linked(),
    #{stream := StreamPid} = opened(World, macula_node_keys:key_id(key()), bidi),
    Link ! {macula_peering, disconnected, self(), peer_gone},
    ?assertMatch({error, {<<"disconnected">>, _}}, macula_stream:await_reply(StreamPid, ?EVENT_MS)).

%% The sessions a disconnect ends are told the reason's name, and none of its terms.
a_disconnect_tells_open_sessions_its_reasons_name_only() ->
    #{link := Link} = World = linked(),
    #{stream := StreamPid} = opened(World, macula_node_keys:key_id(key()), bidi),
    Link ! {macula_peering, disconnected, self(), {peer_gone, ?MARKER}},
    ?assertEqual({error, {<<"disconnected">>, <<"disconnected">>}}, macula_stream:await_reply(StreamPid, ?EVENT_MS)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

key() ->
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    Key.

%% A link that believes it is connected, with this process as its peering connection, and dedicated-stream functions
%% that tell this process what the link opens, writes and closes. Given unconnected, the link's handshake has not
%% completed; given a writer, every write answers what the writer does.
linked() ->
    Test = self(),
    linked(fun(Stream, Bytes) -> Test ! {written, Stream, Bytes}, ok end).

linked(unconnected) ->
    Test = self(),
    started_link(fun(Stream, Bytes) -> Test ! {written, Stream, Bytes}, ok end);
linked(Writer) ->
    #{link := Link} = World = started_link(Writer),
    Test = self(),
    _ = sys:replace_state(Link, fun(S) -> setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Test),
                                                     <<2:256>>) end),
    World.

started_link(Writer) ->
    {ok, _} = application:ensure_all_started(macula),
    Test = self(),
    Key = key(),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    {ok, Admission} = macula_request_admission:start_link(#{caller_quota => 256, share => 1024, cap => 46080,
                                                             reply_bytes => 262144, reply_bytes_total => 16777216}),
    {ok, Link} = macula_station_link:start_link(
                   #{seed => #{host => <<"127.0.0.1">>, port => 1}, expected_node_id => <<1:256>>,
                     node_identity => fun() -> Key end, issuer => Issuer, admission => Admission,
                     share => {seed, {<<"127.0.0.1">>, 1}},
                     connect => fun(_PeeringOpts) -> {error, not_dialed_here} end,
                     open_stream => fun(_Conn) -> Opened = make_ref(), Test ! {opened, Opened}, {ok, Opened} end,
                     send_on_stream => fun(Stream, Bytes) -> Test ! {writing, Stream, Bytes}, Writer(Stream, Bytes) end,
                     close_stream => fun(Stream) -> Test ! {closed, Stream}, ok end}),
    #{link => Link, key => Key}.

stop(Link) ->
    catch macula_station_link:stop(Link),
    ok.

node_id(#{key := Key}) ->
    macula_node_keys:key_id(Key).

with_open_limit(Bytes, Fun) ->
    Old = application:get_env(macula, max_stream_open_bytes),
    ok = application:set_env(macula, max_stream_open_bytes, Bytes),
    try Fun() after restore_open_limit(Old) end.

restore_open_limit(undefined) -> application:unset_env(macula, max_stream_open_bytes);
restore_open_limit({ok, Bytes}) -> application:set_env(macula, max_stream_open_bytes, Bytes).

%% A session the link opens for Target: its stream process, the dedicated stream the link opened, and the STREAM_OPEN
%% it wrote there, as sent and as a verifier reads it.
opened(World, Target, Mode) ->
    opened(World, Target, Mode, #{n => 1}).

opened(World, Target, Mode, Args) ->
    opened(World, Target, Mode, Args, #{}).

opened(World, Target, Mode, Args, Opts) ->
    opened(World, Target, Mode, Args, Opts, written).

opened(#{link := Link}, Target, Mode, Args, Opts, Writes) ->
    {ok, StreamPid} = macula_station_link:call_stream(Link, Target, ?REALM, ?PROCEDURE, Args, Opts#{mode => Mode}),
    Quic = receive {opened, Opened} -> Opened after ?EVENT_MS -> erlang:error(no_stream_opened) end,
    OpenFrame = open_written(Writes, Quic),
    {ok, Open} = macula_frame:verify_request(OpenFrame, profile()),
    #{stream => StreamPid, quic => Quic, open_frame => OpenFrame, open => Open}.

open_written(written, Quic) ->
    {ok, Frame} = written_within(Quic, ?EVENT_MS),
    Frame;
open_written(open_failing, Quic) ->
    receive
        {writing, Quic, Bytes} ->
            {ok, Frame, <<>>} = macula_frame:decode(Bytes),
            Frame
    after ?EVENT_MS ->
        erlang:error(no_open_written)
    end.

%% The next STREAM_OPEN the link wrote on any stream.
open_written_within(Ms) ->
    receive
        {writing, _Stream, Bytes} ->
            {ok, Frame, <<>>} = macula_frame:decode(Bytes),
            Frame
    after Ms ->
        none
    end.

provider_frame(Spec, Provider, Open) ->
    wire(macula_frame:provider_stream(Spec, Provider, Open)).

wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

on_stream(Link, Stream, Frame) ->
    Link ! {quic, macula_frame:encode(Frame), Stream, undefined}.

%% The next frame the link wrote on Stream, decoded.
written_within(Stream, Ms) ->
    receive
        {writing, Stream, Bytes} ->
            receive {written, Stream, Bytes} -> ok after 0 -> ok end,
            {ok, Frame, <<>>} = macula_frame:decode(Bytes),
            {ok, Frame}
    after Ms ->
        none
    end.

reported_within(Ms) ->
    receive
        {'$gen_cast', {object_refused, Kind, Charged}} -> {Kind, Charged}
    after Ms ->
        none
    end.
