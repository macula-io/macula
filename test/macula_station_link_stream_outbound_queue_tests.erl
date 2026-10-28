%% EUnit tests for the outbound side of a link-carried stream whose peer stops taking data (macula#141). A write the peer
%% withholds credit for is not a failed write: the link holds that stream's bytes and writes them when the peer takes data
%% again, and every other session on the link carries on meanwhile. The test process is the link's peering connection and
%% its send_on_stream is the writer, so no shared module is replaced.
-module(macula_station_link_stream_outbound_queue_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<9:256>>).
-define(PROCEDURE, <<"acme/count_v1">>).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(EVENT_MS, 1_000).

outbound_queue_test_() ->
    [{case_name(Case), {spawn, fun() -> process_flag(trap_exit, true), Case() end}}
     || Case <- [fun a_withheld_write_does_not_end_the_session_that_made_it/0,
                 fun a_withheld_write_goes_out_in_order_when_the_peer_takes_data_again/0,
                 fun a_second_sessions_call_completes_while_a_first_is_withheld/0,
                 fun a_session_over_the_queue_cap_ends_alone/0,
                 fun a_withheld_last_frame_ends_the_session_once_it_is_written/0]].

%%------------------------------------------------------------------
%% Cases
%%------------------------------------------------------------------

%% The peer withholding credit on a session's stream is not a failed write: the session stays open, and the bytes it
%% sends next are still taken.
a_withheld_write_does_not_end_the_session_that_made_it() ->
    #{link := Link} = World = linked(),
    #{stream := StreamPid, quic := Quic} = opened(World, target(), bidi),
    withhold(World, Quic),
    ?assertEqual(ok, macula_stream:send(StreamPid, <<"one">>)),
    ?assert(is_process_alive(StreamPid)),
    ?assertEqual(ok, macula_stream:send(StreamPid, <<"two">>)),
    %% Both frames are handed over and withheld before credit returns; the two casts and this
    %% process's send_ready are not ordered against each other, so the test waits for them.
    ?assertEqual([], data_written_on(World, Quic, ?EVENT_MS)),
    grant(World, Quic),
    Link ! {quic, send_ready, Quic, undefined},
    ?assertEqual([<<"one">>, <<"two">>], data_written_on(World, Quic, ?EVENT_MS)),
    stop(Link).

%% The bytes held for a stream are written when the peer takes data again, in the order they were sent, and the link
%% then writes the next one straight through.
a_withheld_write_goes_out_in_order_when_the_peer_takes_data_again() ->
    #{link := Link} = World = linked(),
    #{stream := StreamPid, quic := Quic} = opened(World, target(), bidi),
    withhold(World, Quic),
    [?assertEqual(ok, macula_stream:send(StreamPid, Body)) || Body <- [<<"one">>, <<"two">>, <<"three">>]],
    ?assertEqual([], data_written_on(World, Quic, ?EVENT_MS)),
    grant(World, Quic),
    Link ! {quic, send_ready, Quic, undefined},
    ?assertEqual([<<"one">>, <<"two">>, <<"three">>], data_written_on(World, Quic, ?EVENT_MS)),
    ?assertEqual(ok, macula_stream:send(StreamPid, <<"four">>)),
    ?assertEqual([<<"four">>], data_written_on(World, Quic, ?EVENT_MS)),
    stop(Link).

%% One session whose peer withholds credit does not hold another session on the same link: the second's chunk goes out
%% and its call's reply comes back within the bound, while the first is still withheld.
a_second_sessions_call_completes_while_a_first_is_withheld() ->
    #{link := Link} = World = linked(),
    Provider = key(),
    #{stream := Withheld, quic := WithheldQuic} = opened(World, target(), bidi),
    #{stream := Caller, quic := CallerQuic, open := CallerOpen} =
        opened(World, macula_node_keys:key_id(Provider), bidi),
    withhold(World, WithheldQuic),
    ?assertEqual(ok, macula_stream:send(Withheld, <<"held">>)),
    ?assertEqual(ok, macula_stream:send(Caller, <<"goes">>)),
    ?assertEqual([<<"goes">>], data_written_on(World, CallerQuic, ?EVENT_MS)),
    on_stream(Link, CallerQuic, provider_frame(#{frame_type => stream_reply, seq => 0, payload => #{count => 3}},
                                               Provider, CallerOpen)),
    ?assertEqual({ok, #{{text, <<"count">>} => 3}}, macula_stream:await_reply(Caller, ?EVENT_MS)),
    ?assert(is_process_alive(Withheld)),
    stop(Link).

%% A session whose peer withholds credit while it produces more than the link holds for it ends as a failed write
%% ends it, and the session beside it on the same link carries on: the queue's cap costs one slow session its own
%% stream, never another's.
a_session_over_the_queue_cap_ends_alone() ->
    #{link := Link} = World = linked(),
    #{stream := Over, quic := OverQuic} = opened(World, target(), bidi),
    #{stream := Other, quic := OtherQuic} = opened(World, target(), bidi),
    withhold(World, OverQuic),
    with_out_queue_cap(1,
                       fun() ->
                           ?assertEqual(ok, macula_stream:send(Over, <<"held, the stream is empty">>)),
                           ?assertEqual(ok, macula_stream:send(Over, <<"one frame past the cap">>)),
                           ?assertMatch({error, {transport, {outbound_queue_full, _}}},
                                        macula_stream:recv(Over, ?EVENT_MS))
                       end),
    ?assertEqual(ok, macula_stream:send(Other, <<"carries on">>)),
    ?assertEqual([<<"carries on">>], data_written_on(World, OtherQuic, ?EVENT_MS)),
    stop(Link).

%% A session's last frame is held like any other: it goes out when the peer takes data, and only then does the link
%% close the stream and forget the session. The link is handed the frame directly, because the contract is the link's:
%% a held last frame must not close the stream under the bytes still queued for it.
a_withheld_last_frame_ends_the_session_once_it_is_written() ->
    #{link := Link} = World = linked(),
    #{quic := Quic, open := Open} = opened(World, target(), bidi),
    withhold(World, Quic),
    ok = macula_station_link:send_stream_bytes(Link, maps:get(request_id, Open), <<"the last frame">>, true),
    ?assertEqual(none, closed_on(Quic, 200)),
    grant(World, Quic),
    Link ! {quic, send_ready, Quic, undefined},
    ?assertEqual(Quic, closed_on(Quic, ?EVENT_MS)),
    stop(Link).

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

target() ->
    macula_node_keys:key_id(key()).

%% A link that believes it is connected, with this process as its peering connection. Its writer tells this process
%% every write, and answers {error, busy} for a stream whose credit this world has withheld — what
%% macula_quic:async_send/2 answers for a stream already holding 1 MiB unwritten.
linked() ->
    {ok, _} = application:ensure_all_started(macula),
    Test = self(),
    Credit = ets:new(?MODULE, [set, public]),
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
                     send_on_stream => fun(Stream, Bytes) ->
                                               Taken = credit(Credit, Stream),
                                               Test ! {writing, Stream, Bytes, Taken},
                                               Taken
                                       end,
                     close_stream => fun(Stream) -> Test ! {closed, Stream}, ok end}),
    unlink(Link),
    _ = sys:replace_state(Link, fun(S) -> setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Test),
                                                    <<2:256>>) end),
    #{link => Link, key => Key, credit => Credit,
      verifiers => ets:new(field, [set, public])}.

%% The stream the link closed, if it closed this one within Ms.
closed_on(Quic, Ms) ->
    receive
        {closed, Quic} -> Quic
    after Ms ->
        none
    end.

%% How much the link holds for one session whose peer withholds credit, for the length of Fun.
with_out_queue_cap(Bytes, Fun) ->
    Old = application:get_env(macula, max_stream_out_queue_bytes),
    ok = application:set_env(macula, max_stream_out_queue_bytes, Bytes),
    try Fun() after restore_out_queue_cap(Old) end.

restore_out_queue_cap(undefined) -> application:unset_env(macula, max_stream_out_queue_bytes);
restore_out_queue_cap({ok, Bytes}) -> application:set_env(macula, max_stream_out_queue_bytes, Bytes).

%% The peer takes no more data on this stream: a write there is answered busy, and queues nothing at the QUIC stream.
withhold(#{credit := Credit}, Quic) ->
    true = ets:insert(Credit, {Quic, withheld}).

%% The peer takes data again on this stream.
grant(#{credit := Credit}, Quic) ->
    true = ets:delete(Credit, Quic).

credit(Credit, Stream) ->
    case ets:member(Credit, Stream) of
        true -> {error, busy};
        false -> ok
    end.

stop(Link) ->
    catch macula_station_link:stop(Link),
    ok.

%% A session the link opens for Target: its stream process, the dedicated stream the link opened, and its STREAM_OPEN
%% as a verifier reads it. The verifier is kept for the session's stream, so the bytes it writes later read as the caller
%% frames they are.
opened(World, Target, Mode) ->
    #{link := Link, verifiers := Verifiers} = World,
    {ok, StreamPid} = macula_station_link:call_stream(Link, Target, ?REALM, ?PROCEDURE, #{n => 1},
                                                      #{seal => clear, mode => Mode}),
    Quic = receive {opened, Opened} -> Opened after ?EVENT_MS -> erlang:error(no_stream_opened) end,
    {ok, OpenFrame} = open_written(Quic, ?EVENT_MS),
    {ok, Open} = macula_frame:verify_request(OpenFrame, profile()),
    true = ets:insert(Verifiers, {Quic, macula_frame:open_stream(Open)}),
    #{stream => StreamPid, quic => Quic, open => Open}.

open_written(Quic, Ms) ->
    receive
        {writing, Quic, Bytes, ok} ->
            {ok, Frame, <<>>} = macula_frame:decode(Bytes),
            {ok, Frame}
    after Ms ->
        erlang:error(no_open_written)
    end.

%% The STREAM_DATA bodies this session wrote, in order, waiting up to Ms for the first one. The session's verifier is
%% kept in this world, so a second drain carries on from the sequence number the first one reached.
data_written_on(#{verifiers := Verifiers}, Quic, Ms) ->
    [{Quic, Verifier}] = ets:lookup(Verifiers, Quic),
    data_written_on(Quic, Ms, Verifier, [], Verifiers).

data_written_on(Quic, Ms, Verifier, Acc, Verifiers) ->
    receive
        {writing, Quic, Bytes, Taken} ->
            {Bodies, Next} = read_frame(Bytes, Taken, Verifier),
            true = ets:insert(Verifiers, {Quic, Next}),
            data_written_on(Quic, 0, Next, Acc ++ Bodies, Verifiers)
    after Ms ->
        Acc
    end.

%% Only a frame the stream took counts as written: one it answered busy for was handed over and held, and it is
%% handed over again, in order, once the peer takes data.
read_frame(_Bytes, {error, busy}, Verifier) ->
    {[], Verifier};
read_frame(Bytes, ok, Verifier) ->
    {ok, Frame, <<>>} = macula_frame:decode(Bytes),
    case macula_frame:verify_caller_stream(Frame, Verifier, profile()) of
        {ok, #{frame_type := stream_data, body := Body}, Next} -> {[Body], Next};
        {ok, _Spec, Next} -> {[], Next};
        {error, _} -> {[], Verifier}
    end.

provider_frame(Spec, Provider, Open) ->
    wire(macula_frame:provider_stream(Spec, Provider, Open)).

wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

on_stream(Link, Stream, Frame) ->
    Link ! {quic, macula_frame:encode(Frame), Stream, undefined}.
