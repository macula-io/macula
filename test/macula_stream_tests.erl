%%%-------------------------------------------------------------------
%%% @doc EUnit tests for the streaming RPC SDK (Phase 1, local
%%% dispatch). See PLAN_MACULA_STREAMING.md.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_tests).

-include_lib("eunit/include/eunit.hrl").

%% Data a reason carries that must stay on this node.
-define(MARKER, <<"marker-3f9c-stays-on-this-node">>).
%% Well above what the log gets of a reason, far below a whole large one.
-define(LOGGED_BYTES, 8192).

%%%===================================================================
%%% Test fixtures
%%%===================================================================

setup() ->
    %% macula_root supervises macula_stream_local; start the app so
    %% it's available. application:ensure_all_started/1 is idempotent.
    {ok, _} = application:ensure_all_started(macula),
    %% Wipe any leftover advertisements from prior tests
    [macula:unadvertise_stream(P)
     || {P, _} <- macula_stream_local:list_advertised()],
    ok.

teardown(_) ->
    [macula:unadvertise_stream(P)
     || {P, _} <- macula_stream_local:list_advertised()],
    ok.

with_setup(Tests) ->
    {setup, fun setup/0, fun teardown/1, Tests}.

%%%===================================================================
%%% A stream ends with its owner, who may hand it over
%%%===================================================================

%% A stream ends when its owner ends, however the owner ends.
a_stream_ends_with_its_owner_test_() ->
    [{Name, fun() -> ends_with_its_owner(Exit) end}
     || {Name, Exit} <- [{"the owner ends normally", normal}, {"the owner is killed", kill}]].

ends_with_its_owner(Exit) ->
    Owner = spawn(fun park/0),
    Stream = unpaired_stream(Owner),
    Ref = monitor(process, Stream),
    end_owner(Owner, Exit),
    ?assertMatch({down, _}, down_within(Ref, 1_000)).

%% The owner hands its stream to another process: the stream outlives the
%% first owner and ends with the second, whether that one ends normally or
%% is killed.
a_handed_over_stream_ends_with_its_new_owner_test_() ->
    [{Name, fun() -> handed_over_stream_ends_with(Exit) end}
     || {Name, Exit} <- [{"the new owner ends normally", normal}, {"the new owner is killed", kill}]].

handed_over_stream_ends_with(Exit) ->
    First = spawn(fun park/0),
    Second = spawn(fun park/0),
    Stream = unpaired_stream(First),
    Ref = monitor(process, Stream),
    ?assertEqual(ok, in_process(First, fun() -> macula_stream:controlling_process(Stream, Second) end)),
    end_owner(First, normal),
    ?assertEqual(alive, down_within(Ref, 200)),
    end_owner(Second, Exit),
    ?assertMatch({down, _}, down_within(Ref, 1_000)).

%% Only a stream's owner hands it over; the stream stays with its owner.
only_the_owner_hands_a_stream_over_test() ->
    Owner = spawn(fun park/0),
    Stream = unpaired_stream(Owner),
    ?assertEqual({error, not_owner}, macula_stream:controlling_process(Stream, self())),
    Ref = monitor(process, Stream),
    end_owner(Owner, normal),
    ?assertMatch({down, _}, down_within(Ref, 1_000)).

%% When its session ends, a stream tells its owner once how it ended, and
%% stays until the owner ends. An owner the stream is handed to after that
%% is told as well.
a_stream_tells_its_owner_once_when_its_session_ends_test_() ->
    [{"the peer closes both sides", fun() -> told_once_when(close, closed) end},
     {"the peer aborts", fun() -> told_once_when(abort, {error, {<<"stop">>, <<"why">>}}) end},
     {"handed over after its session ended", fun told_after_it_is_handed_over/0}].

told_once_when(Ending, How) ->
    {Stream, Peer, PeerOwner} = paired_streams(self()),
    ok = end_from_peer(Ending, Peer),
    ?assertEqual({told, How}, told(Stream, 1_000)),
    ok = macula_stream:close(Stream),
    ?assertEqual(not_told, told(Stream, 200)),
    ?assert(is_process_alive(Stream)),
    end_owner(PeerOwner, normal).

told_after_it_is_handed_over() ->
    {Stream, Peer, PeerOwner} = paired_streams(self()),
    ok = end_from_peer(close, Peer),
    {told, closed} = told(Stream, 1_000),
    Test = self(),
    NewOwner = spawn(fun() -> forward_notice(Test) end),
    ?assertEqual(ok, macula_stream:controlling_process(Stream, NewOwner)),
    ?assertEqual({forwarded, Stream, closed}, forwarded(1_000)),
    end_owner(PeerOwner, normal).

%% A local stream call leaves no process behind once its caller and its
%% handler are both done.
local_sessions_leave_no_process_behind_test_() ->
    with_setup([
        {"three local calls, each read to the end by a caller that then ends",
         fun() ->
             ok = macula:advertise_stream(<<"t.sessions">>, server_stream, fun close_serving/2),
             Before = macula_test_sessions:serving(),
             [ok = local_call_read_to_the_end(<<"t.sessions">>) || _ <- lists:seq(1, 3)],
             ?assertEqual([], macula_test_sessions:await_none_new(Before))
         end}
    ]).

%% The server side of a local stream call ends with its handler, even while
%% the caller that opened the call lives on.
a_local_server_stream_ends_with_its_handler_test_() ->
    with_setup([
        {"the caller lives on after reading to the end",
         fun() ->
             Test = self(),
             ok = macula:advertise_stream(<<"t.handler_owned">>, server_stream,
                                          fun(Stream, Args) -> close_telling(Test, Stream, Args) end),
             {ok, Client} = macula:call_stream(<<"t.handler_owned">>, #{}),
             Server = receive
                          {serving, Stream} -> Stream
                      after 1_000 ->
                          erlang:error(not_served)
                      end,
             [] = drain(Client, []),
             Ref = monitor(process, Server),
             ?assertMatch({down, _}, down_within(Ref, 1_000))
         end}
    ]).

%% A handler process waits to be handed the stream it serves. When the process
%% that spawned it ends first, it ends too, without running the handler.
a_handler_process_ends_with_its_spawner_before_serving_test_() ->
    [{Name, fun() -> ends_with_its_spawner_before_serving(Spawn) end}
     || {Name, Spawn} <- handler_spawners()].

%% Once a handler process has its stream, the end of the process that spawned
%% it leaves nothing in the handler's mailbox.
a_serving_handler_holds_no_notice_of_its_spawner_test_() ->
    [{Name, fun() -> holds_no_notice_of_its_spawner(Spawn) end}
     || {Name, Spawn} <- handler_spawners()].

%% The two places that spawn a process for a handler before its stream exists.
handler_spawners() ->
    [{"a station link", fun macula_station_link:spawn_stream_handler/3},
     {"a local call", fun macula_stream_local:spawn_handler/3}].

ends_with_its_spawner_before_serving(Spawn) ->
    Test = self(),
    Spawner = spawn(fun() ->
                        Test ! {handler, Spawn(telling_ran(Test), #{}, <<"t.unserved">>)},
                        park()
                    end),
    Handler = receive {handler, Pid} -> Pid after 1_000 -> erlang:error(no_handler) end,
    Ref = monitor(process, Handler),
    exit(Spawner, kill),
    ?assertMatch({down, _}, down_within(Ref, 1_000)),
    ?assertEqual(not_ran, receive ran -> ran after 0 -> not_ran end).

holds_no_notice_of_its_spawner(Spawn) ->
    Test = self(),
    {Spawner, SpawnerRef} =
        spawn_monitor(fun() ->
                          H = Spawn(reporting_mailbox(Test), #{}, <<"t.served">>),
                          H ! {serve, self()},
                          park()
                      end),
    Serving = receive {serving, Pid} -> Pid after 1_000 -> erlang:error(not_serving) end,
    exit(Spawner, kill),
    ?assertMatch({down, _}, down_within(SpawnerRef, 1_000)),
    ?assertEqual({messages, []}, mailbox_of(Serving)).

telling_ran(Test) ->
    fun(_Stream, _Args) -> Test ! ran end.

reporting_mailbox(Test) ->
    fun(_Stream, _Args) ->
        Test ! {serving, self()},
        receive
            report_mailbox -> Test ! {mailbox, erlang:process_info(self(), messages)}
        end
    end.

%% What is left in a serving handler's mailbox, after a moment for a notice of
%% its spawner's end to arrive.
mailbox_of(Handler) ->
    timer:sleep(20),
    Handler ! report_mailbox,
    receive
        {mailbox, Messages} -> Messages
    after 1_000 ->
        erlang:error(no_mailbox)
    end.

close_telling(Test, Stream, _Args) ->
    Test ! {serving, Stream},
    macula:close_stream(Stream).

close_serving(Stream, _Args) ->
    macula:close_stream(Stream).

local_call_read_to_the_end(Procedure) ->
    {Caller, Ref} = spawn_monitor(fun() -> read_local_call(Procedure) end),
    receive
        {'DOWN', Ref, process, Caller, normal} -> ok
    after 2_000 ->
        erlang:error(caller_did_not_end)
    end.

read_local_call(Procedure) ->
    {ok, Stream} = macula:call_stream(Procedure, #{}),
    [] = drain(Stream, []).

%%%===================================================================
%%% A stream takes only the chunks its mode lets the peer send
%%%===================================================================

%% In server_stream only the server sends chunks, and in client_stream only
%% the client does. A chunk from the side the mode keeps silent, as a peer on
%% another stack could send it, ends the stream that gets it: its owner is
%% told with a stream protocol error, the peer is told the same, and nothing
%% is queued.
a_chunk_the_mode_forbids_ends_the_stream_test_() ->
    [{Name, fun() -> forbidden_chunk_ends_the_stream(Mode, Silent) end}
     || {Name, Mode, Silent} <- [{"a server_stream caller's chunk arrives", server_stream, client},
                                 {"a client_stream provider's chunk arrives", client_stream, server}]].

%% The side a mode keeps silent cannot send: its send is refused, and its
%% peer gets nothing and is not told.
a_send_the_mode_forbids_is_refused_test_() ->
    [{Name, fun() -> forbidden_send_is_refused(Mode, Silent) end}
     || {Name, Mode, Silent} <- [{"a server_stream caller sends", server_stream, client},
                                 {"a client_stream provider sends", client_stream, server}]].

forbidden_send_is_refused(Mode, SilentRole) ->
    {Silent, Peer} = mode_pair(Mode, SilentRole),
    ?assertEqual({error, {send_not_allowed, Mode}}, macula_stream:send(Silent, <<"not yours to send">>)),
    ?assertEqual(not_told, told(Peer, 50)),
    ?assertEqual(0, maps:get(inbox_size, macula_stream:info(Peer))).

%% The sides a mode lets send still reach their peer, and nothing ends.
a_chunk_the_mode_allows_is_received_test_() ->
    [{Name, fun() -> allowed_chunk_is_received(Mode, Sender) end}
     || {Name, Mode, Sender} <- [{"a server_stream provider sends", server_stream, server},
                                 {"a client_stream caller sends", client_stream, client},
                                 {"a bidi caller sends", bidi, client},
                                 {"a bidi provider sends", bidi, server}]].

forbidden_chunk_ends_the_stream(Mode, SilentRole) ->
    {Silent, Receiver} = mode_pair(Mode, SilentRole),
    ok = macula_stream:deliver_chunk(Receiver, raw, <<"not yours to send">>),
    ?assertMatch({told, {error, {<<"stream_protocol_error">>, _}}}, told(Receiver, 1_000)),
    ?assertMatch({told, {error, {<<"stream_protocol_error">>, _}}}, told(Silent, 1_000)),
    ?assertEqual(0, maps:get(inbox_size, macula_stream:info(Receiver))).

allowed_chunk_is_received(Mode, SenderRole) ->
    {Sender, Receiver} = mode_pair(Mode, SenderRole),
    ok = macula_stream:send(Sender, <<"yours to send">>),
    ?assertEqual({chunk, <<"yours to send">>}, macula_stream:recv(Receiver, 1_000)),
    ?assertEqual(not_told, told(Receiver, 50)).

%% A locally paired client and server stream in Mode, both owned by the test
%% process, with the stream of SenderRole first.
mode_pair(Mode, SenderRole) ->
    Id = crypto:strong_rand_bytes(16),
    {ok, Client} = macula_stream:start_link(#{id => Id, role => client, mode => Mode, owner => self()}),
    {ok, Server} = macula_stream:start_link(#{id => Id, role => server, mode => Mode, owner => self()}),
    ok = macula_stream:pair(Client, Server),
    sender_first(SenderRole, Client, Server).

sender_first(client, Client, Server) -> {Client, Server};
sender_first(server, Client, Server) -> {Server, Client}.

%% A process that runs what it is sent, and ends when told.
park() ->
    receive
        {run, From, Fun} -> From ! {ran, Fun()}, park();
        {exit, Reason} -> exit(Reason)
    end.

in_process(Pid, Fun) ->
    Pid ! {run, self(), Fun},
    receive
        {ran, Result} -> Result
    after 1_000 ->
        erlang:error(did_not_run)
    end.

end_owner(Owner, kill) ->
    exit(Owner, kill);
end_owner(Owner, normal) ->
    Owner ! {exit, normal}.

unpaired_stream(Owner) ->
    {ok, Stream} = macula_stream:start_link(#{id => crypto:strong_rand_bytes(16), role => server,
                                              mode => server_stream, owner => Owner}),
    Stream.

%% A stream owned by Owner, paired with a peer stream owned by a parked
%% process.
paired_streams(Owner) ->
    Id = crypto:strong_rand_bytes(16),
    PeerOwner = spawn(fun park/0),
    {ok, Stream} = macula_stream:start_link(#{id => Id, role => client, mode => bidi, owner => Owner}),
    {ok, Peer} = macula_stream:start_link(#{id => Id, role => server, mode => bidi, owner => PeerOwner}),
    ok = macula_stream:pair(Stream, Peer),
    {Stream, Peer, PeerOwner}.

end_from_peer(close, Peer) ->
    macula_stream:close(Peer);
end_from_peer(abort, Peer) ->
    macula_stream:abort(Peer, <<"stop">>, <<"why">>).

down_within(Ref, Ms) ->
    receive
        {'DOWN', Ref, process, _Pid, Reason} -> {down, Reason}
    after Ms ->
        alive
    end.

told(Stream, Ms) ->
    receive
        {macula_stream, ended, Stream, How} -> {told, How}
    after Ms ->
        not_told
    end.

forward_notice(Test) ->
    receive
        {macula_stream, ended, Stream, How} -> Test ! {forwarded, Stream, How}
    end.

forwarded(Ms) ->
    receive
        {forwarded, Stream, How} -> {forwarded, Stream, How}
    after Ms ->
        not_forwarded
    end.

%%%===================================================================
%%% Server-stream
%%%===================================================================

server_stream_test_() ->
    with_setup([
        {"chunk count handler streams 5 binaries then eof",
         fun() ->
             ok = macula:advertise_stream(<<"t.count">>, server_stream,
                  fun(Stream, #{n := N}) ->
                      [ok = macula:send(Stream, integer_to_binary(I))
                       || I <- lists:seq(1, N)],
                      macula:close_stream(Stream)
                  end),
             {ok, S} = macula:call_stream(<<"t.count">>, #{n => 5}),
             Got = drain(S, []),
             ?assertEqual([<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>], Got)
         end},
        {"unadvertised procedure returns not_advertised",
         fun() ->
             ?assertEqual({error, not_advertised},
                          macula:call_stream(<<"t.absent">>, #{}))
         end},
        {"msgpack-encoded chunks decode as {data, Term}",
         fun() ->
             ok = macula:advertise_stream(<<"t.terms">>, server_stream,
                  fun(Stream, _Args) ->
                      ok = macula:send(Stream, #{a => 1}, msgpack),
                      ok = macula:send(Stream, #{a => 2}, msgpack),
                      macula:close_stream(Stream)
                  end),
             {ok, S} = macula:call_stream(<<"t.terms">>, #{}),
             ?assertEqual({data, #{a => 1}}, macula:recv(S)),
             ?assertEqual({data, #{a => 2}}, macula:recv(S)),
             ?assertEqual(eof, macula:recv(S))
         end}
    ]).

%%%===================================================================
%%% Client-stream
%%%===================================================================

client_stream_test_() ->
    with_setup([
        {"client streams 3 chunks; server replies with their concatenation",
         fun() ->
             ok = macula:advertise_stream(<<"t.concat">>, client_stream,
                  fun(Stream, _Args) ->
                      Acc = collect(Stream, <<>>),
                      macula:set_reply(Stream, Acc)
                  end),
             {ok, S} = macula:open_stream(<<"t.concat">>, #{}, #{mode => client_stream}),
             ok = macula:send(S, <<"a">>),
             ok = macula:send(S, <<"bb">>),
             ok = macula:send(S, <<"ccc">>),
             ok = macula:close_send(S),
             ?assertEqual({ok, <<"abbccc">>}, macula:await_reply(S, 1000))
         end}
    ]).

%%%===================================================================
%%% Bidi
%%%===================================================================

bidi_test_() ->
    with_setup([
        {"bidi echo: each ping returns a pong; close ends the loop",
         fun() ->
             ok = macula:advertise_stream(<<"t.echo">>, bidi,
                  fun(Stream, _Args) ->
                      bidi_echo_loop(Stream),
                      macula:set_reply(Stream, done)
                  end),
             {ok, S} = macula:open_stream(<<"t.echo">>, #{}, #{mode => bidi}),
             ok = macula:send(S, <<"ping1">>),
             ?assertEqual({chunk, <<"pong:ping1">>}, macula:recv(S, 1000)),
             ok = macula:send(S, <<"ping2">>),
             ?assertEqual({chunk, <<"pong:ping2">>}, macula:recv(S, 1000)),
             ok = macula:close_send(S),
             ?assertEqual({ok, done}, macula:await_reply(S, 1000))
         end}
    ]).

%%%===================================================================
%%% Error handling
%%%===================================================================

error_test_() ->
    with_setup([
        {"crashing handler aborts the stream and surfaces error to recv",
         fun() ->
             ok = macula:advertise_stream(<<"t.boom">>, server_stream,
                  fun(_Stream, _Args) ->
                      erlang:error({boom, "intentional"})
                  end),
             {ok, S} = macula:call_stream(<<"t.boom">>, #{}),
             Result = macula:recv(S, 1000),
             %% recv returns {error, {Code, Message}} after the abort frame
             ?assertMatch({error, {<<"error">>, _}}, Result)
         end},
        {"a crashing handler's caller gets the reason's name; the log gets the crash",
         fun() ->
             Log = macula_test_log:capture(),
             try
                 ok = macula:advertise_stream(<<"t.boom_with_data">>, server_stream,
                      fun(_Stream, _Args) ->
                          erlang:error({boom, lists:duplicate(10000, ?MARKER)})
                      end),
                 {ok, S} = macula:call_stream(<<"t.boom_with_data">>, #{}),
                 ?assertEqual({error, {<<"error">>, <<"boom">>}}, macula:recv(S, 1000)),
                 Logged = macula_test_log:wait_text(<<"t.boom_with_data">>, 1000),
                 ?assert(byte_size(Logged) < ?LOGGED_BYTES)
             after
                 macula_test_log:release(Log)
             end
         end},
        {"explicit abort propagates to await_reply",
         fun() ->
             ok = macula:advertise_stream(<<"t.kill">>, bidi,
                  fun(Stream, _Args) ->
                      macula:abort(Stream, <<"forbidden">>, <<"nope">>)
                  end),
             {ok, S} = macula:open_stream(<<"t.kill">>, #{}, #{mode => bidi}),
             ?assertMatch({error, {<<"forbidden">>, <<"nope">>}},
                          macula:await_reply(S, 1000))
         end},
        {"recv timeout returns {error, timeout}",
         fun() ->
             ok = macula:advertise_stream(<<"t.silent">>, server_stream,
                  fun(Stream, _Args) ->
                      timer:sleep(500),
                      macula:close_stream(Stream)
                  end),
             {ok, S} = macula:call_stream(<<"t.silent">>, #{}),
             ?assertEqual({error, timeout}, macula:recv(S, 100))
         end}
    ]).

%%%===================================================================
%%% Protocol layer
%%%===================================================================

protocol_roundtrip_test_() ->
    [
        {"stream_open frame encodes/decodes",
         fun() ->
             Msg = #{
                 stream_id => <<0:128>>,
                 procedure => <<"x.y.z">>,
                 mode => server_stream,
                 args => <<>>
             },
             Bin = macula_protocol_encoder:encode(stream_open, Msg),
             ?assertMatch({ok, {stream_open, _}},
                          macula_protocol_decoder:decode(Bin))
         end},
        {"stream_data frame encodes/decodes",
         fun() ->
             Msg = #{
                 stream_id => <<1:128>>,
                 seq => 7,
                 body => <<"hello">>,
                 encoding => raw
             },
             Bin = macula_protocol_encoder:encode(stream_data, Msg),
             {ok, {stream_data, Decoded}} =
                 macula_protocol_decoder:decode(Bin),
             ?assertEqual(7, maps:get(<<"seq">>, Decoded)),
             ?assertEqual(<<"hello">>, maps:get(<<"body">>, Decoded))
         end},
        {"stream_end frame encodes/decodes",
         fun() ->
             Msg = #{stream_id => <<2:128>>, role => both},
             Bin = macula_protocol_encoder:encode(stream_end, Msg),
             ?assertMatch({ok, {stream_end, _}},
                          macula_protocol_decoder:decode(Bin))
         end},
        {"stream_error frame encodes/decodes",
         fun() ->
             Msg = #{stream_id => <<3:128>>,
                     code => <<"failure">>,
                     message => <<"why">>},
             Bin = macula_protocol_encoder:encode(stream_error, Msg),
             ?assertMatch({ok, {stream_error, _}},
                          macula_protocol_decoder:decode(Bin))
         end},
        {"stream_reply frame encodes/decodes",
         fun() ->
             Msg = #{stream_id => <<4:128>>, result => <<"ok">>},
             Bin = macula_protocol_encoder:encode(stream_reply, Msg),
             ?assertMatch({ok, {stream_reply, _}},
                          macula_protocol_decoder:decode(Bin))
         end},
        {"new type names round-trip via message_type_id/name",
         fun() ->
             [begin
                  Id = macula_protocol_types:message_type_id(T),
                  ?assertEqual({ok, T},
                               macula_protocol_types:message_type_name(Id))
              end || T <- [stream_open, stream_data, stream_end,
                           stream_error, stream_reply]]
         end}
    ].

%%%===================================================================
%%% Helpers
%%%===================================================================

drain(Stream, Acc) ->
    case macula:recv(Stream, 1000) of
        {chunk, Bin} -> drain(Stream, [Bin | Acc]);
        eof -> lists:reverse(Acc);
        Other -> erlang:error({unexpected_drain, Other})
    end.

collect(Stream, Acc) ->
    case macula:recv(Stream, 1000) of
        {chunk, Bin} -> collect(Stream, <<Acc/binary, Bin/binary>>);
        eof -> Acc;
        Other -> erlang:error({unexpected_collect, Other})
    end.

bidi_echo_loop(Stream) ->
    case macula:recv(Stream, 1000) of
        {chunk, Bin} ->
            ok = macula:send(Stream, <<"pong:", Bin/binary>>),
            bidi_echo_loop(Stream);
        eof -> ok;
        {error, _} -> ok
    end.
