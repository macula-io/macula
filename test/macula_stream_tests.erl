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

%% Once its session has ended, a stream's await_reply returns how it ended,
%% however late it is called: the peer's abort, the peer closing both sides,
%% or the peer ending. The peer ending afterwards does not replace it.
a_late_await_reply_gets_how_the_session_ended_test_() ->
    [{"after the peer aborts, and then ends",
      fun() -> late_await_reply(abort, {error, {<<"stop">>, <<"why">>}}) end},
     {"after the peer closes both sides, and then ends",
      fun() -> late_await_reply(close, {error, peer_closed}) end},
     {"after the peer ends without a word",
      fun() -> late_await_reply(peer_ends, {error, peer_down}) end}].

late_await_reply(Ending, How) ->
    {Stream, Peer, PeerOwner} = paired_streams(self()),
    _ = end_session_from_peer(Ending, Peer, PeerOwner),
    {told, _} = told(Stream, 1_000),
    end_owner(PeerOwner, kill),
    ?assertEqual(gone, peer_gone_within(Stream, 1_000)),
    ?assertEqual(How, macula_stream:await_reply(Stream, 100)).

%% Once its session has ended with nothing left to read, a stream's recv
%% returns eof only after a clean close. The peer's abort and the peer ending
%% read as how the session ended, so a cut-off stream never reads as complete.
a_late_recv_gets_how_the_session_ended_test_() ->
    [{"after the peer aborts, and then ends",
      fun() -> late_recv(abort, {error, {<<"stop">>, <<"why">>}}) end},
     {"after the peer closes both sides, and then ends",
      fun() -> late_recv(close, eof) end},
     {"after the peer ends without a word",
      fun() -> late_recv(peer_ends, {error, peer_down}) end}].

late_recv(Ending, How) ->
    {Stream, Peer, PeerOwner} = paired_streams(self()),
    _ = end_session_from_peer(Ending, Peer, PeerOwner),
    {told, _} = told(Stream, 1_000),
    end_owner(PeerOwner, kill),
    ?assertEqual(gone, peer_gone_within(Stream, 1_000)),
    ?assertEqual(How, macula_stream:recv(Stream, 100)).

%% A reply that arrives after the session has ended with a result takes no
%% effect: await_reply still returns how the session ended.
a_late_reply_keeps_how_the_session_ended_test_() ->
    [{"after the peer aborts",
      fun() -> reply_after_the_end(abort, {error, {<<"stop">>, <<"why">>}}) end},
     {"after the peer closes both sides",
      fun() -> reply_after_the_end(close, {error, peer_closed}) end}].

reply_after_the_end(Ending, How) ->
    {Stream, Peer, PeerOwner} = paired_streams(self()),
    _ = end_from_peer(Ending, Peer),
    {told, _} = told(Stream, 1_000),
    ok = macula_stream:deliver_reply(Stream, {ok, <<"late">>}),
    Reply = macula_stream:await_reply(Stream, 100),
    end_owner(PeerOwner, normal),
    ?assertEqual(How, Reply).

end_session_from_peer(peer_ends, _Peer, PeerOwner) ->
    end_owner(PeerOwner, kill);
end_session_from_peer(Ending, Peer, _PeerOwner) ->
    end_from_peer(Ending, Peer).

%% Waits until Stream has handled its peer's end, when it forgets its peer.
peer_gone_within(Stream, Ms) ->
    peer_gone(maps:get(peer, macula_stream:info(Stream)), Stream, Ms).

peer_gone(undefined, _Stream, _Ms) -> gone;
peer_gone(_Peer, _Stream, Ms) when Ms =< 0 -> still_paired;
peer_gone(_Peer, Stream, Ms) ->
    timer:sleep(10),
    peer_gone_within(Stream, Ms - 10).

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

%%%===================================================================
%%% A stream keeps a bounded number of bytes no reader has taken
%%%===================================================================

%% Chunks no reader has taken wait in the stream's inbox. A chunk that would
%% take the waiting bytes past the stream's bound ends the session with
%% resource_exhausted, whether it arrives as raw bytes or as a decoded term:
%% the receiving side had no room, and the peer broke no rule.
a_chunk_past_the_inbox_bound_ends_the_stream_test_() ->
    [{Name, fun() -> past_the_bound_ends_the_stream(Chunks) end}
     || {Name, Chunks} <- [{"raw chunks", [{raw, binary:copy(<<1>>, 400)} || _ <- lists:seq(1, 3)]},
                           {"a decoded term", [{msgpack, #{bytes => binary:copy(<<1>>, 1_500)}}]}]].

%% A reader that takes each chunk as it comes never meets the bound, however
%% many bytes pass.
a_reader_that_keeps_up_never_meets_the_inbox_bound_test() ->
    {Server, Client} = bounded_pair(1_000),
    [begin
         ok = macula_stream:send(Server, binary:copy(<<N>>, 400)),
         ?assertEqual({chunk, binary:copy(<<N>>, 400)}, macula_stream:recv(Client, 1_000))
     end || N <- lists:seq(1, 10)],
    ?assertEqual(not_told, told(Client, 50)).

past_the_bound_ends_the_stream(Chunks) ->
    {Server, Client} = bounded_pair(1_000),
    [ok = macula_stream:deliver_chunk(Client, Encoding, Body) || {Encoding, Body} <- Chunks],
    ?assertMatch({told, {error, {<<"resource_exhausted">>, _}}}, told(Client, 1_000)),
    ?assertMatch({told, {error, {<<"resource_exhausted">>, _}}}, told(Server, 1_000)).

%% The bound limits what the stream process holds, not only what it counts.
%% Parts of a larger binary, as a decoded frame's body is, keep none of the
%% rest of it, whether they arrive raw or inside a decoded term; empty chunks
%% and decoded terms count for the memory they take.
%% The stream holds at most a small multiple of the bound once the chunks are
%% queued or the session has ended.
the_inbox_bound_limits_what_the_stream_holds_test_() ->
    [{Name, fun() -> holds_no_more_than_the_bound(Chunks()) end}
     || {Name, Chunks} <- [{"65-byte parts of a 1 MiB binary", fun parts_of_a_large_binary/0},
                           {"empty chunks", fun empty_chunks/0},
                           {"a decoded list of small integers", fun small_integers/0},
                           {"decoded terms holding 65-byte parts of a 1 MiB binary",
                            fun decoded_parts_of_a_large_binary/0}]].

holds_no_more_than_the_bound(Chunks) ->
    Bound = 64_000,
    {_Server, Client} = bounded_pair(Bound),
    Before = held_bytes(Client),
    [ok = macula_stream:deliver_chunk(Client, Encoding, Body) || {Encoding, Body} <- Chunks],
    #{} = macula_stream:info(Client),
    Held = held_bytes(Client) - Before,
    ?assert(Held =< 4 * Bound).

parts_of_a_large_binary() ->
    Large = crypto:strong_rand_bytes(1 bsl 20),
    [{raw, binary:part(Large, N * 65, 65)} || N <- lists:seq(0, 199)].

empty_chunks() ->
    [{raw, <<>>} || _ <- lists:seq(1, 20_000)].

small_integers() ->
    [{msgpack, [N rem 256 || N <- lists:seq(1, 30_000)]}].

decoded_parts_of_a_large_binary() ->
    Large = crypto:strong_rand_bytes(1 bsl 20),
    [{msgpack, #{part => binary:part(Large, N * 65, 65)}} || N <- lists:seq(0, 199)].

%% Served streams share their caller's inbox budget: the streams of one
%% caller together keep no more unread than that budget, a chunk past it ends
%% its session with resource_exhausted, and another caller's stream still
%% takes chunks.
served_streams_of_a_caller_share_its_inbox_budget_test() ->
    {ok, _} = application:ensure_all_started(macula),
    Budget = 1_000_000,
    with_macula_env(#{max_served_inbox_bytes_per_caller => Budget}, fun() ->
        Link = spawn(fun park/0),
        [Caller, Other] = [crypto:strong_rand_bytes(32), crypto:strong_rand_bytes(32)],
        Streams = [served_stream(Link, Caller) || _ <- lists:seq(1, 4)],
        Bystander = served_stream(Link, Other),
        try
            Chunk = crypto:strong_rand_bytes(100_000),
            [ok = macula_stream:deliver_chunk(S, raw, Chunk) || S <- Streams, _ <- lists:seq(1, 5)],
            ?assert(lists:sum([inbox_bytes(S) || S <- Streams]) =< Budget),
            ?assertMatch({told, _Stream, {error, {<<"resource_exhausted">>, _}}}, any_told(1_000)),
            ok = macula_stream:deliver_chunk(Bystander, raw, Chunk),
            ?assert(inbox_bytes(Bystander) > 0)
        after
            end_served_streams([Bystander | Streams], Link)
        end
    end).

%% All served streams on the node share its inbox budget, whichever callers
%% they serve: together they keep no more unread than it, and a chunk past it
%% ends its session with resource_exhausted.
served_streams_on_the_node_share_its_inbox_budget_test() ->
    {ok, _} = application:ensure_all_started(macula),
    Room = 1_000_000,
    with_macula_env(#{max_served_inbox_bytes => macula_stream_sessions:inbox_bytes() + Room}, fun() ->
        Link = spawn(fun park/0),
        Streams = [served_stream(Link, crypto:strong_rand_bytes(32)) || _ <- lists:seq(1, 4)],
        try
            Chunk = crypto:strong_rand_bytes(100_000),
            [ok = macula_stream:deliver_chunk(S, raw, Chunk) || S <- Streams, _ <- lists:seq(1, 5)],
            ?assert(lists:sum([inbox_bytes(S) || S <- Streams]) =< Room),
            ?assertMatch({told, _Stream, {error, {<<"resource_exhausted">>, _}}}, any_told(1_000))
        after
            end_served_streams(Streams, Link)
        end
    end).

%% A chunk a reader takes gives its bytes back to the caller's budget, so the
%% stream then takes as much again.
a_read_chunk_gives_its_bytes_back_test() ->
    {ok, _} = application:ensure_all_started(macula),
    with_macula_env(#{max_served_inbox_bytes_per_caller => 250_000}, fun() ->
        Link = spawn(fun park/0),
        Stream = served_stream(Link, crypto:strong_rand_bytes(32)),
        try
            Chunk = crypto:strong_rand_bytes(100_000),
            [ok = macula_stream:deliver_chunk(Stream, raw, Chunk) || _ <- lists:seq(1, 2)],
            [{chunk, Chunk} = macula_stream:recv(Stream, 1_000) || _ <- lists:seq(1, 2)],
            [ok = macula_stream:deliver_chunk(Stream, raw, Chunk) || _ <- lists:seq(1, 2)],
            ?assert(inbox_bytes(Stream) > 0),
            ?assertEqual(not_told, any_told(100))
        after
            end_served_streams([Stream], Link)
        end
    end).

%% A served bidi stream carried by Link, as a link starts one: with the provider's node identity key, the verified
%% STREAM_OPEN it serves, Link as its connection and the pq_pure profile. It is admitted as a session of Caller and
%% owned by the test process.
served_stream(Link, Caller) ->
    Provider = stream_node_key(),
    OpenSpec = #{request_id => crypto:strong_rand_bytes(16), realm => <<0:256>>, procedure => <<"acme/count_v1">>,
                 target => macula_node_keys:key_id(Provider), deadline => erlang:system_time(millisecond) + 60_000,
                 payload => #{}, mode => bidi},
    {ok, Open} = macula_frame:verify_request(as_received(macula_frame:stream_open(OpenSpec, stream_node_key())),
                                             pq_pure),
    {ok, Stream} = macula_stream:start_link(#{id => crypto:strong_rand_bytes(16), role => server, mode => bidi,
                                              owner => self(), key => Provider, open => Open, conn => Link,
                                              profile => pq_pure}),
    ok = macula_stream:attach_to_link(Stream, Link, crypto:strong_rand_bytes(16)),
    ok = macula_stream_sessions:admit(Caller, Stream),
    Stream.

stream_node_key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.

%% A frame as a peer's bytes arrive: encoded, then decoded.
as_received(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

inbox_bytes(Stream) ->
    maps:get(inbox_bytes, macula_stream:info(Stream)).

%% Ends the streams and their link without taking the test process with them,
%% and drops their notices.
end_served_streams(Streams, Link) ->
    [end_served_stream(S) || S <- Streams],
    exit(Link, kill),
    drop_notices().

end_served_stream(Stream) ->
    true = unlink(Stream),
    exit(Stream, kill).

drop_notices() ->
    receive
        {macula_stream, ended, _Stream, _How} -> drop_notices()
    after 0 ->
        ok
    end.

any_told(Ms) ->
    receive
        {macula_stream, ended, Stream, How} -> {told, Stream, How}
    after Ms ->
        not_told
    end.

with_macula_env(Env, Test) ->
    Old = [{Key, application:get_env(macula, Key)} || Key <- maps:keys(Env)],
    [ok = application:set_env(macula, Key, Value) || {Key, Value} <- maps:to_list(Env)],
    try Test() after [restore_macula_env(Key, Was) || {Key, Was} <- Old] end.

restore_macula_env(Key, undefined) -> application:unset_env(macula, Key);
restore_macula_env(Key, {ok, Value}) -> application:set_env(macula, Key, Value).

%% The bytes a process holds after a collection: its heap, and each binary it
%% references off the heap once, at the size of the whole binary.
held_bytes(Pid) ->
    true = erlang:garbage_collect(Pid),
    [{total_heap_size, Words}, {binary, Binaries}] =
        erlang:process_info(Pid, [total_heap_size, binary]),
    Referenced = lists:usort([{Address, Size} || {Address, Size, _Refs} <- Binaries]),
    Words * erlang:system_info(wordsize) + lists:sum([Size || {_Address, Size} <- Referenced]).

%% A server_stream pair, both owned by the test process, whose client stream
%% keeps at most Bytes no reader has taken.
bounded_pair(Bytes) ->
    Id = crypto:strong_rand_bytes(16),
    {ok, Client} = macula_stream:start_link(#{id => Id, role => client, mode => server_stream,
                                              owner => self(), max_inbox_bytes => Bytes}),
    {ok, Server} = macula_stream:start_link(#{id => Id, role => server, mode => server_stream,
                                              owner => self()}),
    ok = macula_stream:pair(Client, Server),
    {Server, Client}.

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
