%%%-------------------------------------------------------------------
%%% @doc EUnit tests for Phase 2 of `PLAN_MACULA_STREAMING.md` —
%%% streaming RPC wired through `macula_mesh_client` and QUIC.
%%%
%%% Strategy: bring up a real `macula_mesh_client' gen_server but
%%% mock `macula_quic' with `meck` so nothing touches the network.
%%% Captured frames are collected in an ETS table (so meck's capture
%%% callback runs in a different process from the test evaluator) and
%%% synthetic incoming frames are injected via the same `{quic, ...}'
%%% message the real receive loop consumes.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_remote_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CAPTURE_TAB, macula_stream_remote_tests_capture).

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    reset_capture(),
    ok = meck:new(macula_quic, [passthrough, no_link]),
    ok = meck:expect(macula_quic, async_send,
        fun(_Stream, Binary) ->
            capture(Binary),
            ok
        end),
    FakeConn = make_ref(),
    FakeStream = make_ref(),
    ok = meck:expect(macula_quic, connect,
        fun(_Host, _Port, _Opts, _Timeout) -> {ok, FakeConn} end),
    ok = meck:expect(macula_quic, open_stream,
        fun(_Conn) -> {ok, FakeStream} end),
    ok = meck:expect(macula_quic, setopt, fun(_, _, _) -> ok end),
    ok = meck:expect(macula_quic, close, fun(_) -> ok end),
    ClientOpts = #{
        relays => [<<"https://fake.test:4433">>],
        realm => <<"test.realm">>,
        identity => <<"test-node">>,
        tls_verify => none
    },
    {ok, Client} = macula_mesh_client:start_link(ClientOpts),
    wait_connected(Client, 40),
    %% Discard the CONNECT frame mesh_client sends during handshake so
    %% tests only see frames they triggered.
    drain_captured(),
    #{client => Client, stream => FakeStream}.

teardown(#{client := Client}) ->
    catch macula_mesh_client:stop(Client),
    catch meck:unload(macula_quic),
    catch ets:delete(?CAPTURE_TAB),
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

call_stream_emits_stream_open_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(#{client := Client}) ->
        [{"call_stream/4 emits STREAM_OPEN with right fields",
          fun() ->
            {ok, _StreamPid} = macula_mesh_client:call_stream(
                Client, <<"t.remote.count">>,
                #{<<"n">> => 3}, #{}),
            Frames = wait_for_frames(stream_open, 1, 500),
            ?assertMatch([_], Frames),
            [Open] = Frames,
            ?assertEqual(<<"t.remote.count">>, msg_get(procedure, Open)),
            Mode = msg_get(mode, Open),
            ?assert(Mode =:= server_stream orelse Mode =:= <<"server_stream">>),
            Sid = msg_get(stream_id, Open),
            ?assert(is_binary(Sid)),
            ?assertEqual(16, byte_size(Sid))
          end}]
     end}.

stream_data_frame_routes_to_stream_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(#{client := Client, stream := QStream}) ->
        [{"incoming STREAM_DATA is delivered to the matching stream pid",
          fun() ->
            {ok, StreamPid} = macula_mesh_client:call_stream(
                Client, <<"t.remote.data">>, #{}, #{}),
            [Open] = wait_for_frames(stream_open, 1, 500),
            Sid = msg_get(stream_id, Open),
            DataBin = encode_frame(stream_data, #{
                stream_id => Sid,
                seq => 0,
                body => <<"hello">>,
                encoding => raw
            }),
            Client ! {quic, DataBin, QStream, []},
            ?assertEqual({chunk, <<"hello">>},
                         macula_stream_v1:recv(StreamPid, 1000))
          end}]
     end}.

stream_reply_settles_await_reply_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(#{client := Client, stream := QStream}) ->
        [{"incoming STREAM_REPLY settles await_reply",
          fun() ->
            {ok, StreamPid} = macula_mesh_client:open_stream(
                Client, <<"t.remote.reply">>,
                #{}, #{mode => client_stream}),
            [Open] = wait_for_frames(stream_open, 1, 500),
            Sid = msg_get(stream_id, Open),
            ReplyBin = encode_frame(stream_reply, #{
                stream_id => Sid,
                result => <<"done">>
            }),
            Client ! {quic, ReplyBin, QStream, []},
            ?assertEqual({ok, <<"done">>},
                         macula_stream_v1:await_reply(StreamPid, 1000))
          end}]
     end}.

stream_error_propagates_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(#{client := Client, stream := QStream}) ->
        [{"incoming STREAM_ERROR surfaces on recv",
          fun() ->
            {ok, StreamPid} = macula_mesh_client:call_stream(
                Client, <<"t.remote.err">>, #{}, #{}),
            [Open] = wait_for_frames(stream_open, 1, 500),
            Sid = msg_get(stream_id, Open),
            ErrBin = encode_frame(stream_error, #{
                stream_id => Sid,
                code => <<"bad">>,
                message => <<"no good">>
            }),
            Client ! {quic, ErrBin, QStream, []},
            ?assertEqual({error, {<<"bad">>, <<"no good">>}},
                         macula_stream_v1:recv(StreamPid, 1000))
          end}]
     end}.

advertise_stream_registers_procedure_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(#{client := Client}) ->
        [{"advertise_stream emits register_procedure frame upstream",
          fun() ->
            Handler = fun(_S, _A) -> ok end,
            ok = macula_mesh_client:advertise_stream(
                Client, <<"t.remote.adv">>, server_stream, Handler),
            [Reg] = wait_for_frames(register_procedure, 1, 500),
            ?assertEqual(<<"t.remote.adv">>, msg_get(procedure, Reg))
          end}]
     end}.

incoming_stream_open_dispatches_handler_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(#{client := Client, stream := QStream}) ->
        [{"STREAM_OPEN for advertised procedure invokes the handler",
          fun() ->
            Parent = self(),
            Handler = fun(Stream, Args) ->
                Parent ! {handler_ran, Args},
                ok = macula_stream_v1:send(Stream, <<"ack">>),
                macula_stream_v1:close(Stream)
            end,
            ok = macula_mesh_client:advertise_stream(
                Client, <<"t.serve.echo">>, server_stream, Handler),
            %% Ignore the upstream register_procedure frame for this test.
            _ = wait_for_frames(register_procedure, 1, 500),
            drain_captured(),
            Sid = crypto:strong_rand_bytes(16),
            OpenBin = encode_frame(stream_open, #{
                stream_id => Sid,
                procedure => <<"t.serve.echo">>,
                mode => server_stream,
                args => #{<<"x">> => 1}
            }),
            Client ! {quic, OpenBin, QStream, []},
            receive
                {handler_ran, Args} ->
                    ?assertEqual(#{<<"x">> => 1}, Args)
            after 1000 ->
                erlang:error(handler_did_not_run)
            end,
            DataFrames = wait_for_frames(stream_data, 1, 1000),
            ?assert(length(DataFrames) >= 1),
            [D | _] = DataFrames,
            ?assertEqual(Sid, msg_get(stream_id, D)),
            ?assertEqual(<<"ack">>, msg_get(body, D))
          end}]
     end}.

stream_open_for_unknown_procedure_replies_error_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(#{client := Client, stream := QStream}) ->
        [{"unknown procedure ⇒ STREAM_ERROR not_found",
          fun() ->
            drain_captured(),
            Sid = crypto:strong_rand_bytes(16),
            OpenBin = encode_frame(stream_open, #{
                stream_id => Sid,
                procedure => <<"t.nope">>,
                mode => server_stream,
                args => <<>>
            }),
            Client ! {quic, OpenBin, QStream, []},
            ErrFrames = wait_for_frames(stream_error, 1, 500),
            ?assertMatch([_], ErrFrames),
            [E] = ErrFrames,
            ?assertEqual(Sid, msg_get(stream_id, E)),
            ?assertEqual(<<"not_found">>, msg_get(code, E))
          end}]
     end}.

disconnect_aborts_open_streams_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(#{client := Client, stream := QStream}) ->
        [{"connection teardown aborts every open stream",
          fun() ->
            {ok, StreamPid} = macula_mesh_client:call_stream(
                Client, <<"t.remote.tear">>, #{}, #{}),
            _ = wait_for_frames(stream_open, 1, 500),
            Client ! {quic, closed, QStream, test_teardown},
            ?assertMatch({error, {<<"disconnected">>, _}},
                         macula_stream_v1:recv(StreamPid, 1000))
          end}]
     end}.

%%%===================================================================
%%% Helpers
%%%===================================================================

wait_connected(_Client, 0) -> ok;
wait_connected(Client, N) ->
    case (catch macula_mesh_client:is_connected(Client)) of
        true -> ok;
        _ ->
            timer:sleep(25),
            wait_connected(Client, N - 1)
    end.

reset_capture() ->
    catch ets:delete(?CAPTURE_TAB),
    ets:new(?CAPTURE_TAB, [public, named_table, ordered_set]).

capture(Binary) ->
    %% Duplicate-key safe: use a monotonic counter.
    Seq = ets:update_counter(?CAPTURE_TAB, '$seq', {2, 1}, {'$seq', 0}),
    ets:insert(?CAPTURE_TAB, {{frame, Seq}, Binary}),
    ok.

drain_captured() ->
    catch ets:delete(?CAPTURE_TAB),
    ets:new(?CAPTURE_TAB, [public, named_table, ordered_set]),
    ok.

all_captured() ->
    lists:sort(
      [{Seq, Bin}
       || {{frame, Seq}, Bin} <- ets:tab2list(?CAPTURE_TAB)]).

decode_sent(Binary) ->
    {ok, {Type, Msg}} = macula_protocol_decoder:decode(Binary),
    {Type, Msg}.

encode_frame(Type, Msg) ->
    macula_protocol_encoder:encode(Type, Msg).

wait_for_frames(Type, MinCount, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    wait_loop(Type, MinCount, Deadline).

wait_loop(Type, MinCount, Deadline) ->
    Frames = [Msg || {T, Msg} <- decode_all(all_captured()), T =:= Type],
    case length(Frames) >= MinCount of
        true -> Frames;
        false ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true -> Frames;
                false ->
                    timer:sleep(25),
                    wait_loop(Type, MinCount, Deadline)
            end
    end.

decode_all(List) ->
    [decode_sent(Bin) || {_Seq, Bin} <- List].

%% Messages come back with binary keys (msgpack decoder) but we write
%% them with atom keys; accept both.
msg_get(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error ->
            BinKey = atom_to_binary(Key, utf8),
            maps:get(BinKey, Map, undefined)
    end.
