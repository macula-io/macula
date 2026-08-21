%%%-------------------------------------------------------------------
%%% @doc Tests for macula_upload.
%%%
%%% Drives it exactly the way `macula_streamer' itself does: capture
%%% the handler `macula:advertise_stream/5' registers, invoke it with
%%% a stubbed stream, feed chunks via mocked `macula:recv/2' (the same
%%% shape `macula_streamer_client_stream_tests' uses). Mocks
%%% `macula_stream:set_reply'/`set_error' to observe the terminal
%%% reply this module hands back to a would-be `macula_pusher'.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_upload_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_upload).
-export([init/1, handle_uploaded/2]).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(Parent) -> {ok, Parent}.

handle_uploaded(Result, Parent) ->
    Parent ! {uploaded, Result},
    ok.

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    meck:new(macula, [passthrough]),
    meck:expect(macula, advertise_stream,
                fun(_Pool, _Realm, _Proc, Mode, Handler) ->
                    persistent_term:put({?MODULE, handler}, Handler),
                    persistent_term:put({?MODULE, advertised_mode}, Mode),
                    ok
                end),
    meck:expect(macula, publish, fun(_Pool, _Realm, _Topic, _Payload) -> ok end),
    meck:new(macula_stream, [passthrough]),
    meck:expect(macula_stream, close, fun(_Stream) -> ok end),
    meck:expect(macula_stream, abort, fun(_Stream, _Code, _Message) -> ok end),
    meck:expect(macula_stream, set_reply, fun(_Stream, _Value) -> ok end),
    meck:expect(macula_stream, set_error, fun(_Stream, _Reason) -> ok end),
    ok.

teardown(_) ->
    persistent_term:erase({?MODULE, handler}),
    persistent_term:erase({?MODULE, advertised_mode}),
    meck:unload(macula_stream),
    meck:unload(macula).

captured_handler() -> persistent_term:get({?MODULE, handler}).

stream_stub() -> receive stop -> ok end.

recv_returning(Results) ->
    Counter = atomics:new(1, []),
    meck:expect(macula, recv, fun(_Stream, _Timeout) ->
        N = atomics:add_get(Counter, 1, 1),
        lists:nth(N, Results)
    end).

%% The manifest as it would actually arrive over the wire: a plain map
%% with binary-string keys, matching `macula_manifest:from_wire/1''s
%% own "robust to binary-string keys" fallback — exercising the REAL
%% decode path, not assuming atom keys survive the wire round trip.
manifest_stream_args(Manifest) ->
    #{<<"mcid">> => maps:get(mcid, Manifest),
      <<"version">> => maps:get(version, Manifest),
      <<"name">> => maps:get(name, Manifest),
      <<"size">> => maps:get(size, Manifest),
      <<"created">> => maps:get(created, Manifest),
      <<"chunk_size">> => maps:get(chunk_size, Manifest),
      <<"chunk_count">> => maps:get(chunk_count, Manifest),
      <<"hash_algorithm">> => atom_to_binary(maps:get(hash_algorithm, Manifest)),
      <<"root_hash">> => maps:get(root_hash, Manifest),
      <<"chunks">> => [#{<<"index">> => I, <<"offset">> => O,
                         <<"size">> => S, <<"hash">> => H}
                       || #{index := I, offset := O, size := S, hash := H}
                          <- maps:get(chunks, Manifest)]}.

%% `RecvResults' must be installed via `recv_returning/1' BEFORE the
%% handler is invoked — `open/7' spawns the reader synchronously as
%% part of dispatch, and its first `macula:recv/2' call can race ahead
%% of a mock set up afterward (it did, the first time this was
%% written: every case below timed out identically, hanging on the
%% REAL `macula:recv/2' against a stub process that isn't a real
%% gen_server).
open_upload(Bytes, RecvResults) ->
    {ok, Manifest, Chunks} = macula_manifest:create(Bytes),
    recv_returning(RecvResults),
    {ok, _Sup} = macula_upload:advertise(pool, <<0:256>>, <<"bulk.ingest">>, ?MODULE, self()),
    Handler = captured_handler(),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, manifest_stream_args(Manifest)),
    {StreamPid, Manifest, Chunks}.

%%%===================================================================
%%% Tests
%%%===================================================================

upload_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun verified_push_delivers_ok_and_replies_ok/0,
      fun tampered_bytes_deliver_error_and_replies_error/0,
      fun too_many_chunks_aborts_the_stream/0,
      fun bad_manifest_stops_before_any_chunk/0,
      fun direct_dial_forwards_client_stream_mode/0]}.

verified_push_delivers_ok_and_replies_ok() ->
    process_flag(trap_exit, true),
    Bytes = crypto:strong_rand_bytes(3 * macula_manifest:default_chunk_size()),
    {ok, PreManifest, PreChunks} = macula_manifest:create(Bytes),
    Mcid = maps:get(mcid, PreManifest),
    {StreamPid, _Manifest, _Chunks} =
        open_upload(Bytes, [{chunk, C} || C <- PreChunks] ++ [eof]),

    ?assertEqual({uploaded, {ok, Mcid, Bytes}}, wait_msg()),
    ?assertEqual(1, meck:num_calls(macula_stream, set_reply, [StreamPid, Mcid])),
    ?assertEqual(0, meck:num_calls(macula_stream, set_error, ['_', '_'])),
    ?assertEqual([<<"sharing.upload_started_v1">>, <<"sharing.upload_completed_v1">>], topics()),
    ?assertMatch(#{outcome := completed, mcid := Mcid}, completed_payload()).

%% Receiver-side verification, never sender-trusted: bytes that don't
%% match the manifest's own root hash (a genuine transit corruption, or
%% a sender lying about what it's pushing) are caught here, not
%% silently accepted just because the manifest claimed them.
tampered_bytes_deliver_error_and_replies_error() ->
    process_flag(trap_exit, true),
    Bytes = crypto:strong_rand_bytes(macula_manifest:default_chunk_size()),
    {ok, _PreManifest, [PreChunk]} = macula_manifest:create(Bytes),
    %% Flip the first byte via XOR 255 — guaranteed different from the
    %% original regardless of its value (a fixed replacement byte, e.g.
    %% 0, has a 1/256 chance of coincidentally matching it already and
    %% producing a no-op "tamper").
    <<FirstByte, PreChunkRest/binary>> = PreChunk,
    Tampered = <<(FirstByte bxor 255), PreChunkRest/binary>>,
    {StreamPid, _Manifest, _Chunks} = open_upload(Bytes, [{chunk, Tampered}, eof]),

    ?assertMatch({uploaded, {error, root_hash_mismatch}}, wait_msg()),
    ?assertEqual(1, meck:num_calls(macula_stream, set_error, [StreamPid, root_hash_mismatch])),
    ?assertEqual(0, meck:num_calls(macula_stream, set_reply, ['_', '_'])).

%% A sender pushing more chunks than its own manifest declared is
%% stopped, not accumulated without limit — a system-boundary input
%% from an untrusted remote peer.
too_many_chunks_aborts_the_stream() ->
    process_flag(trap_exit, true),
    Bytes = crypto:strong_rand_bytes(macula_manifest:default_chunk_size()),
    {ok, _PreManifest, [PreChunk]} = macula_manifest:create(Bytes),
    {StreamPid, _Manifest, _Chunks} =
        open_upload(Bytes, [{chunk, PreChunk}, {chunk, PreChunk}, eof]),

    ?assertMatch({uploaded, {error, too_many_chunks}}, wait_msg()),
    ?assertEqual(1, meck:num_calls(macula_stream, abort,
                                   [StreamPid, <<"cancelled">>, '_'])).

%% `handle_open/2' rejects a manifest that doesn't decode — no
%% `sharing.upload_started_v1' ever fires, `too_many_chunks''s sibling
%% guard never gets a chance to matter.
bad_manifest_stops_before_any_chunk() ->
    process_flag(trap_exit, true),
    recv_returning([eof]),
    {ok, _Sup} = macula_upload:advertise(pool, <<0:256>>, <<"bulk.ingest">>, ?MODULE, self()),
    Handler = captured_handler(),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{<<"not">> => <<"a manifest">>}),

    ?assertMatch({uploaded, {error, {invalid_manifest, _}}}, wait_msg()),
    ?assertEqual(1, meck:num_calls(macula_stream, set_error, [StreamPid, {invalid_manifest, '_'}])),
    ?assertEqual([], topics()).

direct_dial_forwards_client_stream_mode() ->
    meck:new(macula_direct_dial, [passthrough]),
    meck:expect(macula_direct_dial, publish_advertisement,
               fun(_Pool, _Realm, _Proc, _Identity, _Opts) -> ok end),
    Identity = macula_identity:generate(),

    {ok, _Sup} = macula_upload:advertise_direct(pool, <<0:256>>, <<"bulk.ingest">>,
                                                ?MODULE, self(), Identity),
    _Handler = captured_handler(),
    ?assertEqual(client_stream, persistent_term:get({?MODULE, advertised_mode})),
    ?assertEqual(1, meck:num_calls(macula_direct_dial, publish_advertisement,
                                   [pool, <<0:256>>, <<"bulk.ingest">>, Identity, '_'])),
    meck:unload(macula_direct_dial).

%%%===================================================================
%%% Helpers
%%%===================================================================

topics() ->
    [T || {_, {macula, publish, [_Pool, _Realm, T, _Payload]}, ok} <- meck:history(macula)].

completed_payload() ->
    [{_, {macula, publish, [_, _, _, Payload]}, ok}] =
        [E || {_, {macula, publish, [_, _, T, _]}, ok} = E <- meck:history(macula),
              T =:= <<"sharing.upload_completed_v1">>],
    Payload.

wait_msg() ->
    receive
        Msg -> Msg
    after 1000 -> timeout
    end.
