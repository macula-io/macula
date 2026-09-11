%%%-------------------------------------------------------------------
%%% @doc Tests for macula_upload.
%%%
%%% Drives it exactly the way `macula_streamer' itself does: take the
%%% handler the advertise function gets, invoke it with a stubbed
%%% stream, and feed chunks through a scripted `recv/2' (the same shape
%%% `macula_streamer_client_stream_tests' uses). The recorded
%%% `set_reply'/`set_error' calls show the terminal reply this module
%%% hands back to a would-be `macula_pusher'. Every function comes from
%%% macula_scripted_stream, so no test replaces a module.
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

stream_stub() -> receive stop -> ok end.

%% The manifest as it would actually arrive over the wire: a plain map
%% with binary-string keys, matching `macula_manifest:from_wire/1''s
%% own "robust to binary-string keys" fallback, exercising the REAL
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

%% Advertises this module for uploads whose recv/2 returns RecvResults,
%% and returns the handler the advertise function got. The results are
%% part of the options, so they are in place before any reader starts.
advertised_handler(RecvResults) ->
    {ok, _Sup} = macula_upload:advertise(pool, <<0:256>>, <<"bulk.ingest">>, ?MODULE, self(),
                                         macula_scripted_stream:options(RecvResults)),
    [{<<"bulk.ingest">>, client_stream, Handler, _}] = macula_scripted_stream:advertised(),
    Handler.

open_upload(Bytes, RecvResults) ->
    {ok, Manifest, Chunks} = macula_manifest:create(Bytes),
    Handler = advertised_handler(RecvResults),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, manifest_stream_args(Manifest)),
    {StreamPid, Manifest, Chunks}.

%%%===================================================================
%%% Tests
%%%===================================================================

%% Each test runs in a process of its own.
upload_test_() ->
    [{spawn, Test}
     || Test <- [fun verified_push_delivers_ok_and_replies_ok/0,
                 fun tampered_bytes_deliver_error_and_replies_error/0,
                 fun too_many_chunks_aborts_the_stream/0,
                 fun bad_manifest_stops_before_any_chunk/0,
                 fun relabelled_manifest_is_refused_before_any_chunk/0,
                 fun direct_dial_forwards_client_stream_mode/0,
                 fun a_fact_publish_of_another_arity_is_refused/0,
                 fun advertise_passes_auth_and_reuse_sup_on_to_the_streamer/0]].

verified_push_delivers_ok_and_replies_ok() ->
    process_flag(trap_exit, true),
    Bytes = crypto:strong_rand_bytes(3 * macula_manifest:default_chunk_size()),
    {ok, PreManifest, PreChunks} = macula_manifest:create(Bytes),
    Mcid = maps:get(mcid, PreManifest),
    {StreamPid, _Manifest, _Chunks} =
        open_upload(Bytes, [{chunk, C} || C <- PreChunks] ++ [eof]),

    ?assertEqual({uploaded, {ok, Mcid, Bytes}}, wait_msg()),
    ?assertEqual([{set_reply, [StreamPid, Mcid]}, {close, [StreamPid]}],
                 macula_scripted_stream:calls()),
    Published = macula_scripted_stream:published(),
    ?assertEqual([<<"sharing.upload_started_v1">>, <<"sharing.upload_completed_v1">>],
                 [Topic || {Topic, _} <- Published]),
    ?assertMatch([_, {_, #{outcome := completed, mcid := Mcid}}], Published).

%% Receiver-side verification, never sender-trusted: bytes that don't
%% match the manifest's own root hash (a genuine transit corruption, or
%% a sender lying about what it's pushing) are caught here, not
%% silently accepted just because the manifest claimed them.
tampered_bytes_deliver_error_and_replies_error() ->
    process_flag(trap_exit, true),
    Bytes = crypto:strong_rand_bytes(macula_manifest:default_chunk_size()),
    {ok, _PreManifest, [PreChunk]} = macula_manifest:create(Bytes),
    %% Flip the first byte via XOR 255, guaranteed different from the
    %% original regardless of its value (a fixed replacement byte, e.g.
    %% 0, has a 1/256 chance of coincidentally matching it already and
    %% producing a no-op "tamper").
    <<FirstByte, PreChunkRest/binary>> = PreChunk,
    Tampered = <<(FirstByte bxor 255), PreChunkRest/binary>>,
    {StreamPid, _Manifest, _Chunks} = open_upload(Bytes, [{chunk, Tampered}, eof]),

    ?assertMatch({uploaded, {error, root_hash_mismatch}}, wait_msg()),
    ?assertEqual([{set_error, [StreamPid, root_hash_mismatch]}, {close, [StreamPid]}],
                 macula_scripted_stream:calls()).

%% A sender pushing more chunks than its own manifest declared is
%% stopped, not accumulated without limit: a system-boundary input
%% from an untrusted remote peer.
too_many_chunks_aborts_the_stream() ->
    process_flag(trap_exit, true),
    Bytes = crypto:strong_rand_bytes(macula_manifest:default_chunk_size()),
    {ok, _PreManifest, [PreChunk]} = macula_manifest:create(Bytes),
    {StreamPid, _Manifest, _Chunks} =
        open_upload(Bytes, [{chunk, PreChunk}, {chunk, PreChunk}, eof]),

    ?assertMatch({uploaded, {error, too_many_chunks}}, wait_msg()),
    ?assertEqual([{abort, [StreamPid, <<"cancelled">>, <<"too_many_chunks">>]}],
                 macula_scripted_stream:calls()).

%% `handle_open/2' rejects a manifest that doesn't decode: no
%% `sharing.upload_started_v1' ever fires, and `too_many_chunks''s
%% sibling guard never gets a chance to matter.
bad_manifest_stops_before_any_chunk() ->
    process_flag(trap_exit, true),
    Handler = advertised_handler([eof]),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{<<"not">> => <<"a manifest">>}),

    ?assertMatch({uploaded, {error, {invalid_manifest, _}}}, wait_msg()),
    ?assertMatch([{set_error, [StreamPid, {invalid_manifest, _}]}, {close, [StreamPid]}],
                 macula_scripted_stream:calls()),
    ?assertEqual([], macula_scripted_stream:published()).

%% A manifest whose own `mcid' names other content is refused when the
%% stream opens, the same as one that does not decode, even though the
%% pushed bytes match its size and root hash: no started fact, and both
%% sides see the error.
relabelled_manifest_is_refused_before_any_chunk() ->
    process_flag(trap_exit, true),
    Bytes = crypto:strong_rand_bytes(macula_manifest:default_chunk_size()),
    {ok, Manifest, [Chunk]} = macula_manifest:create(Bytes),
    {ok, Other, _} = macula_manifest:create(crypto:strong_rand_bytes(64)),
    Handler = advertised_handler([{chunk, Chunk}, eof]),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, manifest_stream_args(Manifest#{mcid := maps:get(mcid, Other)})),

    Reason = {invalid_manifest, manifest_mcid_mismatch},
    ?assertEqual({uploaded, {error, Reason}}, wait_msg()),
    ?assertEqual([{set_error, [StreamPid, Reason]}, {close, [StreamPid]}],
                 macula_scripted_stream:calls()),
    ?assertEqual([], macula_scripted_stream:published()).

direct_dial_forwards_client_stream_mode() ->
    Identity = macula_identity:generate(),
    {ok, _Sup} = macula_upload:advertise_direct(pool, <<0:256>>, <<"bulk.ingest">>, ?MODULE, self(),
                                                Identity, macula_scripted_stream:options([])),
    ?assertMatch([{<<"bulk.ingest">>, client_stream, _, _}], macula_scripted_stream:advertised()),
    ?assertMatch([{<<"bulk.ingest">>, Identity, _}],
                 macula_scripted_stream:advertisements_published()).

%% An upload's own fact publish of another arity is refused, and nothing
%% is advertised for it.
a_fact_publish_of_another_arity_is_refused() ->
    Opts = (macula_scripted_stream:options([]))#{fact_publish := fun(_, _, _) -> ok end},
    ?assertError(function_clause,
                 macula_upload:advertise(pool, <<0:256>>, <<"bulk.ingest">>, ?MODULE, self(),
                                         Opts)),
    ?assertEqual([], macula_scripted_stream:advertised()).

%% advertise/6 passes its options on to the streamer as advertise_direct/7
%% does: the auth policy reaches the advertise function, and reuse_sup
%% gives back the supervisor it names.
advertise_passes_auth_and_reuse_sup_on_to_the_streamer() ->
    Policy = {ucan_required, <<7:256>>},
    Opts = (macula_scripted_stream:options([]))#{auth => Policy},
    {ok, Sup} = macula_upload:advertise(pool, <<0:256>>, <<"bulk.ingest">>, ?MODULE, self(),
                                        Opts),
    {ok, Again} = macula_upload:advertise(pool, <<0:256>>, <<"bulk.ingest">>, ?MODULE, self(),
                                          Opts#{reuse_sup => Sup}),
    ?assertEqual(Sup, Again),
    ?assertMatch([{_, client_stream, _, #{auth := Policy}},
                  {_, client_stream, _, #{auth := Policy}}],
                 macula_scripted_stream:advertised()).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% The next message from the upload's callback.
wait_msg() ->
    receive
        {uploaded, _} = Msg -> Msg
    after 1000 -> timeout
    end.
