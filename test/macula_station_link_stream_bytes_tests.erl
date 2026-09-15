%% EUnit tests for the link side of a link-carried stream's writes (identity migration step 3, item 5). The link writes
%% the bytes a stream hands it on that stream's dedicated QUIC stream, forgets the stream after its last frame, and
%% tells the stream when a write fails.
-module(macula_station_link_stream_bytes_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SID, <<5:128>>).

stream_bytes_test_() ->
    [{"the bytes are written on the stream's dedicated stream", fun the_bytes_are_written_on_the_dedicated_stream/0},
     {"nothing is written for a stream after its last frame", fun nothing_is_written_after_the_last_frame/0},
     {"a failed write is reported to the stream, and the link forgets it", fun a_failed_write_is_reported/0}].

the_bytes_are_written_on_the_dedicated_stream() ->
    with_link(fun(Link, QuicStream) ->
        ok = macula_station_link:send_stream_bytes(Link, ?SID, <<"frame one">>, false),
        ok = macula_station_link:send_stream_bytes(Link, ?SID, <<"frame two">>, false),
        _ = sys:get_state(Link),
        ?assert(meck:called(macula_peering, send_on_stream, [QuicStream, <<"frame one">>])),
        ?assert(meck:called(macula_peering, send_on_stream, [QuicStream, <<"frame two">>]))
    end).

nothing_is_written_after_the_last_frame() ->
    with_link(fun(Link, _QuicStream) ->
        ok = macula_station_link:send_stream_bytes(Link, ?SID, <<"last">>, true),
        ok = macula_station_link:send_stream_bytes(Link, ?SID, <<"after">>, false),
        _ = sys:get_state(Link),
        ?assertEqual(1, meck:num_calls(macula_peering, send_on_stream, '_'))
    end).

a_failed_write_is_reported() ->
    with_link(fun(Link, _QuicStream) ->
        ok = meck:expect(macula_peering, send_on_stream, fun(_Stream, _Bytes) -> {error, closed} end),
        ok = macula_station_link:send_stream_bytes(Link, ?SID, <<"frame">>, false),
        ?assertEqual({stream_write_failed, ?SID, closed},
                     receive {stream_write_failed, _, _} = Failed -> Failed after 1000 -> none end),
        ok = macula_station_link:send_stream_bytes(Link, ?SID, <<"again">>, false),
        _ = sys:get_state(Link),
        ?assertEqual(1, meck:num_calls(macula_peering, send_on_stream, '_'))
    end).

%% A link with one client stream entry whose stream process is this test process, and macula_peering's writes mocked.
with_link(Test) ->
    {ok, _} = application:ensure_all_started(macula),
    try meck:unload(macula_peering) catch _:_ -> ok end,
    ok = meck:new(macula_peering, [passthrough]),
    ok = meck:expect(macula_peering, send_on_stream, fun(_Stream, _Bytes) -> ok end),
    {ok, Link} = macula_station_link:start_link(with_link_keys(#{seed => #{host => <<"127.0.0.1">>, port => 1},
                                                   connect_timeout_ms => 2000})),
    unlink(Link),
    QuicStream = make_ref(),
    Self = self(),
    _ = sys:replace_state(Link, fun(S) -> macula_station_link:with_client_stream(S, ?SID, {Self, QuicStream}) end),
    try
        Test(Link, QuicStream)
    after
        gen_server:stop(Link),
        meck:unload(macula_peering)
    end.

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
