%% EUnit tests for the link side of a link-carried stream's writes (identity migration step 3, item 5). The link writes
%% the bytes a stream hands it on that stream's dedicated QUIC stream, through its send_on_stream function, forgets the
%% stream after its last frame, and tells the stream when a write fails. The writer is a start option, so no shared
%% module is replaced.
-module(macula_station_link_stream_bytes_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SID, <<5:128>>).

stream_bytes_test_() ->
    [{"the bytes are written on the stream's dedicated stream", fun the_bytes_are_written_on_the_dedicated_stream/0},
     {"nothing is written for a stream after its last frame", fun nothing_is_written_after_the_last_frame/0},
     {"a failed write is reported to the stream, and the link forgets it", fun a_failed_write_is_reported/0},
     {"a link given a stream function of another shape refuses to start",
      {spawn, fun a_stream_function_of_another_shape_refuses_the_start/0}}].

a_stream_function_of_another_shape_refuses_the_start() ->
    {ok, _} = application:ensure_all_started(macula),
    process_flag(trap_exit, true),
    Base = with_link_keys(#{seed => #{host => <<"127.0.0.1">>, port => 1}, connect_timeout_ms => 2000}),
    ?assertEqual([{error, {open_stream, not_an_opener}}, {error, {send_on_stream, not_a_writer}},
                  {error, {close_stream, not_a_closer}}],
                 [macula_station_link:start_link(Base#{Key => OtherShape})
                  || {Key, OtherShape} <- [{open_stream, fun(_Conn, _More) -> {ok, make_ref()} end},
                                           {send_on_stream, fun(_Stream) -> ok end},
                                           {close_stream, not_a_function}]]).

the_bytes_are_written_on_the_dedicated_stream() ->
    with_link(ok, fun(Link, QuicStream) ->
        ok = macula_station_link:send_stream_bytes(Link, ?SID, <<"frame one">>, false),
        ok = macula_station_link:send_stream_bytes(Link, ?SID, <<"frame two">>, false),
        _ = sys:get_state(Link),
        ?assertEqual([{QuicStream, <<"frame one">>}, {QuicStream, <<"frame two">>}], writes())
    end).

nothing_is_written_after_the_last_frame() ->
    with_link(ok, fun(Link, QuicStream) ->
        ok = macula_station_link:send_stream_bytes(Link, ?SID, <<"last">>, true),
        ok = macula_station_link:send_stream_bytes(Link, ?SID, <<"after">>, false),
        _ = sys:get_state(Link),
        ?assertEqual([{QuicStream, <<"last">>}], writes())
    end).

a_failed_write_is_reported() ->
    with_link({error, closed}, fun(Link, _QuicStream) ->
        ok = macula_station_link:send_stream_bytes(Link, ?SID, <<"frame">>, false),
        ?assertEqual({stream_write_failed, ?SID, closed},
                     receive {stream_write_failed, _, _} = Failed -> Failed after 1000 -> none end),
        ok = macula_station_link:send_stream_bytes(Link, ?SID, <<"again">>, false),
        _ = sys:get_state(Link),
        ?assertEqual(1, length(writes()))
    end).

%% A link with one client stream entry whose stream process is this test process, and a writer that tells this process
%% each write and answers Answer.
with_link(Answer, Test) ->
    {ok, _} = application:ensure_all_started(macula),
    Self = self(),
    Writer = fun(Stream, Bytes) -> Self ! {written, Stream, Bytes}, Answer end,
    {ok, Link} = macula_station_link:start_link(with_link_keys(#{seed => #{host => <<"127.0.0.1">>, port => 1},
                                                                 connect_timeout_ms => 2000,
                                                                 send_on_stream => Writer,
                                                                 close_stream => fun(_Stream) -> ok end})),
    unlink(Link),
    QuicStream = make_ref(),
    _ = sys:replace_state(Link, fun(S) -> macula_station_link:with_client_stream(S, ?SID, {Self, QuicStream}) end),
    try
        Test(Link, QuicStream)
    after
        gen_server:stop(Link)
    end.

%% Every write the link made so far, in order.
writes() ->
    receive
        {written, Stream, Bytes} -> [{Stream, Bytes} | writes()]
    after 100 ->
        []
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
