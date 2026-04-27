%%%-------------------------------------------------------------------
%%% @doc EUnit tests for the streaming RPC SDK (Phase 1, local
%%% dispatch). See PLAN_MACULA_STREAMING.md.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_v1_tests).

-include_lib("eunit/include/eunit.hrl").

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
                      macula:close(Stream)
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
                      macula:close(Stream)
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
                      macula:close(Stream)
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
