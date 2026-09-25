%% A call on a stream whose process has ended answers {error, closed}, as its spec says, and never exits the caller:
%% the other end going away is an expected outcome of a stream (macula#41). Closing one is idempotent: ok.
-module(macula_stream_ended_tests).

-include_lib("eunit/include/eunit.hrl").

ended_stream_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
     fun(_) -> ok end,
     [{"send, recv and abort on an ended stream answer {error, closed}", fun calls_on_an_ended_stream_answer_closed/0},
      {"closing an ended stream is ok", fun closing_an_ended_stream_is_ok/0}]}.

calls_on_an_ended_stream_answer_closed() ->
    Stream = ended_stream(),
    ?assertEqual({error, closed}, macula_stream:send(Stream, <<"x">>)),
    ?assertEqual({error, closed}, macula_stream:send(Stream, #{a => 1}, msgpack)),
    ?assertEqual({error, closed}, macula_stream:recv(Stream, 100)),
    ?assertEqual({error, closed}, macula_stream:abort(Stream, <<"gone">>, <<"gone">>)),
    ?assertEqual({error, closed}, macula_stream:await_reply(Stream, 100)).

closing_an_ended_stream_is_ok() ->
    Stream = ended_stream(),
    ?assertEqual(ok, macula_stream:close(Stream)),
    ?assertEqual(ok, macula_stream:close_send(Stream)).

%% A local stream ended the way a stream ends: its owner has gone.
ended_stream() ->
    Procedure = <<"t.ended_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    ok = macula:advertise_stream(Procedure, server_stream, fun(_S, _A) -> receive never -> ok end end),
    Test = self(),
    {Owner, OwnerMon} = spawn_monitor(fun() -> {ok, S} = macula:call_stream(Procedure, #{}), Test ! {stream, S} end),
    Stream = receive {stream, S} -> S after 1_000 -> error(no_stream) end,
    receive {'DOWN', OwnerMon, process, Owner, _} -> ok end,
    Mon = erlang:monitor(process, Stream),
    receive {'DOWN', Mon, process, Stream, _} -> ok after 1_000 -> error(stream_outlived_its_owner) end,
    ok = macula:unadvertise_stream(Procedure),
    Stream.
