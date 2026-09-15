%% EUnit tests for text a local caller hands the SDK that a frame builder would refuse: a topic over 512 bytes or not
%% UTF-8 in a pool's publish and subscribe, and a STREAM_ERROR code over 64 bytes or not UTF-8 in macula_stream:abort/3.
%% Each is refused by name before anything is built, so the pool and its links keep running, and an abort with such a
%% code still stops its stream.
-module(macula_local_text_refusals_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).

local_text_refusals_test_() ->
    {timeout, 60,
     {setup,
      fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
      fun(ok) -> ok end,
      [{"macula_frame:text_checked/2 names the bound each builder applies", fun text_checked_names_each_bound/0},
       {"a pool refuses a topic it cannot publish or subscribe to, and keeps answering",
        fun a_pool_refuses_a_topic_before_building/0},
       {"an abort code a STREAM_ERROR cannot carry is refused, and the stream still ends",
        {timeout, 15, fun an_abort_code_outside_its_bound_still_ends_the_stream/0}}]}}.

text_checked_names_each_bound() ->
    [begin
         ?assertEqual(ok, macula_frame:text_checked(Field, binary:copy(<<"a">>, Max))),
         ?assertEqual({error, {text_too_long, Field}}, macula_frame:text_checked(Field, binary:copy(<<"a">>, Max + 1))),
         ?assertEqual({error, {invalid_text, Field}}, macula_frame:text_checked(Field, <<16#ff>>))
     end || {Field, Max} <- [{topic, 512}, {procedure, 512}, {code, 64}, {detail, 256}, {message, 256}]].

a_pool_refuses_a_topic_before_building() ->
    {ok, Pool} = macula_client:connect([], #{}),
    Long = binary:copy(<<"t">>, 513),
    Results = [macula_client:subscribe(Pool, ?REALM, Long, self(), #{}),
               macula_client:subscribe(Pool, ?REALM, <<16#ff>>, self(), #{}),
               macula_client:publish(Pool, ?REALM, Long, #{n => 1}, #{}),
               macula_client:publish(Pool, ?REALM, <<16#ff>>, #{n => 1}, #{})],
    Status = macula_client:status(Pool),
    ok = macula_client:close(Pool),
    ?assertEqual([{error, {text_too_long, topic}}, {error, {invalid_text, topic}},
                  {error, {text_too_long, topic}}, {error, {invalid_text, topic}}], Results),
    ?assertMatch({ok, #{}}, Status).

%% A caller that aborts gets its code's refusal by name, and the stream ends all the same, with the fixed code aborted
%% and the caller's message, so a call to abort always stops the stream.
an_abort_code_outside_its_bound_still_ends_the_stream() ->
    [begin
         {ok, Stream} = macula_stream:start_link(#{id => crypto:strong_rand_bytes(16), role => client, mode => bidi,
                                                   owner => self()}),
         Result = macula_stream:abort(Stream, Code, <<"why">>),
         Ended = receive {macula_stream, ended, Stream, How} -> How after 1_000 -> not_ended end,
         true = unlink(Stream),
         exit(Stream, kill),
         ?assertEqual({{error, {Refusal, code}}, {error, {<<"aborted">>, <<"why">>}}}, {Result, Ended})
     end || {Refusal, Code} <- [{text_too_long, binary:copy(<<"c">>, 65)}, {invalid_text, <<16#ff>>}]].
