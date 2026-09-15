%% EUnit tests for text a local caller hands the SDK that a frame builder would refuse: a topic over 512 bytes or not
%% UTF-8 in a pool's publish and subscribe, and a STREAM_ERROR code over 64 bytes or not UTF-8 in macula_stream:abort/3.
%% Each is refused by name before anything is built, so the pool, its links and the stream keep running.
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
       {"an abort code a STREAM_ERROR cannot carry is refused before the stream builds it",
        {timeout, 15, fun an_abort_code_outside_its_bound_is_refused/0}}]}}.

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

%% The refusal comes before any call to the stream, so a process that is not a stream stands in for one.
an_abort_code_outside_its_bound_is_refused() ->
    Stream = spawn(fun() -> receive stop -> ok end end),
    Results = [catch macula_stream:abort(Stream, binary:copy(<<"c">>, 65), <<"why">>),
               catch macula_stream:abort(Stream, <<16#ff>>, <<"why">>)],
    Alive = is_process_alive(Stream),
    Stream ! stop,
    ?assertEqual([{error, {text_too_long, code}}, {error, {invalid_text, code}}], Results),
    ?assert(Alive).
