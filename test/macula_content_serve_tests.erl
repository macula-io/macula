%% EUnit tests for `macula_content_serve': how a sharing node answers a fetch on its content procedure (D27). One
%% content id per stream: the args name it and what is wanted, `root' or `block'; the answer is one DATA body and the
%% end of the stream, or the stream's error with a code. Driven over local in-process streams, which carry the same
%% DATA bodies a station link does.
-module(macula_content_serve_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CHUNK, 262144).

serve_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
     fun(_) -> ok end,
     [{"a raw root is answered as a block", fun a_raw_root_is_answered_as_a_block/0},
      {"a manifest root is answered as its manifest", fun a_manifest_root_is_answered_as_its_manifest/0},
      {"a chunk of a held manifest is answered as a block", fun a_chunk_is_answered_as_a_block/0},
      {"content not held is refused not_shared", fun content_not_held_is_refused/0},
      {"args that name no content id are refused malformed", fun malformed_args_are_refused/0},
      {"wire-shaped args, text keys and values, are read", fun wire_shaped_args_are_read/0},
      {"an answer the stream refuses ends the stream", fun an_answer_the_stream_refuses_ends_it/0}]}.

%% A send the stream refuses: the handler ends the stream with the reason, so a fetcher still waiting is answered at
%% once, and does not crash.
an_answer_the_stream_refuses_ends_it() ->
    {MCID, Store} = macula_content_store:added(<<"hello">>, #{}, macula_content_store:new()),
    Lookup = fun(Want, M) -> macula_content_serve:lookup(Want, M, Store) end,
    Stream = refusing_stream(self()),
    ?assertEqual(ok, macula_content_serve:serve(Stream, #{mcid => MCID, want => root}, Lookup)),
    ?assertEqual([send, abort], received_calls()).

%% A stand-in stream that answers every call `{error, closed}' and reports what it was asked.
refusing_stream(Test) ->
    spawn_link(fun Loop() ->
                   receive
                       {'$gen_call', From, Request} ->
                           Test ! {stream_call, element(1, Request)},
                           gen:reply(From, {error, closed}),
                           Loop()
                   end
               end).

received_calls() ->
    receive {stream_call, Kind} -> [Kind | received_calls()] after 100 -> [] end.

a_raw_root_is_answered_as_a_block() ->
    {MCID, Store} = macula_content_store:added(<<"hello">>, #{}, macula_content_store:new()),
    ?assertEqual({data, #{kind => block, mcid => MCID, bytes => <<"hello">>}},
                 fetched(Store, #{mcid => MCID, want => root})).

a_manifest_root_is_answered_as_its_manifest() ->
    {MCID, Store} = macula_content_store:added(crypto:strong_rand_bytes(?CHUNK + 1), #{}, macula_content_store:new()),
    {manifest, Manifest} = macula_content_store:root(MCID, Store),
    ?assertEqual({data, #{kind => manifest, mcid => MCID, manifest => Manifest}},
                 fetched(Store, #{mcid => MCID, want => root})).

a_chunk_is_answered_as_a_block() ->
    Bytes = crypto:strong_rand_bytes(?CHUNK + 1),
    {MCID, Store} = macula_content_store:added(Bytes, #{}, macula_content_store:new()),
    {manifest, Manifest} = macula_content_store:root(MCID, Store),
    {ok, C1} = macula_manifest:chunk_mcid(Manifest, 1),
    ?assertEqual({data, #{kind => block, mcid => C1, bytes => binary:part(Bytes, ?CHUNK, 1)}},
                 fetched(Store, #{mcid => C1, want => block})).

content_not_held_is_refused() ->
    Store = macula_content_store:new(),
    ?assertMatch({error, {<<"not_shared">>, _}}, fetched(Store, #{mcid => <<2, 16#55, 0:384>>, want => root})),
    %% A raw root is not a chunk: asked for as one, it is not held.
    {MCID, S2} = macula_content_store:added(<<"x">>, #{}, Store),
    ?assertMatch({error, {<<"not_shared">>, _}}, fetched(S2, #{mcid => MCID, want => block})).

malformed_args_are_refused() ->
    Store = macula_content_store:new(),
    [?assertMatch({error, {<<"malformed">>, _}}, fetched(Store, Args))
     || Args <- [#{}, #{mcid => <<1, 2>>, want => root}, #{mcid => <<2, 16#55, 0:384>>, want => everything},
                 not_a_map]].

wire_shaped_args_are_read() ->
    {MCID, Store} = macula_content_store:added(<<"hello">>, #{}, macula_content_store:new()),
    ?assertMatch({data, #{kind := block}},
                 fetched(Store, #{{text, <<"mcid">>} => MCID, {text, <<"want">>} => {text, <<"root">>}})).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% Serve `Store' on a fresh local procedure, fetch once with `Args', and return the first thing received.
fetched(Store, Args) ->
    Procedure = <<"t.content_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Lookup = fun(Want, MCID) -> macula_content_serve:lookup(Want, MCID, Store) end,
    ok = macula:advertise_stream(Procedure, server_stream,
                                 fun(Stream, A) -> macula_content_serve:serve(Stream, A, Lookup) end),
    {ok, S} = macula:call_stream(Procedure, Args),
    First = macula:recv(S, 2_000),
    macula:unadvertise_stream(Procedure),
    First.
