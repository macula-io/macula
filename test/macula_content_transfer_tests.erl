%%%-------------------------------------------------------------------
%%% @doc Tests for macula_content_transfer (PLAN_PUSH_UPLOAD.md Phase 1).
%%%
%%% Mocks at the `macula_client'/`macula_station_link' boundary — the
%%% same layer `macula_feeder_tests'/`macula_download_tests' mock
%%% `macula:put_content'/`get_content' at, one level lower since this
%%% module now owns what those used to delegate to. Genuine two-endpoint
%%% coverage of the underlying QUIC reset lives in
%%% `macula_quic_stream_reset_tests' — this suite is about
%%% `macula_content_transfer''s OWN correctness: does it call the
%%% right functions with the right arguments, does `await/1,2' resolve
%%% correctly, does `cancel/3' behave differently depending on whether
%%% a stream is open yet.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_transfer_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SINGLE_CODEC, 16#55).

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    meck:new(macula_client, [passthrough]),
    meck:new(macula_station_link, [passthrough]),
    ok.

teardown(_) ->
    meck:unload(macula_station_link),
    meck:unload(macula_client),
    ok.

content_transfer_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun single_block_put_resolves_mcid/0,
      fun single_block_get_resolves_bytes/0,
      fun cancel_before_stream_opens_never_aborts/0,
      fun cancel_mid_transfer_aborts_the_open_stream/0,
      fun cancel_after_completion_is_a_pure_reap/0,
      fun await_answers_late_callers_from_cached_result/0,
      fun share_id_resolves_via_the_registry_until_cancelled/0]}.

%%%===================================================================
%%% Tests
%%%===================================================================

single_block_put_resolves_mcid() ->
    Bytes = <<"hello content transfer">>,
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    meck:expect(macula_client, pick_connected_link,
               fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream,
               fun(_LinkPid) -> {ok, Stream} end),
    meck:expect(macula_station_link, call_on_stream,
               fun(_LinkPid, _Stream, _Realm, <<"_content.put_block">>,
                   _Payload, _Tmo) -> {ok, ok}
               end),
    meck:expect(macula_station_link, close_content_stream,
               fun(_LinkPid, _Stream) -> ok end),

    {ok, Pid} = macula_content_transfer:start_put(dummy_pid(), Bytes),
    Hash = macula_blake3_nif:hash(Bytes),
    ExpectedMcid = <<1, ?SINGLE_CODEC, Hash/binary>>,
    ?assertEqual({ok, ExpectedMcid}, macula_content_transfer:await(Pid)),
    ok = macula_content_transfer:cancel(Pid).

single_block_get_resolves_bytes() ->
    Bytes = <<"round tripped bytes">>,
    Hash = macula_blake3_nif:hash(Bytes),
    Mcid = <<1, ?SINGLE_CODEC, Hash/binary>>,
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    meck:expect(macula_client, pick_connected_link,
               fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream,
               fun(_LinkPid) -> {ok, Stream} end),
    meck:expect(macula_station_link, call_on_stream,
               fun(_LinkPid, _Stream, _Realm, <<"_content.get_block">>,
                   _Payload, _Tmo) -> {ok, Bytes}
               end),
    meck:expect(macula_station_link, close_content_stream,
               fun(_LinkPid, _Stream) -> ok end),

    {ok, Pid} = macula_content_transfer:start_get(dummy_pid(), Mcid),
    ?assertEqual({ok, Bytes}, macula_content_transfer:await(Pid)),
    ok = macula_content_transfer:cancel(Pid).

%% Cancel arriving before the worker has even picked a link: nothing
%% is open, so there is nothing to reset — `abort_content_stream/4'
%% must never be called.
cancel_before_stream_opens_never_aborts() ->
    Self = self(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) ->
        Self ! connecting,
        receive proceed -> {ok, dummy_pid()} end
    end),
    meck:expect(macula_station_link, abort_content_stream,
               fun(_, _, _, _) -> ok end),

    {ok, Pid} = macula_content_transfer:start_put(dummy_pid(), <<"x">>),
    receive connecting -> ok after 2_000 -> erlang:error(no_connect_attempt) end,
    ok = macula_content_transfer:cancel(Pid, 7, <<"too slow">>),
    ?assertEqual(0, meck:num_calls(macula_station_link, abort_content_stream, '_')).

%% Cancel arriving once the stream is open but the transfer itself is
%% still blocked on a reply: this IS the case `abort_content_stream/4'
%% exists for — the exact stream/code/message must reach it.
cancel_mid_transfer_aborts_the_open_stream() ->
    Self = self(),
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    meck:expect(macula_client, pick_connected_link,
               fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream,
               fun(_LinkPid) -> {ok, Stream} end),
    meck:expect(macula_station_link, call_on_stream,
               fun(_LinkPid, _Stream, _Realm, _Proc, _Payload, _Tmo) ->
                   Self ! call_in_flight,
                   receive proceed -> {ok, ok} end
               end),
    meck:expect(macula_station_link, abort_content_stream,
               fun(_, _, _, _) -> ok end),

    {ok, Pid} = macula_content_transfer:start_put(dummy_pid(), <<"blocked">>),
    receive call_in_flight -> ok after 2_000 -> erlang:error(no_call_in_flight) end,
    ok = macula_content_transfer:cancel(Pid, 42, <<"bored now">>),
    ?assertEqual([{LinkPid, Stream, 42, <<"bored now">>}],
                 [{A, B, C, D} || {_, {macula_station_link, abort_content_stream,
                                        [A, B, C, D]}, ok}
                                  <- meck:history(macula_station_link)]).

%% Cancel after the transfer already resolved: a pure reap, nothing
%% peer-visible happens because there is nothing left open.
cancel_after_completion_is_a_pure_reap() ->
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    meck:expect(macula_client, pick_connected_link,
               fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream,
               fun(_LinkPid) -> {ok, Stream} end),
    meck:expect(macula_station_link, call_on_stream,
               fun(_, _, _, _, _, _) -> {ok, ok} end),
    meck:expect(macula_station_link, close_content_stream,
               fun(_, _) -> ok end),
    meck:expect(macula_station_link, abort_content_stream,
               fun(_, _, _, _) -> ok end),

    {ok, Pid} = macula_content_transfer:start_put(dummy_pid(), <<"done already">>),
    {ok, _Mcid} = macula_content_transfer:await(Pid),
    ok = macula_content_transfer:cancel(Pid, 1, <<"too late to matter">>),
    ?assertEqual(0, meck:num_calls(macula_station_link, abort_content_stream, '_')).

%% A caller that calls await/1 AFTER the result is already known must
%% get it immediately, not hang — the process does not self-terminate
%% on completion.
await_answers_late_callers_from_cached_result() ->
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    meck:expect(macula_client, pick_connected_link,
               fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream,
               fun(_LinkPid) -> {ok, Stream} end),
    meck:expect(macula_station_link, call_on_stream,
               fun(_, _, _, _, _, _) -> {ok, ok} end),
    meck:expect(macula_station_link, close_content_stream,
               fun(_, _) -> ok end),

    {ok, Pid} = macula_content_transfer:start_put(dummy_pid(), <<"cache me">>),
    First = macula_content_transfer:await(Pid),
    Second = macula_content_transfer:await(Pid),
    ?assertEqual(First, Second),
    ?assertMatch({ok, _}, Second),
    ok = macula_content_transfer:cancel(Pid).

%% share_id resolves through macula_content_transfer_registry while
%% the transfer is alive, and stops resolving once cancel reaps it
%% (monitor-based cleanup).
share_id_resolves_via_the_registry_until_cancelled() ->
    Self = self(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) ->
        Self ! connecting,
        receive proceed -> {ok, dummy_pid()} end
    end),

    {ok, Pid} = macula_content_transfer:start_put(dummy_pid(), <<"id me">>),
    receive connecting -> ok after 2_000 -> erlang:error(no_connect_attempt) end,
    ShareId = macula_content_transfer:share_id(Pid),
    ?assertEqual({ok, Pid}, macula_content_transfer_registry:whereis_share(ShareId)),

    Mon = erlang:monitor(process, Pid),
    ok = macula_content_transfer:cancel(Pid),
    receive {'DOWN', Mon, process, Pid, _} -> ok
    after 2_000 -> erlang:error(cancel_did_not_stop_the_process)
    end,
    %% The registry's OWN monitor on Pid is independent of the one
    %% above — both fire off the same process exit, but delivery order
    %% across two unrelated monitors isn't guaranteed. Poll briefly
    %% rather than assume the registry has already reacted.
    ?assertEqual({error, not_found}, await_not_found(ShareId, 20)).

await_not_found(ShareId, 0) ->
    macula_content_transfer_registry:whereis_share(ShareId);
await_not_found(ShareId, Retries) ->
    case macula_content_transfer_registry:whereis_share(ShareId) of
        {error, not_found} = NotFound -> NotFound;
        {ok, _} -> timer:sleep(10), await_not_found(ShareId, Retries - 1)
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

dummy_pid() ->
    spawn(fun() -> receive stop -> ok end end).
