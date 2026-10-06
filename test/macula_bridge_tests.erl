%% The legacy-application bridge: an unmodified TCP client talks to an
%% unmodified TCP server through one mesh stream per connection. These tests
%% run the bridge's own two ends over a real `macula_stream' pair in this BEAM
%% (`macula_stream_local'), with real TCP sockets on both sides, so every byte
%% crosses the same pump, credit window and half-close a mesh stream would.
%% The pool entry points (`serve/5', `listen/4') are `serve_with/3' and
%% `listen_with/2' over `macula:advertise_stream/6' and `macula:call_stream/5';
%% what they hand those is asserted here too.
-module(macula_bridge_tests).

-include_lib("eunit/include/eunit.hrl").

-export([log/2]).

-define(EVENT_MS, 5_000).
-define(KiB, 1024).
-define(MiB, (1024 * 1024)).

bridge_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
     fun(ok) -> ok end,
     [{timeout, 30, {spawn, Case}}
      || Case <- [fun bytes_cross_both_ways/0,
                  fun megabytes_to_a_slow_reader_arrive_whole/0,
                  fun a_half_close_reaches_the_other_side_and_the_answer_comes_back/0,
                  fun a_half_closed_client_reads_an_answer_larger_than_the_window/0,
                  fun ends_with_different_windows_do_not_stall/0,
                  fun a_provider_that_ignores_credit_is_cut_off/0,
                  fun a_chunk_that_is_not_bridge_bytes_closes_the_connection/0,
                  fun an_unreachable_service_closes_the_local_connection/0,
                  fun a_refused_connection_is_closed_at_once_and_logged/0,
                  fun an_idle_connection_is_closed/0,
                  fun a_client_that_stops_reading_mid_write_is_closed/0,
                  fun the_bridged_end_writes_without_delay/0,
                  fun serve_hands_its_policy_to_the_advertisement/0,
                  fun serve_refuses_to_run_without_an_explicit_policy/0,
                  fun the_listening_end_presents_its_token/0,
                  fun a_window_outside_its_bounds_is_refused/0,
                  fun a_chunk_size_outside_its_bounds_is_refused/0,
                  fun a_client_reset_reaches_the_service_as_a_reset/0,
                  fun a_service_reset_reaches_the_client_as_a_reset/0,
                  fun the_serving_end_grants_the_credit_it_receives_under/0,
                  fun the_serving_end_grants_no_more_than_its_session_share/0,
                  fun serve_refuses_a_session_share_too_small_to_credit/0,
                  fun a_socket_read_is_at_most_one_chunk/0,
                  fun a_plain_stream_client_that_ends_its_input_reads_the_answer/0,
                  fun credit_past_any_window_is_malformed/0,
                  fun a_receiver_that_grants_nothing_holds_no_more_than_the_first_credit/0,
                  fun the_default_chunk_is_at_most_half_the_window/0,
                  fun the_serving_end_keeps_the_share_it_started_with/0]]}.

%% What the client writes reaches the service, and what the service writes
%% reaches the client.
bytes_cross_both_ways() ->
    #{port := Port} = bridge(echo_service()),
    {ok, C} = connect(Port),
    ok = gen_tcp:send(C, <<"hello through the mesh">>),
    ?assertEqual({ok, <<"hello through the mesh">>}, gen_tcp:recv(C, 22, ?EVENT_MS)),
    gen_tcp:close(C).

%% More than the stream's inbox bound (16 MiB, past which the session is
%% aborted), to a service that reads nothing for a while: the sender waits for
%% credit instead of filling the inbox, and every byte arrives in order.
megabytes_to_a_slow_reader_arrive_whole() ->
    Payload = crypto:strong_rand_bytes(24 * ?MiB),
    #{port := Port} = bridge(slow_echo_service(1_500), #{window_bytes => 256 * ?KiB, write_timeout_ms => 5_000}),
    {ok, C} = connect(Port),
    Sender = spawn(fun() -> ok = gen_tcp:send(C, Payload) end),
    %% A failure leaves megabytes queued on sockets, and the VM's halt waits
    %% to flush them: `C' is closed on every way out, and the bridge's own
    %% blocked writes give up after `write_timeout_ms'.
    try
        ?assertEqual({ok, Payload}, gen_tcp:recv(C, byte_size(Payload), 20_000))
    after
        exit(Sender, kill),
        gen_tcp:close(C)
    end.

%% The client finishes sending (FIN) and still reads: the service sees the end
%% of its input, answers, and closes; the client reads the answer, then the
%% close. What psql's COPY and an HTTP request body rely on.
a_half_close_reaches_the_other_side_and_the_answer_comes_back() ->
    #{port := Port} = bridge(count_then_answer_service()),
    {ok, C} = connect(Port),
    ok = gen_tcp:send(C, <<"abcdef">>),
    ok = gen_tcp:shutdown(C, write),
    ?assertEqual({ok, <<"6">>}, gen_tcp:recv(C, 0, ?EVENT_MS)),
    ?assertEqual({error, closed}, gen_tcp:recv(C, 0, ?EVENT_MS)),
    gen_tcp:close(C).

%% After the client's FIN the answer still flows under credit: the client's
%% side keeps acknowledging what it writes, so an answer many windows long
%% arrives whole, then the close.
a_half_closed_client_reads_an_answer_larger_than_the_window() ->
    Answer = crypto:strong_rand_bytes(4 * ?MiB),
    #{port := Port} = bridge(read_all_then_answer_service(Answer), #{window_bytes => 64 * ?KiB}),
    {ok, C} = connect(Port),
    ok = gen_tcp:send(C, <<"q">>),
    ok = gen_tcp:shutdown(C, write),
    ?assertEqual({ok, Answer}, gen_tcp:recv(C, byte_size(Answer), 10_000)),
    ?assertEqual({error, closed}, gen_tcp:recv(C, 0, ?EVENT_MS)),
    gen_tcp:close(C).

%% The two ends run on different nodes and are configured apart: a small
%% window on one side and a large one on the other still move bytes both ways.
ends_with_different_windows_do_not_stall() ->
    Payload = crypto:strong_rand_bytes(4 * ?MiB),
    #{port := Port} = bridge(echo_service(), #{window_bytes => 8 * ?MiB}, #{window_bytes => 64 * ?KiB}),
    {ok, C} = connect(Port),
    Sender = spawn(fun() -> ok = gen_tcp:send(C, Payload) end),
    try
        ?assertEqual({ok, Payload}, gen_tcp:recv(C, byte_size(Payload), 10_000))
    after
        exit(Sender, kill),
        gen_tcp:close(C)
    end.

%% A provider that sends without waiting for credit, to a client that reads
%% nothing: the bridge takes no more from the stream than it can write, so the
%% stream's own inbox bound ends the session instead of the bridge's memory
%% holding everything the provider sends.
a_provider_that_ignores_credit_is_cut_off() ->
    Test = self(),
    Procedure = procedure(),
    Flood = 48 * ?MiB,
    ok = macula_stream_local:advertise(Procedure, bidi,
                                       fun(S, _Args) -> Test ! {flood_ended, flood(S, 64 * ?KiB, Flood, 0)} end),
    {ok, Listener} = macula_bridge:listen_with(local_open(Procedure), #{port => 0, write_timeout_ms => 10_000}),
    {ok, Port} = macula_bridge:local_port(Listener),
    {ok, C} = connect(Port),
    try
        receive
            {flood_ended, Sent} -> ?assert(Sent < Flood)
        after 15_000 ->
            error(the_flood_was_never_cut_off)
        end
    after
        gen_tcp:close(C)
    end.

%% A stream chunk the bridge does not speak (a msgpack term, not tagged
%% bytes) is a broken peer: the connection is closed, not the chunk dropped.
a_chunk_that_is_not_bridge_bytes_closes_the_connection() ->
    Procedure = procedure(),
    ok = macula_stream_local:advertise(Procedure, bidi,
                                       fun(S, _Args) -> ok = macula_stream:send(S, #{a => 1}, msgpack),
                                                        timer:sleep(?EVENT_MS) end),
    {ok, Listener} = macula_bridge:listen_with(local_open(Procedure), #{port => 0}),
    {ok, Port} = macula_bridge:local_port(Listener),
    {ok, C} = connect(Port),
    ?assert(closed_by_bridge(C, ?EVENT_MS)).

%% The service behind the bridge does not answer: the local connection is
%% closed promptly, not left open with nothing behind it.
an_unreachable_service_closes_the_local_connection() ->
    {ok, L} = gen_tcp:listen(0, [binary]),
    {ok, Dead} = inet:port(L),
    ok = gen_tcp:close(L),
    #{port := Port} = bridge({{127, 0, 0, 1}, Dead}),
    {ok, C} = connect(Port),
    ?assert(closed_by_bridge(C, ?EVENT_MS)).

%% The provider refuses the stream (an unadmitted caller): the local
%% connection is closed at once, and the refusal is logged by name.
a_refused_connection_is_closed_at_once_and_logged() ->
    Procedure = procedure(),
    ok = macula_stream_local:advertise(Procedure, bidi,
                                       fun(S, _Args) ->
                                           macula_stream:abort(S, <<"unauthorized">>, <<"not authorized for this procedure">>)
                                       end),
    Handler = capture_log(),
    try
        {ok, Listener} = macula_bridge:listen_with(local_open(Procedure), #{port => 0}),
        {ok, Port} = macula_bridge:local_port(Listener),
        {ok, C} = connect(Port),
        ?assert(closed_by_bridge(C, ?EVENT_MS)),
        ?assert(logged(<<"unauthorized">>, ?EVENT_MS))
    after
        logger:remove_handler(Handler)
    end.

%% A connection with no traffic either way for `idle_ms' is closed.
an_idle_connection_is_closed() ->
    #{port := Port} = bridge(echo_service(), #{idle_ms => 300}),
    {ok, C} = connect(Port),
    ?assert(closed_by_bridge(C, 3_000)).

%% The client stops reading while the service writes more than the socket
%% buffers hold: the bridge's write to the client blocks. That write gives up
%% after `write_timeout_ms' and the connection is closed, instead of the
%% bridge's end sitting in a blocked send, deaf to the stream's end, forever.
%% No `idle_ms': the two are different limits.
a_client_that_stops_reading_mid_write_is_closed() ->
    Flood = crypto:strong_rand_bytes(32 * ?MiB),
    #{port := Port} = bridge(service(fun(S) -> gen_tcp:send(S, Flood) end),
                             #{write_timeout_ms => 500, window_bytes => 8 * ?MiB}),
    {ok, C} = connect(Port),
    {ok, CPort} = inet:port(C),
    ok = gen_tcp:send(C, <<"go">>),
    try
        Bridged = bridged_end_of(CPort, ?EVENT_MS),
        ?assert(is_port(Bridged)),
        ?assert(gone(Bridged, 5_000))
    after
        gen_tcp:close(C)
    end.

%% A small request and its small answer do not wait on Nagle's algorithm at
%% the bridge's end of the client connection.
the_bridged_end_writes_without_delay() ->
    #{port := Port} = bridge(echo_service()),
    {ok, C} = connect(Port),
    {ok, CPort} = inet:port(C),
    Bridged = bridged_end_of(CPort, ?EVENT_MS),
    ?assert(is_port(Bridged)),
    ?assertEqual({ok, [{nodelay, true}]}, inet:getopts(Bridged, [nodelay])),
    gen_tcp:close(C).

%% Who may connect is the procedure's auth policy: `serve_with/3' hands the
%% advertisement exactly the policy and the stations it was given, and none
%% of the bridge's own options.
serve_hands_its_policy_to_the_advertisement() ->
    Test = self(),
    Policy = {ucan_required, <<7:256>>},
    Advertise = fun(Handler, Opts) -> Test ! {advertised, is_function(Handler, 2), Opts}, ok end,
    ?assertEqual(ok, macula_bridge:serve_with(Advertise, {{127, 0, 0, 1}, 5432},
                                              #{auth => Policy, stations => [<<1:256>>],
                                                window_bytes => 64 * ?KiB, idle_ms => 1_000})),
    ?assertEqual({advertised, true, #{auth => Policy, stations => [<<1:256>>]}},
                 receive {advertised, _, _} = A -> A after ?EVENT_MS -> none end).

%% Serving without an explicit policy is refused, not defaulted to open, and
%% nothing is advertised.
serve_refuses_to_run_without_an_explicit_policy() ->
    Test = self(),
    Advertise = fun(_Handler, Opts) -> Test ! {advertised, Opts}, ok end,
    ?assertEqual({error, {auth, required}},
                 macula_bridge:serve_with(Advertise, {{127, 0, 0, 1}, 5432}, #{})),
    ?assertEqual({error, {auth, required}},
                 macula_bridge:serve(self(), <<7:256>>, <<"acme/pg">>, {{127, 0, 0, 1}, 5432}, #{})),
    ?assertEqual(none, receive {advertised, _} = A -> A after 200 -> none end).

%% The listening end opens each stream with its token (and dial timeout), so a
%% procedure served under a policy admits it; the stream is bidi and owned by
%% the connection's own process.
the_listening_end_presents_its_token() ->
    Test = self(),
    #{procedure := Procedure} = bridge(echo_service()),
    Open = fun(CallOpts) ->
               Test ! {opened_with, CallOpts, self()},
               macula_stream_local:open_stream(Procedure, #{}, CallOpts)
           end,
    {ok, Listener} = macula_bridge:listen_with(Open, #{port => 0, ucan_token => <<"a token">>,
                                                       dial_timeout_ms => 1_234, window_bytes => 64 * ?KiB}),
    {ok, Port} = macula_bridge:local_port(Listener),
    {ok, C} = connect(Port),
    receive
        {opened_with, CallOpts, Opener} ->
            ?assertEqual(#{ucan_token => <<"a token">>, dial_timeout_ms => 1_234, mode => bidi, owner => Opener},
                         CallOpts)
    after ?EVENT_MS ->
        error(never_opened)
    end,
    gen_tcp:close(C).

%% A window below the bridge's acknowledgement step would never be credited,
%% and one near the stream's inbox bound would trip it: both are refused.
a_window_outside_its_bounds_is_refused() ->
    Advertise = fun(_Handler, _Opts) -> ok end,
    Open = fun(_CallOpts) -> {error, unused} end,
    [?assertEqual({error, {window_bytes, W}},
                  macula_bridge:serve_with(Advertise, {{127, 0, 0, 1}, 1}, #{auth => open, window_bytes => W}))
     || W <- [1024, 64 * ?MiB, 0, not_a_size]],
    [?assertEqual({error, {window_bytes, W}}, macula_bridge:listen_with(Open, #{window_bytes => W}))
     || W <- [1024, 64 * ?MiB]].

%% A chunk size of zero would never shrink what is left to send, and one
%% larger than half the window would outrun its credit: refused, like a chunk
%% too small to be worth a signed frame or too large for one.
a_chunk_size_outside_its_bounds_is_refused() ->
    Advertise = fun(_Handler, _Opts) -> ok end,
    Open = fun(_CallOpts) -> {error, unused} end,
    [?assertEqual({error, {chunk_bytes, C}},
                  macula_bridge:serve_with(Advertise, {{127, 0, 0, 1}, 1}, #{auth => open, chunk_bytes => C}))
     || C <- [0, -1, 512, 2 * ?MiB, not_a_size]],
    ?assertEqual({error, {chunk_bytes, 64 * ?KiB}},
                 macula_bridge:listen_with(Open, #{window_bytes => 64 * ?KiB, chunk_bytes => 64 * ?KiB})).

%% A client that dies mid-send (a TCP reset) is not a finished request: the
%% service sees a reset too, never a clean end of what it was sent.
a_client_reset_reaches_the_service_as_a_reset() ->
    Test = self(),
    #{port := Port} = bridge(service(fun(S) -> Test ! {service_read, read_to_end(S, <<>>)} end,
                                     [{show_econnreset, true}])),
    {ok, C} = connect(Port),
    ok = gen_tcp:send(C, <<"half an upload">>),
    timer:sleep(200),
    ok = inet:setopts(C, [{linger, {true, 0}}]),
    ok = gen_tcp:close(C),
    ?assertMatch({service_read, {reset, _}},
                 receive {service_read, _} = R -> R after ?EVENT_MS -> none end).

%% And the other way: a service that dies is a reset at the client, not a
%% clean close it could take for a complete answer.
a_service_reset_reaches_the_client_as_a_reset() ->
    #{port := Port} = bridge(service(fun(S) ->
                                         ok = gen_tcp:send(S, <<"part">>),
                                         timer:sleep(200),
                                         ok = inet:setopts(S, [{linger, {true, 0}}]),
                                         gen_tcp:close(S)
                                     end)),
    {ok, C} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {show_econnreset, true}], ?EVENT_MS),
    ?assertMatch({reset, _}, read_to_end(C, <<>>)).

%% Credit is the receiver's to grant: a serving end with a small window keeps
%% its stream's unread bytes near that window, whatever the listening end is
%% configured with.
the_serving_end_grants_the_credit_it_receives_under() ->
    Max = max_unread_at_the_serving_end(#{window_bytes => 64 * ?KiB, chunk_bytes => 16 * ?KiB},
                                        #{window_bytes => 8 * ?MiB, chunk_bytes => 4 * ?KiB}),
    ?assert(Max > 16 * ?KiB),
    ?assert(Max =< 64 * ?KiB).

%% A served session's unread bytes count against its caller's budget, shared
%% by that caller's sessions: the serving end grants no more than one
%% session's share of it, so a stalled session never starves another.
the_serving_end_grants_no_more_than_its_session_share() ->
    with_env(max_served_inbox_bytes_per_caller, 2 * ?MiB,
             fun() ->
                 Max = max_unread_at_the_serving_end(#{window_bytes => 8 * ?MiB, chunk_bytes => 16 * ?KiB},
                                                     #{window_bytes => 8 * ?MiB, chunk_bytes => 4 * ?KiB}),
                 ?assert(Max > 64 * ?KiB),
                 ?assert(Max =< 128 * ?KiB)
             end).

%% A share too small to hold the smallest window is refused
%% when serving starts, not discovered as refused chunks later.
serve_refuses_a_session_share_too_small_to_credit() ->
    Advertise = fun(_Handler, _Opts) -> ok end,
    with_env(max_served_inbox_bytes_per_caller, 512 * ?KiB,
             fun() ->
                 ?assertEqual({error, {session_share, 32 * ?KiB}},
                              macula_bridge:serve_with(Advertise, {{127, 0, 0, 1}, 1}, #{auth => open}))
             end).

%% One socket read is at most one chunk (and at most the credit left), so a
%% chunk is the size asked for.
a_socket_read_is_at_most_one_chunk() ->
    #{port := Port} = bridge(echo_service(), #{chunk_bytes => 16 * ?KiB}),
    {ok, C} = connect(Port),
    {ok, CPort} = inet:port(C),
    Bridged = bridged_end_of(CPort, ?EVENT_MS),
    ?assert(is_port(Bridged)),
    ?assertEqual({ok, [{buffer, 16 * ?KiB}]}, inet:getopts(Bridged, [buffer])),
    gen_tcp:close(C).

%% A plain macula stream client, not a bridge, that sends its bytes and ends
%% its side (STREAM_END) still reads the service's answer: the end of its
%% input is a half-close at the service, as a FIN is.
a_plain_stream_client_that_ends_its_input_reads_the_answer() ->
    #{procedure := Procedure} = bridge(count_then_answer_service()),
    {ok, S} = macula_stream_local:open_stream(Procedure, #{}, #{mode => bidi}),
    ok = macula_stream:send(S, <<0, "abc">>),
    ok = macula_stream:close_send(S),
    ?assertEqual({chunk, <<0, "3">>}, next_data(S)).

%% A provider granting more credit than any window can hold is broken: the
%% connection is reset, not trusted.
credit_past_any_window_is_malformed() ->
    Procedure = procedure(),
    ok = macula_stream_local:advertise(Procedure, bidi,
                                       fun(S, _Args) -> ok = macula_stream:send(S, <<1, 16#7FFFFFFF:32>>),
                                                        timer:sleep(?EVENT_MS) end),
    {ok, Listener} = macula_bridge:listen_with(local_open(Procedure), #{port => 0}),
    {ok, Port} = macula_bridge:local_port(Listener),
    {ok, C} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {show_econnreset, true}], ?EVENT_MS),
    ?assertEqual({error, econnreset}, gen_tcp:recv(C, 0, ?EVENT_MS)).

%% Credit counts what a chunk costs the receiver's stream, not only its
%% bytes: a receiver that never reads nor grants holds at most the first
%% credit (64 KiB) as its inbox counts it, even in small chunks, each of
%% which the inbox charges about a hundred bytes more than it carries.
a_receiver_that_grants_nothing_holds_no_more_than_the_first_credit() ->
    Test = self(),
    Procedure = procedure(),
    ok = macula_stream_local:advertise(Procedure, bidi, fun(S, _Args) -> Test ! {holding, S}, timer:sleep(3_000) end),
    {ok, Listener} = macula_bridge:listen_with(local_open(Procedure), #{port => 0, chunk_bytes => ?KiB}),
    {ok, Port} = macula_bridge:local_port(Listener),
    {ok, C} = connect(Port),
    Sender = spawn(fun() -> gen_tcp:send(C, binary:copy(<<"z">>, 4 * ?MiB)) end),
    try
        S = receive {holding, Held} -> Held after ?EVENT_MS -> error(never_opened) end,
        timer:sleep(1_000),
        #{inbox_bytes := Unread} = macula_stream:info(S),
        ?assert(Unread > 32 * ?KiB),
        ?assert(Unread =< 64 * ?KiB)
    after
        exit(Sender, kill),
        gen_tcp:close(C)
    end.

%% A window given without a chunk size gets a chunk of at most half of it,
%% as an explicit chunk size must be.
the_default_chunk_is_at_most_half_the_window() ->
    #{port := Port} = bridge(echo_service(), #{window_bytes => 64 * ?KiB}),
    {ok, C} = connect(Port),
    {ok, CPort} = inet:port(C),
    Bridged = bridged_end_of(CPort, ?EVENT_MS),
    ?assert(is_port(Bridged)),
    ?assertEqual({ok, [{buffer, 32 * ?KiB}]}, inet:getopts(Bridged, [buffer])),
    gen_tcp:close(C).

%% The serving end's share of its caller's budget is read when serving
%% starts: a budget lowered afterwards changes nothing for the procedure
%% already served, rather than turning its first grant into nonsense.
the_serving_end_keeps_the_share_it_started_with() ->
    #{port := Port} = bridge(echo_service()),
    with_env(max_served_inbox_bytes_per_caller, 512 * ?KiB,
             fun() ->
                 {ok, C} = connect(Port),
                 ok = gen_tcp:send(C, <<"still bridged">>),
                 ?assertEqual({ok, <<"still bridged">>}, gen_tcp:recv(C, 13, ?EVENT_MS)),
                 gen_tcp:close(C)
             end).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% The most bytes the serving end's stream holds unread while a client sends
%% 64 MiB to a service that reads nothing for two seconds, behind a small
%% receive buffer, so the sockets between fill and the stream's inbox is what
%% holds the rest. Once the serving end blocks writing, its inbox holds the
%% window less what it wrote since its last grant (under 32 KiB) and the one
%% chunk it holds (the listening end's chunk size: keep it small to see the
%% window fill).
max_unread_at_the_serving_end(ServeOpts, ListenOpts) ->
    Test = self(),
    Procedure = procedure(),
    Service = service(fun(S) -> timer:sleep(2_000), echo(S) end, [{recbuf, 4096}]),
    ok = macula_bridge:serve_with(fun(Handler, _Opts) ->
                                      macula_stream_local:advertise(Procedure, bidi,
                                                                    fun(S, A) -> Test ! {served, S}, Handler(S, A) end)
                                  end, Service, ServeOpts#{auth => open, write_timeout_ms => 5_000}),
    {ok, Listener} = macula_bridge:listen_with(local_open(Procedure), ListenOpts#{port => 0, write_timeout_ms => 5_000}),
    {ok, Port} = macula_bridge:local_port(Listener),
    {ok, C} = connect(Port),
    Sender = spawn(fun() -> gen_tcp:send(C, binary:copy(<<"y">>, 64 * ?MiB)) end),
    try
        Served = receive {served, S} -> S after ?EVENT_MS -> error(never_served) end,
        max_unread(Served, 1_800, 0)
    after
        exit(Sender, kill),
        gen_tcp:close(C)
    end.

max_unread(_Stream, Ms, Max) when Ms =< 0 ->
    Max;
max_unread(Stream, Ms, Max) ->
    Unread = unread(catch macula_stream:info(Stream)),
    timer:sleep(10),
    max_unread(Stream, Ms - 10, max(Max, Unread)).

unread(#{inbox_bytes := Bytes}) -> Bytes;
unread(_Ended) -> 0.

with_env(Key, Value, Fun) ->
    Old = application:get_env(macula, Key),
    ok = application:set_env(macula, Key, Value),
    try Fun() after restore_env(Key, Old) end.

restore_env(Key, undefined) -> application:unset_env(macula, Key);
restore_env(Key, {ok, Value}) -> application:set_env(macula, Key, Value).

%% The bridge ended the connection. A close that races the client's receive,
%% or a socket closed with unread data, reaches the client as the peer's
%% reset instead of a FIN: the same closed connection (macula-io/macula#52).
closed_by_bridge(C, Ms) ->
    lists:member(gen_tcp:recv(C, 0, Ms), [{error, closed}, {error, econnreset}]).

%% What a socket reads until its end: `{eof, Bytes}' for a clean close,
%% `{reset, Bytes}' for a reset (with `show_econnreset').
read_to_end(S, Acc) ->
    case gen_tcp:recv(S, 0, ?EVENT_MS) of
        {ok, Data} -> read_to_end(S, <<Acc/binary, Data/binary>>);
        {error, closed} -> {eof, Acc};
        {error, econnreset} -> {reset, Acc};
        {error, Other} -> {Other, Acc}
    end.

%% The next data chunk on a plain stream, past the bridge's credit chunks.
next_data(S) ->
    case macula_stream:recv(S, ?EVENT_MS) of
        {chunk, <<1, _:32>>} -> next_data(S);
        Other -> Other
    end.

%% A bridge to `Service' over a local stream pair: the serving end advertised
%% under a fresh procedure, a listener on an ephemeral port. One set of
%% options for both ends, or the serving end's and the listening end's.
bridge(Service) ->
    bridge(Service, #{}).

bridge(Service, Opts) ->
    bridge(Service, Opts, Opts).

bridge(Service, ServeOpts, ListenOpts) ->
    Procedure = procedure(),
    ok = macula_bridge:serve_with(fun(Handler, _Opts) -> macula_stream_local:advertise(Procedure, bidi, Handler) end,
                                  Service, ServeOpts#{auth => open}),
    {ok, Listener} = macula_bridge:listen_with(local_open(Procedure), ListenOpts#{port => 0}),
    {ok, Port} = macula_bridge:local_port(Listener),
    #{port => Port, listener => Listener, procedure => Procedure}.

local_open(Procedure) ->
    fun(CallOpts) -> macula_stream_local:open_stream(Procedure, #{}, CallOpts) end.

procedure() ->
    <<"test/bridge_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>.

connect(Port) ->
    gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], ?EVENT_MS).

%% Tagged bridge bytes, sent without waiting for any credit, until `Total'
%% or the stream refuses one: how many bytes went out.
flood(_S, _Size, Total, Sent) when Sent >= Total ->
    Sent;
flood(S, Size, Total, Sent) ->
    flooded(macula_stream:send(S, <<0, (binary:copy(<<"x">>, Size))/binary>>), S, Size, Total, Sent).

flooded(ok, S, Size, Total, Sent) -> flood(S, Size, Total, Sent + Size);
flooded({error, _}, _S, _Size, _Total, Sent) -> Sent.

%% A TCP service that echoes what it reads.
echo_service() ->
    service(fun echo/1).

%% An echo that reads nothing for `Ms' first.
slow_echo_service(Ms) ->
    service(fun(S) -> timer:sleep(Ms), echo(S) end).

%% Reads to the end of its input, answers with the byte count, then closes.
count_then_answer_service() ->
    service(fun(S) ->
                {ok, All} = read_all(S, <<>>),
                ok = gen_tcp:send(S, integer_to_binary(byte_size(All))),
                gen_tcp:close(S)
            end).

%% Reads to the end of its input, answers with `Answer', then closes.
read_all_then_answer_service(Answer) ->
    service(fun(S) ->
                {ok, _All} = read_all(S, <<>>),
                ok = gen_tcp:send(S, Answer),
                gen_tcp:close(S)
            end).

service(Serve) ->
    service(Serve, []).

service(Serve, SocketOpts) ->
    Test = self(),
    Pid = spawn(fun() ->
                    {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {exit_on_close, false} | SocketOpts]),
                    {ok, P} = inet:port(L),
                    Test ! {service_port, P},
                    {ok, S} = gen_tcp:accept(L),
                    Serve(S)
                end),
    receive {service_port, P} -> {{127, 0, 0, 1}, P} after ?EVENT_MS -> error({no_service, Pid}) end.

echo(S) ->
    case gen_tcp:recv(S, 0) of
        {ok, Data} -> ok = gen_tcp:send(S, Data), echo(S);
        {error, _} -> gen_tcp:close(S)
    end.

read_all(S, Acc) ->
    case gen_tcp:recv(S, 0, ?EVENT_MS) of
        {ok, Data} -> read_all(S, <<Acc/binary, Data/binary>>);
        {error, closed} -> {ok, Acc}
    end.

%% The bridge's own socket for the client connection on local port `CPort'.
bridged_end_of(CPort, Ms) when Ms > 0 ->
    case [P || P <- erlang:ports(), erlang:port_info(P, name) =:= {name, "tcp_inet"},
               inet:peername(P) =:= {ok, {{127, 0, 0, 1}, CPort}}] of
        [P] -> P;
        [] -> timer:sleep(50), bridged_end_of(CPort, Ms - 50)
    end;
bridged_end_of(_CPort, _Ms) ->
    none.

gone(Port, Ms) when Ms > 0 ->
    erlang:port_info(Port) =:= undefined orelse (timer:sleep(50) =:= ok andalso gone(Port, Ms - 50));
gone(_Port, _Ms) ->
    false.

capture_log() ->
    Name = list_to_atom("bridge_capture_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = logger:add_handler(Name, ?MODULE, #{config => #{test => self()}, level => warning}),
    Name.

log(#{msg := Msg}, #{config := #{test := Test}}) ->
    Test ! {logged, iolist_to_binary(text(Msg))}.

text({Format, Args}) when is_list(Format) -> io_lib:format(Format, Args);
text({string, String}) -> String;
text({report, Report}) -> io_lib:format("~0p", [Report]).

logged(Needle, Ms) ->
    receive
        {logged, Text} -> binary:match(Text, Needle) =/= nomatch orelse logged(Needle, Ms)
    after Ms -> false
    end.
