%%%-------------------------------------------------------------------
%%% @doc The in-flight reservation table behind the node-wide bound on
%%% received frame bytes, driven without a QUIC connection.
%%%
%%% A reservation ends when its handling ends, however the handling ends,
%%% or when the process holding it exits, and it ends at most once; handing
%%% it over, or forwarding it with a message, moves that duty to the new
%%% holder. Reserved bytes stay under each connection's limit and under the
%%% node limit, and a reader that does not fit waits and is admitted in
%%% arrival order, even behind a smaller request that would fit. Streams on a
%%% connection use at most three quarters of its limit, so its control stream
%%% keeps room to read. A station link's control stream is admitted past the
%%% node limit up to a hard ceiling of 1.25 times it, each refusal there is
%%% counted, and its control frames of at most 4 KiB are admitted at the
%%% ceiling from a small fixed reserve. The station role comes only from the
%%% node itself, at open or later with set_role/2. A reservation older than
%%% the maximum age is counted as expired and stays reserved until its holder
%%% ends. The table outlives the process that owns it, so an owner restart
%%% neither loses reserved bytes nor leaves them stuck. inflight_usage/0 reads
%%% the table directly and reports what a station needs to shed load before
%%% the limit. A stream or control stream reads frames no larger than what
%%% fits with its decode transient. The settings are checked when macula
%%% starts and when a connection opens: a per-caller session budget that is
%%% not below a connection's stream share is refused.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_inflight_tests).

-include_lib("eunit/include/eunit.hrl").

-define(KIB, 1024).
-define(MIB, (1 bsl 20)).
-define(FRAME_CAP, 16#FFFFFF).
-define(OWNER, macula_peering_inflight).
-define(EVENT_MS, 5_000).
-define(SESSION_BUDGET, max_served_inbox_bytes_per_caller).
-define(SETTINGS, [inflight_node_bytes, inflight_connection_bytes, inflight_station_reserve_bytes,
                   inflight_max_reservation_age_ms, ?SESSION_BUDGET]).

inflight_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
     fun(ok) -> ok end,
     [{Description, {timeout, 30, Test}} || {Description, Test} <-
     [{"handle_reserved releases when the handling returns, throws, errors or exits",
       fun handle_reserved_releases_whatever_the_handling_does/0},
      {"a handed over reservation is released only by its new holder",
       fun a_handed_over_reservation_is_released_only_by_its_new_holder/0},
      {"forward_reserved hands a reservation over and sends the message, so only the receiver releases it",
       fun forward_reserved_hands_over_then_sends/0},
      {"a holder that exits releases what it held",
       fun a_holder_that_exits_releases_what_it_held/0},
      {"a second release changes nothing",
       fun a_second_release_changes_nothing/0},
      {"a connection over its byte limit is refused until a release",
       fun a_connection_over_its_byte_limit_is_refused_until_a_release/0},
      {"streams use at most three quarters of a connection's limit, and the control stream keeps the rest",
       fun streams_leave_the_control_stream_a_quarter/0},
      {"the node limit bounds reserved bytes across connections",
       fun the_node_limit_bounds_reserved_bytes_across_connections/0},
      {"a station link's control stream is admitted up to 1.25 times the node limit, and a refusal there is counted",
       fun a_station_control_stream_is_admitted_up_to_the_ceiling/0},
      {"a station link's control frames of at most 4 KiB are admitted at the ceiling from the reserve, and counted",
       fun small_station_control_frames_use_the_reserve/0},
      {"set_role to station lets a connection's control stream use the ceiling",
       fun set_role_to_station_lets_a_connection_use_the_ceiling/0},
      {"waiting readers are admitted in arrival order, a smaller request included",
       fun waiting_readers_are_admitted_in_arrival_order/0},
      {"a reservation still counts after its connection ends, until it is released",
       fun a_reservation_still_counts_after_its_connection_ends/0},
      {"the table survives an owner restart, and holders are monitored again",
       fun the_table_survives_an_owner_restart/0},
      {"a holder that exits while the owner is down is released when the owner is back",
       fun a_holder_that_exits_while_the_owner_is_down_is_released/0},
      {"a reservation older than the maximum age is counted once as expired and stays reserved until its holder ends",
       fun an_expired_reservation_is_counted_and_kept_until_its_holder_ends/0},
      {"inflight_usage reports what shedding needs",
       fun inflight_usage_reports_what_shedding_needs/0},
      {"inflight_usage answers while the owner is suspended",
       fun inflight_usage_answers_while_the_owner_is_suspended/0},
      {"a stream or control stream reads frames no larger than what fits with its decode transient",
       fun the_frame_cap_is_the_largest_frame_that_fits/0},
      {"unset limits are 64 MiB for the node and for a connection",
       fun unset_limits_are_64_mib/0},
      {"a setting out of its range is a configuration error",
       fun a_bad_setting_is_a_configuration_error/0},
      {"a session budget not below the stream share of the connection limit is a configuration error",
       fun a_session_budget_not_below_the_stream_share_is_a_configuration_error/0},
      {"a connection whose stream share is not above the session budget is refused when it opens",
       fun a_connection_whose_stream_share_is_not_above_the_session_budget_is_refused/0}]]}.

%%%===================================================================
%%% Release
%%%===================================================================

handle_reserved_releases_whatever_the_handling_does() ->
    with_connection(64 * ?MIB, fun(Conn) ->
        Before = reserved(),
        Endings = [fun() -> done end,
                   fun() -> throw(thrown) end,
                   fun() -> error(failed) end,
                   fun() -> exit(stopped) end],
        Outcomes = [handled_with(Conn, Ending) || Ending <- Endings],
        During = Before + ?MIB,
        ?assertEqual({[{During, done}, {During, {throw, thrown}},
                       {During, {error, failed}}, {During, {exit, stopped}}],
                      Before},
                     {Outcomes, reserved()})
    end).

a_handed_over_reservation_is_released_only_by_its_new_holder() ->
    with_connection(64 * ?MIB, fun(Conn) ->
        Before = reserved(),
        Reservation = admitted(Conn, ?MIB),
        Test = self(),
        Worker = spawn_link(fun() -> handle_when_told(Test) end),
        ok = macula_peering:hand_over(Reservation, Worker),
        release(Reservation),
        AfterOldHolder = reserved(),
        Worker ! {handle, Reservation},
        receive {handled, Worker} -> ok after ?EVENT_MS -> error(worker_did_not_handle) end,
        ?assertEqual({Before + ?MIB, Before}, {AfterOldHolder, reserved()})
    end).

%% The worker waits for go before it handles what it was forwarded, so the
%% old holder's release is measured first.
forward_reserved_hands_over_then_sends() ->
    with_connection(64 * ?MIB, fun(Conn) ->
        Before = reserved(),
        Reservation = admitted(Conn, ?MIB),
        Test = self(),
        Worker = spawn_link(fun() -> handle_forwarded(Test) end),
        ok = macula_peering:forward_reserved(Reservation, Worker, {work, Reservation}),
        release(Reservation),
        AfterOldHolder = reserved(),
        Worker ! go,
        receive {handled, Worker} -> ok after ?EVENT_MS -> error(worker_did_not_handle) end,
        ?assertEqual({Before + ?MIB, Before}, {AfterOldHolder, reserved()})
    end).

a_holder_that_exits_releases_what_it_held() ->
    with_connection(64 * ?MIB, fun(Conn) ->
        Before = reserved(),
        {Holder, _Reservation} = held_elsewhere(Conn, ?MIB),
        Held = reserved(),
        stop_idle(Holder),
        ?assertEqual({Before + ?MIB, Before}, {Held, reserved_settles_at(Before)})
    end).

a_second_release_changes_nothing() ->
    with_connection(64 * ?MIB, fun(Conn) ->
        Before = reserved(),
        Released = admitted(Conn, ?MIB),
        Kept = admitted(Conn, 2 * ?MIB),
        release(Released),
        release(Released),
        Counted = reserved(),
        release(Kept),
        ?assertEqual({Before + 2 * ?MIB, Before}, {Counted, reserved()})
    end).

%%%===================================================================
%%% Limits and waiting
%%%===================================================================

a_connection_over_its_byte_limit_is_refused_until_a_release() ->
    with_env([], fun() ->
        with_connection(2 * ?MIB, fun(Conn) ->
            First = admitted(Conn, ?MIB + ?MIB div 2),
            Refused = macula_peering_inflight:try_admit(Conn, control, ?MIB, call),
            release(First),
            Second = admitted(Conn, ?MIB),
            release(Second),
            ?assertEqual(full, Refused)
        end)
    end).

%% On a 4 MiB connection, streams stop at 3 MiB and the control stream still
%% gets its quarter.
streams_leave_the_control_stream_a_quarter() ->
    with_env([], fun() ->
        with_connection(4 * ?MIB, fun(Conn) ->
            Streams = admitted(Conn, stream, 3 * ?MIB, stream_data),
            StreamRefused = macula_peering_inflight:try_admit(Conn, stream, ?MIB, stream_data),
            Control = macula_peering_inflight:try_admit(Conn, control, ?MIB, call),
            release(Streams),
            release_admitted(Control),
            ?assertMatch({full, {ok, _}}, {StreamRefused, Control})
        end)
    end).

the_node_limit_bounds_reserved_bytes_across_connections() ->
    with_env([{inflight_node_bytes, 3 * ?MIB}], fun() ->
        with_connections(2, 64 * ?MIB, fun([A, B]) ->
            AtA = admitted(A, 2 * ?MIB),
            Refused = macula_peering_inflight:try_admit(B, control, 2 * ?MIB, call),
            release(AtA),
            AtB = admitted(B, 2 * ?MIB),
            release(AtB),
            ?assertEqual(full, Refused)
        end)
    end).

%% Node limit 4 MiB, so the ceiling is 5 MiB. With 3 MiB reserved by a client
%% link, the station link's control stream gets 2 MiB more, and then no more;
%% the client link's control stream and the station link's streams get nothing
%% past the node limit.
a_station_control_stream_is_admitted_up_to_the_ceiling() ->
    with_env([{inflight_node_bytes, 4 * ?MIB}], fun() ->
        with_connections(1, 64 * ?MIB, fun([Client]) ->
            with_station_connection(64 * ?MIB, fun(Station) ->
                Before = usage_of(ceiling_pauses),
                AtClient = admitted(Client, 3 * ?MIB),
                AtStation = macula_peering_inflight:try_admit(Station, control, 2 * ?MIB, call),
                PastCeiling = macula_peering_inflight:try_admit(Station, control, ?MIB, call),
                ClientPastLimit = macula_peering_inflight:try_admit(Client, control, ?MIB, call),
                StreamPastLimit = macula_peering_inflight:try_admit(Station, stream, ?MIB, stream_data),
                Counted = usage_of(ceiling_pauses) - Before,
                release(AtClient),
                release_admitted(AtStation),
                ?assertMatch({{ok, _}, full, full, full, 1},
                             {AtStation, PastCeiling, ClientPastLimit, StreamPastLimit, Counted})
            end)
        end)
    end).

%% Node limit 4 MiB, ceiling 5 MiB, all of it held, and an 8 KiB reserve: a
%% station link's 2 KiB control frames are admitted from the reserve and
%% counted until it is used, a 16 KiB frame is not, and a client link gets
%% nothing from it.
small_station_control_frames_use_the_reserve() ->
    with_env([{inflight_node_bytes, 4 * ?MIB}, {inflight_station_reserve_bytes, 8 * ?KIB}], fun() ->
        with_connections(1, 64 * ?MIB, fun([Client]) ->
            with_station_connection(64 * ?MIB, fun(Station) ->
                Before = usage_of(reserve_admits),
                Ceiling = admitted(Station, 5 * ?MIB),
                Small = [macula_peering_inflight:try_admit(Station, control, 2 * ?KIB, swim_ping)
                         || _ <- lists:seq(1, 5)],
                Large = macula_peering_inflight:try_admit(Station, control, 16 * ?KIB, store),
                ClientSmall = macula_peering_inflight:try_admit(Client, control, 2 * ?KIB, ping),
                Counted = usage_of(reserve_admits) - Before,
                lists:foreach(fun release_admitted/1, Small),
                release(Ceiling),
                ?assertMatch({[{ok, _}, {ok, _}, {ok, _}, {ok, _}, full], full, full, 4},
                             {Small, Large, ClientSmall, Counted})
            end)
        end)
    end).

set_role_to_station_lets_a_connection_use_the_ceiling() ->
    with_env([{inflight_node_bytes, 4 * ?MIB}], fun() ->
        with_connection(64 * ?MIB, fun(Conn) ->
            AsClient = macula_peering_inflight:try_admit(Conn, control, 5 * ?MIB, call),
            ok = macula_peering_inflight:set_role(Conn, station),
            AsStation = macula_peering_inflight:try_admit(Conn, control, 5 * ?MIB, call),
            release_admitted(AsStation),
            ?assertMatch({full, {ok, _}}, {AsClient, AsStation})
        end)
    end).

%% With 1 MiB free, a 3 MiB request waits, and a 1 MiB request after it waits
%% too instead of passing it. A release admits both, the first one first.
waiting_readers_are_admitted_in_arrival_order() ->
    with_env([{inflight_node_bytes, 4 * ?MIB}], fun() ->
        with_connections(3, 64 * ?MIB, fun([Holder, First, Second]) ->
            Held = admitted(Holder, 3 * ?MIB),
            FirstQueued = macula_peering_inflight:admit(First, control, 3 * ?MIB, call),
            SecondQueued = macula_peering_inflight:admit(Second, control, ?MIB, call),
            release(Held),
            Admitted = [admitted_message(), admitted_message()],
            lists:foreach(fun({_Conn, R}) -> release(R) end, Admitted),
            ?assertEqual({queued, queued, [First, Second]},
                         {FirstQueued, SecondQueued, [Conn || {Conn, _R} <- Admitted]})
        end)
    end).

a_reservation_still_counts_after_its_connection_ends() ->
    Before = reserved(),
    Conn = spawn(fun idle/0),
    ok = macula_peering_inflight:open_connection(Conn, client_opts(64 * ?MIB)),
    Reservation = admitted(Conn, ?MIB),
    stop_idle(Conn),
    sync_owner(),
    AfterEnd = reserved(),
    release(Reservation),
    ?assertEqual({Before + ?MIB, Before}, {AfterEnd, reserved()}).

%%%===================================================================
%%% Owner restarts and expiry
%%%===================================================================

the_table_survives_an_owner_restart() ->
    with_connection(64 * ?MIB, fun(Conn) ->
        Before = reserved(),
        {Holder, _Reservation} = held_elsewhere(Conn, ?MIB),
        restart_owner(),
        AfterRestart = reserved(),
        stop_idle(Holder),
        ?assertEqual({Before + ?MIB, Before}, {AfterRestart, reserved_settles_at(Before)})
    end).

a_holder_that_exits_while_the_owner_is_down_is_released() ->
    with_connection(64 * ?MIB, fun(Conn) ->
        Before = reserved(),
        {Holder, _Reservation} = held_elsewhere(Conn, ?MIB),
        ok = supervisor:terminate_child(macula_peering_sup, ?OWNER),
        stop_idle(Holder),
        {ok, _Owner} = supervisor:restart_child(macula_peering_sup, ?OWNER),
        ?assertEqual(Before, reserved_settles_at(Before))
    end).

%% A reservation handed to a live process that never handles it: once older
%% than the maximum age it is counted as expired, once, and stays reserved.
%% Only the holder's end releases it.
an_expired_reservation_is_counted_and_kept_until_its_holder_ends() ->
    with_env([{inflight_max_reservation_age_ms, 200}], fun() ->
        with_connection(64 * ?MIB, fun(Conn) ->
            Before = reserved(),
            ExpiredBefore = usage_of(expired),
            Reservation = admitted(Conn, ?MIB),
            Holder = spawn(fun idle/0),
            ok = macula_peering:hand_over(Reservation, Holder),
            _ = eventually(fun() -> usage_of(expired) end, fun(E) -> E > ExpiredBefore end),
            timer:sleep(500),
            Expired = usage_of(expired) - ExpiredBefore,
            StillReserved = reserved(),
            stop_idle(Holder),
            ?assertEqual({1, Before + ?MIB, Before},
                         {Expired, StillReserved, reserved_settles_at(Before)})
        end)
    end).

%%%===================================================================
%%% Usage, frame caps and settings
%%%===================================================================

inflight_usage_reports_what_shedding_needs() ->
    with_env([{inflight_node_bytes, 8 * ?MIB}], fun() ->
        with_connections(2, 64 * ?MIB, fun([A, B]) ->
            Oldest = admitted(A, control, 5 * ?MIB, store),
            Newer = admitted(A, 2 * ?MIB),
            queued = macula_peering_inflight:admit(B, control, 4 * ?MIB, call),
            Usage = macula_peering:inflight_usage(),
            release(Oldest),
            release(Newer),
            {B, Waited} = admitted_message(),
            release(Waited),
            ?assertMatch(#{node_limit := 8 * ?MIB, reserved := 7 * ?MIB, waiting := 1,
                           reserved_by_role := #{client := 7 * ?MIB, station := 0},
                           ceiling_pauses := Pauses, reserve_admits := Admits, expired := Expired,
                           oldest := #{frame_type := store, age_ms := Age}}
                           when is_integer(Pauses) andalso is_integer(Admits)
                                andalso is_integer(Expired)
                                andalso is_integer(Age) andalso Age >= 0,
                         Usage)
        end)
    end).

inflight_usage_answers_while_the_owner_is_suspended() ->
    ok = sys:suspend(?OWNER),
    try
        {Micros, Usage} = timer:tc(macula_peering, inflight_usage, []),
        ?assertMatch({true, #{reserved := _}}, {Micros < 100_000, Usage})
    after
        sys:resume(?OWNER)
    end.

%% A 64 MiB connection reads frames up to the 16 MiB frame cap on both kinds;
%% an 8 MiB connection only frames whose wire bytes plus decode transient fit
%% in its 6 MiB stream share or its 8 MiB limit.
the_frame_cap_is_the_largest_frame_that_fits() ->
    with_env([], fun() ->
        with_connection(8 * ?MIB, fun(Small) ->
            with_connection(64 * ?MIB, fun(Large) ->
                ?assertEqual([true, true, true, true],
                             [largest_that_fits(Small, stream, 6 * ?MIB),
                              largest_that_fits(Small, control, 8 * ?MIB),
                              largest_that_fits(Large, stream, 48 * ?MIB),
                              largest_that_fits(Large, control, 64 * ?MIB)])
            end)
        end)
    end).

unset_limits_are_64_mib() ->
    with_env([], fun() ->
        Usage = macula_peering:inflight_usage(),
        ?assertMatch({#{node_limit := 64 * ?MIB}, 64 * ?MIB},
                     {Usage, macula_peering_inflight:connection_bytes()})
    end).

a_bad_setting_is_a_configuration_error() ->
    Bad = [{inflight_node_bytes, [<<"67108864">>, ?MIB - 1, 0, -1]},
           {inflight_connection_bytes, [<<"67108864">>, ?MIB - 1, 0, -1]},
           {inflight_station_reserve_bytes, [<<"8192">>, 4 * ?KIB - 1, -1]},
           {inflight_max_reservation_age_ms, [<<"200">>, 0, -1]}],
    Pairs = [{Setting, Value} || {Setting, Values} <- Bad, Value <- Values],
    ?assertEqual([{bad_config, {macula, Setting, Value}} || {Setting, Value} <- Pairs],
                 [limits_check([Pair]) || Pair <- Pairs]).

%% A 64 MiB connection leaves streams 48 MiB, so a 48 MiB session budget is
%% refused and a budget just below it is accepted.
a_session_budget_not_below_the_stream_share_is_a_configuration_error() ->
    AtShare = [{inflight_connection_bytes, 64 * ?MIB}, {?SESSION_BUDGET, 48 * ?MIB}],
    BelowShare = [{inflight_connection_bytes, 64 * ?MIB}, {?SESSION_BUDGET, 48 * ?MIB - 1}],
    ?assertEqual({{bad_config, #{?SESSION_BUDGET => 48 * ?MIB,
                                 inflight_connection_bytes => 64 * ?MIB}},
                  accepted},
                 {limits_check(AtShare), limits_check(BelowShare)}).

%% A 16 MiB session budget against connections of 20 MiB (15 MiB for streams)
%% and 24 MiB (18 MiB for streams).
a_connection_whose_stream_share_is_not_above_the_session_budget_is_refused() ->
    with_env([{?SESSION_BUDGET, 16 * ?MIB}], fun() ->
        Small = spawn(fun idle/0),
        Large = spawn(fun idle/0),
        Refused = macula_peering_inflight:open_connection(Small, client_opts(20 * ?MIB)),
        Opened = macula_peering_inflight:open_connection(Large, client_opts(24 * ?MIB)),
        stop_idle(Small),
        stop_idle(Large),
        ?assertEqual({{error, {bad_config, #{?SESSION_BUDGET => 16 * ?MIB,
                                             connection_bytes => 20 * ?MIB}}},
                      ok},
                     {Refused, Opened})
    end).

%%%===================================================================
%%% Helpers
%%%===================================================================

reserved() ->
    usage_of(reserved).

usage_of(Key) ->
    maps:get(Key, macula_peering:inflight_usage()).

release(Reservation) ->
    done = macula_peering:handle_reserved(Reservation, fun() -> done end).

release_admitted({ok, Reservation}) ->
    release(Reservation);
release_admitted(full) ->
    ok.

admitted(Conn, Bytes) ->
    admitted(Conn, control, Bytes, call).

admitted(Conn, Kind, Bytes, FrameType) ->
    {ok, Reservation} = macula_peering_inflight:try_admit(Conn, Kind, Bytes, FrameType),
    Reservation.

%% The next admission of a waiting reader: its connection and reservation.
admitted_message() ->
    receive
        {macula_peering_inflight, admitted, Conn, Reservation} -> {Conn, Reservation}
    after ?EVENT_MS ->
        error(not_admitted)
    end.

%% Whether Conn's frame cap for Kind fits Room with its decode transient, and
%% is either the frame cap itself or the largest frame that fits.
largest_that_fits(Conn, Kind, Room) ->
    Cap = macula_peering_inflight:frame_cap(Conn, Kind),
    fits(Cap, Room) andalso (Cap =:= ?FRAME_CAP orelse not fits(Cap + 1, Room)).

fits(Wire, Room) ->
    Wire + macula_peering_inflight:decode_transient_bytes(Wire) =< Room.

%% What reserved/0 said during the handling, and how the handling ended.
handled_with(Conn, Ending) ->
    Reservation = admitted(Conn, ?MIB),
    Test = self(),
    Handling = fun() -> Test ! {during, reserved()}, Ending() end,
    Ended = try
                macula_peering:handle_reserved(Reservation, Handling)
            catch
                Class:Reason -> {Class, Reason}
            end,
    receive {during, During} -> {During, Ended} after 0 -> {not_run, Ended} end.

handle_when_told(Test) ->
    receive
        {handle, Reservation} ->
            release(Reservation),
            Test ! {handled, self()}
    end.

handle_forwarded(Test) ->
    receive
        {work, Reservation} ->
            receive go -> ok end,
            release(Reservation),
            Test ! {handled, self()}
    end.

%% A process holding a reservation of Bytes on Conn until it is stopped. A
%% holder that ends before it holds fails the test with its reason.
held_elsewhere(Conn, Bytes) ->
    Test = self(),
    {Holder, Mon} = spawn_monitor(fun() -> hold(Conn, Bytes, Test) end),
    receive
        {holding, Holder, Reservation} ->
            true = erlang:demonitor(Mon, [flush]),
            {Holder, Reservation};
        {'DOWN', Mon, process, Holder, Reason} ->
            error({holder_ended, Reason})
    after ?EVENT_MS ->
        error(not_holding)
    end.

hold(Conn, Bytes, Test) ->
    Test ! {holding, self(), admitted(Conn, Bytes)},
    idle().

client_opts(Limit) ->
    #{connection_bytes => Limit, role => client}.

with_connection(Limit, Fun) ->
    with_connections(1, Limit, fun([Conn]) -> Fun(Conn) end).

%% Runs Fun with N stand-in client connection processes, each opened with Limit.
with_connections(N, Limit, Fun) ->
    with_opened([spawn(fun idle/0) || _ <- lists:seq(1, N)], client_opts(Limit), Fun).

with_station_connection(Limit, Fun) ->
    with_opened([spawn(fun idle/0)], #{connection_bytes => Limit, role => station},
                fun([Conn]) -> Fun(Conn) end).

with_opened(Conns, Opts, Fun) ->
    try
        lists:foreach(fun(C) -> ok = macula_peering_inflight:open_connection(C, Opts) end, Conns),
        Fun(Conns)
    after
        lists:foreach(fun stop_idle/1, Conns)
    end.

%% Runs Fun with the in-flight settings unset except for Settings. The session
%% budget is 256 KiB unless Settings name it, so connections of a few MiB open.
with_env(Settings, Fun) ->
    lists:foreach(fun(Key) -> application:unset_env(macula, Key) end, ?SETTINGS),
    Applied = maps:to_list(maps:merge(#{?SESSION_BUDGET => 256 * ?KIB}, maps:from_list(Settings))),
    lists:foreach(fun({Key, Value}) -> application:set_env(macula, Key, Value) end, Applied),
    try
        Fun()
    after
        lists:foreach(fun(Key) -> application:unset_env(macula, Key) end, ?SETTINGS)
    end.

limits_check(Settings) ->
    with_env(Settings, fun() ->
        try macula_peering_inflight:check_limits() of
            ok -> accepted
        catch
            error:Reason -> Reason
        end
    end).

reserved_settles_at(Expected) ->
    eventually(fun reserved/0, fun(Seen) -> Seen =:= Expected end).

%% Fun's value once Pred accepts it, or its value after EVENT_MS.
eventually(Fun, Pred) ->
    eventually(Fun, Pred, erlang:monotonic_time(millisecond) + ?EVENT_MS).

eventually(Fun, Pred, Deadline) ->
    Value = Fun(),
    accepted(Pred(Value) orelse erlang:monotonic_time(millisecond) >= Deadline,
             Value, Fun, Pred, Deadline).

accepted(true, Value, _Fun, _Pred, _Deadline) ->
    Value;
accepted(false, _Value, Fun, Pred, Deadline) ->
    timer:sleep(10),
    eventually(Fun, Pred, Deadline).

%% Kills the owner and waits until its supervisor has started a new one.
restart_owner() ->
    Old = whereis(?OWNER),
    exit(Old, kill),
    New = eventually(fun() -> whereis(?OWNER) end,
                     fun(Pid) -> is_pid(Pid) andalso Pid =/= Old end),
    true = is_pid(New) andalso New =/= Old,
    sync_owner().

%% Returns once the owner has handled the messages already sent to it.
sync_owner() ->
    _ = sys:get_state(?OWNER),
    ok.

idle() ->
    receive stop -> ok end.

stop_idle(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! stop,
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after ?EVENT_MS ->
        erlang:demonitor(Ref, [flush]),
        exit({idle_not_stopping, Pid})
    end.
