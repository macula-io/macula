%% EUnit tests for the level a station link's diagnostic events log at. A link that loses its connection, by a
%% disconnect notice or by its peering process exiting, logs that event at notice, which OTP's default primary level
%% lets through, so a lost connection shows in a node's log without raising the level. The link's other events stay
%% at info.
-module(macula_station_link_diagnostics_tests).

-include_lib("eunit/include/eunit.hrl").

%% The capture handler's callback.
-export([log/2]).

-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(HANDLER, macula_station_link_diagnostics_capture).
-define(EVENT_MS, 2_000).

diagnostics_test_() ->
    [{"a link whose connection closes logs the disconnect at notice",
      {spawn, fun a_disconnect_logs_at_notice/0}},
     {"a link whose peering process exits logs the exit at notice",
      {spawn, fun a_peering_exit_logs_at_notice/0}},
     {"a link's failed connect stays at info",
      {spawn, fun a_failed_connect_stays_at_info/0}},
     {"a disconnect whose reason holds a key logs no form of it at notice, with the redaction filter removed",
      {spawn, fun a_disconnect_reason_logs_only_its_name/0}},
     {"a peering exit whose reason holds a key logs no form of it at notice, with the redaction filter removed",
      {spawn, fun a_peering_exit_reason_logs_only_its_name/0}},
     {"a failed connect whose reason holds a key logs no form of it, with the redaction filter removed",
      {spawn, fun a_failed_connect_reason_logs_only_its_name/0}}].

%% A connect that fails with a reason holding a key logs its event at info with the reason's name only: with the
%% redaction filter removed, no connect_failed event's term or formatted text holds a form of the key.
a_failed_connect_reason_logs_only_its_name() ->
    Key = key(),
    Events = unfiltered(fun() ->
                            captured(fun() ->
                                         {ok, Link} = macula_station_link:start_link(
                                                        with_link_keys(#{seed => #{host => <<"127.0.0.1">>, port => 1},
                                                                         connect => fun(_PeeringOpts) ->
                                                                                        {error, crash_reason(Key)}
                                                                                    end})),
                                         unlink(Link),
                                         ok = macula_station_link:stop(Link)
                                     end)
                        end),
    Failed = on_topic(<<"_macula.station_link.connect_failed">>, Events),
    ?assertMatch([_ | _], Failed),
    ?assertEqual([{info, []}], lists:usort([{maps:get(level, Event), leaked(Event, Key)} || Event <- Failed])).

%% A reason that holds a key in a stack frame's arguments, as a crash reason can, reaches the default level as its name
%% only: with the redaction filter removed, neither the event's term nor its formatted text holds a form of the key.
a_disconnect_reason_logs_only_its_name() ->
    Key = key(),
    Events = unfiltered(fun() ->
                            captured(fun() ->
                                         {Link, Peer} = link_with_peer(),
                                         Mon = erlang:monitor(process, Link),
                                         Link ! {macula_peering, disconnected, Peer, crash_reason(Key)},
                                         ok = ended(Mon, Link),
                                         Peer ! stop
                                     end)
                        end),
    [Event] = on_topic(<<"_macula.station_link.disconnected">>, Events),
    ?assertEqual({notice, []}, {maps:get(level, Event), leaked(Event, Key)}).

a_peering_exit_reason_logs_only_its_name() ->
    Key = key(),
    Events = unfiltered(fun() ->
                            captured(fun() ->
                                         {Link, Peer} = link_with_peer(),
                                         Mon = erlang:monitor(process, Link),
                                         Peer ! {exit, crash_reason(Key)},
                                         ok = ended(Mon, Link)
                                     end)
                        end),
    [Event] = on_topic(<<"_macula.station_link.peering_exit">>, Events),
    ?assertEqual({notice, []}, {maps:get(level, Event), leaked(Event, Key)}).

a_disconnect_logs_at_notice() ->
    Events = captured(fun() ->
                          {Link, Peer} = link_with_peer(),
                          Mon = erlang:monitor(process, Link),
                          Link ! {macula_peering, disconnected, Peer, peer_closed},
                          ok = ended(Mon, Link),
                          Peer ! stop
                      end),
    ?assertEqual([notice], levels(<<"_macula.station_link.disconnected">>, Events)).

a_peering_exit_logs_at_notice() ->
    Events = captured(fun() ->
                          {Link, Peer} = link_with_peer(),
                          Mon = erlang:monitor(process, Link),
                          Peer ! {exit, peering_worker_ended},
                          ok = ended(Mon, Link)
                      end),
    ?assertEqual([notice], levels(<<"_macula.station_link.peering_exit">>, Events)).

a_failed_connect_stays_at_info() ->
    Events = captured(fun() ->
                          {Link, Peer} = link_with_peer(),
                          ok = macula_station_link:stop(Link),
                          Peer ! stop
                      end),
    ?assertEqual([info], lists:usort(levels(<<"_macula.station_link.connect_failed">>, Events))).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% The log events of every level while Fun runs. The primary level is opened for the capture and put back after.
captured(Fun) ->
    {ok, _} = application:ensure_all_started(macula),
    #{level := Primary} = logger:get_primary_config(),
    ok = logger:set_primary_config(level, all),
    ok = logger:add_handler(?HANDLER, ?MODULE, #{level => all, config => #{test => self()}}),
    try
        Fun(),
        drained([])
    after
        _ = logger:remove_handler(?HANDLER),
        ok = logger:set_primary_config(level, Primary)
    end.

log(Event, #{config := #{test := Test}}) ->
    Test ! {captured, Event}.

drained(Events) ->
    receive
        {captured, Event} -> drained([Event | Events])
    after 200 ->
        lists:reverse(Events)
    end.

%% The level of each captured diagnostic event on Topic, in order.
levels(Topic, Events) ->
    [Level || #{level := Level, msg := {report, #{event := On}}} <- Events, On =:= Topic].

%% The captured diagnostic events on Topic, in order.
on_topic(Topic, Events) ->
    [Event || #{msg := {report, #{event := On}}} = Event <- Events, On =:= Topic].

%% A node identity key in the node's profile.
key() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    Key.

%% An exit reason as a crash leaves one, with the key among a stack frame's arguments.
crash_reason(Key) ->
    {function_clause, [{macula_peering_conn, handle_frame, [Key, <<"frame">>],
                        [{file, "macula_peering_conn.erl"}, {line, 1}]}]}.

%% Runs Fun with the key redaction filter removed, checks the filter stayed removed, and installs it again after.
unfiltered(Fun) ->
    {ok, _} = application:ensure_all_started(macula),
    _ = logger:remove_primary_filter(macula_key_redaction),
    try
        Result = Fun(),
        ?assertNot(redaction_installed()),
        Result
    after
        _ = macula_node_keys:install_log_redaction()
    end.

redaction_installed() ->
    #{filters := Filters} = logger:get_primary_config(),
    lists:keymember(macula_key_redaction, 1, Filters).

%% The forms of Key's private halves in an event's term or in its formatted text.
leaked(Event, Key) ->
    Text = iolist_to_binary(logger_formatter:format(Event, #{single_line => true, legacy_header => false})),
    macula_key_leak_sample:found([term_to_binary(Event), Text], Key).

%% A link that believes it is connected, whose connect never dials, with a process of this test as its peering
%% connection, linked to the link as a peering process is.
link_with_peer() ->
    {ok, Link} = macula_station_link:start_link(
                   with_link_keys(#{seed => #{host => <<"127.0.0.1">>, port => 1},
                                    connect => fun(_PeeringOpts) -> {error, not_dialed_here} end})),
    unlink(Link),
    Peer = spawn(fun() ->
                     true = link(Link),
                     receive
                         {exit, Reason} -> exit(Reason);
                         stop -> ok
                     end
                 end),
    _ = sys:replace_state(Link, fun(S) ->
                                    setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Peer), <<7:256>>)
                                end),
    {Link, Peer}.

ended(Mon, Link) ->
    receive
        {'DOWN', Mon, process, Link, _Reason} -> ok
    after ?EVENT_MS ->
        erlang:error(link_still_running)
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
