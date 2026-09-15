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
      {spawn, fun a_failed_connect_stays_at_info/0}}].

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
    Opts#{node_identity => fun() -> Key end, issuer => Issuer, expected_node_id => <<1:256>>}.
