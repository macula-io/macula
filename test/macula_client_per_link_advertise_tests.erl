%% #29: a pool of several links sends each station an advertisement
%% naming THAT station, and a link that comes back connected to another
%% station signs again naming it. The pool hands its links the unsigned
%% spec; each link signs per send.
%%
%% Real macula_station_link workers against unreachable seeds (as in
%% macula_link_respawn_replay_tests), each told it is connected to a
%% distinct station with this test process as its peer, so the frames
%% it sends arrive here.
-module(macula_client_per_link_advertise_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).
-define(PROCEDURE, <<"acme.count_v1">>).
-define(SEEDS, [#{host => <<"127.0.0.1">>, port => 1, expected_node_id => <<1:256>>},
                #{host => <<"127.0.0.1">>, port => 2, expected_node_id => <<2:256>>}]).

each_link_advertises_naming_its_own_station_test_() ->
    {timeout, 10,
     fun() ->
         Pool = pool(),
         {ok, Links} = macula_client:links(Pool),
         ?assertEqual(2, length(Links)),
         Stations = [connect_to(Pid, <<(20 + N):256>>)
                     || {N, #{pid := Pid}} <- lists:enumerate(Links)],
         ok = macula_client:advertise(Pool, ?REALM, ?PROCEDURE,
                                      fun(_) -> {ok, counted} end, open, spec()),
         ?assertEqual(lists:sort(Stations), lists:sort(sent_stations(2))),
         ok = macula_client:close(Pool)
     end}.

each_link_advertises_a_stream_naming_its_own_station_test_() ->
    {timeout, 10,
     fun() ->
         Pool = pool(),
         {ok, Links} = macula_client:links(Pool),
         Stations = [connect_to(Pid, <<(30 + N):256>>)
                     || {N, #{pid := Pid}} <- lists:enumerate(Links)],
         ok = macula_client:advertise_stream(Pool, ?REALM, ?PROCEDURE, bidi,
                                             fun(_, _) -> ok end, open, spec()),
         ?assertEqual(lists:sort(Stations), lists:sort(sent_stations(2))),
         ok = macula_client:close(Pool)
     end}.

%% The pool keeps the spec, not a signed advertisement, so a respawned
%% link connecting to a station none of the others knew signs one
%% naming it.
a_respawned_link_signs_for_the_station_it_reaches_test_() ->
    {timeout, 10,
     fun() ->
         Pool = pool(),
         ok = macula_client:advertise(Pool, ?REALM, ?PROCEDURE,
                                      fun(_) -> {ok, counted} end, open, spec()),
         {ok, [#{pid := OldPid} | _]} = macula_client:links(Pool),
         Mon = erlang:monitor(process, OldPid),
         exit(OldPid, kill),
         receive {'DOWN', Mon, process, OldPid, _} -> ok
         after 2_000 -> error(link_did_not_die)
         end,
         NewPid = wait_for_new_link(Pool, [OldPid], 30),
         Station = connect_to(NewPid, <<40:256>>),
         ?assertEqual([Station], sent_stations(1)),
         ok = macula_client:close(Pool)
     end}.

%% Withdrawing a streaming procedure sends each link a withdrawal and
%% keeps the pool alive.
unadvertising_a_stream_sends_the_withdrawal_test_() ->
    {timeout, 10,
     fun() ->
         Pool = pool(),
         {ok, Links} = macula_client:links(Pool),
         _ = [connect_to(Pid, <<(50 + N):256>>)
              || {N, #{pid := Pid}} <- lists:enumerate(Links)],
         ok = macula_client:advertise_stream(Pool, ?REALM, ?PROCEDURE, bidi,
                                             fun(_, _) -> ok end, open, spec()),
         _ = sent_stations(2),
         ?assertEqual(ok, macula_client:unadvertise_stream(Pool, ?REALM, ?PROCEDURE)),
         ?assertMatch([#{frame_type := unadvertise}, #{frame_type := unadvertise}],
                      sent_frames(2)),
         ?assert(is_process_alive(Pool)),
         ok = macula_client:close(Pool)
     end}.

%% The stored stream registration carries its mode: withdrawing it
%% must not take the pool down, whatever form its advertisement has.
unadvertising_a_pre_signed_stream_keeps_the_pool_test_() ->
    {timeout, 10,
     fun() ->
         Pool = pool(),
         _ = macula_client:advertise_stream(Pool, ?REALM, ?PROCEDURE, bidi,
                                            fun(_, _) -> ok end, open, <<1, 2, 3>>),
         ?assertEqual(ok, macula_client:unadvertise_stream(Pool, ?REALM, ?PROCEDURE)),
         ?assert(is_process_alive(Pool)),
         ok = macula_client:close(Pool)
     end}.

%% A map that is not an advertisement spec is refused in the caller,
%% before the pool stores it: stored, it would crash every link on
%% every handshake.
a_malformed_spec_is_refused_before_the_pool_keeps_it_test_() ->
    {timeout, 10,
     fun() ->
         Pool = pool(),
         Handler = fun(_) -> {ok, counted} end,
         Stream = fun(_, _) -> ok end,
         Bad = [#{}, #{authorization => #{}, not_after => 1},
                #{authorization => #{org_directory => <<"d">>, procedure_delegation => <<"p">>},
                  not_after => soon},
                #{authorization => #{org_directory => <<"d">>, procedure_delegation => <<"p">>},
                  not_after => 1, ttl_ms => 0}],
         [?assertError(function_clause,
                       macula_client:advertise(Pool, ?REALM, ?PROCEDURE, Handler, open, B))
          || B <- Bad],
         [?assertError(function_clause,
                       macula_client:advertise_stream(Pool, ?REALM, ?PROCEDURE, bidi, Stream, open, B))
          || B <- Bad],
         {ok, [#{pid := Link} | _]} = macula_client:links(Pool),
         [?assertError(function_clause,
                       macula_station_link:advertise(Link, ?REALM, ?PROCEDURE, Handler, open, B))
          || B <- Bad],
         [?assertError(function_clause,
                       macula_station_link:advertise_stream(Link, ?REALM, ?PROCEDURE, bidi, Stream, open, B))
          || B <- Bad],
         ok = macula_client:close(Pool)
     end}.

%% A registration with no advertisement (the local-only form the
%% distribution pool uses) still reaches every link's handler table.
a_registration_without_an_advertisement_reaches_every_link_test_() ->
    {timeout, 10,
     fun() ->
         Pool = pool(),
         {ok, Links} = macula_client:links(Pool),
         ?assertEqual(ok, macula_client:advertise(Pool, ?REALM, ?PROCEDURE,
                                                  fun(_) -> {ok, counted} end)),
         [?assert(maps:is_key({?REALM, ?PROCEDURE}, registered(procedures, Pid)))
          || #{pid := Pid} <- Links],
         ?assertEqual([], sent_frames(1)),
         ok = macula_client:close(Pool)
     end}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

pool() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Pool} = macula_client:connect(?SEEDS, #{node_identity => Key}),
    Pool.

spec() ->
    #{authorization => #{org_directory => <<"org directory wire">>,
                         procedure_delegation => <<"delegation wire">>},
      not_after => erlang:system_time(millisecond) + 3_600_000}.

%% Make this process the link's peer and complete its handshake as
%% `Station'.
connect_to(Pid, Station) ->
    Peer = self(),
    _ = sys:replace_state(Pid, fun(S) ->
            setelement(macula_station_link:state_field_index(peer_pid), S, Peer)
        end),
    Pid ! {macula_peering, connected, Peer, Station},
    Station = element(macula_station_link:state_field_index(peer_node_id), sys:get_state(Pid)),
    Station.

%% The serving stations named by the next `N' ADVERTISE frames.
sent_stations(N) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    [begin
         #{frame_type := advertise, advertisement := Encoded} = F,
         {ok, Record} = macula_record:verify(Encoded, Profile),
         maps:get(serving_station, macula_record:read_procedure_advertisement(Record))
     end || F <- sent_frames(N)].

%% Up to `N' ADVERTISE or UNADVERTISE frames sent to this process
%% within a second each; liveness probes are skipped.
sent_frames(0) -> [];
sent_frames(N) ->
    receive
        {'$gen_cast', {send_frame, #{frame_type := T} = Frame}}
          when T =:= advertise; T =:= unadvertise ->
            [Frame | sent_frames(N - 1)]
    after 1_000 -> []
    end.

registered(Field, Pid) ->
    element(macula_station_link:state_field_index(Field), sys:get_state(Pid)).

wait_for_new_link(_Pool, _Old, 0) ->
    error(no_new_link);
wait_for_new_link(Pool, Old, N) ->
    {ok, Links} = macula_client:links(Pool),
    case [P || #{pid := P} <- Links, is_pid(P), not lists:member(P, Old)] of
        [New | _] -> New;
        [] -> timer:sleep(100), wait_for_new_link(Pool, Old, N - 1)
    end.
