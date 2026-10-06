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

%% macula#33: the advertisement a link sends its station is the record a
%% resolving caller finds, signed once: each link puts in the DHT exactly the
%% bytes of the ADVERTISE it sends, naming its own station, so a caller seals
%% to the key the station admitted.
each_link_puts_the_advertisement_it_sends_in_the_dht_test_() ->
    {timeout, 10,
     fun() ->
         Pool = pool(),
         {ok, Links} = macula_client:links(Pool),
         Stations = [connect_to(Pid, <<(50 + N):256>>)
                     || {N, #{pid := Pid}} <- lists:enumerate(Links)],
         ok = macula_client:advertise(Pool, ?REALM, ?PROCEDURE,
                                      fun(_) -> {ok, counted} end, open, spec()),
         {Advertised, Put} = advertised_and_put(4),
         ?assertEqual(2, length(Advertised)),
         ?assertEqual(lists:sort(Advertised), lists:sort(Put)),
         ?assertEqual(lists:sort(Stations), lists:sort([serving_station(A) || A <- Put])),
         ok = macula_client:close(Pool)
     end}.

%% The withdrawal on unadvertise is put in the DHT too, as the same bytes the
%% UNADVERTISE carries: signed later than the advertisement, it replaces the
%% provider's entry (one per signer per slot, D28), so a caller stops resolving
%% a withdrawn provider at once instead of when its record expires.
unadvertising_puts_the_withdrawal_in_the_dht_test_() ->
    {timeout, 10,
     fun() ->
         Pool = pool(),
         {ok, Links} = macula_client:links(Pool),
         _ = [connect_to(Pid, <<(60 + N):256>>)
              || {N, #{pid := Pid}} <- lists:enumerate(Links)],
         ok = macula_client:advertise(Pool, ?REALM, ?PROCEDURE,
                                      fun(_) -> {ok, counted} end, open, spec()),
         {[_, _], AdsPut} = advertised_and_put(4),
         ok = macula_client:unadvertise(Pool, ?REALM, ?PROCEDURE),
         {Withdrawn, Put} = withdrawn_and_put(4),
         ?assertEqual(2, length(Withdrawn)),
         ?assertEqual(lists:sort(Withdrawn), lists:sort(Put)),
         %% It replaces the advertisement because its version is later: the
         %% DHT keeps a signer's later version (macula_dht_slots, D28).
         [?assert(version(W) > version(A)) || W <- Withdrawn, A <- AdsPut],
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

%% A spec's ttl_ms lies within what the signer accepts: from a second up to the advertisement type's own maximum
%% lifetime. Past it every signing would be refused while advertise/6 answered ok, and the caller would believe it was
%% routable.
a_spec_ttl_is_bounded_by_the_advertisement_type_test_() ->
    {timeout, 10,
     fun() ->
         Pool = pool(),
         Max = macula_record:procedure_advertisement_max_lifetime_ms(),
         Spec = fun(Ttl) -> (spec())#{ttl_ms => Ttl} end,
         Handler = fun(_) -> {ok, counted} end,
         [?assertError(function_clause,
                       macula_client:advertise(Pool, ?REALM, ?PROCEDURE, Handler, open, Spec(Ttl)))
          || Ttl <- [999, Max + 1]],
         [?assertMatch(ok, macula_client:advertise(Pool, ?REALM, ?PROCEDURE, Handler, open, Spec(Ttl)))
          || Ttl <- [1_000, Max]],
         ok = macula_client:close(Pool)
     end}.

%% A node's own namespace takes a spec with no authorization and no bound (D25 item 6, revised 2026-09-24); an org
%% procedure never does, so it cannot be advertised without its chain.
an_own_namespace_spec_is_for_a_tilde_procedure_only_test_() ->
    {timeout, 10,
     fun() ->
         Pool = pool(),
         Handler = fun(_) -> {ok, counted} end,
         Own = <<"~", (binary:encode_hex(<<5:256>>, lowercase))/binary, "/ring">>,
         ?assertEqual(ok, macula_client:advertise(Pool, ?REALM, Own, Handler, open, #{})),
         ?assertEqual(ok, macula_client:advertise(Pool, ?REALM, Own, Handler, open, #{ttl_ms => 60_000})),
         ?assertError(function_clause, macula_client:advertise(Pool, ?REALM, ?PROCEDURE, Handler, open, #{})),
         ?assertError(function_clause, macula_client:advertise(Pool, ?REALM, Own, Handler, open,
                                                               #{not_after => 1})),
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

%% Every test here runs in one process and is its links' peer, so frames an
%% earlier test's links sent and it did not read (its `_dht.put_record'
%% CALLs, say) would be read as this one's: each test starts with none.
pool() ->
    ok = flushed(),
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Pool} = macula_client:connect(?SEEDS, #{node_identity => Key}),
    Pool.

spec() ->
    #{authorization => #{org_directory => <<"org directory wire">>,
                         procedure_delegation => <<"delegation wire">>},
      not_after => erlang:system_time(millisecond) + 3_600_000}.

%% An earlier test's links put from workers, so a put can still land after
%% its pool closed: drained until the mailbox has been quiet for 200 ms.
flushed() ->
    receive {'$gen_cast', _} -> flushed() after 200 -> ok end.

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
        {'$gen_cast', {send_frame, _, #{frame_type := T} = Frame}}
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

%% The advertisement bytes of the next ADVERTISE frames and the record bytes
%% of the next `_dht.put_record' CALLs, `N' frames in all.
advertised_and_put(N) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    lists:foldl(fun(Frame, {Ads, Puts}) -> sorted_frame(Frame, Profile, Ads, Puts) end,
                {[], []}, ads_and_puts(N)).

sorted_frame(#{frame_type := advertise, advertisement := Encoded}, _Profile, Ads, Puts) ->
    {[Encoded | Ads], Puts};
sorted_frame(#{frame_type := unadvertise, withdrawal := Encoded}, _Profile, Ads, Puts) ->
    {[Encoded | Ads], Puts};
sorted_frame(#{frame_type := call} = Frame, Profile, Ads, Puts) ->
    {ok, #{payload := Wire}} = macula_frame:verify_request(Frame, Profile),
    {Ads, [Wire | Puts]}.

%% The withdrawal bytes of the next UNADVERTISE frames and the record bytes of
%% the next `_dht.put_record' CALLs, `N' frames in all.
withdrawn_and_put(N) ->
    advertised_and_put(N).

ads_and_puts(0) -> [];
ads_and_puts(N) ->
    receive
        {'$gen_cast', {send_frame, _, #{frame_type := T} = Frame}}
          when T =:= advertise; T =:= unadvertise ->
            [Frame | ads_and_puts(N - 1)];
        {'$gen_cast', {send_frame, _, #{frame_type := call} = Frame}} ->
            put_call(Frame, N)
    after 1_000 -> []
    end.

%% A CALL counts when it is a `_dht.put_record'; a liveness probe does not.
put_call(Frame, N) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, #{procedure := Procedure}} = macula_frame:verify_request(Frame, Profile),
    put_call_named(Procedure, Frame, N).

put_call_named(Procedure, Frame, N) when Procedure =:= <<"_dht.put_record">>;
                                         Procedure =:= {text, <<"_dht.put_record">>} ->
    [Frame | ads_and_puts(N - 1)];
put_call_named(_Probe, _Frame, N) ->
    ads_and_puts(N).

serving_station(Encoded) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Record} = macula_record:verify(Encoded, Profile),
    maps:get(serving_station, macula_record:read_procedure_advertisement(Record)).

version(Encoded) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, #{version := Version}} = macula_record:verify(Encoded, Profile),
    Version.

%% A put is ok when the station answers ok, which crosses the wire as text;
%% anything else is named.
a_put_reply_reads_ok_only_when_the_station_says_ok_test() ->
    ?assertEqual(ok, macula_station_link:wire_put({ok, ok})),
    ?assertEqual(ok, macula_station_link:wire_put({ok, {text, <<"ok">>}})),
    ?assertEqual({error, {unexpected_reply, {text, <<"stored 0">>}}},
                 macula_station_link:wire_put({ok, {text, <<"stored 0">>}})),
    ?assertEqual({error, timeout}, macula_station_link:wire_put({error, timeout})).
