%% A direct-dial advertisement names where the procedure is registered: with
%% `stations', the first of them the pool is connected to, so a caller dials a
%% station the handler is registered at.
-module(macula_direct_dial_publish_stations_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).
-define(A, <<1:256>>).
-define(B, <<2:256>>).
-define(C, <<3:256>>).

the_record_names_a_station_of_the_set_test() ->
    ?assertEqual({ok, ?B}, published(#{stations => [?C, ?B]}, [linked(?A, true), linked(?B, true)])).

a_station_of_the_set_not_yet_connected_is_passed_over_test() ->
    ?assertEqual({ok, ?B}, published(#{stations => [?A, ?B]}, [linked(?A, false), linked(?B, true)])).

no_connected_station_of_the_set_puts_nothing_test() ->
    ?assertEqual({error, no_healthy_link}, published(#{stations => [?C]}, [linked(?A, true)])).

without_stations_the_first_connected_station_is_named_test() ->
    ?assertEqual({ok, ?A}, published(#{}, [linked(?A, true), linked(?B, true)])).

%%%===================================================================
%%% Helpers
%%%===================================================================

linked(Station, Connected) -> #{node_id => Station, connected => Connected}.

%% Publish an own-namespace advertisement over `Links', and answer the station
%% the record it put names.
published(Opts, Links) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    Procedure = <<"~", (binary:encode_hex(macula_node_keys:key_id(Key), lowercase))/binary, "/echo">>,
    Test = self(),
    Io = #{links => fun(_Pool) -> {ok, Links} end,
           put_record => fun(_Pool, Record) -> Test ! {put, Record}, ok end},
    named(macula_direct_dial:publish_advertisement(self(), ?REALM, Procedure, Key, Opts#{dial_io => Io})).

named(ok) ->
    receive
        {put, Record} -> {ok, maps:get(serving_station, macula_record:read_procedure_advertisement(Record))}
    after 1_000 -> error(nothing_put)
    end;
named({error, _} = Error) ->
    receive {put, _} -> error(put_on_error) after 100 -> Error end.
