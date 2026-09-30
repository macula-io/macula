%% The station endpoint vectors (test/vectors/station_endpoint_v1.json): endpoint records a station signed once, in
%% both crypto profiles, one naming its release and one from before the field, each with what every SDK's reader must
%% get back at the vector's clock. The file is committed as generated, since ML-DSA-87 and RSA-PSS signing are
%% randomized; this module re-derives every reading from it on each run, so the file can never drift from
%% macula_record.
-module(macula_station_endpoint_vectors_tests).

-include_lib("eunit/include/eunit.hrl").

vectors_test_() ->
    Profiles = maps:get(<<"profiles">>, vectors()),
    [{"both profiles", ?_assertEqual([<<"pq_hybrid">>, <<"pq_pure">>], lists:sort(maps:keys(Profiles)))}]
    ++ [{binary_to_list(<<Name/binary, " ", (maps:get(<<"name">>, C))/binary>>),
         fun() -> case_checked(profile(Name), P, C) end}
        || Name := P <- Profiles, C <- maps:get(<<"cases">>, P)].

profile(<<"pq_pure">>) -> pq_pure;
profile(<<"pq_hybrid">>) -> pq_hybrid.

%% The record verifies at its clock, under the signer the file names, and reads back the port, hosts and release the
%% file pins (no release key where the file has null).
case_checked(Profile, #{<<"signer_node_id">> := Signer}, #{<<"record">> := Record, <<"now_ms">> := Now} = C) ->
    {ok, Verified} = macula_record:verify(binary:decode_hex(Record), Profile, Now),
    ?assertEqual(binary:decode_hex(Signer), macula_record:key_id(Verified)),
    Read = macula_record:read_station_endpoint(Verified),
    ?assertEqual(maps:get(<<"quic_port">>, C), maps:get(quic_port, Read)),
    ?assertEqual(maps:get(<<"host_advertised">>, C), maps:get(host_advertised, Read)),
    ?assertEqual(maps:get(<<"station_version">>, C), maps:get(station_version, Read, null)).

vectors() ->
    {ok, Bytes} = file:read_file(vector_file()),
    json:decode(Bytes).

%% The source tree's vector file, from the project root eunit runs in, or from the build tree's copy of the
%% application.
vector_file() ->
    Name = "station_endpoint_v1.json",
    first_existing([filename:join("test/vectors", Name), filename:join("../../test/vectors", Name)]
                   ++ [filename:join([Dir, "..", "..", "..", "..", "test", "vectors", Name])
                       || Dir <- [code:lib_dir(macula)], is_list(Dir)]).

first_existing([F | Rest]) ->
    first_existing(filelib:is_regular(F), F, Rest).

first_existing(true, F, _Rest) -> F;
first_existing(false, _F, Rest) -> first_existing(Rest).
