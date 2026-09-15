%% EUnit tests for reading a node record's coordinates. Reading a verified record never raises on a coordinate its
%% signer chose: text that does not spell a finite float or integer of at most 32 bytes, and a value that is not text,
%% reads as no coordinate, and a finite value still reads.
-module(macula_record_node_geo_tests).

-include_lib("eunit/include/eunit.hrl").

a_coordinate_that_is_no_finite_number_reads_as_none_test_() ->
    Id = key(),
    [?_assertEqual({Value, undefined, undefined}, read_geo(Value, Id))
     || Value <- [{text, <<"NaN">>}, {text, <<"+Inf">>}, {text, <<"-Inf">>}, {text, <<"abc">>}, {text, <<>>},
                  {text, binary:copy(<<"1">>, 1024)}, {text, <<"12abc">>}, {text, <<"1.5 ">>}, {text, <<"1e400">>},
                  12, #{}, [1]]].

a_finite_coordinate_still_reads_test() ->
    Id = key(),
    ?assertEqual({50.8503, 4}, geo_read({text, <<"50.8503">>}, {text, <<"4">>}, Id)).

%% Signs a node record whose lat and lng both hold Value, verifies it as a peer would, and reads it back: the value
%% with the coordinates read.
read_geo(Value, Id) ->
    {Lat, Lng} = geo_read(Value, Value, Id),
    {Value, Lat, Lng}.

geo_read(LatValue, LngValue, Id) ->
    #{payload := Payload} = Unsigned = macula_record:node_record(macula_node_keys:key_id(Id), [], 0),
    Geo = Payload#{{text, <<"lat">>} => LatValue, {text, <<"lng">>} => LngValue},
    Signed = macula_record:sign(Unsigned#{payload := Geo}, Id),
    {ok, Verified} = macula_record:verify(macula_record:encode(Signed), pq_pure),
    #{lat := Lat, lng := Lng} = macula_record:read_node_record(Verified),
    {Lat, Lng}.

key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.
