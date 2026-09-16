%% EUnit tests for a node record's coordinates. A coordinate is text as both builders write it: an optional leading
%% minus, digits, then optionally a dot and digits, in at most 32 bytes, within [-90, 90] for lat and [-180, 180] for
%% lng. Reading a verified record never raises on a coordinate its signer chose: any other value, text or not, reads as
%% no coordinate. The builder refuses a coordinate that is not a number in range by name, and fails no other way.
-module(macula_record_node_geo_tests).

-include_lib("eunit/include/eunit.hrl").

%% Values that read as no coordinate, as lat and as lng alike.
a_value_that_is_no_coordinate_reads_as_none_test_() ->
    Id = key(),
    [?_assertEqual({Value, undefined, undefined}, read_geo(Value, Id))
     || Value <- [{text, <<"NaN">>}, {text, <<"+Inf">>}, {text, <<"-Inf">>}, {text, <<"infinity">>},
                  {text, <<"abc">>}, {text, <<>>}, {text, <<"-">>}, {text, <<".5">>}, {text, <<"+.5">>},
                  {text, <<"-.5">>}, {text, <<"5.">>}, {text, <<"1e5">>}, {text, <<"1e+5">>}, {text, <<"1.5e1">>},
                  {text, <<"0x10">>}, {text, <<"1_000">>}, {text, <<"1,5">>}, {text, <<"+1.5">>}, {text, <<"+4">>},
                  {text, <<"--1.0">>}, {text, <<"1.0.0">>}, {text, <<"1.5 ">>}, {text, <<" 1.5">>},
                  {text, <<"12abc">>}, {text, <<"1.0e400">>}, {text, <<"4.9e-324">>},
                  {text, <<"１２"/utf8>>}, {text, binary:copy(<<"1">>, 33)}, {text, binary:copy(<<"1">>, 1024)},
                  {text, <<"180.5">>}, {text, <<"-180.000001">>},
                  <<"50.8">>, 12, 1.5, #{}, [1]]].

%% The range edges: lat reads within [-90, 90] and lng within [-180, 180], both inclusive.
a_coordinate_reads_only_within_its_range_test() ->
    Id = key(),
    ?assertEqual([{90, 180}, {-90.0, -180.0}, {undefined, 90.000001}, {0, undefined}, {undefined, undefined}],
                 [geo_read({text, Lat}, {text, Lng}, Id)
                  || {Lat, Lng} <- [{<<"90">>, <<"180">>}, {<<"-90.0">>, <<"-180.0">>},
                                    {<<"90.000001">>, <<"90.000001">>}, {<<"-0">>, <<"180.5">>},
                                    {<<"-90.000001">>, <<"-180.5">>}]]).

%% Text as both builders write it reads as its number, leading zeros and a negative zero included.
a_coordinate_as_the_builders_write_it_reads_test() ->
    Id = key(),
    ?assertEqual([{50.8503, 4}, {-0.0, 7}, {12.5, -4}, {0.0, 0}],
                 [geo_read({text, Lat}, {text, Lng}, Id)
                  || {Lat, Lng} <- [{<<"50.8503">>, <<"4">>}, {<<"-0.0">>, <<"007">>}, {<<"00012.50">>, <<"-4">>},
                                    {<<"0.0">>, <<"0">>}]]).

%% The builder refuses, by name, a lat or lng that is not a number within its range, a number too large to render
%% included, and fails no other way.
the_builder_refuses_a_coordinate_that_is_no_number_in_range_by_name_test_() ->
    [?_assertError({badmatch, {error, {invalid_coordinate, Field}}},
                   macula_record:node_record(<<1:256>>, [], 0, #{Field => Value}))
     || {Field, Value} <- [{lat, 90.000001}, {lat, -91}, {lng, 180.5}, {lng, -181}, {lat, 1.0e300}, {lng, 1.0e30},
                           {lat, <<"50">>}, {lng, not_a_number}]].

%% What the builder writes, at the edges of the range, reads back.
what_the_builder_writes_reads_back_test() ->
    Id = key(),
    Built = macula_record:node_record(macula_node_keys:key_id(Id), [], 0, #{lat => 90, lng => -179.123456}),
    {ok, Verified} = macula_record:verify(macula_record:encode(macula_record:sign(Built, Id)), pq_pure),
    ?assertMatch(#{lat := 90, lng := -179.123456}, macula_record:read_node_record(Verified)).

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
