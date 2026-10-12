%% EUnit tests for what a wire 1.0 reader tolerates (macula#83 decisions 5 and 6, macula#99): a frame of a type this
%% node does not know is read as its envelope only, counted, dropped and never relayed, and the frames after it are
%% read; an unknown value of a per-hop enum reads as that enum's fallback and is counted. The envelope must still
%% decode: a frame of an unknown type whose version is not this node's, or whose frame_type is not text, ends the
%% parse as bad_frame, as any frame that does not decode does.
-module(macula_frame_wire_tolerance_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FUTURE, <<"a_future_frame_type">>).

%%------------------------------------------------------------------
%% Unknown frame types
%%------------------------------------------------------------------

an_unknown_frame_type_is_dropped_and_the_parse_goes_on_test() ->
    Before = count(unknown_frame_type),
    Bytes = <<(ping_wire())/binary, (future_wire(#{}))/binary, (ping_wire())/binary>>,
    ?assertMatch({ok, [#{frame_type := ping}, {unknown_frame_type, ?FUTURE}, #{frame_type := ping}], <<>>},
                 macula_frame:parse_received(Bytes)),
    ?assertEqual(Before + 1, count(unknown_frame_type)).

the_body_of_an_unknown_frame_type_is_not_read_test() ->
    Body = #{<<"anything">> => [1, #{{text, <<"nested">>} => <<0:8>>}], <<"capabilities">> => 7},
    ?assertMatch({ok, [{unknown_frame_type, ?FUTURE}], <<>>}, macula_frame:parse_received(future_wire(Body))).

an_unknown_frame_type_of_another_version_does_not_decode_test() ->
    ?assertEqual({malformed, [], bad_frame},
                 macula_frame:parse_received(wire(#{<<"version">> => version() + 1, <<"frame_type">> => {text, ?FUTURE}}))),
    ?assertEqual({malformed, [], bad_frame},
                 macula_frame:parse_received(wire(#{<<"frame_type">> => {text, ?FUTURE}}))).

a_frame_type_that_is_not_text_does_not_decode_test() ->
    ?assertEqual({malformed, [], bad_frame},
                 macula_frame:parse_received(wire(#{<<"version">> => version(), <<"frame_type">> => ?FUTURE}))).

decode_names_an_unknown_frame_type_test() ->
    ?assertEqual({error, {unknown_frame_type, ?FUTURE}}, macula_frame:decode(future_wire(#{}))).

read_wire_names_an_unknown_frame_type_test() ->
    <<_Len:32/big, Cbor/binary>> = future_wire(#{}),
    {ok, Wire} = macula_record_cbor:decode_strict(Cbor),
    ?assertEqual({error, {unknown_frame_type, ?FUTURE}}, macula_frame:read_wire(Wire)).

a_relay_never_passes_on_an_unknown_frame_type_test() ->
    Bytes = <<(future_wire(#{}))/binary, (ping_wire())/binary>>,
    ?assertMatch({ok, [{unknown_frame_type, ?FUTURE}, {#{frame_type := ping}, _Unit}], <<>>},
                 macula_frame:parse_for_relay(Bytes, 16#FFFFFF)).

%%------------------------------------------------------------------
%% Per-hop enum fallbacks
%%------------------------------------------------------------------

an_unknown_neighbor_priority_reads_as_low_test() ->
    Before = count(neighbor_priority),
    Neighbour = macula_frame:hyparview_neighbor(#{realm => fill(1), priority => high}),
    ?assertMatch({ok, #{priority := low}, <<>>}, decode_map(Neighbour#{priority => medium})),
    ?assertEqual(Before + 1, count(neighbor_priority)).

an_unknown_delivered_via_reads_as_direct_test() ->
    Before = count(delivered_via),
    {ok, Publisher} = macula_node_keys:generate(identity, pq_pure),
    #{publication := Publication} =
        macula_frame:publish(#{realm => fill(1), topic => <<"t">>, seq => 0, published_at => 1, payload => 1},
                             Publisher),
    Event = macula_frame:event(#{publication => Publication, delivered_via => plumtree}),
    ?assertMatch({ok, #{delivered_via := direct}, <<>>}, decode_map(Event#{delivered_via => dht})),
    ?assertEqual(Before + 1, count(delivered_via)).

an_update_with_an_unknown_swim_state_is_ignored_test() ->
    Before = count(swim_state),
    Alive = swim_update(1, alive),
    Ping = macula_frame:swim_ping(#{round => 1, incarnation => 0, piggyback => [Alive, Alive#{target := fill(3)}]}),
    Unknown = Ping#{piggyback := [Alive, (swim_update(2, alive))#{state := gone_fishing}, Alive#{target := fill(3)}]},
    {ok, #{piggyback := Read}, <<>>} = decode_map(Unknown),
    ?assertEqual([fill(1), fill(3)], [Target || #{target := Target} <- Read]),
    ?assertEqual(Before + 1, count(swim_state)).

an_enum_value_that_is_not_text_still_refuses_the_frame_test() ->
    Neighbour = macula_frame:hyparview_neighbor(#{realm => fill(1), priority => high}),
    ?assertEqual({error, bad_frame}, decode_map(Neighbour#{priority => 3})).

%%------------------------------------------------------------------
%% Extensions (macula#83 decision 8)
%%------------------------------------------------------------------

an_unknown_non_critical_extension_is_ignored_test() ->
    Ping = macula_frame:ping(#{nonce => binary:copy(<<7>>, 16)}),
    ?assertMatch({ok, [#{frame_type := ping}], <<>>},
                 macula_frame:parse_received(macula_frame:encode(Ping#{ext => ext(0)}))).

an_unknown_critical_extension_refuses_the_frame_and_the_parse_goes_on_test() ->
    Ping = macula_frame:ping(#{nonce => binary:copy(<<7>>, 16)}),
    Bytes = <<(macula_frame:encode(Ping#{ext => ext(1)}))/binary, (ping_wire())/binary>>,
    ?assertMatch({ok, [{invalid_frame, ping, ext}, #{frame_type := ping}], <<>>}, macula_frame:parse_received(Bytes)).

an_extension_of_another_shape_does_not_decode_test() ->
    Ping = macula_frame:ping(#{nonce => binary:copy(<<7>>, 16)}),
    Shapes = [#{a_future_extension => #{crit => 2, v => 1}},
              #{a_future_extension => #{v => 1}},
              #{a_future_extension => #{crit => 0, v => 1, more => 1}},
              #{a_future_extension => 1},
              [1]],
    [?assertEqual({malformed, [], bad_frame}, macula_frame:parse_received(macula_frame:encode(Ping#{ext => Ext})))
     || Ext <- Shapes].

a_per_hop_extension_on_a_signed_frame_leaves_its_signature_valid_test() ->
    {Call, _Caller} = call(),
    {ok, Received, <<>>} = macula_frame:decode(macula_frame:encode(Call#{ext => ext(0)})),
    ?assertMatch({ok, _}, macula_frame:verify_request(Received, pq_pure)).

a_signed_extension_is_read_by_its_criticality_test() ->
    {Call, Caller} = call(),
    ?assertMatch({ok, _}, macula_frame:verify_request(with_signed_ext(Call, Caller, 0), pq_pure)),
    ?assertEqual({error, malformed_frame}, macula_frame:verify_request(with_signed_ext(Call, Caller, 1), pq_pure)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% One extension no node knows yet, critical (1) or not (0).
ext(Crit) ->
    #{a_future_extension => #{crit => Crit, v => #{{text, <<"anything">>} => 1}}}.

call() ->
    {ok, Caller} = macula_node_keys:generate(identity, pq_pure),
    {ok, Provider} = macula_node_keys:generate(identity, pq_pure),
    {ok, Target} = macula_node_keys:node_id(Provider),
    Spec = #{request_id => binary:copy(<<1>>, 16), realm => fill(1), procedure => <<"probe.echo">>, target => Target,
             deadline => erlang:system_time(millisecond) + 60_000, payload => #{n => 1}},
    {macula_frame:call(Spec, Caller), Caller}.

%% The CALL's request signed again by its caller with an extension inside the tbs.
with_signed_ext(#{request := #{tbs := Tbs}} = Call, Caller, Crit) ->
    {ok, Fields} = macula_record_cbor:decode_strict(Tbs),
    Ext = #{{text, <<"a_future_extension">>} => #{{text, <<"crit">>} => Crit, {text, <<"v">>} => 1}},
    Signed = macula_signed_object:sign(<<"MACULA-PQ-REQUEST-V1">>,
                                       (maps:remove({text, <<"alg">>}, Fields))#{{text, <<"ext">>} => Ext}, Caller),
    {ok, Received, <<>>} = macula_frame:decode(macula_frame:encode(Call#{request := Signed})),
    Received.

count(Name) ->
    maps:get(Name, macula_wire_tolerance:counters()).

ping_wire() ->
    macula_frame:encode(macula_frame:ping(#{nonce => binary:copy(<<7>>, 16)})).

%% A frame of a type no node knows yet: a version and frame_type envelope around any body.
future_wire(Body) ->
    wire(Body#{<<"version">> => version(), <<"frame_type">> => {text, ?FUTURE}}).

%% A CBOR map with text keys, length-prefixed as a frame.
wire(Map) ->
    Cbor = macula_record_cbor:encode(maps:fold(fun(K, V, Acc) -> Acc#{{text, K} => V} end, #{}, Map)),
    <<(byte_size(Cbor)):32/big, Cbor/binary>>.

version() ->
    macula_frame:version(macula_frame:ping(#{nonce => binary:copy(<<7>>, 16)})).

decode_map(Frame) ->
    macula_frame:decode(macula_frame:encode(Frame)).

swim_update(Target, State) ->
    macula_frame:swim_update(#{target => fill(Target), state => State, incarnation => 0, observed_at => 1,
                               by => fill(9)}).

fill(Byte) ->
    binary:copy(<<Byte>>, 32).
