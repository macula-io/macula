%%%-------------------------------------------------------------------
%%% @doc What `macula_frame:validate_received/1' accepts and refuses, on the
%%% post-quantum frame shapes (D26).
%%%
%%% Every builder's output passes after the wire codec. A required field that
%%% is missing, or holds a value of the wrong type or of a length its rule does
%%% not allow, refuses the frame and is named in the error, and so does a
%%% field the frame type does not have, a version other than the protocol's, a
%%% FORWARD_JOIN prwl above its arwl, a SWIM piggyback entry without one of its
%%% fields, SUBSCRIBE options that are not a map, a SUBSCRIBE or UNSUBSCRIBE
%%% topic that is not UTF-8 of at most 512 bytes, a SUBSCRIBE filter, a GOODBYE
%%% detail that is not UTF-8 of at most 256 bytes, and a NODES entry whose
%%% addresses are not at most 4 of exactly a host, a port and quic. A
%%% fixed-length field in a list entry or in the optional header is checked as
%%% a top-level one is. A
%%% frame type that carries one of several signed objects needs exactly one of
%%% them. A control frame signed for its neighbour holds version, frame_type
%%% and neighbour and nothing else; its own fields are checked when it is
%%% opened. A frame type this node does not know, and a frame without a type,
%%% are refused. Every field a rule names is a field of the frame type's table
%%% in macula_frame and the other way round, so the rules and the table cannot
%%% drift apart. The table in samples/0 states each frame type's required
%%% fields apart from the validator, so a change to either one shows up here.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_frame_received_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PROFILE, pq_pure).
%% Every frame type macula_frame reads.
-define(FRAME_TYPES,
        [connect, hello, goodbye, swim_ping, swim_ack, swim_suspect, swim_confirm, ping, pong, find_node, nodes,
         find_value, value, store, store_ack, call, result, error, hyparview_join, hyparview_forward_join,
         hyparview_neighbor, hyparview_disconnect, hyparview_shuffle, hyparview_shuffle_reply, plumtree_gossip,
         plumtree_ihave, plumtree_graft, plumtree_prune, overlay_relay, publish, subscribe, unsubscribe, event,
         advertise, unadvertise, stream_open, stream_data, stream_end, stream_error, stream_reply, want, have,
         block, manifest_req, manifest_res, cancel]).
%% The rules that fix the length of a field's bytes.
-define(FIXED_LENGTH, [key, id16, hash48, mcid, country]).
%% The control frame types a neighbour signature may carry in pq_hybrid.
-define(NEIGHBOUR_SIGNED,
        [swim_ping, swim_ack, swim_suspect, swim_confirm, ping, pong, find_node, nodes, find_value, value,
         store, store_ack, advertise, unadvertise, subscribe, unsubscribe, overlay_relay, hyparview_join,
         hyparview_forward_join, hyparview_neighbor, hyparview_disconnect, hyparview_shuffle,
         hyparview_shuffle_reply, plumtree_ihave, plumtree_graft, plumtree_prune, goodbye]).

%%------------------------------------------------------------------
%% Every builder's output passes
%%------------------------------------------------------------------

every_builder_output_passes_test_() ->
    [{atom_to_list(Type), ?_assertEqual(ok, macula_frame:validate_received(over_the_wire(Frame)))}
     || {Type, Frame, _Required, _AnyValue} <- samples()].

the_samples_cover_every_frame_type_test() ->
    ?assertEqual(lists:sort(?FRAME_TYPES), lists:usort([Type || {Type, _Frame, _Required, _AnyValue} <- samples()])).

%%------------------------------------------------------------------
%% A missing, mistyped or extra field is refused, and named
%%------------------------------------------------------------------

a_missing_required_field_is_named_test_() ->
    [{lists:concat([Type, " without ", Field]),
      ?_assertEqual({error, {invalid_frame, Type, Field}},
                    macula_frame:validate_received(maps:remove(Field, over_the_wire(Frame))))}
     || {Type, Frame, Required, _AnyValue} <- samples(), Field <- Required].

a_required_field_of_the_wrong_type_is_named_test_() ->
    [{lists:concat([Type, " with a mistyped ", Field]),
      ?_assertEqual({error, {invalid_frame, Type, Field}},
                    macula_frame:validate_received((over_the_wire(Frame))#{Field => {not_a_valid_value}}))}
     || {Type, Frame, Required, AnyValue} <- samples(), Field <- Required -- AnyValue].

a_field_the_frame_type_does_not_have_is_named_test_() ->
    [{lists:concat([Type, " with an extra field"]),
      ?_assertEqual({error, {invalid_frame, Type, zz_extra}},
                    macula_frame:validate_received((over_the_wire(Frame))#{zz_extra => 1}))}
     || {Type, Frame, _Required, _AnyValue} <- samples()].

%% Each field a sample holds under a rule that fixes its length, one byte short and one byte long.
a_fixed_length_field_of_another_length_is_named_test_() ->
    [{lists:concat([Type, " with a ", Field, " of ", byte_size(Other), " bytes"]),
      ?_assertEqual({error, {invalid_frame, Type, Field}}, macula_frame:validate_received(Received#{Field => Other}))}
     || {Type, Frame, _Required, _AnyValue} <- samples(),
        Received <- [over_the_wire(Frame)],
        {Field, Rule} <- all_rules(Type),
        lists:member(bare(Rule), ?FIXED_LENGTH),
        Value <- [maps:get(Field, Received, undefined)],
        is_binary(Value),
        Other <- [binary:part(Value, 0, byte_size(Value) - 1), <<Value/binary, 0>>]].

a_frame_of_another_version_is_named_test_() ->
    [{atom_to_list(Type),
      ?_assertEqual({error, {invalid_frame, Type, version}},
                    macula_frame:validate_received((over_the_wire(Frame))#{version => version() + 1}))}
     || {Type, Frame, _Required, _AnyValue} <- samples()].

%%------------------------------------------------------------------
%% Exactly one of several signed objects
%%------------------------------------------------------------------

a_stream_frame_needs_exactly_one_of_its_objects_test() ->
    #{provider := Provider, caller := Caller} = keys(),
    Open = verified_open(),
    Provided = over_the_wire(macula_frame:provider_stream(chunk(0), Provider, Open)),
    Called = over_the_wire(macula_frame:caller_stream(chunk(0), Caller, Open)),
    ?assertEqual(ok, macula_frame:validate_received(Called)),
    ?assertEqual({error, {invalid_frame, stream_data, stream}},
                 macula_frame:validate_received(maps:remove(stream, Provided))),
    ?assertEqual({error, {invalid_frame, stream_data, caller_stream}},
                 macula_frame:validate_received(Provided#{caller_stream => maps:get(caller_stream, Called)})).

an_error_needs_exactly_one_of_a_reply_and_a_relay_error_test() ->
    #{provider := Provider, station := Station} = keys(),
    Request = verified_call(),
    Replied = over_the_wire(macula_frame:provider_error(#{request => Request, code => <<"closed">>}, Provider)),
    Relayed = over_the_wire(macula_frame:relay_error(#{frame_type => error, request => Request,
                                                       code => unknown_next_peer}, Station)),
    ?assertEqual(ok, macula_frame:validate_received(Relayed)),
    ?assertEqual({error, {invalid_frame, error, reply}},
                 macula_frame:validate_received(maps:remove(reply, Replied))),
    ?assertEqual({error, {invalid_frame, error, relay_error}},
                 macula_frame:validate_received(Replied#{relay_error => maps:get(relay_error, Relayed)})).

%%------------------------------------------------------------------
%% A control frame signed for its neighbour
%%------------------------------------------------------------------

a_neighbour_signed_control_frame_holds_only_its_neighbour_object_test_() ->
    Held = #{tbs => <<"the frame's own fields">>, signature => <<"a signature">>},
    [{atom_to_list(Type),
      ?_test(begin
                 Frame = #{version => version(), frame_type => Type, neighbour => Held},
                 ?assertEqual(ok, macula_frame:validate_received(Frame)),
                 ?assertEqual({error, {invalid_frame, Type, frame_id}},
                              macula_frame:validate_received(Frame#{frame_id => <<0:128>>})),
                 ?assertEqual({error, {invalid_frame, Type, version}},
                              macula_frame:validate_received(maps:remove(version, Frame))),
                 ?assertEqual({error, {invalid_frame, Type, version}},
                              macula_frame:validate_received(Frame#{version => version() + 1})),
                 ?assertEqual({error, {invalid_frame, Type, neighbour}},
                              macula_frame:validate_received(Frame#{neighbour => <<"not a held object">>}))
             end)}
     || Type <- ?NEIGHBOUR_SIGNED].

a_neighbour_object_on_another_frame_type_is_named_test() ->
    Held = #{tbs => <<"fields">>, signature => <<"a signature">>},
    ?assertEqual({error, {invalid_frame, block, neighbour}},
                 macula_frame:validate_received((over_the_wire(sample(block)))#{neighbour => Held})).

%%------------------------------------------------------------------
%% Entries, unknown types, and a GOODBYE's reason
%%------------------------------------------------------------------

the_entries_of_a_list_are_checked_test() ->
    Mcid = mcid(),
    Want = over_the_wire(macula_frame:want(#{blocks => [#{mcid => Mcid}]})),
    ?assertEqual({error, {invalid_frame, want, blocks}},
                 macula_frame:validate_received(Want#{blocks => [#{priority => 1}]})),
    ?assertEqual({error, {invalid_frame, want, blocks}},
                 macula_frame:validate_received(Want#{blocks => [#{mcid => Mcid} | improper]})),
    Nodes = over_the_wire(macula_frame:nodes(#{key => key(), nodes => [station_ref()]})),
    [Ref] = maps:get(nodes, Nodes),
    ?assertEqual({error, {invalid_frame, nodes, nodes}},
                 macula_frame:validate_received(Nodes#{nodes => [Ref#{tier => 9}]})),
    Value = over_the_wire(macula_frame:value(#{key => key(), records => [record_bytes()]})),
    ?assertEqual({error, {invalid_frame, value, records}},
                 macula_frame:validate_received(Value#{records => [#{type => 1}]})),
    Cancel = over_the_wire(macula_frame:cancel(#{blocks => [Mcid]})),
    ?assertEqual({error, {invalid_frame, cancel, blocks}},
                 macula_frame:validate_received(Cancel#{blocks => [<<1, 16#55, 0:256>>]})).

a_value_outside_its_rule_is_refused_test() ->
    Refused = fun(#{frame_type := Type} = Frame, Field) ->
                  ?assertEqual({error, {invalid_frame, Type, Field}}, macula_frame:validate_received(Frame))
              end,
    Event = over_the_wire(sample(event)),
    Refused(Event#{delivered_via => dht}, delivered_via),
    Neighbor = over_the_wire(macula_frame:hyparview_neighbor(#{realm => key(), priority => high})),
    Refused(Neighbor#{priority => medium}, priority),
    Ping = over_the_wire(macula_frame:swim_ping(#{round => 0, incarnation => 0})),
    Refused(Ping#{round => -1}, round),
    Store = over_the_wire(macula_frame:store(#{record => record_bytes()})),
    Refused(Store#{record => #{type => 1}}, record),
    Block = over_the_wire(sample(block)),
    Refused(Block#{mcid => <<1, 16#55, 0:256>>}, mcid).

a_frame_type_this_node_does_not_know_is_refused_test() ->
    ?assertEqual({error, {invalid_frame, unknown, frame_type}},
                 macula_frame:validate_received(#{version => version(), frame_type => a_future_frame_type})),
    ?assertEqual({error, {invalid_frame, unknown, frame_type}},
                 macula_frame:validate_received(#{version => version(),
                                                  frame_type => {text, <<"a_future_frame_type">>}})).

a_frame_without_a_type_is_refused_test() ->
    ?assertEqual({error, {invalid_frame, unknown, frame_type}},
                 macula_frame:validate_received(#{version => version()})).

a_goodbye_reason_is_text_test() ->
    Goodbye = over_the_wire(macula_frame:goodbye(normal, undefined)),
    ?assertEqual(ok, macula_frame:validate_received(
                       Goodbye#{reason => {text, <<"a reason this node has no atom for">>}})),
    ?assertEqual({error, {invalid_frame, goodbye, reason}},
                 macula_frame:validate_received(Goodbye#{reason => 42})).

a_forward_join_whose_prwl_is_above_its_arwl_is_named_test() ->
    Joined = over_the_wire(macula_frame:hyparview_forward_join(#{realm => key(), new_member => key(), ttl => 2,
                                                                 arwl => 4, prwl => 4})),
    ?assertEqual(ok, macula_frame:validate_received(Joined)),
    ?assertEqual({error, {invalid_frame, hyparview_forward_join, prwl}},
                 macula_frame:validate_received(Joined#{prwl => 5})).

%% A SWIM update travels in PING and ACK piggyback with each of the five fields swim_update/1 writes.
a_piggyback_entry_needs_each_of_its_fields_test_() ->
    Update = macula_frame:swim_update(#{target => key(), state => suspect, incarnation => 1, observed_at => 1,
                                        by => key()}),
    Ping = over_the_wire(macula_frame:swim_ping(#{round => 0, incarnation => 0, piggyback => [Update]})),
    Ack = over_the_wire(macula_frame:swim_ack(#{round => 0, responder => key(), incarnation => 0,
                                                piggyback => [Update]})),
    Missing = [{lists:concat(["without ", Field]), maps:remove(Field, Update)} || Field <- maps:keys(Update)],
    Mistyped = [{"with a state SWIM does not have", Update#{state => dead}},
                {"with a target one byte short", Update#{target => <<1:248>>}},
                {"observed at 0", Update#{observed_at => 0}},
                {"with a negative incarnation", Update#{incarnation => -1}}],
    [{"a ping and an ack with a whole entry pass",
      [?_assertEqual(ok, macula_frame:validate_received(Ping)),
       ?_assertEqual(ok, macula_frame:validate_received(Ack))]}
     | [{lists:concat([Type, " piggyback entry ", Name]),
         ?_assertEqual({error, {invalid_frame, Type, piggyback}},
                       macula_frame:validate_received(Frame#{piggyback => [Entry]}))}
        || #{frame_type := Type} = Frame <- [Ping, Ack], {Name, Entry} <- Missing ++ Mistyped]].

subscribe_options_are_optional_and_a_map_test() ->
    Subscribe = over_the_wire(macula_frame:subscribe(#{topic => <<"probe.topic">>, realm => key(),
                                                       subscriber => key(), options => #{}})),
    ?assertEqual(ok, macula_frame:validate_received(Subscribe)),
    ?assertEqual(ok, macula_frame:validate_received(maps:remove(options, Subscribe))),
    ?assertEqual({error, {invalid_frame, subscribe, options}},
                 macula_frame:validate_received(Subscribe#{options => [ordered]})),
    ?assertEqual({error, {invalid_frame, subscribe, options}},
                 macula_frame:validate_received(Subscribe#{options => 1})).

%% A SUBSCRIBE or UNSUBSCRIBE topic is bytes of valid UTF-8, at most 512 of them, in the builder and in the receiver.
a_subscription_topic_is_utf8_of_at_most_512_bytes_test_() ->
    Long = binary:copy(<<"t">>, 512),
    Spec = #{topic => Long, realm => key(), subscriber => key()},
    [{atom_to_list(Type),
      fun() ->
          Received = over_the_wire(Build(Spec)),
          ?assertEqual(ok, macula_frame:validate_received(Received)),
          [?assertEqual({error, {invalid_frame, Type, topic}}, macula_frame:validate_received(Received#{topic => Topic}))
           || Topic <- [<<Long/binary, "t">>, <<16#ff, 16#fe>>, {text, <<"probe.topic">>}]],
          ?assertError({badmatch, {error, {text_too_long, topic}}}, Build(Spec#{topic => <<Long/binary, "t">>})),
          ?assertError({badmatch, {error, {invalid_text, topic}}}, Build(Spec#{topic => <<16#ff, 16#fe>>}))
      end}
     || {Type, Build} <- [{subscribe, fun macula_frame:subscribe/1}, {unsubscribe, fun macula_frame:unsubscribe/1}]].

%% A SUBSCRIBE has no filter: the builder refuses one, a received SUBSCRIBE that holds one is refused by name, and
%% bytes that carry one do not decode.
a_subscribe_has_no_filter_test() ->
    Spec = #{topic => <<"probe.topic">>, realm => key(), subscriber => key()},
    Built = macula_frame:subscribe(Spec),
    Subscribe = over_the_wire(Built),
    ?assertNot(maps:is_key(filter, Subscribe)),
    ?assertEqual({error, {invalid_frame, subscribe, filter}},
                 macula_frame:validate_received(Subscribe#{filter => <<"weather.*">>})),
    ?assertEqual({error, bad_frame}, macula_frame:decode(macula_frame:encode(Built#{filter => <<"weather.*">>}))),
    ?assertError(function_clause, macula_frame:subscribe(Spec#{filter => <<"weather.*">>})).

%% A GOODBYE detail is optional bytes of valid UTF-8, at most 256 of them, in the builder and in the receiver.
a_goodbye_detail_is_utf8_of_at_most_256_bytes_test() ->
    Long = binary:copy(<<"d">>, 256),
    Goodbye = over_the_wire(macula_frame:goodbye(draining, Long)),
    ?assertEqual(ok, macula_frame:validate_received(Goodbye)),
    ?assertEqual(ok, macula_frame:validate_received(over_the_wire(macula_frame:goodbye(draining, undefined)))),
    [?assertEqual({error, {invalid_frame, goodbye, detail}}, macula_frame:validate_received(Goodbye#{detail => Detail}))
     || Detail <- [<<Long/binary, "d">>, <<16#ff, 16#fe>>, {text, <<"bye">>}]],
    Unbounded = (macula_frame:goodbye(draining, undefined))#{detail => <<Long/binary, "d">>},
    ?assertEqual({error, {invalid_frame, goodbye, detail}}, macula_frame:decode(macula_frame:encode(Unbounded))),
    ?assertError({badmatch, {error, {text_too_long, detail}}}, macula_frame:goodbye(draining, <<Long/binary, "d">>)),
    ?assertError({badmatch, {error, {invalid_text, detail}}}, macula_frame:goodbye(draining, <<16#ff, 16#fe>>)).

%% A NODES entry lists at most 4 addresses, each exactly a host of 1 to 253 bytes, a port from 1 to 65535 and the quic
%% transport, in the builder and in the receiver. A received entry reads back as the entry its builder took.
a_nodes_address_is_a_host_a_port_and_quic_test() ->
    Address = #{host => <<"station.example">>, port => 4433, transport => quic},
    Nodes = over_the_wire(macula_frame:nodes(#{key => key(), nodes => [station_ref([Address])]})),
    [Entry] = maps:get(nodes, Nodes),
    ?assertEqual([Address], maps:get(addresses, Entry)),
    Received = fun(Addresses) -> macula_frame:validate_received(Nodes#{nodes => [Entry#{addresses => Addresses}]}) end,
    Label = binary:copy(<<"h">>, 63),
    Host253 = <<Label/binary, ".", Label/binary, ".", Label/binary, ".", (binary:copy(<<"h">>, 61))/binary>>,
    Longest = Address#{host => Host253, port => 65535},
    ?assertEqual(ok, Received([Longest, Address#{port => 1}, Address, Address])),
    Refused = [lists:duplicate(5, Address), [Address#{host => <<>>}], [Address#{host => <<Host253/binary, "h">>}],
               [Address#{host => <<"bad host">>}], [Address#{port => 0}], [Address#{port => 65536}],
               [Address#{transport => tcp}], [maps:remove(transport, Address)], [Address#{via => relay}], [#{}]],
    [?assertEqual({error, {invalid_frame, nodes, nodes}}, Received(Addresses)) || Addresses <- Refused],
    [?assertError({badmatch, {error, invalid_addresses}}, station_ref(Addresses)) || Addresses <- Refused].

%% A NODES address host is an IP literal without a zone, or a host name: labels of 1 to 63 letters, digits and hyphens
%% that neither start nor end with a hyphen, and no trailing dot. The check is macula_frame:addresses_checked/1, which
%% a station can run on the addresses it stores before it lists them.
a_nodes_address_host_is_an_ip_literal_or_a_host_name_test() ->
    Label = binary:copy(<<"a">>, 63),
    Named = fun(Host) -> [#{host => Host, port => 4433, transport => quic}] end,
    Hosts = [<<"station-de-frankfurt.macula.io">>, <<"beam00">>, <<"A1-b2.EXAMPLE">>, <<Label/binary, ".example">>,
             <<"127.0.0.1">>, <<"2001:db8::1">>, <<"::">>],
    NotHosts = [<<"bad host">>, <<"a_b.example">>, <<"-a.example">>, <<"a-.example">>, <<"example.">>, <<"a..b">>,
                <<".a">>, <<"fe80::1%eth0">>, <<"a", 0>>, <<"station.example\n">>, <<Label/binary, "a.example">>,
                <<"ex", 16#C3, 16#A9, ".example">>],
    [?assertEqual(ok, macula_frame:addresses_checked(Named(Host))) || Host <- Hosts],
    [?assertEqual({error, invalid_addresses}, macula_frame:addresses_checked(Named(Host))) || Host <- NotHosts].

%% Each fixed-length field of a list entry, one byte short and one byte long: a NODES entry's node_id, station_id and
%% country, the mcid of a WANT or HAVE block, a SWIM update's target and by, and an mcid a CANCEL lists.
a_fixed_length_field_in_a_list_entry_of_another_length_is_named_test_() ->
    Mcid = mcid(),
    Update = macula_frame:swim_update(#{target => key(), state => suspect, incarnation => 1, observed_at => 1,
                                        by => key()}),
    Lists = [{nodes, macula_frame:nodes(#{key => key(), nodes => [station_ref()]})},
             {blocks, macula_frame:want(#{blocks => [#{mcid => Mcid}]})},
             {blocks, macula_frame:have(#{blocks => [#{mcid => Mcid, size => 10}]})},
             {piggyback, macula_frame:swim_ping(#{round => 0, incarnation => 0, piggyback => [Update]})},
             {piggyback, macula_frame:swim_ack(#{round => 0, responder => key(), incarnation => 0,
                                                 piggyback => [Update]})}],
    Cancel = over_the_wire(macula_frame:cancel(#{blocks => [Mcid]})),
    [{lists:concat([Type, " ", List, " entry with a ", Field, " of ", byte_size(Other), " bytes"]),
      ?_assertEqual({error, {invalid_frame, Type, List}},
                    macula_frame:validate_received(Received#{List => [Entry#{Field => Other}]}))}
     || {List, Frame} <- Lists,
        Received <- [over_the_wire(Frame)],
        #{frame_type := Type} <- [Received],
        [Entry] <- [maps:get(List, Received)],
        {Field, Value} <- maps:to_list(Entry),
        lists:member(Field, [node_id, station_id, country, mcid, target, by]),
        Other <- [binary:part(Value, 0, byte_size(Value) - 1), <<Value/binary, 0>>]]
    ++ [{lists:concat(["cancel with an mcid of ", byte_size(Other), " bytes"]),
         ?_assertEqual({error, {invalid_frame, cancel, blocks}},
                       macula_frame:validate_received(Cancel#{blocks => [Other]}))}
        || Other <- [binary:part(Mcid, 0, 49), <<Mcid/binary, 0>>]].

%% The optional header fields of a fixed length, realm and call_id, one byte short and one byte long.
an_optional_header_field_of_another_length_is_named_test_() ->
    Ping = over_the_wire(macula_frame:ping(#{nonce => id()})),
    [{"a ping with a realm and a call_id passes",
      ?_assertEqual(ok, macula_frame:validate_received(Ping#{realm => key(), call_id => id()}))}
     | [{lists:concat(["a ping with a ", Field, " of ", byte_size(Other), " bytes"]),
         ?_assertEqual({error, {invalid_frame, ping, Field}}, macula_frame:validate_received(Ping#{Field => Other}))}
        || {Field, Value} <- [{realm, key()}, {call_id, id()}],
           Other <- [binary:part(Value, 0, byte_size(Value) - 1), <<Value/binary, 0>>]]].

%%------------------------------------------------------------------
%% Relayed frames taken without a frame signature
%%------------------------------------------------------------------

%% A station relays the HyParView JOIN, FORWARD_JOIN, NEIGHBOR and SHUFFLE that D17 leaves unsigned in pq_pure from a
%% connection it authenticated, so a receiver takes them without a frame signature, with the relay's origin as their
%% sender. Exactly those four frame types are taken so; every other type keeps its own verification.
relayed_without_signature_names_exactly_the_unsigned_overlay_types_test() ->
    ?assertEqual([hyparview_join, hyparview_forward_join, hyparview_neighbor, hyparview_shuffle],
                 [Type || Type <- ?FRAME_TYPES, macula_frame:relayed_without_signature(Type)]),
    ?assertNot(macula_frame:relayed_without_signature(a_future_frame_type)).

%%------------------------------------------------------------------
%% The rules and the table name the same fields
%%------------------------------------------------------------------

every_rule_field_is_a_field_of_its_table_test_() ->
    [{atom_to_list(Type), ?_assertEqual([], rule_fields(Type) -- table_fields(Type))} || Type <- ?FRAME_TYPES].

every_table_field_has_a_rule_test_() ->
    [{atom_to_list(Type), ?_assertEqual([], table_fields(Type) -- ([version, frame_type, neighbour] ++ rule_fields(Type)))}
     || Type <- ?FRAME_TYPES].

rule_fields(Type) ->
    lists:usort([Field || {Field, _Rule} <- all_rules(Type)]).

all_rules(Type) ->
    #{required := Required, one_of := Groups, optional := Optional} = macula_frame:received_rules(Type),
    Required ++ lists:append(Groups) ++ Optional.

bare({optional, Rule}) -> Rule;
bare(Rule) -> Rule.

table_fields(Type) ->
    lists:usort([Field || {Field, _Kind} <- maps:values(macula_frame:field_table(Type))]).

%%------------------------------------------------------------------
%% One frame per type, with its required fields
%%------------------------------------------------------------------

%% {Type, Frame, required fields, required fields any value fills}
samples() ->
    #{caller := Caller, provider := Provider, station := Station} = keys(),
    Now = erlang:system_time(millisecond),
    Mcid = mcid(),
    Record = record_bytes(),
    Request = verified_call(),
    Open = verified_open(),
    [{connect, macula_frame:connect(#{node_id => key(), station_id => key(), realms => [key()],
                                     capabilities => 0, puzzle_evidence => key()}),
      [node_id, station_id, realms, capabilities, puzzle_evidence], []},
     {hello, macula_frame:hello(#{node_id => key(), station_id => key(), realms => [],
                                 capabilities => 0, accepted => true, negotiated_capabilities => 0}),
      [node_id, station_id, realms, capabilities, accepted, negotiated_capabilities], []},
     {goodbye, macula_frame:goodbye(normal, <<"bye">>), [reason], []},
     {swim_ping, macula_frame:swim_ping(#{round => 1, incarnation => 0}), [round, incarnation], []},
     {swim_ack, macula_frame:swim_ack(#{round => 1, responder => key(), incarnation => 0}),
      [round, responder, incarnation], []},
     {swim_suspect, macula_frame:swim_suspect(suspect_spec()), [target, target_incarnation, suspected_by, ttl], []},
     {swim_confirm, macula_frame:swim_confirm(suspect_spec()), [target, target_incarnation, suspected_by, ttl], []},
     {ping, macula_frame:ping(#{nonce => id()}), [nonce], []},
     {pong, macula_frame:pong(#{nonce => id()}), [nonce], []},
     {find_node, macula_frame:find_node(#{key => key(), origin => key(), depth => 0}), [key, origin, depth], []},
     {nodes, macula_frame:nodes(#{key => key(), nodes => [station_ref()]}), [key, nodes], []},
     {find_value, macula_frame:find_value(#{key => key(), origin => key()}), [key, origin], []},
     {value, macula_frame:value(#{key => key(), records => [Record]}), [key, records], []},
     {store, macula_frame:store(#{record => Record}), [record], []},
     {store_ack, macula_frame:store_ack(#{key => key(), stored => true}), [key, stored], []},
     {call, macula_frame:call(request_spec(), Caller), [request], []},
     {result, macula_frame:result(#{request => Request, payload => #{n => 1}}, Provider), [reply], []},
     {error, macula_frame:provider_error(#{request => Request, code => <<"closed">>}, Provider), [reply], []},
     %% A relayed error holds the second object of its one-of group: a frame
     %% without either is named by the first, which the one-of test covers.
     {error, macula_frame:relay_error(#{frame_type => error, request => Request, code => unknown_next_peer},
                                      Station),
      [], []},
     {hyparview_join, macula_frame:hyparview_join(#{realm => key(), new_member => key()}), [realm, new_member], []},
     {hyparview_forward_join,
      macula_frame:hyparview_forward_join(#{realm => key(), new_member => key(), ttl => 3, arwl => 3, prwl => 2}),
      [realm, new_member, ttl, arwl, prwl], []},
     {hyparview_neighbor, macula_frame:hyparview_neighbor(#{realm => key(), priority => high}),
      [realm, priority], []},
     {hyparview_disconnect, macula_frame:hyparview_disconnect(#{realm => key()}), [realm], []},
     {hyparview_shuffle,
      macula_frame:hyparview_shuffle(#{realm => key(), origin => key(), ttl => 2, peer_sample => [key()]}),
      [realm, origin, ttl, peer_sample], []},
     {hyparview_shuffle_reply, macula_frame:hyparview_shuffle_reply(#{realm => key(), peer_sample => [key()]}),
      [realm, peer_sample], []},
     {plumtree_gossip, macula_frame:plumtree_gossip(#{publication => publication(), round => 0}),
      [publication, round], []},
     {plumtree_ihave, macula_frame:plumtree_ihave(#{realm => key(), msg_id => msg_id(), round => 0}),
      [realm, msg_id, round], []},
     {plumtree_graft, macula_frame:plumtree_graft(#{realm => key(), msg_id => msg_id(), round => 0}),
      [realm, msg_id, round], []},
     {plumtree_prune, macula_frame:plumtree_prune(#{realm => key()}), [realm], []},
     {overlay_relay, macula_frame:overlay_relay(#{peer => key(), payload => <<"frame bytes">>}), [peer, payload], []},
     {publish, macula_frame:publish(publish_spec(Now), Provider), [publication], []},
     {subscribe, macula_frame:subscribe(#{topic => <<"probe.topic">>, realm => key(), subscriber => key()}),
      [topic, realm, subscriber], []},
     {unsubscribe, macula_frame:unsubscribe(#{topic => <<"probe.topic">>, realm => key(), subscriber => key()}),
      [topic, realm, subscriber], []},
     {event, sample(event), [publication, delivered_via], []},
     {advertise, macula_frame:advertise(#{advertisement => Record}), [advertisement], []},
     {unadvertise, macula_frame:unadvertise(#{withdrawal => Record}), [withdrawal], []},
     {stream_open, macula_frame:stream_open((request_spec())#{mode => bidi}, Caller), [request], []},
     {stream_data, macula_frame:provider_stream(chunk(0), Provider, Open), [stream], []},
     {stream_end, macula_frame:provider_stream(#{frame_type => stream_end, seq => 1, role => both}, Provider, Open),
      [stream], []},
     {stream_error,
      macula_frame:provider_stream(#{frame_type => stream_error, seq => 1, code => <<"stopped">>,
                                     message => <<"no more">>}, Provider, Open),
      [stream], []},
     {stream_reply, macula_frame:provider_stream(#{frame_type => stream_reply, seq => 1, payload => #{n => 1}},
                                                 Provider, Open),
      [stream], []},
     {want, macula_frame:want(#{blocks => [#{mcid => Mcid, priority => 1}]}), [blocks], []},
     {have, macula_frame:have(#{blocks => [#{mcid => Mcid, size => 10}]}), [blocks], []},
     {block, sample(block), [mcid, payload], []},
     {manifest_req, macula_frame:manifest_req(#{mcid => Mcid}), [mcid], []},
     {manifest_res, macula_frame:manifest_res(#{mcid => Mcid, manifest => #{size => 1}}), [mcid, manifest], []},
     {cancel, macula_frame:cancel(#{blocks => [Mcid]}), [blocks], []}].

sample(event) ->
    macula_frame:event(#{publication => publication(), delivered_via => direct});
sample(block) ->
    macula_frame:block(#{mcid => mcid(), payload => <<"block">>}).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% A frame as a peer's bytes arrive: encoded, then decoded.
over_the_wire(Frame) ->
    {ok, Received, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Received.

%% The caller, provider and station keys every sample uses, made once.
keys() ->
    keys(persistent_term:get({?MODULE, keys}, undefined)).

keys(undefined) ->
    Keys = #{caller => identity(), provider => identity(), station => identity()},
    persistent_term:put({?MODULE, keys}, Keys),
    Keys;
keys(Keys) ->
    Keys.

identity() ->
    {ok, Key} = macula_node_keys:generate(identity, ?PROFILE),
    Key.

version() ->
    macula_frame:version(macula_frame:ping(#{nonce => id()})).

request_spec() ->
    #{provider := Provider} = keys(),
    {ok, Target} = macula_node_keys:node_id(Provider),
    #{request_id => id(), realm => key(), procedure => <<"probe.echo">>, target => Target,
      deadline => erlang:system_time(millisecond) + 60_000, payload => #{n => 1}}.

%% The verified request of a CALL as its builder made it. The samples stay
%% clear of decode/1, so a decode that refuses a frame fails the test that
%% decodes it and not the sample list.
verified_call() ->
    #{caller := Caller} = keys(),
    {ok, Request} = macula_frame:verify_request(macula_frame:call(request_spec(), Caller), ?PROFILE),
    Request.

verified_open() ->
    #{caller := Caller} = keys(),
    Open = macula_frame:stream_open((request_spec())#{mode => bidi}, Caller),
    {ok, Request} = macula_frame:verify_request(Open, ?PROFILE),
    Request.

chunk(Seq) ->
    #{frame_type => stream_data, seq => Seq, encoding => raw, body => <<"chunk">>}.

publish_spec(Now) ->
    #{realm => key(), topic => <<"probe.topic">>, seq => 0, published_at => Now, payload => #{n => 1}}.

publication() ->
    #{provider := Provider} = keys(),
    #{publication := Publication} = macula_frame:publish(publish_spec(erlang:system_time(millisecond)), Provider),
    Publication.

record_bytes() ->
    #{station := Station} = keys(),
    {ok, NodeId} = macula_node_keys:node_id(Station),
    macula_record:encode(macula_record:sign(macula_record:node_record(NodeId, [], 0), Station)).

key() -> crypto:strong_rand_bytes(32).

id() -> crypto:strong_rand_bytes(16).

%% A plumtree message id: the SHA-384 of the publication it names.
msg_id() -> crypto:strong_rand_bytes(48).

mcid() -> <<2, 16#55, (crypto:strong_rand_bytes(48))/binary>>.

suspect_spec() ->
    #{target => key(), target_incarnation => 0, suspected_by => key(), ttl => 3}.

station_ref() ->
    station_ref([#{host => <<"station.example">>, port => 4433, transport => quic}]).

station_ref(Addresses) ->
    macula_frame:station_ref(#{node_id => key(), station_id => key(), tier => 2, country => <<"BE">>,
                               last_seen_at => erlang:system_time(millisecond), addresses => Addresses}).
