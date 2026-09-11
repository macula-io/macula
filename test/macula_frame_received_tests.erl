%%%-------------------------------------------------------------------
%%% @doc What `macula_frame:validate_received/1' accepts and refuses.
%%%
%%% Every builder's output passes after the wire codec, and so does every
%%% sample frame another SDK sends (`test/fixtures/sdk_frames/<sdk>/'). A
%%% required field that is missing, or holds a value of the wrong type,
%%% refuses the frame and is named in the error. The table in `samples/0'
%%% states each frame type's required fields apart from the validator, so a
%%% change to either one shows up here.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_frame_received_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% Every builder's output passes
%%------------------------------------------------------------------

every_builder_output_passes_test_() ->
    [{atom_to_list(Type),
      ?_assertEqual(ok, macula_frame:validate_received(over_the_wire(Frame)))}
     || {Type, Frame, _Required, _AnyValue} <- samples()].

%%------------------------------------------------------------------
%% A missing or mistyped required field is refused, and named
%%------------------------------------------------------------------

a_missing_required_field_is_named_test_() ->
    [{lists:concat([Type, " without ", Field]),
      ?_assertEqual({error, {invalid_frame, Type, Field}},
                    macula_frame:validate_received(
                      maps:remove(Field, over_the_wire(Frame))))}
     || {Type, Frame, Required, _AnyValue} <- samples(), Field <- Required].

a_required_field_of_the_wrong_type_is_named_test_() ->
    [{lists:concat([Type, " with a mistyped ", Field]),
      ?_assertEqual({error, {invalid_frame, Type, Field}},
                    macula_frame:validate_received(
                      (over_the_wire(Frame))#{Field => {not_a_valid_value}}))}
     || {Type, Frame, Required, AnyValue} <- samples(), Field <- Required -- AnyValue].

%%------------------------------------------------------------------
%% Entries, unknown types, and a GOODBYE's reason
%%------------------------------------------------------------------

the_entries_of_a_list_are_checked_test() ->
    Mcid = sample_mcid(),
    Want = over_the_wire(macula_frame:want(#{blocks => [#{mcid => Mcid}]})),
    ?assertEqual({error, {invalid_frame, want, blocks}},
                 macula_frame:validate_received(Want#{blocks => [#{priority => 1}]})),
    ?assertEqual({error, {invalid_frame, want, blocks}},
                 macula_frame:validate_received(Want#{blocks => [#{mcid => Mcid} | improper]})),
    Nodes = over_the_wire(macula_frame:nodes(#{key => key(), nodes => [sample_station_ref()]})),
    [Ref] = maps:get(nodes, Nodes),
    ?assertEqual({error, {invalid_frame, nodes, nodes}},
                 macula_frame:validate_received(Nodes#{nodes => [Ref#{tier => 9}]})),
    Value = over_the_wire(macula_frame:value(#{key => key(), records => [sample_record()]})),
    ?assertEqual({error, {invalid_frame, value, records}},
                 macula_frame:validate_received(
                   Value#{records => [#{type => node_record, key => <<0:8>>, payload => #{}}]})).

a_value_outside_its_rule_is_refused_test() ->
    Now = erlang:system_time(millisecond),
    Refused = fun(#{frame_type := Type} = Frame, Field) ->
                  ?assertEqual({error, {invalid_frame, Type, Field}},
                               macula_frame:validate_received(Frame))
              end,
    Call = over_the_wire(macula_frame:call(#{call_id => id(), procedure => <<"probe.echo">>,
                                            realm => key(), payload => #{},
                                            deadline_ms => Now, caller => key()})),
    Refused(Call#{caller => <<0:248>>}, caller),
    Refused(Call#{call_id => <<0:120>>}, call_id),
    Refused(Call#{procedure => {text, <<"probe.echo">>}}, procedure),
    Open = over_the_wire(macula_frame:stream_open(#{stream_id => id(), procedure => <<"probe.feed">>,
                                                   realm => key(), mode => bidi, args => #{},
                                                   deadline_ms => Now, caller => key()})),
    Refused(Open#{mode => sideways}, mode),
    Error = over_the_wire(macula_frame:call_error(#{call_id => id(), code => 1,
                                                   reported_by => key()})),
    Refused(Error#{code => 256}, code),
    Ping = over_the_wire(macula_frame:swim_ping(#{round => 0, incarnation => 0})),
    Refused(Ping#{round => -1}, round).

a_frame_type_this_node_does_not_know_passes_test() ->
    ?assertEqual(ok, macula_frame:validate_received(#{frame_type => a_future_frame_type})),
    ?assertEqual(ok, macula_frame:validate_received(
                       #{frame_type => {text, <<"a_future_frame_type">>}})).

a_frame_without_a_type_is_refused_test() ->
    ?assertEqual({error, {invalid_frame, unknown, frame_type}},
                 macula_frame:validate_received(#{call_id => <<0:128>>})).

a_goodbye_reason_may_be_text_test() ->
    Goodbye = over_the_wire(macula_frame:goodbye(normal, undefined)),
    ?assertEqual(ok, macula_frame:validate_received(
                       Goodbye#{reason => {text, <<"a reason this node has no atom for">>}})),
    ?assertEqual(ok, macula_frame:validate_received(Goodbye#{reason => <<"shutting down">>})),
    ?assertEqual({error, {invalid_frame, goodbye, reason}},
                 macula_frame:validate_received(Goodbye#{reason => 42})).

%%------------------------------------------------------------------
%% Frames the other SDKs send pass
%%------------------------------------------------------------------

frames_other_sdks_send_pass_test_() ->
    [{lists:concat([Sdk, " ", filename:basename(File)]),
      ?_assertEqual(ok, received_sample(File))}
     || {Sdk, File} <- sdk_samples()].

the_sdk_samples_are_there_test() ->
    Sdks = lists:usort([Sdk || {Sdk, _File} <- sdk_samples()]),
    ?assert(lists:member("go", Sdks)).

sdk_samples() ->
    Root = filename:join([filename:dirname(?FILE), "fixtures", "sdk_frames"]),
    [{filename:basename(filename:dirname(File)), File}
     || File <- lists:sort(filelib:wildcard(filename:join([Root, "*", "*.bin"])))].

received_sample(File) ->
    {ok, Bytes} = file:read_file(File),
    {ok, Frame, <<>>} = macula_frame:decode(Bytes),
    macula_frame:validate_received(Frame).

%%------------------------------------------------------------------
%% One frame per type, with its required fields
%%------------------------------------------------------------------

%% {Type, Frame, required fields, required fields any value fills}
samples() ->
    Now = erlang:system_time(millisecond),
    Mcid = sample_mcid(),
    [{connect, macula_frame:connect(#{node_id => key(), station_id => key(), realms => [key()],
                                     capabilities => 0, puzzle_evidence => key()}),
      [node_id, station_id, realms, capabilities, puzzle_evidence], []},
     {hello, macula_frame:hello(#{node_id => key(), station_id => key(), realms => [],
                                 capabilities => 0, accepted => true,
                                 negotiated_capabilities => 0}),
      [node_id, station_id, realms, capabilities, accepted, negotiated_capabilities], []},
     {goodbye, macula_frame:goodbye(normal, <<"bye">>), [reason], []},
     {swim_ping, macula_frame:swim_ping(#{round => 1, incarnation => 0}),
      [round, incarnation], []},
     {swim_ack, macula_frame:swim_ack(#{round => 1, responder => key(), incarnation => 0}),
      [round, responder, incarnation], []},
     {swim_suspect, macula_frame:swim_suspect(suspect_spec()),
      [target, target_incarnation, suspected_by, ttl], []},
     {swim_confirm, macula_frame:swim_confirm(suspect_spec()),
      [target, target_incarnation, suspected_by, ttl], []},
     {ping, macula_frame:ping(#{nonce => id()}), [nonce], []},
     {pong, macula_frame:pong(#{nonce => id()}), [nonce], []},
     {find_node, macula_frame:find_node(#{key => key(), origin => key(), depth => 0}),
      [key, origin, depth], []},
     {nodes, macula_frame:nodes(#{key => key(), nodes => [sample_station_ref()]}),
      [key, nodes], []},
     {find_value, macula_frame:find_value(#{key => key(), origin => key()}), [key, origin], []},
     {value, macula_frame:value(#{key => key(), records => [sample_record()]}),
      [key, records], []},
     {store, macula_frame:store(#{record => sample_record()}), [record], []},
     {store_ack, macula_frame:store_ack(#{key => key(), stored => true}), [key, stored], []},
     {replicate, macula_frame:replicate(#{record => sample_record(), new_custodian => false}),
      [record, new_custodian], []},
     {replicate_ack, macula_frame:replicate_ack(#{key => key(), accepted => true}),
      [key, accepted], []},
     {call, macula_frame:call(#{call_id => id(), procedure => <<"probe.echo">>, realm => key(),
                               payload => #{n => 1}, deadline_ms => Now + 5_000,
                               caller => key()}),
      [call_id, procedure, realm, payload, deadline_ms, caller], [payload]},
     {result, macula_frame:result(#{call_id => id(), payload => #{n => 1},
                                   responded_by => key()}),
      [call_id, payload, responded_by], [payload]},
     {error, macula_frame:call_error(#{call_id => id(), code => 1, reported_by => key()}),
      [call_id, code, reported_by], []},
     {hyparview_join, macula_frame:hyparview_join(#{realm => key(), new_member => key()}),
      [realm, new_member], []},
     {hyparview_forward_join,
      macula_frame:hyparview_forward_join(#{realm => key(), new_member => key(), ttl => 3,
                                            arwl => 3, prwl => 2}),
      [realm, new_member, ttl, arwl, prwl], []},
     {hyparview_neighbor, macula_frame:hyparview_neighbor(#{realm => key(), priority => high}),
      [realm, priority], []},
     {hyparview_disconnect, macula_frame:hyparview_disconnect(#{realm => key()}), [realm], []},
     {hyparview_shuffle,
      macula_frame:hyparview_shuffle(#{realm => key(), origin => key(), ttl => 2,
                                       peer_sample => [key()]}),
      [realm, origin, ttl, peer_sample], []},
     {hyparview_shuffle_reply,
      macula_frame:hyparview_shuffle_reply(#{realm => key(), peer_sample => [key()]}),
      [realm, peer_sample], []},
     {plumtree_gossip, macula_frame:plumtree_gossip(#{realm => key(), msg_id => id(), round => 0,
                                                     payload => #{n => 1}}),
      [realm, msg_id, round, payload], [payload]},
     {plumtree_ihave, macula_frame:plumtree_ihave(#{realm => key(), msg_id => id(), round => 0}),
      [realm, msg_id, round], []},
     {plumtree_graft, macula_frame:plumtree_graft(#{realm => key(), msg_id => id(), round => 0}),
      [realm, msg_id, round], []},
     {plumtree_prune, macula_frame:plumtree_prune(#{realm => key()}), [realm], []},
     {overlay_relay, macula_frame:overlay_relay(#{peer => key(), payload => <<"frame bytes">>}),
      [peer, payload], []},
     {publish, macula_frame:publish(#{topic => <<"probe.topic">>, realm => key(),
                                     publisher => key(), seq => 0, payload => #{n => 1},
                                     published_at_ms => Now}),
      [topic, realm, publisher, seq, payload, published_at_ms], [payload]},
     {subscribe, macula_frame:subscribe(#{topic => <<"probe.topic">>, realm => key(),
                                         subscriber => key()}),
      [topic, realm, subscriber], []},
     {unsubscribe, macula_frame:unsubscribe(#{topic => <<"probe.topic">>, realm => key(),
                                             subscriber => key()}),
      [topic, realm, subscriber], []},
     {event, macula_frame:event(#{topic => <<"probe.topic">>, realm => key(), publisher => key(),
                                 seq => 0, payload => #{n => 1}, delivered_via => direct}),
      [topic, realm, publisher, seq, payload, delivered_via], [payload]},
     {advertise, macula_frame:advertise(#{realm => key(), procedure => <<"probe.echo">>,
                                         advertiser => key()}),
      [realm, procedure, advertiser], []},
     {unadvertise, macula_frame:unadvertise(#{realm => key(), procedure => <<"probe.echo">>,
                                             advertiser => key()}),
      [realm, procedure, advertiser], []},
     {stream_open, macula_frame:stream_open(#{stream_id => id(), procedure => <<"probe.feed">>,
                                             realm => key(), mode => server_stream,
                                             args => #{}, deadline_ms => Now + 5_000,
                                             caller => key()}),
      [stream_id, procedure, realm, mode, args, deadline_ms, caller], [args]},
     {stream_data, macula_frame:stream_data(#{stream_id => id(), seq => 0, encoding => raw,
                                             body => <<"chunk">>}),
      [stream_id, seq, encoding, body], [body]},
     {stream_end, macula_frame:stream_end(#{stream_id => id(), role => both}),
      [stream_id, role], []},
     {stream_error, macula_frame:stream_error(#{stream_id => id(), code => <<"error">>,
                                               message => <<"stopped">>}),
      [stream_id, code, message], []},
     {stream_reply, macula_frame:stream_reply(#{stream_id => id(), payload => #{n => 1},
                                               responded_by => key()}),
      [stream_id, payload, responded_by], [payload]},
     {want, macula_frame:want(#{blocks => [#{mcid => Mcid, priority => 1}]}), [blocks], []},
     {have, macula_frame:have(#{blocks => [#{mcid => Mcid, size => 10}]}), [blocks], []},
     {block, macula_frame:block(#{mcid => Mcid, payload => <<"block">>}), [mcid, payload], []},
     {manifest_req, macula_frame:manifest_req(#{mcid => Mcid}), [mcid], []},
     {manifest_res, macula_frame:manifest_res(#{mcid => Mcid, manifest => #{size => 1}}),
      [mcid, manifest], []},
     {cancel, macula_frame:cancel(#{blocks => [Mcid]}), [blocks], []}].

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% A frame as a peer's bytes arrive: encoded, then decoded.
over_the_wire(Frame) ->
    {ok, Received, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Received.

key() -> crypto:strong_rand_bytes(32).

id() -> crypto:strong_rand_bytes(16).

sample_mcid() -> <<1, 0, (crypto:strong_rand_bytes(32))/binary>>.

suspect_spec() ->
    #{target => key(), target_incarnation => 0, suspected_by => key(), ttl => 3}.

sample_station_ref() ->
    macula_frame:station_ref(#{node_id => key(), station_id => key(), addresses => [],
                               tier => 2, asn => 64512, country => <<"BE">>,
                               last_seen_at => erlang:system_time(millisecond)}).

sample_record() ->
    Kp = macula_identity:generate(),
    Node = macula_record:node_record(macula_identity:public(Kp), [], 0),
    macula_record:sign(Node, Kp).
