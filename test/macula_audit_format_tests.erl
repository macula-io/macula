%% EUnit tests for the audit chain's refusals (macula#175 WP1): what the entry, the checkpoint, the export and the file
%% sink refuse, and the small surface the facade adds. The happy paths are in the chain and verifier suites; this file
%% is the other half of the format — the shapes a writer must not be able to smuggle into a chain or an export.
-module(macula_audit_format_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NOW, 1761000000).
-define(NODE, binary:copy(<<16#11>>, 32)).

%%------------------------------------------------------------------
%% The entry
%%------------------------------------------------------------------

the_kinds_an_entry_may_name_are_the_designs_list_test() ->
    ?assertEqual([<<"handshake_refused">>, <<"store_refused">>, <<"call_refused">>, <<"limits_changed">>,
                  <<"key_rotated">>, <<"checkpoint_overdue">>, <<"events_dropped">>],
                 macula_audit:kinds()).

an_entry_keeps_its_sequence_number_test() ->
    {ok, Entry} = macula_audit_entry:new(#{k => <<"store_refused">>, n => 7, t => ?NOW, p => <<>>}),
    ?assertEqual(7, macula_audit_entry:seq(Entry)).

an_entry_missing_a_field_is_refused_test() ->
    ?assertEqual({error, {missing_field, t}},
                 macula_audit_entry:new(#{k => <<"store_refused">>, n => 1, p => <<>>})).

an_entry_with_a_field_nobody_knows_is_refused_test() ->
    ?assertEqual({error, {unknown_field, sneaky}},
                 macula_audit_entry:new(#{k => <<"store_refused">>, n => 1, t => ?NOW, p => <<>>, sneaky => 1})).

an_entry_field_of_the_wrong_type_is_refused_test() ->
    ?assertEqual({error, {bad_field, n}},
                 macula_audit_entry:new(#{k => <<"store_refused">>, n => 0, t => ?NOW, p => <<>>})),
    ?assertEqual({error, {bad_field, s}},
                 macula_audit_entry:new(#{k => <<"store_refused">>, n => 1, t => ?NOW, p => <<>>,
                                          s => <<"not-a-node-id">>})),
    ?assertEqual({error, {bad_field, p}},
                 macula_audit_entry:new(#{k => <<"store_refused">>, n => 1, t => ?NOW,
                                          p => binary:copy(<<0>>, 32)})).

an_entry_of_another_version_is_refused_test() ->
    Bytes = macula_record_cbor:encode(cbor_entry(2)),
    ?assertEqual({error, {bad_field, v}}, macula_audit_entry:decode(Bytes)).

an_entry_with_a_key_outside_the_format_is_refused_test() ->
    Bytes = macula_record_cbor:encode((cbor_entry(1))#{{text, <<"x">>} => 1}),
    ?assertEqual({error, {malformed, {unknown_key, <<"x">>}}}, macula_audit_entry:decode(Bytes)).

an_entry_with_a_key_that_is_not_text_is_refused_test() ->
    Bytes = macula_record_cbor:encode((cbor_entry(1))#{1 => <<"x">>}),
    ?assertEqual({error, {malformed, {bad_key, 1}}}, macula_audit_entry:decode(Bytes)).

bytes_that_are_not_an_entry_are_refused_test() ->
    ?assertEqual({error, {malformed, not_a_map}}, macula_audit_entry:decode(macula_record_cbor:encode([1, 2]))),
    ?assertEqual({error, {malformed, not_bytes}}, macula_audit_entry:decode(not_bytes)).

%%------------------------------------------------------------------
%% The checkpoint
%%------------------------------------------------------------------

a_checkpoint_missing_a_field_cannot_be_signed_test() ->
    ?assertEqual({error, {bad_field, node}},
                 macula_audit_checkpoint:sign(#{v => 1, cs => 1, n => 0, head => <<>>, prev => <<>>, i => 60}, key())).

a_checkpoint_with_a_field_of_the_wrong_shape_cannot_be_signed_test() ->
    ?assertEqual({error, {bad_field, node}},
                 macula_audit_checkpoint:sign(#{v => 1, node => <<"short">>, cs => 1, n => 0,
                                                head => <<>>, prev => <<>>, i => 60}, key())),
    ?assertEqual({error, {bad_field, i}},
                 macula_audit_checkpoint:sign(#{v => 1, node => ?NODE, cs => 1, n => 0,
                                                head => <<>>, prev => <<>>, i => 0}, key())).

a_checkpoint_that_is_not_a_map_cannot_be_signed_test() ->
    ?assertEqual({error, {malformed, not_a_map}}, macula_audit_checkpoint:sign(not_a_map, key())).

bytes_that_are_not_a_checkpoint_are_refused_test() ->
    ?assertEqual({error, {malformed, not_bytes}}, macula_audit_checkpoint:verify(not_bytes, pq_pure)).

%% A checkpoint signed OUTSIDE the module's own field checks: the signature is good, the fields are not the format's.
a_signed_checkpoint_with_fields_outside_the_format_is_refused_test() ->
    Key = key(),
    Object = macula_signed_object:sign(macula_audit_checkpoint:label(),
                                       #{{text, <<"v">>} => 1, {text, <<"node">>} => ?NODE,
                                         {text, <<"cs">>} => 0, {text, <<"n">>} => 0,
                                         {text, <<"head">>} => <<>>, {text, <<"prev">>} => <<>>,
                                         {text, <<"i">>} => 60}, Key),
    ?assertEqual({error, {bad_field, cs}},
                 macula_audit_checkpoint:verify(macula_signed_object:encode(Object), pq_pure)).

a_signed_checkpoint_missing_a_field_is_refused_test() ->
    Key = key(),
    Object = macula_signed_object:sign(macula_audit_checkpoint:label(),
                                       #{{text, <<"v">>} => 1, {text, <<"node">>} => ?NODE,
                                         {text, <<"cs">>} => 1, {text, <<"n">>} => 0,
                                         {text, <<"head">>} => <<>>, {text, <<"prev">>} => <<>>}, Key),
    ?assertEqual({error, {bad_field, i}}, macula_audit_checkpoint:verify(macula_signed_object:encode(Object),
                                                                         pq_pure)).

%%------------------------------------------------------------------
%% The export
%%------------------------------------------------------------------

bytes_that_are_not_an_entry_cannot_be_exported_test() ->
    Header = #{node => ?NODE, segment => 1, sealed => false},
    ?assertEqual({error, {not_an_entry, not_bytes}},
                 macula_audit_export:encode(Header, [not_bytes], [])),
    ?assertEqual({error, {malformed, export_header}},
                 macula_audit_export:encode(#{node => ?NODE}, [], [])).

an_export_that_is_not_bytes_is_refused_test() ->
    ?assertEqual({error, not_bytes}, macula_audit_export:parse(not_bytes)),
    ?assertEqual({error, empty_export}, macula_audit_export:parse(<<>>)).

an_entry_line_missing_a_field_is_refused_test() ->
    ?assertMatch({error, {malformed_entry, 2, {missing, _}}},
                 macula_audit_export:parse(export_with_entry_line(<<"{\"entry\": \"00\", \"n\": 1}\n">>))).

an_entry_line_whose_hex_is_not_hex_is_refused_test() ->
    Line = <<"{\"entry\": \"zz\", \"n\": 1, \"k\": \"store_refused\", \"t\": 1, \"c\": 1,"
             " \"p\": \"\", \"h\": \"\"}\n">>,
    ?assertEqual({error, {malformed_entry, 2, fields}}, macula_audit_export:parse(export_with_entry_line(Line))).

a_checkpoint_line_missing_a_field_is_refused_test() ->
    Line = <<"{\"checkpoint\": \"00\", \"cs\": 1}\n">>,
    ?assertMatch({error, {malformed_checkpoint, {missing, _}}},
                 macula_audit_export:parse(export_with_entry_line(Line))).

a_line_that_is_not_an_object_is_refused_test() ->
    ?assertMatch({error, {malformed_line, 1, not_a_map}}, macula_audit_export:parse(<<"[1,2,3]\n">>)).

%%------------------------------------------------------------------
%% The file sink
%%------------------------------------------------------------------

a_file_with_nothing_in_it_holds_no_entries_test() ->
    Path = filename:join("/tmp", "absent-" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".frames"),
    {ok, Sink} = macula_audit_file_sink:open(Path),
    ?assertEqual({ok, []}, macula_audit_file_sink:entries(Sink)),
    ok = file:delete(Path).

a_truncated_frame_is_refused_rather_than_read_as_an_entry_test() ->
    Path = filename:join("/tmp", "short-" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".frames"),
    try
        ok = file:write_file(Path, <<16#000000FF, 1, 2, 3>>),
        {ok, Sink} = macula_audit_file_sink:open(Path),
        ?assertEqual({error, truncated_frame}, macula_audit_file_sink:entries(Sink))
    after
        file:delete(Path)
    end.

%%------------------------------------------------------------------
%% The facade
%%------------------------------------------------------------------

the_facade_hashes_an_entrys_bytes_the_way_the_chain_does_test() ->
    {ok, Entry} = macula_audit_entry:new(#{k => <<"store_refused">>, n => 1, t => ?NOW, p => <<>>}),
    Bytes = macula_audit_entry:encode(Entry),
    ?assertEqual(crypto:hash(sha384, Bytes), macula_audit:entry_hash(Bytes)).

the_facade_answers_the_receipt_check_test() ->
    Chain = chain(),
    {ok, Checkpoint, _} = macula_audit:checkpoint(Chain, key(), 60),
    ?assertEqual(ok, macula_audit:received(undefined, Checkpoint)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

export_with_entry_line(Line) ->
    Header = <<"{\"chain\": {\"v\": 1, \"node\": \"1111111111111111111111111111111111111111111111111111111111111111\","
               " \"segment\": 1, \"sealed\": false}}\n">>,
    iolist_to_binary([Header, Line]).

cbor_entry(Version) ->
    #{{text, <<"v">>} => Version, {text, <<"n">>} => 1, {text, <<"k">>} => {text, <<"store_refused">>},
      {text, <<"t">>} => ?NOW, {text, <<"c">>} => 1, {text, <<"p">>} => <<>>}.

chain() ->
    {ok, Sink} = macula_audit_memory_sink:new(#{}),
    {ok, Chain} = macula_audit:start(#{node => ?NODE, sink => {macula_audit_memory_sink, Sink}}),
    {ok, Chain1} = macula_audit:append(Chain, <<"store_refused">>, #{t => ?NOW}),
    Chain1.

key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.
