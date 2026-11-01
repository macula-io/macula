%% EUnit tests for the audit chain's verifier (macula#175 WP1): one test per finding the design names, plus the two
%% rules that decide between "intact" and "watched" — a sealed chain must be anchored, and the only clock the verifier
%% reads is the anchor destination's receipt time.
%%
%% Each test breaks ONE thing in an export that is otherwise clean, so the finding it expects is the first bad place
%% and not a consequence of an earlier one.
-module(macula_audit_verifier_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NOW, 1761000000).
-define(INTERVAL, 3600).
-define(NODE, binary:copy(<<16#11>>, 32)).

%%------------------------------------------------------------------
%% The clean chain
%%------------------------------------------------------------------

an_intact_chain_with_a_fresh_anchor_passes_test() ->
    #{export := Export, anchor := Anchor, node := Node} = fixture(),
    {ok, Report} = macula_audit:verify(Export, [Anchor], #{now => ?NOW}),
    ?assertEqual(3, maps:get(entries, Report)),
    ?assertEqual(1, maps:get(checkpoints, Report)),
    ?assertEqual(Node, maps:get(node, Report)).

a_chain_nobody_anchored_is_overdue_and_never_ok_test() ->
    #{export := Export} = fixture(),
    ?assertMatch({overdue, _}, macula_audit:verify(Export, [], #{now => ?NOW})).

an_anchor_older_than_the_promised_interval_is_overdue_test() ->
    #{export := Export, anchor := Anchor} = fixture(),
    Stale = Anchor#{received_at => ?NOW - ?INTERVAL - 60},
    {overdue, Report} = macula_audit:verify(Export, [Stale], #{now => ?NOW}),
    ?assertEqual(?INTERVAL + 60, maps:get(overdue_by, Report)).

an_export_supplied_timestamp_cannot_change_the_overdue_result_test() ->
    %% Every entry in this chain claims to have been written a year in the future, and the bytes agree with the
    %% fields. The verifier must still read the chain as overdue, because an entry's `t' is a hint the node writes
    %% about itself and never a clock.
    #{export := Export, anchor := Anchor} = fixture(?NOW + 31536000),
    Stale = Anchor#{received_at => ?NOW - ?INTERVAL - 60},
    ?assertMatch({overdue, _}, macula_audit:verify(Export, [Stale], #{now => ?NOW})).

%%------------------------------------------------------------------
%% The entries
%%------------------------------------------------------------------

an_edited_entry_byte_is_caught_test() ->
    #{export := Export, anchor := Anchor} = fixture(),
    Parsed = parsed(Export),
    [First, Second, Third] = maps:get(entries, Parsed),
    Edited = Second#{bytes => flip(maps:get(bytes, Second), 2)},
    ?assertEqual({error, [{entry_hash_mismatch, 2}]},
                 macula_audit:verify(Parsed#{entries => [First, Edited, Third]}, [Anchor], #{now => ?NOW})).

a_readable_field_that_disagrees_with_the_bytes_is_caught_test() ->
    #{export := Export, anchor := Anchor} = fixture(),
    Parsed = parsed(Export),
    [First | Rest] = maps:get(entries, Parsed),
    Lying = First#{k => <<"store_refused">>},
    ?assertEqual({error, [{fields_bytes_mismatch, 1}]},
                 macula_audit:verify(Parsed#{entries => [Lying | Rest]}, [Anchor], #{now => ?NOW})).

a_deleted_entry_is_caught_test() ->
    #{export := Export, anchor := Anchor} = fixture(),
    Parsed = parsed(Export),
    [First, _Second, Third] = maps:get(entries, Parsed),
    ?assertEqual({error, [{sequence_gap, 2, 3}]},
                 macula_audit:verify(Parsed#{entries => [First, Third]}, [Anchor], #{now => ?NOW})).

a_rewritten_link_is_caught_test() ->
    #{export := Export, anchor := Anchor} = fixture(),
    Parsed = parsed(Export),
    [First, _Second, Third] = maps:get(entries, Parsed),
    WrongLink = line(2, binary:copy(<<16#AB>>, 48), <<"store_refused">>),
    ?assertEqual({error, [{prev_hash_mismatch, 2}]},
                 macula_audit:verify(Parsed#{entries => [First, WrongLink, Third]}, [Anchor], #{now => ?NOW})).

entries_cut_after_a_checkpoint_are_caught_test() ->
    #{export := Export, anchor := Anchor} = fixture(),
    Parsed = parsed(Export),
    [First, Second, _Third] = maps:get(entries, Parsed),
    ?assertEqual({error, [{truncated_after_checkpoint, 1, 3}]},
                 macula_audit:verify(Parsed#{entries => [First, Second]}, [Anchor], #{now => ?NOW})).

%% The case a checkpoint catches with no anchor at all: the entries were rewritten CONSISTENTLY, so every hash and
%% every link in the export agrees with every other, and the only thing that disagrees is the checkpoint signed before
%% the rewrite.
a_chain_rewritten_after_a_checkpoint_is_caught_by_the_checkpoint_alone_test() ->
    #{export := Export} = fixture(),
    Parsed = parsed(Export),
    ?assertEqual({error, [{checkpoint_head_mismatch, 1, 3}]},
                 macula_audit:verify(Parsed#{entries => rewrite()}, [], #{now => ?NOW})).

a_sealed_chain_cut_short_and_exported_as_unsealed_is_never_clean_test() ->
    %% A hostile or careless exporter says "still running" after the node rotated its key and the tail was cut. It
    %% must not read as clean: the checkpoint claims entries the export does not hold.
    #{export := Export, anchor := Anchor} = fixture(),
    Parsed = parsed(Export),
    [First, _Second, _Third] = maps:get(entries, Parsed),
    ?assertMatch({error, _},
                 macula_audit:verify(Parsed#{header => (maps:get(header, Parsed))#{sealed => false},
                                             entries => [First]}, [Anchor], #{now => ?NOW})).

%%------------------------------------------------------------------
%% The checkpoints and the anchors
%%------------------------------------------------------------------

a_forged_checkpoint_is_caught_test() ->
    #{export := Export, anchor := Anchor, key := Key, node := Node} = fixture(),
    Parsed = parsed(Export),
    [Checkpoint] = maps:get(checkpoints, Parsed),
    Fields = #{{text, <<"v">>} => 1, {text, <<"node">>} => Node, {text, <<"cs">>} => 1,
               {text, <<"n">>} => 3, {text, <<"head">>} => macula_audit:entry_hash(<<>>),
               {text, <<"prev">>} => <<>>, {text, <<"i">>} => ?INTERVAL},
    Transport = macula_signed_object:encode(macula_signed_object:sign(<<"MACULA-FRAME-V1">>, Fields, Key)),
    ?assertEqual({error, [{checkpoint_signature_invalid, 1}]},
                 macula_audit:verify(Parsed#{checkpoints => [Checkpoint#{bytes => Transport}]},
                                     [Anchor], #{now => ?NOW})).

a_checkpoint_that_does_not_extend_the_previous_one_is_caught_test() ->
    #{export := Export, anchor := Anchor, key := Key, checkpoint := First, node := Node} = fixture(),
    Parsed = parsed(Export),
    Fork = element(2, macula_audit_checkpoint:sign(#{v => 1, node => Node, cs => 2, n => 3,
                                                     head => macula_audit_checkpoint:head(First),
                                                     prev => binary:copy(<<16#AB>>, 48),
                                                     i => ?INTERVAL}, Key)),
    Lines = maps:get(checkpoints, Parsed) ++ [#{bytes => macula_audit_checkpoint:encode_bytes(Fork),
                                                cs => 2, n => 3, head => <<>>, prev => <<>>}],
    ?assertEqual({error, [{checkpoint_not_extending, 2}]},
                 macula_audit:verify(Parsed#{checkpoints => Lines}, [Anchor], #{now => ?NOW})).

a_replayed_checkpoint_is_caught_test() ->
    #{export := Export, anchor := Anchor, key := Key, node := Node} = fixture(),
    Parsed = parsed(Export),
    [Checkpoint] = maps:get(checkpoints, Parsed),
    Other = element(2, macula_audit_checkpoint:sign(#{v => 1, node => Node, cs => 1, n => 2,
                                                      head => binary:copy(<<16#CD>>, 48), prev => <<>>,
                                                      i => ?INTERVAL}, Key)),
    Replayed = Checkpoint#{bytes => macula_audit_checkpoint:encode_bytes(Other), cs => 1},
    ?assertEqual({error, [{checkpoint_replayed, 1}]},
                 macula_audit:verify(Parsed#{checkpoints => [Checkpoint, Replayed]}, [Anchor], #{now => ?NOW})).

an_anchor_that_disagrees_with_its_checkpoint_is_caught_test() ->
    #{export := Export, anchor := Anchor} = fixture(),
    ?assertEqual({error, [{anchor_mismatch, 1}]},
                 macula_audit:verify(Export, [Anchor#{head => binary:copy(<<16#EE>>, 48)}], #{now => ?NOW})).

an_anchor_whose_checkpoint_was_cut_out_is_caught_test() ->
    #{export := Export, anchor := Anchor} = fixture(),
    Parsed = parsed(Export),
    ?assertEqual({error, [{truncated_after_checkpoint, 1, 3}]},
                 macula_audit:verify(Parsed#{checkpoints => []}, [Anchor], #{now => ?NOW})).

a_sealed_chain_must_be_anchored_test() ->
    #{export := Export, anchor := Anchor} = fixture(),
    Parsed = parsed(Export),
    Sealed = Parsed#{header => (maps:get(header, Parsed))#{sealed => true}},
    ?assertEqual({error, [{sealed_without_anchor, 1}]}, macula_audit:verify(Sealed, [], #{now => ?NOW})),
    ?assertMatch({ok, _}, macula_audit:verify(Sealed, [Anchor], #{now => ?NOW})).

%%------------------------------------------------------------------
%% The order of the lines
%%------------------------------------------------------------------

an_entry_after_a_checkpoint_is_refused_test() ->
    #{export := Export} = fixture(),
    Lines = lines(Export),
    [Header | Rest] = Lines,
    {Entries, [Checkpoint | _More]} = split_entries(Rest),
    Out_Of_Order = join([Header, Checkpoint] ++ Entries),
    ?assertMatch({error, [{malformed_export, {out_of_order, entry_after_checkpoint}}]},
                 macula_audit:verify(Out_Of_Order, [], #{now => ?NOW})).

a_second_header_is_refused_test() ->
    #{export := Export} = fixture(),
    Lines = lines(Export),
    [Header | Rest] = Lines,
    Doubled = join([Header, Header] ++ Rest),
    ?assertMatch({error, [{malformed_export, {out_of_order, second_header}}]},
                 macula_audit:verify(Doubled, [], #{now => ?NOW})).

a_repeated_entry_sequence_is_refused_test() ->
    #{export := Export} = fixture(),
    Lines = lines(Export),
    [Header, First, Second | Rest] = Lines,
    Repeated = join([Header, First, Second, First] ++ Rest),
    ?assertMatch({error, [{malformed_export, {out_of_order, {entry, 1}}}]},
                 macula_audit:verify(Repeated, [], #{now => ?NOW})).

a_line_that_is_not_json_is_refused_test() ->
    #{export := Export} = fixture(),
    Lines = lines(Export),
    [Header | Rest] = Lines,
    Broken = join([Header, <<"{\"entry\": ">>] ++ Rest),
    ?assertMatch({error, [{malformed_export, _}]},
                 macula_audit:verify(Broken, [], #{now => ?NOW})).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

fixture() ->
    fixture(?NOW).

fixture(Time) ->
    Key = key(),
    Node = node_of(Key),
    {ok, Sink} = macula_audit_memory_sink:new(#{}),
    {ok, C0} = macula_audit:start(#{node => Node, sink => {macula_audit_memory_sink, Sink}}),
    {ok, C1} = macula_audit:append(C0, <<"handshake_refused">>, #{t => Time, s => binary:copy(<<16#22>>, 32)}),
    {ok, C2} = macula_audit:append(C1, <<"store_refused">>, #{t => Time + 1, r => <<"no space">>}),
    {ok, C3} = macula_audit:append(C2, <<"call_refused">>, #{t => Time + 2, c => 5}),
    {ok, Checkpoint, C4} = macula_audit:checkpoint(C3, Key, ?INTERVAL),
    {ok, Export} = macula_audit:export(C4, false),
    #{key => Key, node => Node, checkpoint => Checkpoint, export => iolist_to_binary(Export),
      anchor => anchor(Checkpoint, ?NOW)}.

anchor(Checkpoint, ReceivedAt) ->
    #{cs => macula_audit_checkpoint:cs(Checkpoint),
      n => macula_audit_checkpoint:count(Checkpoint),
      head => macula_audit_checkpoint:head(Checkpoint),
      prev => maps:get(prev, Checkpoint),
      hash => macula_audit_checkpoint:hash(Checkpoint),
      received_at => ReceivedAt}.

parsed(Export) ->
    {ok, Parsed} = macula_audit_export:parse(Export),
    Parsed.

lines(Export) ->
    [Line || Line <- binary:split(Export, <<"\n">>, [global]), Line =/= <<>>].

join(Lines) ->
    iolist_to_binary([[Line, <<"\n">>] || Line <- Lines]).

split_entries(Lines) ->
    lists:splitwith(fun(Line) -> binary:match(Line, <<"\"entry\"">>) =/= nomatch end, Lines).

%% Three entries that agree with each other in every hash and every link, and disagree with the signed checkpoint.
rewrite() ->
    One = line(1, <<>>, <<"call_refused">>),
    Two = line(2, maps:get(h, One), <<"call_refused">>),
    Three = line(3, maps:get(h, Two), <<"call_refused">>),
    [One, Two, Three].

line(Seq, Prev, Kind) ->
    {ok, Entry} = macula_audit_entry:new(#{k => Kind, n => Seq, t => ?NOW + 900, p => Prev}),
    Bytes = macula_audit_entry:encode(Entry),
    #{bytes => Bytes, h => macula_audit:entry_hash(Bytes), n => Seq, k => Kind, t => ?NOW + 900,
      c => 1, p => Prev}.

flip(Bytes, At) ->
    <<Head:At/binary, Byte, Tail/binary>> = Bytes,
    <<Head/binary, (Byte bxor 1), Tail/binary>>.

key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.

node_of(Key) ->
    {ok, Node} = macula_node_keys:node_id(Key),
    Node.
