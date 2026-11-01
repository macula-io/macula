%% EUnit tests for the audit chain's writing side (macula#175 WP1): entries numbered and linked by the hash of the
%% previous entry's own bytes, a restart that continues a chain instead of replaying it, a checkpoint that refuses to
%% sign what the sink has not synced, and the fork and replay check a destination runs when a checkpoint arrives.
-module(macula_audit_chain_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NOW, 1760000000).
-define(INTERVAL, 3600).

%%------------------------------------------------------------------
%% Entries
%%------------------------------------------------------------------

entries_are_numbered_from_one_and_linked_by_hash_test() ->
    Chain = three_entries(),
    {ok, Stored} = macula_audit_chain:entries(Chain),
    Entries = [decode(Bytes) || Bytes <- Stored],
    ?assertEqual([1, 2, 3], [maps:get(n, Entry) || Entry <- Entries]),
    [First, Second, Third] = Entries,
    ?assertEqual(<<>>, maps:get(p, First)),
    ?assertEqual(macula_audit:entry_hash(lists:nth(1, Stored)), maps:get(p, Second)),
    ?assertEqual(macula_audit:entry_hash(lists:nth(2, Stored)), maps:get(p, Third)).

the_entrys_bytes_carry_its_own_hash_so_nothing_is_reencoded_test() ->
    Chain = three_entries(),
    {ok, [First | _]} = macula_audit_chain:entries(Chain),
    ?assertEqual(48, byte_size(macula_audit:entry_hash(First))),
    ?assertEqual(macula_audit:entry_hash(First), crypto:hash(sha384, First)).

a_kind_outside_the_list_is_refused_test() ->
    Chain = chain(sink()),
    ?assertEqual({error, {bad_field, k}},
                 macula_audit:append(Chain, <<"something_happened">>, #{t => ?NOW})).

an_entry_the_sink_refuses_leaves_the_chain_unchanged_test() ->
    Sink = sink(#{append => {error, disk_full}}),
    Chain = chain(Sink),
    ?assertEqual({error, {sink_refused, disk_full}},
                 macula_audit:append(Chain, <<"store_refused">>, #{t => ?NOW})),
    ?assertEqual(0, macula_audit_chain:count(Chain)),
    ?assertEqual({ok, []}, macula_audit_chain:entries(Chain)).

a_restart_continues_the_chain_the_sink_holds_test() ->
    Chain = three_entries(),
    {ok, Restarted} = macula_audit:start(#{node => node_id(),
                                         sink => {macula_audit_memory_sink, sink_state(Chain)}}),
    ?assertEqual(3, macula_audit_chain:count(Restarted)),
    {ok, Continued} = macula_audit:append(Restarted, <<"call_refused">>, #{t => ?NOW}),
    {ok, Stored} = macula_audit_chain:entries(Continued),
    ?assertEqual(4, maps:get(n, decode(lists:nth(4, Stored)))),
    ?assertEqual(macula_audit:entry_hash(lists:nth(3, Stored)),
                 maps:get(p, decode(lists:nth(4, Stored)))).

a_sink_that_holds_a_broken_chain_is_refused_at_start_test() ->
    %% Two entries whose link does not hold: the second names a previous hash nobody wrote.
    Sink = sink(),
    Broken = [entry_bytes(1, <<>>), entry_bytes(2, binary:copy(<<16#AA>>, 48))],
    Primed = lists:foldl(fun(Bytes, {macula_audit_memory_sink, State}) ->
                                 {ok, Next} = macula_audit_memory_sink:append(Bytes, State),
                                 {macula_audit_memory_sink, Next}
                         end, Sink, Broken),
    ?assertMatch({error, {sink_chain_broken, _}},
                 macula_audit:start(#{node => node_id(), sink => Primed})).

%%------------------------------------------------------------------
%% Checkpoints
%%------------------------------------------------------------------

a_file_sink_carries_the_chain_across_a_restart_test() ->
    Path = filename:join("/tmp", "audit-" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".frames"),
    try
        {ok, Sink} = macula_audit_file_sink:open(Path),
        {ok, Chain0} = macula_audit:start(#{node => node_id(), sink => {macula_audit_file_sink, Sink}}),
        Chain = three_entries(Chain0),
        {ok, Checkpoint, _} = macula_audit:checkpoint(Chain, key(), ?INTERVAL),
        ?assertEqual(3, macula_audit_checkpoint:count(Checkpoint)),
        {ok, Reopened} = macula_audit_file_sink:open(Path),
        {ok, Restarted} = macula_audit:start(#{node => node_id(), sink => {macula_audit_file_sink, Reopened}}),
        ?assertEqual(3, macula_audit_chain:count(Restarted)),
        {ok, Stored} = macula_audit_chain:entries(Restarted),
        ?assertEqual(3, length(Stored))
    after
        file:delete(Path)
    end.

a_checkpoint_is_refused_when_the_sink_cannot_sync_test() ->
    Sink = sink(#{sync => {error, disk_full}}),
    Chain = chain(Sink),
    {ok, Chain1} = macula_audit:append(Chain, <<"store_refused">>, #{t => ?NOW}),
    ?assertEqual({error, {sink_unsynced, disk_full}},
                 macula_audit:checkpoint(Chain1, key(), ?INTERVAL)),
    ?assertEqual([], macula_audit_chain:checkpoints(Chain1)).

the_sink_is_synced_before_a_checkpoint_is_signed_test() ->
    Sink = sink(),
    Chain = three_entries(chain(Sink)),
    ?assertEqual(#{appends => 3, syncs => 0}, macula_audit_memory_sink:calls(sink_state(Chain))),
    {ok, _Checkpoint, Chain1} = macula_audit:checkpoint(Chain, key(), ?INTERVAL),
    ?assertEqual(#{appends => 3, syncs => 1}, macula_audit_memory_sink:calls(sink_state(Chain1))).

a_first_checkpoint_has_the_first_sequence_and_no_previous_hash_test() ->
    Chain = three_entries(),
    {ok, Checkpoint, _} = macula_audit:checkpoint(Chain, key(), ?INTERVAL),
    ?assertEqual(1, macula_audit_checkpoint:cs(Checkpoint)),
    ?assertEqual(3, macula_audit_checkpoint:count(Checkpoint)),
    ?assertEqual(<<>>, maps:get(prev, Checkpoint)),
    {ok, Stored} = macula_audit_chain:entries(Chain),
    ?assertEqual(macula_audit:entry_hash(lists:nth(3, Stored)), macula_audit_checkpoint:head(Checkpoint)).

a_second_checkpoint_extends_the_first_test() ->
    Chain = three_entries(),
    {ok, First, Chain1} = macula_audit:checkpoint(Chain, key(), ?INTERVAL),
    {ok, Chain2} = macula_audit:append(Chain1, <<"key_rotated">>, #{t => ?NOW + 3}),
    {ok, Second, _} = macula_audit:checkpoint(Chain2, key(), ?INTERVAL),
    ?assertEqual(2, macula_audit_checkpoint:cs(Second)),
    ?assertEqual(macula_audit_checkpoint:hash(First), maps:get(prev, Second)),
    ?assertEqual(4, macula_audit_checkpoint:count(Second)).

a_checkpoint_verifies_under_its_own_label_test() ->
    Chain = three_entries(),
    {ok, Checkpoint, _} = macula_audit:checkpoint(Chain, key(), ?INTERVAL),
    Bytes = macula_audit_checkpoint:encode_bytes(Checkpoint),
    ?assertMatch({ok, _}, macula_audit_checkpoint:verify(Bytes, pq_pure)).

a_checkpoint_is_not_a_transport_object_test() ->
    Chain = three_entries(),
    Key = key(),
    {ok, Checkpoint, _} = macula_audit:checkpoint(Chain, Key, ?INTERVAL),
    Fields = #{{text, <<"v">>} => 1, {text, <<"node">>} => node_id(),
               {text, <<"cs">>} => 1, {text, <<"n">>} => 3,
               {text, <<"head">>} => macula_audit_checkpoint:head(Checkpoint),
               {text, <<"prev">>} => <<>>, {text, <<"i">>} => ?INTERVAL},
    Transport = macula_signed_object:encode(macula_signed_object:sign(<<"MACULA-FRAME-V1">>, Fields, Key)),
    ?assertMatch({error, _}, macula_audit_checkpoint:verify(Transport, pq_pure)).

an_edited_checkpoint_fails_its_signature_test() ->
    Chain = three_entries(),
    {ok, Checkpoint, _} = macula_audit:checkpoint(Chain, key(), ?INTERVAL),
    Bytes = macula_audit_checkpoint:encode_bytes(Checkpoint),
    Edited = flip(Bytes, byte_size(Bytes) - 3),
    ?assertMatch({error, _}, macula_audit_checkpoint:verify(Edited, pq_pure)).

%%------------------------------------------------------------------
%% Receipt: the fork and replay check a destination runs
%%------------------------------------------------------------------

an_unchanged_checkpoint_delivered_twice_is_not_an_event_test() ->
    Chain = three_entries(),
    {ok, First, _} = macula_audit:checkpoint(Chain, key(), ?INTERVAL),
    ?assertEqual(ok, macula_audit:received(First, First)).

a_checkpoint_that_does_not_extend_the_one_held_is_refused_test() ->
    Chain = three_entries(),
    Key = key(),
    {ok, First, Chain1} = macula_audit:checkpoint(Chain, Key, ?INTERVAL),
    %% A checkpoint with the right sequence number and the WRONG previous hash: a fork.
    {ok, Forked} = macula_audit_checkpoint:sign(#{v => 1, node => node_id(), cs => 2, n => 3,
                                                  head => macula_audit_checkpoint:head(First),
                                                  prev => binary:copy(<<16#AB>>, 48), i => ?INTERVAL}, Key),
    ?assertEqual({error, {checkpoint_not_extending, 2}}, macula_audit:received(First, Forked)),
    %% The honest next one, from the same chain, does extend it.
    ?assertEqual(ok, macula_audit:received(First, second_half(Chain1, Key))).

a_replayed_checkpoint_is_refused_test() ->
    Chain = three_entries(),
    Key = key(),
    {ok, First, Chain1} = macula_audit:checkpoint(Chain, Key, ?INTERVAL),
    {ok, Second, _} = macula_audit:checkpoint(Chain1, Key, ?INTERVAL),
    ?assertEqual({error, {checkpoint_replayed, 1}}, macula_audit:received(Second, First)),
    %% The same sequence number reaching a destination with a different hash is a fork too, and is refused as a replay.
    Other = element(2, macula_audit_checkpoint:sign(#{v => 1, node => node_id(), cs => 2, n => 9,
                                                      head => binary:copy(<<16#CD>>, 48),
                                                      prev => macula_audit_checkpoint:hash(First),
                                                      i => ?INTERVAL}, Key)),
    ?assertEqual({error, {checkpoint_replayed, 2}},
                 macula_audit:received(Second, Other)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

sink() -> sink(#{}).

sink(Opts) ->
    {ok, State} = macula_audit_memory_sink:new(Opts),
    {macula_audit_memory_sink, State}.

chain(Sink) ->
    {ok, Chain} = macula_audit:start(#{node => node_id(), sink => Sink}),
    Chain.

sink_state(Chain) ->
    %% The chain is opaque, so the sink state it threads is read out of the record: the test owns that sink and needs
    %% to ask it what the chain did to it.
    element(5, Chain).

three_entries() ->
    three_entries(chain(sink())).

three_entries(Chain) ->
    {ok, Chain1} = macula_audit:append(Chain, <<"handshake_refused">>,
                                       #{t => ?NOW, s => binary:copy(<<16#22>>, 32)}),
    {ok, Chain2} = macula_audit:append(Chain1, <<"store_refused">>,
                                       #{t => ?NOW + 1, r => <<"no space">>, d => binary:copy(<<16#33>>, 48)}),
    {ok, Chain3} = macula_audit:append(Chain2, <<"call_refused">>, #{t => ?NOW + 2, c => 5}),
    Chain3.

second_half(Chain, Key) ->
    {ok, Second, _} = macula_audit:checkpoint(Chain, Key, ?INTERVAL),
    Second.

entry_bytes(Seq, Prev) ->
    {ok, Entry} = macula_audit_entry:new(#{k => <<"store_refused">>, n => Seq, t => ?NOW, p => Prev}),
    macula_audit_entry:encode(Entry).

decode(Bytes) ->
    {ok, Entry} = macula_audit_entry:decode(Bytes),
    Entry.

flip(Bytes, At) ->
    <<Head:At/binary, Byte, Tail/binary>> = Bytes,
    <<Head/binary, (Byte bxor 1), Tail/binary>>.

node_id() ->
    binary:copy(<<16#11>>, 32).

key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.
