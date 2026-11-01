%% The three probes Terra wrote against the first cut of macula#175 WP1, kept as regression tests. Each one is a chain
%% the verifier used to call clean when it was not, so each one is a hole that is now closed in code and can only stay
%% closed if it is exercised:
%%
%%   P1  a checkpoint re-signed by a stranger's key, claiming the node's chain, with no anchor to catch it.
%%   P2  an old anchor and an unanchored later checkpoint promising a huge interval, so an abandoned chain reads
%%       watched.
%%   P3  a chain exported as sealed whose last checkpoint does not cover its tail, so the tail hides behind the seal.
-module(macula_audit_probe_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NOW, 1761000000).
-define(I, 3600).

key() -> {ok, Key} = macula_node_keys:generate(identity, pq_pure), Key.
nid(Key) -> {ok, Node} = macula_node_keys:node_id(Key), Node.

chain(Key, Count) ->
    Node = nid(Key),
    {ok, Sink} = macula_audit_memory_sink:new(#{}),
    {ok, C0} = macula_audit:start(#{node => Node, sink => {macula_audit_memory_sink, Sink}}),
    lists:foldl(fun(Time, C) -> {ok, C1} = macula_audit:append(C, <<"call_refused">>, #{t => Time}), C1 end,
                C0, lists:seq(1, Count)).

anchor(Checkpoint, At) ->
    #{cs => macula_audit_checkpoint:cs(Checkpoint), n => macula_audit_checkpoint:count(Checkpoint),
      head => macula_audit_checkpoint:head(Checkpoint), prev => maps:get(prev, Checkpoint),
      hash => macula_audit_checkpoint:hash(Checkpoint), received_at => At}.

parsed(Chain, Sealed) ->
    {ok, Export} = macula_audit:export(Chain, Sealed),
    {ok, Parsed} = macula_audit_export:parse(iolist_to_binary(Export)),
    Parsed.

%% P1: entries rewritten consistently, checkpoint re-signed by a stranger's key, and no anchor to catch it. The
%% signature verifies — the key travels with it — so the only thing that refuses this is holding the key to the node
%% the checkpoint names.
p1_a_strangers_key_cannot_sign_this_nodes_checkpoint_test() ->
    Node = nid(key()),
    Stranger = key(),
    {ok, Sink} = macula_audit_memory_sink:new(#{}),
    {ok, C0} = macula_audit:start(#{node => Node, sink => {macula_audit_memory_sink, Sink}}),
    {ok, C1} = macula_audit:append(C0, <<"call_refused">>, #{t => 1}),
    {ok, _Checkpoint, C2} = macula_audit:checkpoint(C1, Stranger, ?I),
    ?assertMatch({error, _}, macula_audit:verify(parsed(C2, false), [], #{now => ?NOW})).

%% P2: the anchor is thirty days old and promises an interval of an hour; the node's key then adds a second,
%% unanchored checkpoint promising a billion seconds. The interval that counts is the anchored one's.
p2_an_unanchored_checkpoint_cannot_stretch_the_interval_test() ->
    Key = key(),
    Chain = chain(Key, 2),
    {ok, First, Chain1} = macula_audit:checkpoint(Chain, Key, ?I),
    {ok, Chain2} = macula_audit:append(Chain1, <<"call_refused">>, #{t => 9}),
    {ok, _Lax, Chain3} = macula_audit:checkpoint(Chain2, Key, 1000000000),
    Stale = anchor(First, ?NOW - 30 * 86400),
    ?assertMatch({overdue, _}, macula_audit:verify(parsed(Chain3, false), [Stale], #{now => ?NOW})).

%% P3: sealed, and an entry written after the last checkpoint. A seal that does not cover the tail seals nothing.
%%
%% The assertion is on the export that CARRIES the hole: three entries, a last checkpoint at two, and the seal. The
%% second assertion records the other side, so nobody reads the first as more than it is: a verifier handed the same
%% chain with the tail CUT is handed a chain that claims to have ended at two entries, sealed and anchored, which no
%% verifier can tell from one that really did. A removed tail is caught by an anchor of a later checkpoint, which a
%% sealed chain does not have; what this rule catches is the tail that was never sealed at all.
p3_a_sealed_chain_whose_tail_is_not_checkpointed_is_refused_test() ->
    Key = key(),
    Chain = chain(Key, 2),
    {ok, Checkpoint, Chain1} = macula_audit:checkpoint(Chain, Key, ?I),
    {ok, Chain2} = macula_audit:append(Chain1, <<"key_rotated">>, #{t => 9}),
    Full = parsed(Chain2, true),
    Anchor = [anchor(Checkpoint, ?NOW - 30 * 86400)],
    ?assertMatch({error, _}, macula_audit:verify(Full, Anchor, #{now => ?NOW})),
    [First, Second, _Tail] = maps:get(entries, Full),
    ?assertMatch({ok, _}, macula_audit:verify(Full#{entries => [First, Second]}, Anchor, #{now => ?NOW})).
