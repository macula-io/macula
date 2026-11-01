%% @doc A running audit chain: appending business events and taking checkpoints (macula#175 WP1), as
%% docs/design/DESIGN_AUDIT_CHAIN.md lays it out.
%%
%% The chain is a value, not a process: it holds the node id, the segment, the sink it writes through, the last sequence
%% number and the hash the next entry links to. A caller may keep it in a gen_server or in a process dictionary of its
%% own, and the format does not care which.
%%
%% A checkpoint asks the sink to sync first and refuses to sign if that fails, so a checkpoint only ever covers entries
%% that are already durable. `checkpoint/3' takes the interval the node promises for the next one, and the sequence
%% number continues across a restart because the last checkpoint's sequence number is handed back in at `start/1'.
-module(macula_audit_chain).

-export([start/1, append/3, checkpoint/3, entries/1, checkpoints/1, node/1, segment/1, head/1, count/1]).

-export_type([chain/0]).

-define(NODE_ID_BYTES, 32).

-record(chain, {node :: binary(),
                segment :: pos_integer(),
                sink :: module(),
                sink_state :: term(),
                n = 0 :: non_neg_integer(),
                head = <<>> :: binary(),
                last = undefined :: undefined | #{cs := pos_integer(), hash := binary()},
                checkpoints = [] :: [macula_audit_checkpoint:checkpoint()]}).

-opaque chain() :: #chain{}.

%% @doc A chain over a sink. `node' is the node id the chain belongs to, `sink' is `{Module, State}', `segment'
%% defaults to 1, and `checkpoint' is the last checkpoint the caller holds, `#{cs := C, hash := H}', so a restarted
%% node continues the sequence instead of replaying it.
%%
%% Whatever the sink already holds is adopted: the chain resumes at the next sequence number and the last hash, and
%% refuses to start at all if what the sink holds is not a consistent chain.
-spec start(map()) -> {ok, chain()} | {error, term()}.
start(#{node := Node, sink := {Module, State}} = Opts)
  when is_binary(Node), byte_size(Node) =:= ?NODE_ID_BYTES, is_atom(Module) ->
    Chain = #chain{node = Node,
                   segment = maps:get(segment, Opts, 1),
                   sink = Module,
                   sink_state = State,
                   last = maps:get(checkpoint, Opts, undefined)},
    case Module:entries(State) of
        {ok, Stored} -> resumed(Chain, Stored);
        {error, Reason} -> {error, {sink_unreadable, Reason}}
    end;
start(_Opts) ->
    {error, {malformed, start_opts}}.

%% @doc Append one event. The kind is a business verb from `macula_audit_entry:kinds/0'; the fields carry the time, the
%% verified node id, the reason, the request digest and the coalesced count where the kind has them.
-spec append(chain(), binary(), map()) -> {ok, chain()} | {error, term()}.
append(#chain{n = Count, head = Head} = Chain, Kind, Fields)
  when is_binary(Kind), is_map(Fields) ->
    case macula_audit_entry:new(Fields#{k => Kind, n => Count + 1, p => Head}) of
        {ok, Entry}      -> written(Chain, macula_audit_entry:encode(Entry));
        {error, _} = Error -> Error
    end.

written(#chain{n = Count, sink = Module, sink_state = State} = Chain, Bytes) ->
    case Module:append(Bytes, State) of
        {ok, State1} ->
            {ok, Chain#chain{n = Count + 1, head = macula_audit_entry:hash(Bytes), sink_state = State1}};
        {error, Reason} ->
            {error, {sink_refused, Reason}}
    end.

%% @doc Sign a checkpoint over everything the chain holds, after the sink has synced it. A sink that cannot sync
%% refuses the checkpoint rather than signing a head a crash could lose.
-spec checkpoint(chain(), macula_node_keys:node_key(), pos_integer()) ->
        {ok, macula_audit_checkpoint:checkpoint(), chain()} | {error, term()}.
checkpoint(#chain{sink = Module, sink_state = State} = Chain, Key, Interval)
  when is_integer(Interval), Interval >= 1 ->
    case Module:sync(State) of
        ok               -> signed(Chain, Key, Interval);
        {error, Reason}  -> {error, {sink_unsynced, Reason}}
    end.

signed(#chain{n = Count, head = Head, node = Node, last = Last} = Chain, Key, Interval) ->
    {Cs, Prev} = next(Last),
    Fields = #{v => 1, node => Node, cs => Cs, n => Count, head => Head, prev => Prev, i => Interval},
    case macula_audit_checkpoint:sign(Fields, Key) of
        {ok, Checkpoint} ->
            {ok, Checkpoint,
             Chain#chain{last = #{cs => Cs, hash => macula_audit_checkpoint:hash(Checkpoint)},
                         checkpoints = Chain#chain.checkpoints ++ [Checkpoint]}};
        {error, _} = Error ->
            Error
    end.

%% @doc The entries the sink holds, in chain order, as the bytes they were written and hashed as.
-spec entries(chain()) -> {ok, [binary()]} | {error, term()}.
entries(#chain{sink = Module, sink_state = State}) ->
    Module:entries(State).

%% @doc The checkpoints taken through this chain, oldest first.
-spec checkpoints(chain()) -> [macula_audit_checkpoint:checkpoint()].
checkpoints(#chain{checkpoints = Checkpoints}) ->
    Checkpoints.

%% @doc The node id this chain belongs to.
-spec node(chain()) -> binary().
node(#chain{node = Node}) -> Node.

%% @doc The segment number this chain writes.
-spec segment(chain()) -> pos_integer().
segment(#chain{segment = Segment}) -> Segment.

%% @doc The hash the next entry will link to.
-spec head(chain()) -> binary().
head(#chain{head = Head}) -> Head.

%% @doc How many entries this chain holds.
-spec count(chain()) -> non_neg_integer().
count(#chain{n = Count}) -> Count.

%%------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------

next(undefined) -> {1, <<>>};
next(#{cs := Cs, hash := Hash}) -> {Cs + 1, Hash}.

resumed(Chain, Stored) ->
    case replay(Stored, 0, <<>>) of
        {ok, Count, Head} -> {ok, Chain#chain{n = Count, head = Head}};
        {error, _} = Error -> Error
    end.

replay([], Count, Head) ->
    {ok, Count, Head};
replay([Bytes | Rest], Count, Head) ->
    case macula_audit_entry:decode(Bytes) of
        {ok, #{n := Seq, p := Prev}} when Seq =:= Count + 1, Prev =:= Head ->
            replay(Rest, Seq, macula_audit_entry:hash(Bytes));
        {ok, #{n := Seq, p := Prev}} ->
            {error, {sink_chain_broken, #{expected_seq => Count + 1, expected_prev => Head,
                                          found_seq => Seq, found_prev => Prev}}};
        {error, Why} ->
            {error, {sink_chain_broken, Why}}
    end.
