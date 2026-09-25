%% @doc Fetches content from the node that shares it (D27): content is available while its sharer is online, and
%% stations only relay.
%%
%% == The fetch ==
%%
%% <ol>
%%   <li>Find the content id's announcements in the DHT (`macula_record:content_key/1'). Keep those that name the
%%       content id, a realm, a serving station and a procedure bound to their announcer: `~<hex>/content_v1' or
%%       `<org>/content_v1_<hex>', where `<hex>' is the announcer's node id. An announcement cannot point a fetch at
%%       another node's procedure.</li>
%%   <li>Try the sharers one at a time, in a random order.</li>
%%   <li>Resolve the serving station's own endpoint record and open a `server_stream' through it, pinned to the
%%       station and targeted at the sharer, in the announced realm. No advertisement or realm-key check applies:
%%       content verifies itself by its content id, so a fetcher that trusts no realm can still fetch.</li>
%%   <li>Ask for the root. A block must hash to the content id; a manifest must match it
%%       (`macula_manifest:verify_mcid/2') before any of its sizes is read, and then fit the caller's bounds before a
%%       chunk is asked for.</li>
%%   <li>Ask for each chunk on its own stream, `parallel' at a time, each verified against its own content id as it
%%       arrives; then assemble in order and verify the whole against the manifest.</li>
%% </ol>
%%
%% A sharer that fails in any step moves the fetch to the next; when every one has failed the answer is
%% `{error, {unavailable, [{Sharer, Reason}]}}', naming each. Nothing of a failed attempt is kept, and nothing is
%% resumed. No announcement at all is `{error, not_shared}'.
%%
%% == Bounds ==
%%
%% `max_bytes' (256 MiB by default) bounds the content, `max_chunks' (16,384, 4 GiB of 256 KiB chunks) a manifest,
%% `chunk_timeout_ms' (15 s) each stream, `parallel' (4) the chunk streams open at once. A DATA body is at most one
%% frame, which the stream layer bounds; a block larger than a chunk is refused.
-module(macula_content_fetch).

-export([get/3, io/0]).

-define(MAX_BYTES, 268435456).
-define(MAX_CHUNKS, 16384).
-define(CHUNK_TIMEOUT_MS, 15_000).
-define(RESOLVE_TIMEOUT_MS, 5_000).
-define(PARALLEL, 4).
-define(CODEC_RAW, 16#55).
-define(CODEC_MANIFEST, 16#56).

%% @doc The facade's own functions, the io a fetch uses when not given another.
-spec io() -> map().
io() ->
    #{find_records => fun macula:find_records/3,
      resolve_station_endpoint => fun macula_direct_dial:resolve_station_endpoint/3,
      call_stream_station => fun macula:call_stream_station/7}.

%% @doc Fetch `MCID' from a node that shares it. `Opts': `max_bytes', `max_chunks', `chunk_timeout_ms', `parallel',
%% and `io' (see `io/0').
-spec get(pid(), macula:mcid(), map()) -> {ok, binary()} | {error, term()}.
get(Pool, <<2, Codec, _:48/binary>> = MCID, Opts)
  when is_pid(Pool), (Codec =:= ?CODEC_RAW orelse Codec =:= ?CODEC_MANIFEST), is_map(Opts) ->
    Ctx = context(Pool, MCID, Opts),
    from_sharers(sharers(found(Ctx), MCID, Opts), Ctx, []);
get(_Pool, _MCID, _Opts) ->
    {error, invalid_mcid}.

context(Pool, MCID, Opts) ->
    #{pool => Pool, mcid => MCID,
      io => maps:merge(io(), maps:get(io, Opts, #{})),
      max_bytes => maps:get(max_bytes, Opts, ?MAX_BYTES),
      max_chunks => maps:get(max_chunks, Opts, ?MAX_CHUNKS),
      chunk_timeout_ms => maps:get(chunk_timeout_ms, Opts, ?CHUNK_TIMEOUT_MS),
      parallel => maps:get(parallel, Opts, ?PARALLEL)}.

found(#{pool := Pool, mcid := MCID, io := #{find_records := Find}}) ->
    Find(Pool, macula_record:content_key(MCID), ?RESOLVE_TIMEOUT_MS).

%%====================================================================
%% The sharers
%%====================================================================

%% The announcements that name `MCID' and a procedure bound to their announcer, in the order they are tried:
%% random, unless a test asks for `order => as_given'.
sharers({ok, Records}, MCID, Opts) ->
    ordered(maps:get(order, Opts, random),
            [Sharer || R <- Records, {true, Sharer} <- [sharer(R, MCID)]]);
sharers({error, _Reason}, _MCID, _Opts) ->
    [].

sharer(#{type := _} = Record, MCID) ->
    try macula_record:read_content_announcement(Record) of
        #{announcer_node := Node, mcid := MCID, realm_id := <<_:256>> = Realm,
          serving_station := <<_:256>> = Station, procedure := Procedure} when is_binary(Procedure) ->
            bound(bound_to(Procedure, Node), #{node => Node, realm => Realm, station => Station,
                                               procedure => Procedure});
        _Other -> false
    catch _:_ -> false
    end;
sharer(_NotARecord, _MCID) ->
    false.

bound(true, Sharer) -> {true, Sharer};
bound(false, _Sharer) -> false.

%% A content procedure names its announcer: `~<hex>/content_v1', or `<org>/content_v1_<hex>' for an org without "/".
bound_to(Procedure, Node) ->
    Hex = binary:encode_hex(Node, lowercase),
    Procedure =:= <<"~", Hex/binary, "/content_v1">>
        orelse org_bound(binary:split(Procedure, <<"/">>), <<"content_v1_", Hex/binary>>).

org_bound([Org, Name], Name) -> Org =/= <<>> andalso binary:first(Org) =/= $~ andalso Org =/= <<"_">>;
org_bound(_Split, _Name) -> false.

ordered(as_given, Sharers) -> Sharers;
ordered(random, Sharers) -> [S || {_, S} <- lists:sort([{rand:uniform(), S} || S <- Sharers])].

%% Try each sharer in turn; the first to deliver verified content ends the fetch.
from_sharers([], _Ctx, []) ->
    {error, not_shared};
from_sharers([], _Ctx, Failures) ->
    {error, {unavailable, lists:reverse(Failures)}};
from_sharers([#{node := Node} = Sharer | Rest], Ctx, Failures) ->
    tried(from_sharer(Sharer, Ctx), Rest, Ctx, [Node | Failures], Node).

tried({ok, Bytes}, _Rest, _Ctx, _Failures, _Node) -> {ok, Bytes};
tried({error, Reason}, Rest, Ctx, [Node | Failures], Node) -> from_sharers(Rest, Ctx, [{Node, Reason} | Failures]).

from_sharer(#{station := Station} = Sharer, #{pool := Pool, io := #{resolve_station_endpoint := Resolve}} = Ctx) ->
    through(Resolve(Pool, Station, ?RESOLVE_TIMEOUT_MS), Sharer, Ctx).

through({ok, Url}, Sharer, #{mcid := MCID} = Ctx) ->
    Dial = Sharer#{url => Url},
    root(asked(Dial, MCID, root, Ctx), Dial, Ctx);
through({error, Reason}, _Sharer, _Ctx) ->
    {error, {station_unresolved, Reason}}.

%%====================================================================
%% The root and the chunks
%%====================================================================

root({ok, #{kind := block, bytes := Bytes}}, _Dial, #{mcid := MCID, max_bytes := Max}) ->
    within_bytes(byte_size(Bytes), Max, fun() -> block_verified(MCID, Bytes) end);
root({ok, #{kind := manifest, manifest := Wire}}, Dial, #{mcid := MCID} = Ctx) ->
    manifest(macula_manifest:from_wire(Wire), MCID, Dial, Ctx);
root({ok, _Other}, _Dial, _Ctx) ->
    {error, unexpected_body};
root({error, _} = Error, _Dial, _Ctx) ->
    Error.

manifest({ok, Manifest}, MCID, Dial, Ctx) ->
    matched(macula_manifest:verify_mcid(Manifest, MCID), Manifest, Dial, Ctx);
manifest({error, _} = Error, _MCID, _Dial, _Ctx) ->
    Error.

%% Only a manifest that is the one asked for has its sizes read.
matched(ok, #{size := Size, chunk_count := Count} = Manifest, Dial, #{max_bytes := MaxBytes, max_chunks := MaxChunks} = Ctx) ->
    within_bytes(Size, MaxBytes,
                 fun() -> within_chunks(Count, MaxChunks, fun() -> chunks(Manifest, Dial, Ctx) end) end);
matched({error, _} = Error, _Manifest, _Dial, _Ctx) ->
    Error.

within_bytes(Size, Max, _Next) when Size > Max -> {error, {too_large, Size}};
within_bytes(_Size, _Max, Next) -> Next().

within_chunks(Count, Max, _Next) when Count > Max -> {error, {too_many_chunks, Count}};
within_chunks(_Count, _Max, Next) -> Next().

chunks(#{chunk_count := Count} = Manifest, Dial, #{parallel := Parallel} = Ctx) ->
    ChunkIds = [begin {ok, C} = macula_manifest:chunk_mcid(Manifest, I), C end || I <- lists:seq(0, Count - 1)],
    assembled(in_batches(ChunkIds, Parallel, Dial, Ctx, []), Manifest).

in_batches([], _Parallel, _Dial, _Ctx, Acc) ->
    {ok, lists:reverse(Acc)};
in_batches(ChunkIds, Parallel, Dial, Ctx, Acc) ->
    {Batch, Rest} = lists:split(min(Parallel, length(ChunkIds)), ChunkIds),
    batch_done(batch(Batch, Dial, Ctx), Rest, Parallel, Dial, Ctx, Acc).

batch_done({ok, Blocks}, Rest, Parallel, Dial, Ctx, Acc) ->
    in_batches(Rest, Parallel, Dial, Ctx, lists:reverse(Blocks, Acc));
batch_done({error, _} = Error, _Rest, _Parallel, _Dial, _Ctx, _Acc) ->
    Error.

%% One stream per chunk, the batch at once; every block verified against its own content id.
batch(ChunkIds, Dial, Ctx) ->
    Self = self(),
    Workers = [spawn_monitor(fun() -> Self ! {chunk, self(), chunk(C, Dial, Ctx)} end) || C <- ChunkIds],
    collected([Pid || {Pid, _Mon} <- Workers], maps:get(chunk_timeout_ms, Ctx) + 1_000, []).

collected([], _Timeout, Acc) ->
    {ok, lists:reverse(Acc)};
collected([Pid | Rest], Timeout, Acc) ->
    receive
        {chunk, Pid, {ok, Bytes}} -> collected(Rest, Timeout, [Bytes | Acc]);
        {chunk, Pid, {error, _} = Error} -> stopped(Rest), Error;
        {'DOWN', _Mon, process, Pid, Reason} when Reason =/= normal -> stopped(Rest), {error, {chunk_worker, Reason}}
    after Timeout ->
        stopped([Pid | Rest]), {error, chunk_timeout}
    end.

stopped(Pids) -> [exit(P, kill) || P <- Pids], ok.

chunk(ChunkId, Dial, #{max_bytes := _} = Ctx) ->
    block_of(asked(Dial, ChunkId, block, Ctx), ChunkId).

block_of({ok, #{kind := block, bytes := Bytes}}, ChunkId) when byte_size(Bytes) =< 262144 ->
    block_verified(ChunkId, Bytes);
block_of({ok, #{kind := block}}, _ChunkId) ->
    {error, block_too_large};
block_of({ok, _Other}, _ChunkId) ->
    {error, unexpected_body};
block_of({error, _} = Error, _ChunkId) ->
    Error.

block_verified(<<2, ?CODEC_RAW, Hash:48/binary>>, Bytes) ->
    hash_matched(crypto:hash(sha384, Bytes) =:= Hash, Bytes);
block_verified(_NotRaw, _Bytes) ->
    {error, block_mcid_mismatch}.

hash_matched(true, Bytes) -> {ok, Bytes};
hash_matched(false, _Bytes) -> {error, block_mcid_mismatch}.

assembled({ok, Blocks}, Manifest) ->
    Bytes = iolist_to_binary(Blocks),
    whole(macula_manifest:verify(Manifest, Bytes), Bytes);
assembled({error, _} = Error, _Manifest) ->
    Error.

whole(ok, Bytes) -> {ok, Bytes};
whole({error, _} = Error, _Bytes) -> Error.

%%====================================================================
%% One stream
%%====================================================================

%% Open a stream to the sharer for one content id and read the one DATA body it answers with.
asked(#{url := Url, node := Node, realm := Realm, procedure := Procedure, station := Station}, MCID, Want,
      #{pool := Pool, io := #{call_stream_station := Open}, chunk_timeout_ms := Timeout}) ->
    opened(Open(Pool, Url, Node, Realm, Procedure, #{mcid => MCID, want => Want},
                #{expected_node_id => Station, timeout_ms => Timeout}), Timeout).

opened({ok, Stream}, Timeout) ->
    Answer = body(macula:recv(Stream, Timeout)),
    _ = catch macula:close_stream(Stream),
    Answer;
opened({error, _} = Error, _Timeout) ->
    Error.

body({data, Body}) when is_map(Body) -> {ok, read_body(Body)};
body({data, _NotAMap}) -> {error, unexpected_body};
body({chunk, _Raw}) -> {error, unexpected_body};
body(eof) -> {error, no_answer};
body({error, {Code, _Message}}) when is_binary(Code) -> {error, {sharer_refused, Code}};
body({error, _} = Error) -> Error.

%% A DATA body as the stream delivers it, keys and text values tagged or not.
read_body(Body) ->
    #{kind => kind(macula_record:payload_field(Body, <<"kind">>)),
      mcid => macula_record:payload_field(Body, <<"mcid">>),
      bytes => macula_record:payload_field(Body, <<"bytes">>),
      manifest => macula_record:payload_field(Body, <<"manifest">>)}.

kind(block) -> block;
kind(manifest) -> manifest;
kind(<<"block">>) -> block;
kind(<<"manifest">>) -> manifest;
kind(_Other) -> undefined.
