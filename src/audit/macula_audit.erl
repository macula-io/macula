%% @doc The audit chain, one entry point (macula#175 WP1).
%%
%% A station, a provider or the guard records what it refused, served or reconfigured on a hash chain, and a destination
%% holds signed checkpoints of that chain. This module is the whole public surface: start a chain over a sink, append an
%% event, take a checkpoint, export it as JSON lines, and verify an export against what a destination holds.
%%
%%   {ok, Chain0} = macula_audit:start(#{node => NodeId, sink => {macula_audit_file_sink, Sink}}),
%%   {ok, Chain1} = macula_audit:append(Chain0, <<"handshake_refused">>, #{t => Now, s => PeerId}),
%%   {ok, Checkpoint, Chain2} = macula_audit:checkpoint(Chain1, IdentityKey, 3600),
%%   {ok, Lines} = macula_audit:export(Chain2, false),
%%   {ok, _Report} = macula_audit:verify(iolist_to_binary(Lines), Anchors, #{}).
%%
%% The format, what a checkpoint signs and what the verifier refuses are docs/design/DESIGN_AUDIT_CHAIN.md. The verifier
%% is the only module here that trusts nothing: it reads bytes it was given, hashes them as received, and reports the
%% first bad place.
-module(macula_audit).

-export([start/1, append/3, checkpoint/3, export/2, verify/3, received/2, kinds/0, entry_hash/1]).

-export_type([chain/0, checkpoint/0, finding/0, report/0]).

-type chain() :: macula_audit_chain:chain().
-type checkpoint() :: macula_audit_checkpoint:checkpoint().
-type finding() :: macula_audit_verifier:finding().
-type report() :: macula_audit_verifier:report().

%% @doc Start a chain over a sink. See `macula_audit_chain:start/1'.
-spec start(map()) -> {ok, chain()} | {error, term()}.
start(Opts) ->
    macula_audit_chain:start(Opts).

%% @doc Append one security event to the chain.
-spec append(chain(), binary(), map()) -> {ok, chain()} | {error, term()}.
append(Chain, Kind, Fields) ->
    macula_audit_chain:append(Chain, Kind, Fields).

%% @doc Take a checkpoint, after the sink has synced everything the chain holds.
-spec checkpoint(chain(), macula_node_keys:node_key(), pos_integer()) ->
        {ok, checkpoint(), chain()} | {error, term()}.
checkpoint(Chain, Key, Interval) ->
    macula_audit_chain:checkpoint(Chain, Key, Interval).

%% @doc The chain as JSON lines. `Sealed' says the chain ended because the node's identity key rotated and its last
%% checkpoint is anchored; `false' says it was still being written when the export was taken.
-spec export(chain(), boolean()) -> {ok, iodata()} | {error, term()}.
export(Chain, Sealed) ->
    case macula_audit_chain:entries(Chain) of
        {ok, Entries} ->
            Header = #{node => macula_audit_chain:node(Chain),
                       segment => macula_audit_chain:segment(Chain),
                       sealed => Sealed},
            macula_audit_export:encode(Header, Entries, macula_audit_chain:checkpoints(Chain));
        {error, _} = Error ->
            Error
    end.

%% @doc Verify an export against the anchors a destination holds for the node.
-spec verify(binary() | map(), [map()], map()) ->
        {ok, report()} | {overdue, report()} | {error, [finding()]}.
verify(Export, Anchors, Opts) ->
    macula_audit_verifier:verify(Export, Anchors, Opts).

%% @doc The fork and replay check a destination runs the moment a checkpoint arrives.
-spec received(checkpoint() | undefined, checkpoint()) -> ok | {error, term()}.
received(Last, New) ->
    macula_audit_checkpoint:received(Last, New).

%% @doc The event kinds a chain may record.
-spec kinds() -> [binary()].
kinds() ->
    macula_audit_entry:kinds().

%% @doc The hash of an entry's bytes, which is what a checkpoint's head and the next entry's link name.
-spec entry_hash(binary()) -> binary().
entry_hash(Bytes) ->
    macula_audit_entry:hash(Bytes).
