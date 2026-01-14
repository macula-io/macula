%%%-------------------------------------------------------------------
%%% @doc
%%% Content transfer module for Macula content-addressed storage.
%%%
%%% Implements the want/have/block exchange protocol for transferring
%%% content blocks between mesh peers. Handles request tracking,
%%% block verification, and message creation/parsing.
%%%
%%% == Transfer Flow ==
%%% 1. Requester sends WANT message with list of MCIDs
%%% 2. Provider responds with BLOCK messages for each available block
%%% 3. Requester verifies each block hash matches MCID
%%% 4. Requester stores verified blocks
%%%
%%% == Example Usage ==
%%% ```
%%% %% Request blocks from a provider
%%% {ok, RequestId} = macula_content_transfer:request_blocks(MCIDs, ProviderNode),
%%%
%%% %% Handle incoming want (as provider)
%%% {ok, Response} = macula_content_transfer:handle_want(WantMsg),
%%%
%%% %% Handle incoming block (as requester)
%%% ok = macula_content_transfer:handle_block(BlockMsg).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_transfer).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    %% Message creation
    generate_request_id/0,
    create_want_msg/2,
    create_have_msg/2,
    create_have_msg/3,
    create_block_msg/4,
    create_manifest_req_msg/2,
    create_manifest_res_msg/4,
    create_cancel_msg/2,
    %% Message parsing
    parse_want_msg/1,
    parse_have_msg/1,
    parse_block_msg/1,
    parse_manifest_req_msg/1,
    parse_manifest_res_msg/1,
    parse_cancel_msg/1,
    %% Request tracking
    request_blocks/2,
    pending_requests/0,
    complete_request/1,
    cancel_request/1,
    request_info/1,
    %% Message handling
    handle_want/1,
    handle_block/1,
    handle_manifest_req/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PRIORITY, 128).
-define(REQUEST_TIMEOUT_MS, 30000).

-record(state, {
    node_id :: binary(),
    requests :: ets:tid()  %% RequestId -> {MCIDs, TargetNode, CreatedAt, Status}
}).

-record(request, {
    mcids :: [binary()],
    target_node :: binary(),
    created_at :: integer(),
    status :: pending | complete | cancelled
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the content transfer server.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Generate a unique request ID.
-spec generate_request_id() -> binary().
generate_request_id() ->
    crypto:strong_rand_bytes(16).

%% @doc Create a WANT message.
%% MCIDs can be binary MCIDs or {MCID, Priority} tuples.
-spec create_want_msg([binary() | {binary(), 1..255}], binary()) -> map().
create_want_msg(MCIDs, FromNode) ->
    Wants = lists:map(fun
        ({MCID, Priority}) ->
            #{mcid => MCID, priority => Priority};
        (MCID) when is_binary(MCID) ->
            #{mcid => MCID, priority => ?DEFAULT_PRIORITY}
    end, MCIDs),
    #{
        request_id => generate_request_id(),
        wants => Wants,
        from_node => FromNode
    }.

%% @doc Create a HAVE message.
-spec create_have_msg([binary()], binary()) -> map().
create_have_msg(MCIDs, FromNode) ->
    #{
        haves => MCIDs,
        from_node => FromNode
    }.

%% @doc Create a HAVE message with options.
-spec create_have_msg([binary()], binary(), map()) -> map().
create_have_msg(MCIDs, FromNode, Opts) ->
    Base = #{
        haves => MCIDs,
        from_node => FromNode
    },
    case maps:get(manifest_mcid, Opts, undefined) of
        undefined -> Base;
        ManifestMCID -> Base#{manifest_mcid => ManifestMCID}
    end.

%% @doc Create a BLOCK message.
-spec create_block_msg(binary(), binary(), binary(), binary()) -> map().
create_block_msg(RequestId, MCID, Data, FromNode) ->
    #{
        request_id => RequestId,
        mcid => MCID,
        data => Data,
        from_node => FromNode
    }.

%% @doc Create a MANIFEST_REQ message.
-spec create_manifest_req_msg(binary(), binary()) -> map().
create_manifest_req_msg(MCID, FromNode) ->
    #{
        request_id => generate_request_id(),
        mcid => MCID,
        from_node => FromNode
    }.

%% @doc Create a MANIFEST_RES message.
-spec create_manifest_res_msg(binary(), binary(), binary(), binary()) -> map().
create_manifest_res_msg(RequestId, MCID, ManifestBin, FromNode) ->
    #{
        request_id => RequestId,
        mcid => MCID,
        manifest => ManifestBin,
        from_node => FromNode
    }.

%% @doc Create a CANCEL message.
-spec create_cancel_msg(binary(), binary()) -> map().
create_cancel_msg(RequestId, FromNode) ->
    #{
        request_id => RequestId,
        from_node => FromNode
    }.

%% @doc Parse a WANT message.
-spec parse_want_msg(map()) -> {ok, map()} | {error, invalid_msg}.
parse_want_msg(Msg) when is_map(Msg) ->
    case {maps:get(wants, Msg, undefined), maps:get(from_node, Msg, undefined)} of
        {undefined, _} -> {error, invalid_msg};
        {_, undefined} -> {error, invalid_msg};
        {Wants, FromNode} ->
            {ok, #{
                request_id => maps:get(request_id, Msg),
                wants => Wants,
                from_node => FromNode,
                max_blocks => maps:get(max_blocks, Msg, undefined)
            }}
    end;
parse_want_msg(_) ->
    {error, invalid_msg}.

%% @doc Parse a HAVE message.
-spec parse_have_msg(map()) -> {ok, map()} | {error, invalid_msg}.
parse_have_msg(Msg) when is_map(Msg) ->
    case {maps:get(haves, Msg, undefined), maps:get(from_node, Msg, undefined)} of
        {undefined, _} -> {error, invalid_msg};
        {_, undefined} -> {error, invalid_msg};
        {Haves, FromNode} ->
            {ok, #{
                haves => Haves,
                from_node => FromNode,
                manifest_mcid => maps:get(manifest_mcid, Msg, undefined)
            }}
    end;
parse_have_msg(_) ->
    {error, invalid_msg}.

%% @doc Parse a BLOCK message.
-spec parse_block_msg(map()) -> {ok, map()} | {error, invalid_msg}.
parse_block_msg(Msg) when is_map(Msg) ->
    case {maps:get(mcid, Msg, undefined), maps:get(data, Msg, undefined)} of
        {undefined, _} -> {error, invalid_msg};
        {_, undefined} -> {error, invalid_msg};
        {MCID, Data} ->
            {ok, #{
                request_id => maps:get(request_id, Msg),
                mcid => MCID,
                data => Data,
                from_node => maps:get(from_node, Msg, undefined)
            }}
    end;
parse_block_msg(_) ->
    {error, invalid_msg}.

%% @doc Parse a MANIFEST_REQ message.
-spec parse_manifest_req_msg(map()) -> {ok, map()} | {error, invalid_msg}.
parse_manifest_req_msg(Msg) when is_map(Msg) ->
    case maps:get(mcid, Msg, undefined) of
        undefined -> {error, invalid_msg};
        MCID ->
            {ok, #{
                request_id => maps:get(request_id, Msg),
                mcid => MCID,
                from_node => maps:get(from_node, Msg, undefined)
            }}
    end;
parse_manifest_req_msg(_) ->
    {error, invalid_msg}.

%% @doc Parse a MANIFEST_RES message.
-spec parse_manifest_res_msg(map()) -> {ok, map()} | {error, invalid_msg}.
parse_manifest_res_msg(Msg) when is_map(Msg) ->
    case {maps:get(mcid, Msg, undefined), maps:get(manifest, Msg, undefined)} of
        {undefined, _} -> {error, invalid_msg};
        {_, undefined} -> {error, invalid_msg};
        {MCID, Manifest} ->
            {ok, #{
                request_id => maps:get(request_id, Msg),
                mcid => MCID,
                manifest => Manifest,
                from_node => maps:get(from_node, Msg, undefined)
            }}
    end;
parse_manifest_res_msg(_) ->
    {error, invalid_msg}.

%% @doc Parse a CANCEL message.
-spec parse_cancel_msg(map()) -> {ok, map()} | {error, invalid_msg}.
parse_cancel_msg(Msg) when is_map(Msg) ->
    case maps:get(request_id, Msg, undefined) of
        undefined -> {error, invalid_msg};
        RequestId ->
            {ok, #{
                request_id => RequestId,
                from_node => maps:get(from_node, Msg, undefined)
            }}
    end;
parse_cancel_msg(_) ->
    {error, invalid_msg}.

%% @doc Request blocks from a target node.
%% Returns a request ID for tracking.
-spec request_blocks([binary()], binary()) -> {ok, binary()}.
request_blocks(MCIDs, TargetNode) ->
    gen_server:call(?SERVER, {request_blocks, MCIDs, TargetNode}).

%% @doc Get list of pending request IDs.
-spec pending_requests() -> [binary()].
pending_requests() ->
    gen_server:call(?SERVER, pending_requests).

%% @doc Mark a request as complete.
-spec complete_request(binary()) -> ok.
complete_request(RequestId) ->
    gen_server:call(?SERVER, {complete_request, RequestId}).

%% @doc Cancel a pending request.
-spec cancel_request(binary()) -> ok.
cancel_request(RequestId) ->
    gen_server:call(?SERVER, {cancel_request, RequestId}).

%% @doc Get info about a request.
-spec request_info(binary()) -> {ok, map()} | {error, not_found}.
request_info(RequestId) ->
    gen_server:call(?SERVER, {request_info, RequestId}).

%% @doc Handle an incoming WANT message.
%% Returns the block data if we have it.
-spec handle_want(map()) -> {ok, map()} | {error, not_found | term()}.
handle_want(Msg) ->
    gen_server:call(?SERVER, {handle_want, Msg}).

%% @doc Handle an incoming BLOCK message.
%% Verifies hash and stores the block.
-spec handle_block(map()) -> ok | {error, term()}.
handle_block(Msg) ->
    gen_server:call(?SERVER, {handle_block, Msg}).

%% @doc Handle an incoming MANIFEST_REQ message.
-spec handle_manifest_req(map()) -> {ok, map()} | {error, not_found | term()}.
handle_manifest_req(Msg) ->
    gen_server:call(?SERVER, {handle_manifest_req, Msg}).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init(Opts) ->
    NodeId = maps:get(node_id, Opts, generate_request_id()),
    Requests = ets:new(content_requests, [set, private]),
    {ok, #state{
        node_id = NodeId,
        requests = Requests
    }}.

handle_call({request_blocks, MCIDs, TargetNode}, _From, State) ->
    RequestId = generate_request_id(),
    Request = #request{
        mcids = MCIDs,
        target_node = TargetNode,
        created_at = erlang:system_time(millisecond),
        status = pending
    },
    ets:insert(State#state.requests, {RequestId, Request}),
    {reply, {ok, RequestId}, State};

handle_call(pending_requests, _From, State) ->
    Pending = ets:foldl(fun
        ({ReqId, #request{status = pending}}, Acc) -> [ReqId | Acc];
        (_, Acc) -> Acc
    end, [], State#state.requests),
    {reply, Pending, State};

handle_call({complete_request, RequestId}, _From, State) ->
    ets:delete(State#state.requests, RequestId),
    {reply, ok, State};

handle_call({cancel_request, RequestId}, _From, State) ->
    ets:delete(State#state.requests, RequestId),
    {reply, ok, State};

handle_call({request_info, RequestId}, _From, State) ->
    case ets:lookup(State#state.requests, RequestId) of
        [] ->
            {reply, {error, not_found}, State};
        [{RequestId, #request{} = Req}] ->
            Info = #{
                mcids => Req#request.mcids,
                target_node => Req#request.target_node,
                created_at => Req#request.created_at,
                status => Req#request.status
            },
            {reply, {ok, Info}, State}
    end;

handle_call({handle_want, Msg}, _From, State) ->
    case parse_want_msg(Msg) of
        {ok, Parsed} ->
            Result = process_want(Parsed, State),
            {reply, Result, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({handle_block, Msg}, _From, State) ->
    case parse_block_msg(Msg) of
        {ok, Parsed} ->
            Result = process_block(Parsed, State),
            {reply, Result, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({handle_manifest_req, Msg}, _From, State) ->
    case parse_manifest_req_msg(Msg) of
        {ok, Parsed} ->
            Result = process_manifest_req(Parsed, State),
            {reply, Result, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
process_want(#{wants := Wants} = _Parsed, State) ->
    %% For now, just handle the first want
    case Wants of
        [#{mcid := MCID} | _] ->
            case macula_content_store:get_block(MCID) of
                {ok, Data} ->
                    RequestId = generate_request_id(),
                    {ok, #{
                        type => content_block,
                        request_id => RequestId,
                        mcid => MCID,
                        data => Data,
                        from_node => State#state.node_id
                    }};
                {error, Reason} ->
                    {error, Reason}
            end;
        [] ->
            {error, empty_wants}
    end.

%% @private
process_block(#{mcid := MCID, data := Data} = _Parsed, _State) ->
    %% Verify the block hash matches the MCID
    <<_Version:8, _Codec:8, ExpectedHash:32/binary>> = MCID,
    Blake3Hash = macula_content_hasher:hash(blake3, Data),
    Sha256Hash = macula_content_hasher:hash(sha256, Data),
    case Blake3Hash =:= ExpectedHash orelse Sha256Hash =:= ExpectedHash of
        true ->
            %% Store the verified block
            macula_content_store:put_block(MCID, Data);
        false ->
            {error, hash_mismatch}
    end.

%% @private
process_manifest_req(#{mcid := MCID, request_id := RequestId} = _Parsed, State) ->
    case macula_content_store:get_manifest(MCID) of
        {ok, Manifest} ->
            {ok, ManifestBin} = macula_content_manifest:encode(Manifest),
            {ok, #{
                type => content_manifest_res,
                request_id => RequestId,
                mcid => MCID,
                manifest => ManifestBin,
                from_node => State#state.node_id
            }};
        {error, Reason} ->
            {error, Reason}
    end.
