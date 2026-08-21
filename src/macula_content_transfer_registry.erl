%%%-------------------------------------------------------------------
%%% @doc Correlation-id registry for `macula_content_transfer' handles.
%%%
%%% Each transfer mints a `share_id' (already published in
%%% `sharing.put_started_v1' / `sharing.get_started_v1' etc. by
%%% `macula_feeder' / `macula_download'). A caller that only saw the
%%% id in a published mesh fact — not the pid — needs a way to reach
%%% `cancel/1,3'; this table is that lookup. Monitor-based cleanup:
%%% an entry disappears the instant its owning process exits, no
%%% caller needs to unregister explicitly.
%%%
%%% Table is `protected' — any process can read it directly via
%%% `ets:lookup/2' (see `whereis_share/1'), only this gen_server ever
%%% writes to it.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_transfer_registry).

-behaviour(gen_server).

-export([start_link/0]).
-export([register_share/2, whereis_share/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).
-define(TABLE, macula_content_transfer_shares).

-record(state, {monitors = #{} :: #{reference() => binary()}}).

%% @doc Start the registry. One per node, under `macula_root'.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register `Pid' under `ShareId'. Overwrites any prior owner for
%% the same id (last writer wins — ids are `crypto:strong_rand_bytes/1'
%% output, a collision here means a caller deliberately reused one).
-spec register_share(binary(), pid()) -> ok.
register_share(ShareId, Pid) when is_binary(ShareId), is_pid(Pid) ->
    gen_server:call(?SERVER, {register, ShareId, Pid}).

%% @doc Look up the pid owning `ShareId', if it's still alive.
-spec whereis_share(binary()) -> {ok, pid()} | {error, not_found}.
whereis_share(ShareId) when is_binary(ShareId) ->
    case ets:lookup(?TABLE, ShareId) of
        [{ShareId, Pid}] -> {ok, Pid};
        []                -> {error, not_found}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    ets:new(?TABLE, [set, protected, named_table, {read_concurrency, true}]),
    {ok, #state{}}.

%% @private
handle_call({register, ShareId, Pid}, _From, #state{monitors = Mons} = State) ->
    true = ets:insert(?TABLE, {ShareId, Pid}),
    Ref = erlang:monitor(process, Pid),
    {reply, ok, State#state{monitors = Mons#{Ref => ShareId}}};
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info({'DOWN', Ref, process, _Pid, _Reason}, #state{monitors = Mons} = State) ->
    NewMons = on_owner_down(maps:take(Ref, Mons), Mons),
    {noreply, State#state{monitors = NewMons}};
handle_info(_Msg, State) ->
    {noreply, State}.

on_owner_down(error, Mons) ->
    Mons;
on_owner_down({ShareId, NewMons}, _Mons) ->
    ets:delete(?TABLE, ShareId),
    NewMons.
