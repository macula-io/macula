%% @doc A node's KEM keys (E2E design, Amendment A1), one current key per
%% node identity, shared by every pool of that identity in the VM.
%%
%% A provider's advertisement carries its current key, which a caller seals
%% requests to. The keys live only in memory: none is ever written to disk,
%% and a restart makes new ones. Every `rotate_after_ms' (24 hours) a node
%% gets a new current key. The key it replaces still opens calls for
%% `retain_ms' (30 minutes), which covers the last advertisement that named
%% it (5 minutes), the clock tolerance (5), the longest deadline (10) and
%% admission's tolerance past a deadline (5), and is then deleted. A stolen
%% key therefore opens at most about 24.5 hours of calls.
%%
%% This process owns the key table and runs under `macula_root'. Callers read
%% the table directly, so opening a sealed request never waits on this
%% process; only making, rotating and deleting keys go through it. The table
%% is protected, and no private key is ever in this process's state, so no
%% crash report or state dump can print one.
%%
%% Precondition: one node identity runs in one VM. Two VMs of one identity
%% would each advertise their own key (see the design's Amendment A1).
-module(macula_kem_keyring).
-behaviour(gen_server).

-export([start_link/0, start_link/1,
         ensure/2, ensure/3, current/1, current/2, holder/1, holder/2, rotate/1, rotate/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([options/0, current/0]).

-define(ROTATE_AFTER_MS, 24 * 60 * 60 * 1000).
-define(RETAIN_MS, 30 * 60 * 1000).

%% `table' names the key table; `rotate_after_ms' and `retain_ms' exist for
%% tests.
-type options() :: #{table => atom(), rotate_after_ms => pos_integer(), retain_ms => pos_integer()}.
%% A node's current key, as its advertisement carries it, and that key's id.
-type current() :: #{key_id := <<_:64>>, key := binary()}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link(Options) when is_map(Options) ->
    gen_server:start_link(?MODULE, Options, []).

%% @doc Make sure the node `NodeId' has a current key in `Profile'. A node
%% that has one keeps it, so every pool of one identity shares it.
-spec ensure(<<_:256>>, macula_seal:profile()) -> ok.
ensure(NodeId, Profile) ->
    ensure(?MODULE, NodeId, Profile).

%% @doc As `ensure/2', in the key table `Table'.
-spec ensure(atom(), <<_:256>>, macula_seal:profile()) -> ok.
ensure(Table, <<_:256>> = NodeId, Profile) when Profile =:= pq_pure; Profile =:= pq_hybrid ->
    ensured(current(Table, NodeId), Table, NodeId, Profile).

ensured({ok, _Current}, _Table, _NodeId, _Profile) -> ok;
ensured(error, Table, NodeId, Profile) -> gen_server:call(ets:info(Table, owner), {ensure, NodeId, Profile}).

%% @doc The node's current key and its id, for its advertisements. A renewal
%% reads it when it signs, so a rotated key reaches the next advertisement.
-spec current(<<_:256>>) -> {ok, current()} | error.
current(NodeId) ->
    current(?MODULE, NodeId).

%% @doc As `current/1', from the key table `Table'.
-spec current(atom(), <<_:256>>) -> {ok, current()} | error.
current(Table, <<_:256>> = NodeId) ->
    current_of(ets:lookup(Table, {current, NodeId})).

current_of([{{current, _NodeId}, KeyId, Carried, _Profile}]) -> {ok, #{key_id => KeyId, key => Carried}};
current_of([]) -> error.

%% @doc What the node opens sealed requests with: a lookup of a key it holds
%% by the key's id, and the id of its current key, which a refusal names
%% (`macula_sealed_call:holder()'). The lookup reads the table when it runs,
%% so a key deleted since answers `error'.
-spec holder(<<_:256>>) -> {ok, macula_sealed_call:holder()} | error.
holder(NodeId) ->
    holder(?MODULE, NodeId).

%% @doc As `holder/1', from the key table `Table'.
-spec holder(atom(), <<_:256>>) -> {ok, macula_sealed_call:holder()} | error.
holder(Table, <<_:256>> = NodeId) ->
    holder_of(current(Table, NodeId), Table, NodeId).

holder_of({ok, #{key_id := Current}}, Table, NodeId) ->
    {ok, #{current_key_id => Current, lookup => fun(KeyId) -> held(ets:lookup(Table, {key, NodeId, KeyId})) end}};
holder_of(error, _Table, _NodeId) ->
    error.

held([{{key, _NodeId, _KeyId}, Private, Carried}]) -> {ok, Private, Carried};
held([]) -> error.

%% @doc Give the node a new current key now; the one it replaces opens calls
%% until it is retired.
-spec rotate(<<_:256>>) -> ok | error.
rotate(NodeId) ->
    rotate(?MODULE, NodeId).

%% @doc As `rotate/1', in the key table `Table'.
-spec rotate(atom(), <<_:256>>) -> ok | error.
rotate(Table, <<_:256>> = NodeId) ->
    gen_server:call(ets:info(Table, owner), {rotate, NodeId}).

%%====================================================================
%% gen_server
%%====================================================================

init(Options) ->
    Table = ets:new(maps:get(table, Options, ?MODULE), [named_table, protected, set, {read_concurrency, true}]),
    {ok, #{table => Table,
           rotate_after_ms => maps:get(rotate_after_ms, Options, ?ROTATE_AFTER_MS),
           retain_ms => maps:get(retain_ms, Options, ?RETAIN_MS)}}.

handle_call({ensure, NodeId, Profile}, _From, #{table := Table} = S) ->
    {reply, made_unless_held(ets:member(Table, {current, NodeId}), NodeId, Profile, S), S};
handle_call({rotate, NodeId}, _From, #{table := Table} = S) ->
    {reply, rotated(ets:lookup(Table, {current, NodeId}), NodeId, S), S};
handle_call(_Request, _From, S) ->
    {reply, {error, unknown_call}, S}.

handle_cast(_Message, S) ->
    {noreply, S}.

handle_info({rotate, NodeId, KeyId}, #{table := Table} = S) ->
    _ = scheduled_rotation(ets:lookup(Table, {current, NodeId}), KeyId, NodeId, S),
    {noreply, S};
handle_info({retire, NodeId, KeyId}, #{table := Table} = S) ->
    true = ets:delete(Table, {key, NodeId, KeyId}),
    {noreply, S};
handle_info(_Message, S) ->
    {noreply, S}.

%%====================================================================
%% Internal
%%====================================================================

made_unless_held(true, _NodeId, _Profile, _S) -> ok;
made_unless_held(false, NodeId, Profile, S) -> made(NodeId, Profile, S).

%% A new current key for the node, and its rotation scheduled.
made(NodeId, Profile, #{table := Table, rotate_after_ms := RotateAfter}) ->
    {Public, Private} = macula_seal:generate_key(Profile),
    Carried = macula_seal:key_as_carried(Public),
    KeyId = macula_seal:key_id(Carried),
    true = ets:insert(Table, [{{key, NodeId, KeyId}, Private, Carried}, {{current, NodeId}, KeyId, Carried, Profile}]),
    _ = erlang:send_after(RotateAfter, self(), {rotate, NodeId, KeyId}),
    ok.

rotated([{{current, NodeId}, OldKeyId, _Carried, Profile}], NodeId, #{retain_ms := Retain} = S) ->
    ok = made(NodeId, Profile, S),
    _ = erlang:send_after(Retain, self(), {retire, NodeId, OldKeyId}),
    ok;
rotated([], _NodeId, _S) ->
    error.

%% A scheduled rotation for a key that is still current; one for a key an
%% earlier rotation replaced is stale.
scheduled_rotation([{{current, NodeId}, KeyId, _Carried, _Profile}] = Current, KeyId, NodeId, S) ->
    rotated(Current, NodeId, S);
scheduled_rotation(_NotCurrent, _KeyId, _NodeId, _S) ->
    ok.
