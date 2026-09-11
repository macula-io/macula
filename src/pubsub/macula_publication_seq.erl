%% @doc The seq counter of the publications a node signs, one per key.
%%
%% All publications a node signs with one key share one counter
%% (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, Publications), so a pool and a
%% pubsub server signing with the same key never interleave two counters,
%% which an ordering subscriber would read as a restart at every switch.
%% A key's counter starts at the wall clock in microseconds the first time
%% the key publishes, and adds one for each publication after that.
%%
%% This process owns the counter table and runs under `macula_root'.
%% `next/1' updates the table atomically from the caller's process. If the
%% table is lost with its owner, the clock seeds each counter again, which
%% puts the next number above every number handed out before, as long as
%% the key published less than once per microsecond on average.
-module(macula_publication_seq).
-behaviour(gen_server).

-export([start_link/0, start_link/1, next/1, next/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-export_type([options/0]).

%% `table' names the counter table, and `clock' gives microseconds since the
%% Unix epoch; both exist for tests.
-type options() :: #{table => atom(), clock => fun(() -> non_neg_integer())}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link(Options) when is_map(Options) ->
    gen_server:start_link(?MODULE, Options, []).

%% @doc The next seq for a publication signed with the key whose key id is
%% `KeyId'.
-spec next(<<_:256>>) -> non_neg_integer().
next(KeyId) ->
    next(?MODULE, KeyId).

%% @doc As `next/1', from the counter table `Table'.
-spec next(atom(), <<_:256>>) -> non_neg_integer().
next(Table, <<_:256>> = KeyId) ->
    [{clock, Clock}] = ets:lookup(Table, clock),
    ets:update_counter(Table, {seq, KeyId}, 1, {{seq, KeyId}, Clock() - 1}).

init(Options) ->
    Table = ets:new(maps:get(table, Options, ?MODULE), [named_table, public, set, {write_concurrency, true}]),
    true = ets:insert(Table, {clock, maps:get(clock, Options, fun wall_clock_microseconds/0)}),
    {ok, Table}.

handle_call(_Request, _From, Table) ->
    {reply, {error, unknown_call}, Table}.

handle_cast(_Message, Table) ->
    {noreply, Table}.

wall_clock_microseconds() ->
    erlang:system_time(microsecond).
