%% @doc UUIDv7 (RFC 9562, section 5.7): random ids, and record versions that strictly increase on a node.
%%
%% Bit layout (128 bits, 16 bytes):
%% <pre>
%%  48b unix_ts_ms | 4b ver=7 | 12b rand_a | 2b var=10 | 62b rand_b
%% </pre>
%%
%% `v7/0' fills rand_a and rand_b with random bits, for ids that need no order. `v7_monotonic/1' is the version of a
%% record this node signs (RFC 9562, section 6.2, method 1): rand_a is a counter within the millisecond, reseeded with
%% a random 11-bit value at each new millisecond, and the 60 bits of millisecond and counter are the larger of that
%% fresh prefix and the last one issued plus one. A counter that runs out moves to the next millisecond. So the
%% versions a node issues strictly increase, also across a wall-clock step back.
%%
%% A version is an order, not a time. A record's created_at stays the wall-clock fact; after a clock step back a
%% version's millisecond field can run ahead of it, and nothing reads that field.
%%
%% This process owns the table holding the last prefix issued and runs under `macula_root'. Callers advance it
%% themselves with a compare-and-swap, so no version waits on a message. Without the table, between an owner's exit
%% and its restart, and after a restart, the next version starts again from the clock.
-module(macula_record_uuid).
-behaviour(gen_server).

-export([v7/0, v7_monotonic/1, v7_monotonic/2, start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-export_type([options/0]).

%% `table' names the table of the last prefix issued; it exists for tests.
-type options() :: #{table => atom()}.

%% @doc A random UUIDv7 at the wall clock, for an id that needs no order.
-spec v7() -> <<_:128>>.
v7() ->
    <<RandA:12, RandB:62, _Pad:6>> = crypto:strong_rand_bytes(10),
    <<(erlang:system_time(millisecond)):48, 7:4, RandA:12, 2:2, RandB:62>>.

%% @doc The next record version at wall-clock millisecond `Ms': above every version this node issued before.
-spec v7_monotonic(non_neg_integer()) -> <<_:128>>.
v7_monotonic(Ms) ->
    v7_monotonic(?MODULE, Ms).

%% @doc As `v7_monotonic/1', from the table `Table'.
-spec v7_monotonic(atom(), non_neg_integer()) -> <<_:128>>.
v7_monotonic(Table, Ms) when is_atom(Table), is_integer(Ms), Ms >= 0 ->
    <<Seed:11, RandB:62, _Pad:7>> = crypto:strong_rand_bytes(10),
    Prefix = issued(ets:whereis(Table), Table, (Ms bsl 12) bor Seed),
    <<(Prefix bsr 12):48, 7:4, (Prefix band 16#FFF):12, 2:2, RandB:62>>.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link(Options) when is_map(Options) ->
    gen_server:start_link(?MODULE, Options, []).

init(Options) ->
    {ok, ets:new(maps:get(table, Options, ?MODULE), [named_table, public, set, {write_concurrency, true}])}.

handle_call(_Request, _From, Table) ->
    {reply, {error, unknown_call}, Table}.

handle_cast(_Message, Table) ->
    {noreply, Table}.

%% The prefix to issue: the fresh one, or the last one issued plus one when that is larger.
issued(undefined, _Table, Fresh) ->
    Fresh;
issued(_Tid, Table, Fresh) ->
    claimed(ets:lookup(Table, last), Table, Fresh).

claimed([], Table, Fresh) ->
    inserted(ets:insert_new(Table, {last, Fresh}), Table, Fresh);
claimed([{last, Last}], Table, Fresh) ->
    Next = max(Fresh, Last + 1),
    swapped(ets:select_replace(Table, [{{last, Last}, [], [{{last, Next}}]}]), Table, Fresh, Next).

inserted(true, _Table, Fresh) -> Fresh;
inserted(false, Table, Fresh) -> issued(ets:whereis(Table), Table, Fresh).

%% Another caller issued a prefix between the read and the swap: read again.
swapped(1, _Table, _Fresh, Next) -> Next;
swapped(0, Table, Fresh, _Next) -> issued(ets:whereis(Table), Table, Fresh).
