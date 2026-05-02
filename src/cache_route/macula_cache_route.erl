%%%-------------------------------------------------------------------
%%% @doc TTL-bounded route cache for macula-net.
%%%
%%% Phase 2 (PLAN_MACULA_NET_PHASE2.md §4.3). Replaces Phase 1's
%%% static station table in {@link macula_route_packet}: route_packet
%%% consults this cache first, falls back to {@link
%%% macula_resolve_address} on miss, and inserts the result.
%%%
%%% Lookups are O(1) on an ETS set keyed by macula-net IPv6 address.
%%% A periodic sweep (gen_server timer) evicts entries whose
%%% `expires_at' has passed; lookups also expire-on-read so a stale
%%% entry never escapes.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cache_route).

-behaviour(gen_server).

-export([
    start_link/0, start_link/1,
    stop/0,
    lookup/1,
    insert/2,
    invalidate/1,
    sweep/0,
    size/0
]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([route_entry/0, lookup_result/0]).

-define(SERVER, ?MODULE).
-define(TABLE,  macula_cache_route_table).
-define(DEFAULT_SWEEP_MS, 30_000).

-type route_entry() :: #{
    station_pubkey  := <<_:256>>,
    host            := binary(),
    port            := 1..65535,
    expires_at      := pos_integer()
}.

-type lookup_result() :: {ok, route_entry()} | miss | expired.

-record(state, {
    sweep_ms :: pos_integer(),
    timer    :: reference() | undefined
}).

%% =============================================================================
%% Public API
%% =============================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

-spec stop() -> ok.
stop() ->
    case whereis(?SERVER) of
        undefined -> ok;
        _ -> gen_server:stop(?SERVER)
    end.

%% @doc Look up a route. Returns `{ok, Entry}' for a live cache hit,
%% `expired' if the entry was found but past its TTL (also evicted),
%% or `miss' if no entry was present.
-spec lookup(<<_:128>>) -> lookup_result().
lookup(Addr) when is_binary(Addr), byte_size(Addr) =:= 16 ->
    case ets:info(?TABLE) of
        undefined -> miss;
        _ ->
            case ets:lookup(?TABLE, Addr) of
                [{_, Entry}] -> check_expiry(Addr, Entry);
                []           -> miss
            end
    end.

-spec insert(<<_:128>>, route_entry()) -> ok.
insert(Addr, #{expires_at := X} = Entry)
  when is_binary(Addr), byte_size(Addr) =:= 16,
       is_integer(X), X > 0 ->
    ensure_table(),
    true = ets:insert(?TABLE, {Addr, Entry}),
    ok.

-spec invalidate(<<_:128>>) -> ok.
invalidate(Addr) when is_binary(Addr), byte_size(Addr) =:= 16 ->
    case ets:info(?TABLE) of
        undefined -> ok;
        _ -> _ = ets:delete(?TABLE, Addr), ok
    end.

%% @doc Force a sweep right now. Useful for tests.
-spec sweep() -> ok.
sweep() ->
    case whereis(?SERVER) of
        undefined -> sweep_table();
        Pid       -> gen_server:call(Pid, sweep)
    end.

-spec size() -> non_neg_integer().
size() ->
    case ets:info(?TABLE) of
        undefined -> 0;
        Info      -> proplists:get_value(size, Info, 0)
    end.

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init(Opts) ->
    ensure_table(),
    SweepMs = maps:get(sweep_ms, Opts, ?DEFAULT_SWEEP_MS),
    Timer = erlang:send_after(SweepMs, self(), sweep_tick),
    {ok, #state{sweep_ms = SweepMs, timer = Timer}}.

handle_call(sweep, _From, State) ->
    sweep_table(),
    {reply, ok, State};
handle_call(_Other, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(sweep_tick, #state{sweep_ms = Ms} = State) ->
    sweep_table(),
    Timer = erlang:send_after(Ms, self(), sweep_tick),
    {noreply, State#state{timer = Timer}};
handle_info(_Other, State) ->
    {noreply, State}.

terminate(_Reason, #state{timer = Timer}) ->
    cancel(Timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Internals
%% =============================================================================

ensure_table() ->
    case ets:info(?TABLE) of
        undefined ->
            _ = ets:new(?TABLE, [named_table, public, set,
                                 {read_concurrency, true}]),
            ok;
        _ ->
            ok
    end.

check_expiry(Addr, #{expires_at := X} = Entry) ->
    Now = erlang:system_time(millisecond),
    case Now < X of
        true  -> {ok, Entry};
        false ->
            _ = ets:delete(?TABLE, Addr),
            expired
    end.

%% Walk the table, drop entries whose expires_at has passed.
sweep_table() ->
    case ets:info(?TABLE) of
        undefined -> ok;
        _ ->
            Now = erlang:system_time(millisecond),
            ets:foldl(
              fun({Addr, #{expires_at := X}}, _Acc) when X =< Now ->
                      _ = ets:delete(?TABLE, Addr),
                      ok;
                 (_, _Acc) -> ok
              end, ok, ?TABLE)
    end.

cancel(undefined) -> ok;
cancel(T) when is_reference(T) ->
    _ = erlang:cancel_timer(T),
    ok.
