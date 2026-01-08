%% @doc Authorization Audit Logging Module.
%%
%% Provides comprehensive audit logging for all authorization decisions in the
%% Macula mesh. Uses telemetry for real-time metrics and optionally stores
%% recent entries in ETS for debugging and analysis.
%%
%% == Telemetry Events ==
%%
%% - `[macula, authorization, allowed]' - Authorization succeeded
%% - `[macula, authorization, denied]' - Authorization denied
%% - `[macula, authorization, error]' - Authorization check error
%%
%% == Event Metadata ==
%%
%% All events include:
%% - `operation' - The operation type (call, publish, subscribe, announce)
%% - `caller' - The caller's DID
%% - `resource' - The topic or procedure
%% - `timestamp' - Unix timestamp
%%
%% Denied events also include:
%% - `reason' - Why authorization failed
%%
%% == Usage ==
%%
%% ```
%% %% Log an authorized operation
%% macula_authorization_audit:log_authorized(call, CallerDID, Procedure).
%%
%% %% Log a denied operation
%% macula_authorization_audit:log_denied(publish, CallerDID, Topic, unauthorized).
%%
%% %% Query recent audit entries (for debugging)
%% Entries = macula_authorization_audit:get_recent(100).
%% ```
%%
%% == Performance ==
%%
%% Designed for <1ms overhead:
%% - Telemetry events are synchronous but fast
%% - ETS writes are non-blocking
%% - Periodic cleanup prevents unbounded growth
%%
%% @author Claude
-module(macula_authorization_audit).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/0,
    stop/1
]).

%% Audit logging
-export([
    log_authorized/3,
    log_authorized/4,
    log_denied/4,
    log_denied/5,
    log_error/4,
    log_error/5
]).

%% Query API
-export([
    get_recent/1,
    get_recent/2,
    get_by_caller/2,
    get_by_caller/3,
    get_by_resource/2,
    get_by_resource/3,
    get_stats/0,
    get_stats/1,
    clear/0,
    clear/1
]).

%% Configuration
-export([
    set_retention/1,
    set_retention/2,
    set_max_entries/1,
    set_max_entries/2,
    is_enabled/0,
    is_enabled/1,
    enable/0,
    enable/1,
    disable/0,
    disable/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%%====================================================================
%% Types
%%====================================================================

-type operation() :: call | publish | subscribe | announce | atom().
-type did() :: binary().
-type resource() :: binary().
-type reason() :: unauthorized
                | invalid_ucan
                | expired_ucan
                | revoked_ucan
                | insufficient_capability
                | invalid_did
                | namespace_mismatch
                | atom().

-type audit_entry() :: #{
    id := binary(),
    timestamp := integer(),
    operation := operation(),
    caller := did(),
    resource := resource(),
    result := allowed | denied | error,
    reason => reason(),
    metadata => map()
}.

-type opts() :: #{
    retention_seconds => pos_integer(),
    max_entries => pos_integer(),
    enabled => boolean(),
    cleanup_interval => pos_integer()
}.

-export_type([audit_entry/0, opts/0]).

%%====================================================================
%% Macros
%%====================================================================

-define(DEFAULT_SERVER, ?MODULE).
-define(AUDIT_TABLE, macula_authorization_audit_log).
-define(DEFAULT_RETENTION, 3600).          %% 1 hour
-define(DEFAULT_MAX_ENTRIES, 10000).       %% 10k entries
-define(DEFAULT_CLEANUP_INTERVAL, 60000).  %% 1 minute

%%====================================================================
%% State
%%====================================================================

-record(state, {
    enabled = true :: boolean(),
    retention_seconds = ?DEFAULT_RETENTION :: pos_integer(),
    max_entries = ?DEFAULT_MAX_ENTRIES :: pos_integer(),
    cleanup_interval = ?DEFAULT_CLEANUP_INTERVAL :: pos_integer(),
    stats = #{} :: map()
}).

%%====================================================================
%% API - Start/Stop
%%====================================================================

%% @doc Start the audit server with default name.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the audit server with options.
-spec start_link(opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?DEFAULT_SERVER}, ?MODULE, Opts, []).

%% @doc Stop the default audit server.
-spec stop() -> ok.
stop() ->
    stop(?DEFAULT_SERVER).

%% @doc Stop a specific audit server.
-spec stop(pid() | atom()) -> ok.
stop(ServerRef) ->
    gen_server:stop(ServerRef).

%%====================================================================
%% API - Audit Logging
%%====================================================================

%% @doc Log an authorized operation.
-spec log_authorized(operation(), did(), resource()) -> ok.
log_authorized(Operation, CallerDID, Resource) ->
    log_authorized(?DEFAULT_SERVER, Operation, CallerDID, Resource).

%% @doc Log an authorized operation to specific server.
-spec log_authorized(pid() | atom(), operation(), did(), resource()) -> ok.
log_authorized(ServerRef, Operation, CallerDID, Resource) ->
    Now = erlang:system_time(second),

    %% Emit telemetry event (always, even if ETS storage disabled)
    telemetry:execute(
        [macula, authorization, allowed],
        #{count => 1, duration_ms => 0},
        #{operation => Operation, caller => CallerDID, resource => Resource, timestamp => Now}
    ),

    %% Store in ETS if enabled
    gen_server:cast(ServerRef, {log, allowed, Operation, CallerDID, Resource, undefined, #{}, Now}).

%% @doc Log a denied operation.
-spec log_denied(operation(), did(), resource(), reason()) -> ok.
log_denied(Operation, CallerDID, Resource, Reason) ->
    log_denied(?DEFAULT_SERVER, Operation, CallerDID, Resource, Reason).

%% @doc Log a denied operation to specific server.
-spec log_denied(pid() | atom(), operation(), did(), resource(), reason()) -> ok.
log_denied(ServerRef, Operation, CallerDID, Resource, Reason) ->
    Now = erlang:system_time(second),

    %% Emit telemetry event
    telemetry:execute(
        [macula, authorization, denied],
        #{count => 1},
        #{operation => Operation, caller => CallerDID, resource => Resource,
          reason => Reason, timestamp => Now}
    ),

    %% Store in ETS if enabled
    gen_server:cast(ServerRef, {log, denied, Operation, CallerDID, Resource, Reason, #{}, Now}).

%% @doc Log an error during authorization check.
-spec log_error(operation(), did(), resource(), term()) -> ok.
log_error(Operation, CallerDID, Resource, Error) ->
    log_error(?DEFAULT_SERVER, Operation, CallerDID, Resource, Error).

%% @doc Log an error to specific server.
-spec log_error(pid() | atom(), operation(), did(), resource(), term()) -> ok.
log_error(ServerRef, Operation, CallerDID, Resource, Error) ->
    Now = erlang:system_time(second),

    %% Emit telemetry event
    telemetry:execute(
        [macula, authorization, error],
        #{count => 1},
        #{operation => Operation, caller => CallerDID, resource => Resource,
          error => Error, timestamp => Now}
    ),

    %% Store in ETS if enabled
    gen_server:cast(ServerRef, {log, error, Operation, CallerDID, Resource, Error, #{}, Now}).

%%====================================================================
%% API - Query
%%====================================================================

%% @doc Get recent audit entries (most recent first).
-spec get_recent(pos_integer()) -> [audit_entry()].
get_recent(Limit) ->
    get_recent(?DEFAULT_SERVER, Limit).

%% @doc Get recent audit entries from specific server.
-spec get_recent(pid() | atom(), pos_integer()) -> [audit_entry()].
get_recent(_ServerRef, Limit) ->
    try
        All = [Entry || {_Id, Entry} <- ets:tab2list(?AUDIT_TABLE)],
        Sorted = lists:sort(fun(A, B) ->
            maps:get(timestamp, A) > maps:get(timestamp, B)
        end, All),
        lists:sublist(Sorted, Limit)
    catch
        error:badarg -> []
    end.

%% @doc Get audit entries for a specific caller.
-spec get_by_caller(did(), pos_integer()) -> [audit_entry()].
get_by_caller(CallerDID, Limit) ->
    get_by_caller(?DEFAULT_SERVER, CallerDID, Limit).

%% @doc Get audit entries for a specific caller from specific server.
-spec get_by_caller(pid() | atom(), did(), pos_integer()) -> [audit_entry()].
get_by_caller(_ServerRef, CallerDID, Limit) ->
    try
        All = [Entry || {_Id, Entry} <- ets:tab2list(?AUDIT_TABLE)],
        Matching = [E || E <- All, maps:get(caller, E) =:= CallerDID],
        Sorted = lists:sort(fun(A, B) ->
            maps:get(timestamp, A) > maps:get(timestamp, B)
        end, Matching),
        lists:sublist(Sorted, Limit)
    catch
        error:badarg -> []
    end.

%% @doc Get audit entries for a specific resource.
-spec get_by_resource(resource(), pos_integer()) -> [audit_entry()].
get_by_resource(Resource, Limit) ->
    get_by_resource(?DEFAULT_SERVER, Resource, Limit).

%% @doc Get audit entries for a specific resource from specific server.
-spec get_by_resource(pid() | atom(), resource(), pos_integer()) -> [audit_entry()].
get_by_resource(_ServerRef, Resource, Limit) ->
    try
        All = [Entry || {_Id, Entry} <- ets:tab2list(?AUDIT_TABLE)],
        Matching = [E || E <- All, maps:get(resource, E) =:= Resource],
        Sorted = lists:sort(fun(A, B) ->
            maps:get(timestamp, A) > maps:get(timestamp, B)
        end, Matching),
        lists:sublist(Sorted, Limit)
    catch
        error:badarg -> []
    end.

%% @doc Get audit statistics.
-spec get_stats() -> map().
get_stats() ->
    get_stats(?DEFAULT_SERVER).

%% @doc Get audit statistics from specific server.
-spec get_stats(pid() | atom()) -> map().
get_stats(ServerRef) ->
    gen_server:call(ServerRef, get_stats).

%% @doc Clear all audit entries.
-spec clear() -> ok.
clear() ->
    clear(?DEFAULT_SERVER).

%% @doc Clear all audit entries from specific server.
-spec clear(pid() | atom()) -> ok.
clear(ServerRef) ->
    gen_server:call(ServerRef, clear).

%%====================================================================
%% API - Configuration
%%====================================================================

%% @doc Set retention period in seconds.
-spec set_retention(pos_integer()) -> ok.
set_retention(Seconds) ->
    set_retention(?DEFAULT_SERVER, Seconds).

%% @doc Set retention period for specific server.
-spec set_retention(pid() | atom(), pos_integer()) -> ok.
set_retention(ServerRef, Seconds) ->
    gen_server:call(ServerRef, {set_retention, Seconds}).

%% @doc Set maximum number of entries.
-spec set_max_entries(pos_integer()) -> ok.
set_max_entries(MaxEntries) ->
    set_max_entries(?DEFAULT_SERVER, MaxEntries).

%% @doc Set maximum entries for specific server.
-spec set_max_entries(pid() | atom(), pos_integer()) -> ok.
set_max_entries(ServerRef, MaxEntries) ->
    gen_server:call(ServerRef, {set_max_entries, MaxEntries}).

%% @doc Check if audit logging is enabled.
-spec is_enabled() -> boolean().
is_enabled() ->
    is_enabled(?DEFAULT_SERVER).

%% @doc Check if audit logging is enabled for specific server.
-spec is_enabled(pid() | atom()) -> boolean().
is_enabled(ServerRef) ->
    gen_server:call(ServerRef, is_enabled).

%% @doc Enable audit logging.
-spec enable() -> ok.
enable() ->
    enable(?DEFAULT_SERVER).

%% @doc Enable audit logging for specific server.
-spec enable(pid() | atom()) -> ok.
enable(ServerRef) ->
    gen_server:call(ServerRef, enable).

%% @doc Disable audit logging (telemetry still emits, ETS storage disabled).
-spec disable() -> ok.
disable() ->
    disable(?DEFAULT_SERVER).

%% @doc Disable audit logging for specific server.
-spec disable(pid() | atom()) -> ok.
disable(ServerRef) ->
    gen_server:call(ServerRef, disable).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init(Opts) ->
    %% Create ETS table for audit log storage
    %% Stores {Id, Entry} tuples where Entry is a map
    ets:new(?AUDIT_TABLE, [
        named_table,
        public,
        set,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),

    State = #state{
        enabled = maps:get(enabled, Opts, true),
        retention_seconds = maps:get(retention_seconds, Opts, ?DEFAULT_RETENTION),
        max_entries = maps:get(max_entries, Opts, ?DEFAULT_MAX_ENTRIES),
        cleanup_interval = maps:get(cleanup_interval, Opts, ?DEFAULT_CLEANUP_INTERVAL),
        stats = #{
            allowed_count => 0,
            denied_count => 0,
            error_count => 0,
            cleanup_count => 0
        }
    },

    %% Schedule periodic cleanup
    schedule_cleanup(State#state.cleanup_interval),

    {ok, State}.

%% @private
handle_call(get_stats, _From, State = #state{stats = Stats}) ->
    TableInfo = try
        #{
            table_size => ets:info(?AUDIT_TABLE, size),
            memory_bytes => ets:info(?AUDIT_TABLE, memory) * erlang:system_info(wordsize)
        }
    catch
        error:badarg -> #{table_size => 0, memory_bytes => 0}
    end,
    FullStats = maps:merge(Stats, TableInfo),
    FullStats2 = FullStats#{
        enabled => State#state.enabled,
        retention_seconds => State#state.retention_seconds,
        max_entries => State#state.max_entries
    },
    {reply, FullStats2, State};

handle_call(clear, _From, State) ->
    try ets:delete_all_objects(?AUDIT_TABLE) catch error:badarg -> ok end,
    NewStats = #{
        allowed_count => 0,
        denied_count => 0,
        error_count => 0,
        cleanup_count => 0
    },
    {reply, ok, State#state{stats = NewStats}};

handle_call({set_retention, Seconds}, _From, State) ->
    {reply, ok, State#state{retention_seconds = Seconds}};

handle_call({set_max_entries, MaxEntries}, _From, State) ->
    {reply, ok, State#state{max_entries = MaxEntries}};

handle_call(is_enabled, _From, State = #state{enabled = Enabled}) ->
    {reply, Enabled, State};

handle_call(enable, _From, State) ->
    {reply, ok, State#state{enabled = true}};

handle_call(disable, _From, State) ->
    {reply, ok, State#state{enabled = false}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({log, Result, _Operation, _CallerDID, _Resource, _Reason, _Metadata, _Timestamp},
            State = #state{enabled = false}) ->
    %% Logging disabled, just update stats
    NewStats = update_stats(Result, State#state.stats),
    {noreply, State#state{stats = NewStats}};

handle_cast({log, Result, Operation, CallerDID, Resource, Reason, Metadata, Timestamp},
            State = #state{enabled = true, max_entries = MaxEntries}) ->
    %% Create audit entry
    EntryId = generate_entry_id(),
    Entry = #{
        id => EntryId,
        timestamp => Timestamp,
        operation => Operation,
        caller => CallerDID,
        resource => Resource,
        result => Result,
        reason => Reason,
        metadata => Metadata
    },

    %% Store in ETS as {Id, Entry} tuple
    try
        ets:insert(?AUDIT_TABLE, {EntryId, Entry}),

        %% Check if we need to evict old entries
        CurrentSize = ets:info(?AUDIT_TABLE, size),
        case CurrentSize > MaxEntries of
            true -> evict_oldest(CurrentSize - MaxEntries);
            false -> ok
        end
    catch
        error:badarg -> ok
    end,

    NewStats = update_stats(Result, State#state.stats),
    {noreply, State#state{stats = NewStats}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(cleanup, State = #state{retention_seconds = Retention, cleanup_interval = Interval}) ->
    Expired = cleanup_expired(Retention),
    NewStats = maps:update_with(cleanup_count, fun(C) -> C + Expired end, Expired, State#state.stats),
    schedule_cleanup(Interval),
    {noreply, State#state{stats = NewStats}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    try ets:delete(?AUDIT_TABLE) catch error:badarg -> ok end,
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Generate unique entry ID
-spec generate_entry_id() -> binary().
generate_entry_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(16#FFFFFFFF),
    list_to_binary(io_lib:format("~16.16.0b-~8.16.0b", [Timestamp, Random])).

%% @private Update statistics
-spec update_stats(allowed | denied | error, map()) -> map().
update_stats(allowed, Stats) ->
    maps:update_with(allowed_count, fun(C) -> C + 1 end, 1, Stats);
update_stats(denied, Stats) ->
    maps:update_with(denied_count, fun(C) -> C + 1 end, 1, Stats);
update_stats(error, Stats) ->
    maps:update_with(error_count, fun(C) -> C + 1 end, 1, Stats).

%% @private Schedule cleanup timer
-spec schedule_cleanup(pos_integer()) -> reference().
schedule_cleanup(Interval) ->
    erlang:send_after(Interval, self(), cleanup).

%% @private Remove expired entries
-spec cleanup_expired(pos_integer()) -> non_neg_integer().
cleanup_expired(RetentionSeconds) ->
    try
        Cutoff = erlang:system_time(second) - RetentionSeconds,
        All = ets:tab2list(?AUDIT_TABLE),
        Expired = [Id || {Id, Entry} <- All, maps:get(timestamp, Entry) < Cutoff],
        lists:foreach(fun(Id) -> ets:delete(?AUDIT_TABLE, Id) end, Expired),
        length(Expired)
    catch
        error:badarg -> 0
    end.

%% @private Evict oldest entries when over capacity
-spec evict_oldest(pos_integer()) -> ok.
evict_oldest(Count) when Count =< 0 ->
    ok;
evict_oldest(Count) ->
    try
        All = ets:tab2list(?AUDIT_TABLE),
        Sorted = lists:sort(fun({_IdA, A}, {_IdB, B}) ->
            maps:get(timestamp, A) < maps:get(timestamp, B)
        end, All),
        ToDelete = lists:sublist(Sorted, Count),
        lists:foreach(fun({Id, _Entry}) -> ets:delete(?AUDIT_TABLE, Id) end, ToDelete),
        ok
    catch
        error:badarg -> ok
    end.
