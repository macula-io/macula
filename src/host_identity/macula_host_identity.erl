%%%-------------------------------------------------------------------
%%% @doc Hosted-identity table for a macula-net station.
%%%
%%% PLAN_MACULA_NET_PHASE3.md §6.1. Tracks daemons that have attached
%%% to *this* station. On `attach/4', verifies the delegation,
%%% records the daemon, builds a signed `hosted_address_map' record
%%% and pushes it through the configured `put_fn'. On `detach/1',
%%% drops the entry; the record expires by TTL (Phase 4 may publish
%%% an explicit tombstone).
%%%
%%% The slice is a `gen_server' so it can refresh the hosted records
%%% on a timer at the same cadence {@link macula_advertise_station}
%%% uses for the station's own records.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_host_identity).

-behaviour(gen_server).

-export([
    start_link/1,
    stop/0,
    attach/4,
    detach/1,
    hosted/1,
    lookup/1,
    hosted_addresses/0,
    hosted_records/0,
    refresh_now/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([config/0, attach_conn/0]).

-define(SERVER, ?MODULE).
-define(TABLE,  macula_host_identity_table).
-define(DEFAULT_REFRESH_MS, 60_000).

-type attach_conn() :: term().

-type config() :: #{
    realm_pubkey  := <<_:256>>,
    host_pubkey   := <<_:256>>,
    host_privkey  := macula_identity:key_pair() | macula_identity:privkey(),
    put_fn        := macula_advertise_station:put_fn(),
    refresh_ms    => pos_integer()
}.

-record(entry, {
    daemon_pubkey :: <<_:256>>,
    delegation    :: macula_record:host_delegation(),
    attach_conn   :: attach_conn()
}).

-record(state, {
    config       :: config(),
    refresh_ms   :: pos_integer(),
    timer        :: reference() | undefined
}).

%% =============================================================================
%% Public API
%% =============================================================================

-spec start_link(config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

-spec stop() -> ok.
stop() ->
    case whereis(?SERVER) of
        undefined -> ok;
        _ -> gen_server:stop(?SERVER)
    end.

%% @doc Register a daemon as hosted by this station.
%%
%% `Delegation' must already be signed by the daemon (i.e.
%% {@link macula_record:sign_host_delegation/2}). The station
%% verifies the delegation and that its fields agree with the
%% station's identity + realm; any mismatch yields an error and the
%% daemon is NOT registered.
-spec attach(DaemonAddr  :: <<_:128>>,
             DaemonPubkey:: <<_:256>>,
             Delegation  :: macula_record:host_delegation(),
             AttachConn  :: attach_conn()) ->
    ok | {error, term()}.
attach(DaemonAddr, DaemonPubkey, Delegation, AttachConn)
  when is_binary(DaemonAddr), byte_size(DaemonAddr) =:= 16,
       is_binary(DaemonPubkey), byte_size(DaemonPubkey) =:= 32 ->
    gen_server:call(?SERVER,
                    {attach, DaemonAddr, DaemonPubkey, Delegation, AttachConn}).

-spec detach(<<_:128>>) -> ok.
detach(DaemonAddr) when is_binary(DaemonAddr), byte_size(DaemonAddr) =:= 16 ->
    gen_server:call(?SERVER, {detach, DaemonAddr}).

-spec hosted(<<_:128>>) -> boolean().
hosted(DaemonAddr) ->
    case ets:info(?TABLE) of
        undefined -> false;
        _ -> ets:member(?TABLE, DaemonAddr)
    end.

-spec lookup(<<_:128>>) -> {ok, attach_conn()} | not_found.
lookup(DaemonAddr) ->
    case ets:info(?TABLE) of
        undefined -> not_found;
        _ ->
            case ets:lookup(?TABLE, DaemonAddr) of
                [{_, #entry{attach_conn = C}}] -> {ok, C};
                []                              -> not_found
            end
    end.

-spec hosted_addresses() -> [<<_:128>>].
hosted_addresses() ->
    case ets:info(?TABLE) of
        undefined -> [];
        _ ->
            ets:foldl(fun({A, _}, Acc) -> [A | Acc] end, [], ?TABLE)
    end.

%% @doc Snapshot the current set of hosted_address_map records (signed).
-spec hosted_records() -> [macula_record:record()].
hosted_records() ->
    gen_server:call(?SERVER, hosted_records).

-spec refresh_now() -> ok.
refresh_now() ->
    gen_server:call(?SERVER, refresh_now).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init(#{realm_pubkey  := _,
       host_pubkey   := _,
       host_privkey  := _,
       put_fn        := PutFn} = Config) when is_function(PutFn, 1) ->
    process_flag(trap_exit, true),
    ensure_table(),
    RefreshMs = maps:get(refresh_ms, Config, ?DEFAULT_REFRESH_MS),
    Timer = erlang:send_after(RefreshMs, self(), refresh_tick),
    {ok, #state{config = Config, refresh_ms = RefreshMs, timer = Timer}}.

handle_call({attach, Addr, DaemonPk, Delegation, Conn}, _From,
            #state{config = Config} = State) ->
    Reply = handle_attach(Addr, DaemonPk, Delegation, Conn, Config),
    {reply, Reply, State};

handle_call({detach, Addr}, _From, State) ->
    _ = ets:delete(?TABLE, Addr),
    {reply, ok, State};

handle_call(hosted_records, _From, #state{config = Config} = State) ->
    {reply, build_all_records(Config), State};

handle_call(refresh_now, _From, State) ->
    publish_all(State),
    {reply, ok, State};

handle_call(_Other, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh_tick, #state{refresh_ms = Ms} = State) ->
    publish_all(State),
    NewTimer = erlang:send_after(Ms, self(), refresh_tick),
    {noreply, State#state{timer = NewTimer}};
handle_info(_Other, State) ->
    {noreply, State}.

terminate(_Reason, #state{timer = Timer}) ->
    cancel(Timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Internals — attach validation
%% =============================================================================

handle_attach(Addr, DaemonPk, Delegation, Conn, Config) ->
    case validate_attach(Addr, DaemonPk, Delegation, Config) of
        ok ->
            true = ets:insert(?TABLE,
                {Addr, #entry{daemon_pubkey = DaemonPk,
                              delegation    = Delegation,
                              attach_conn   = Conn}}),
            publish_one(Addr, DaemonPk, Delegation, Config),
            ok;
        {error, _} = Err ->
            Err
    end.

validate_attach(Addr, DaemonPk, Delegation,
                #{realm_pubkey := Realm, host_pubkey := HostPk}) ->
    %% derive_address(realm, daemon_pk) must equal Addr.
    case macula_address:derive(Realm, DaemonPk) of
        Addr -> validate_delegation_fields(Delegation, DaemonPk, HostPk, Realm);
        _    -> {error, address_does_not_match_daemon_pubkey}
    end.

validate_delegation_fields(#{daemon_pubkey := DPk,
                              host_pubkey   := HPk,
                              realm_pubkey  := RPk,
                              not_after_ms  := NA} = Delegation,
                            DaemonPk, HostPk, Realm) ->
    Now = erlang:system_time(millisecond),
    case {DPk =:= DaemonPk, HPk =:= HostPk, RPk =:= Realm, Now < NA} of
        {true, true, true, true} ->
            verify_delegation(Delegation);
        {false, _, _, _} -> {error, delegation_daemon_mismatch};
        {_, false, _, _} -> {error, delegation_host_mismatch};
        {_, _, false, _} -> {error, delegation_realm_mismatch};
        {_, _, _, false} -> {error, delegation_expired}
    end;
validate_delegation_fields(_Other, _DaemonPk, _HostPk, _Realm) ->
    {error, malformed_delegation}.

verify_delegation(Delegation) ->
    case macula_record:verify_host_delegation(Delegation) of
        {ok, _}    -> ok;
        {error, R} -> {error, {bad_delegation, R}}
    end.

%% =============================================================================
%% Record building + publication
%% =============================================================================

publish_one(Addr, DaemonPk, Delegation,
            #{host_pubkey := HostPk, host_privkey := Privkey,
              put_fn := PutFn}) ->
    Record = build_record(HostPk, Privkey, Addr, DaemonPk, Delegation),
    log_put(PutFn(Record), Record).

publish_all(#state{config = Config}) ->
    lists:foreach(fun(R) ->
        log_put((maps:get(put_fn, Config))(R), R)
    end, build_all_records(Config)).

build_all_records(#{host_pubkey := HostPk, host_privkey := Privkey}) ->
    ets:foldl(fun({Addr, #entry{daemon_pubkey = DPk, delegation = Del}},
                  Acc) ->
        [build_record(HostPk, Privkey, Addr, DPk, Del) | Acc]
    end, [], ?TABLE).

build_record(HostPk, Privkey, Addr, DaemonPk, Delegation) ->
    %% Re-attach the daemon_pubkey explicitly even though it's also in
    %% the delegation — the record's payload exposes it directly so
    %% resolvers can extract it without re-parsing the inner CBOR
    %% before the signature is checked.
    _ = DaemonPk,
    macula_record:sign(
      macula_record:hosted_address_map(HostPk, Addr, Delegation),
      Privkey).

log_put(ok, _Record) ->
    ok;
log_put({error, Reason}, Record) ->
    error_logger:warning_msg(
      "[host_identity] put_fn failed for hosted ~p: ~p",
      [binary:part(macula_record:key(Record), 0, 4), Reason]).

%% =============================================================================
%% ETS housekeeping
%% =============================================================================

ensure_table() ->
    case ets:info(?TABLE) of
        undefined ->
            _ = ets:new(?TABLE, [named_table, public, set,
                                 {read_concurrency, true},
                                 {keypos, 1}]),
            ok;
        _ -> ok
    end.

cancel(undefined) -> ok;
cancel(T) when is_reference(T) ->
    _ = erlang:cancel_timer(T),
    ok.
