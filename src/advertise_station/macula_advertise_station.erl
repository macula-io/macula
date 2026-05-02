%%%-------------------------------------------------------------------
%%% @doc Periodic DHT advertisement of a macula-net station.
%%%
%%% Mirrors the shape of {@link macula_dist_discovery}: build a signed
%%% record once, publish it via a caller-supplied `put_fn', refresh
%%% on a timer at well below the record's TTL.
%%%
%%% Phase 2 publishes two record kinds per call:
%%%
%%% <ul>
%%%   <li>One {@link macula_record:station_endpoint/3} — keyed under
%%%       `sha256("station_endpoint" || pubkey)' — telling resolvers
%%%       how to reach the station's QUIC port.</li>
%%%   <li>One {@link macula_record:address_pubkey_map/3} per
%%%       macula-net address the station hosts — keyed under
%%%       `sha256("address_pubkey_map" || addr)' — letting resolvers
%%%       turn a bare IPv6 into a station pubkey.</li>
%%% </ul>
%%%
%%% The `put_fn' decoupling lets the same code drive co-located
%%% deployments (where the relay's internal API is reachable) and
%%% client-only deployments (where a `macula_mesh_client' Pid is the
%%% right primitive). PLAN_MACULA_NET_PHASE2.md §4.1 / §5.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_advertise_station).

-behaviour(gen_server).

-export([
    start_link/1,
    stop/0,
    refresh_now/0,
    current_records/0
]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([config/0, put_fn/0]).

-define(SERVER, ?MODULE).
-define(DEFAULT_REFRESH_MS, 60_000). %% 1 minute (TTL is 5 minutes).

-type put_fn() :: fun((macula_record:record()) -> ok | {error, term()}).

-type config() :: #{
    realm_pubkey     := <<_:256>>,
    identity_pubkey  := <<_:256>>,
    identity_privkey := macula_identity:key_pair() | macula_identity:privkey(),
    quic_port        := 1..65535,
    addresses        := [<<_:128>>],
    put_fn           := put_fn(),
    advertised_ips   => [binary()],
    alpn             => binary(),
    refresh_ms       => pos_integer()
}.

-record(state, {
    config       :: config(),
    refresh_ms   :: pos_integer(),
    timer        :: reference() | undefined,
    last_records :: [macula_record:record()]
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

%% @doc Force an immediate re-advertise. Useful from tests.
-spec refresh_now() -> ok.
refresh_now() ->
    gen_server:call(?SERVER, refresh_now).

%% @doc Return the most recently published record set.
-spec current_records() -> [macula_record:record()].
current_records() ->
    gen_server:call(?SERVER, current_records).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init(#{realm_pubkey    := _,
       identity_pubkey := _,
       identity_privkey:= _,
       quic_port       := _,
       addresses       := Addresses,
       put_fn          := PutFn} = Config)
  when is_list(Addresses), is_function(PutFn, 1) ->
    process_flag(trap_exit, true),
    RefreshMs = maps:get(refresh_ms, Config, ?DEFAULT_REFRESH_MS),
    State0 = #state{config = Config, refresh_ms = RefreshMs,
                    last_records = []},
    State1 = publish_all(State0),
    {ok, schedule_refresh(State1)}.

handle_call(refresh_now, _From, State) ->
    {reply, ok, schedule_refresh(publish_all(State))};
handle_call(current_records, _From, #state{last_records = R} = State) ->
    {reply, R, State};
handle_call(_Other, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh_advertisement, State) ->
    {noreply, schedule_refresh(publish_all(State))};
handle_info(_Other, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% TTL-driven cleanup. Phase 4 may add gratuitous tombstones.
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Internals — record building + publication
%% =============================================================================

publish_all(#state{config = Config} = State) ->
    Records = build_records(Config),
    PutFn = maps:get(put_fn, Config),
    lists:foreach(fun(R) -> log_put(PutFn(R), R) end, Records),
    State#state{last_records = Records}.

%% Build the (signed) station_endpoint + per-address redirect records.
build_records(#{identity_pubkey  := Pk,
                identity_privkey := Privkey,
                quic_port        := Port,
                addresses        := Addresses} = Config) ->
    EndpointOpts = endpoint_opts(Config),
    Endpoint = macula_record:sign(
                 macula_record:station_endpoint(Pk, Port, EndpointOpts),
                 Privkey),
    Redirects = [
        macula_record:sign(
          macula_record:address_pubkey_map(Pk, Addr),
          Privkey)
        || Addr <- Addresses
    ],
    [Endpoint | Redirects].

endpoint_opts(Config) ->
    Opts0 = #{},
    Opts1 = case maps:find(advertised_ips, Config) of
        {ok, []}   -> Opts0;
        {ok, IPs}  -> Opts0#{host_advertised => IPs};
        error      -> Opts0
    end,
    case maps:find(alpn, Config) of
        {ok, A}  -> Opts1#{alpn => A};
        error    -> Opts1
    end.

log_put(ok, _Record) ->
    ok;
log_put({error, Reason}, Record) ->
    error_logger:warning_msg(
      "[advertise_station] put_fn failed for type ~p key ~p: ~p",
      [macula_record:type(Record),
       binary:part(macula_record:key(Record), 0, 4),
       Reason]).

schedule_refresh(#state{timer = OldTimer, refresh_ms = Ms} = State) ->
    cancel(OldTimer),
    NewTimer = erlang:send_after(Ms, self(), refresh_advertisement),
    State#state{timer = NewTimer}.

cancel(undefined) -> ok;
cancel(Timer) when is_reference(Timer) ->
    _ = erlang:cancel_timer(Timer),
    ok.
