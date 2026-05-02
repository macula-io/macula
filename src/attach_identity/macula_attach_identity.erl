%%%-------------------------------------------------------------------
%%% @doc Daemon-side attachment to a host station.
%%%
%%% PLAN_MACULA_NET_PHASE3.md §6.2. The daemon dials its host, sends
%%% a signed attach-request frame on a bidi stream, and from then on
%%% sends/receives macula-net envelopes through that same stream.
%%%
%%% The handshake reuses the existing
%%% {@link macula_net_transport_quic} pipe — daemons share the
%%% transport's framing logic with stations even though they don't
%%% accept inbound connections themselves. The handshake frame and
%%% subsequent envelopes are length-prefixed CBOR maps.
%%%
%%% Phase 3 MVP: no heartbeat (QUIC keepalive holds the connection),
%%% no automatic reconnect (Phase 4). Detach is explicit.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_attach_identity).

-behaviour(gen_server).

-export([
    attach/4,
    detach/1,
    send/2,
    set_inbound_handler/2,
    daemon_address/1
]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([config/0, handle/0, host_endpoint/0, inbound_handler/0]).

-type inbound_handler() :: fun((Envelope :: binary()) -> any()).

-define(SEND_TIMEOUT, 5000).
-define(DELEGATION_TTL_MS, 5 * 60 * 1000).

-type host_endpoint() :: #{
    station_pubkey := <<_:256>>,
    host           := binary() | string(),
    port           := 1..65535
}.

-type config() :: #{
    host_endpoint   := host_endpoint(),
    realm_pubkey    := <<_:256>>,
    daemon_keypair  := macula_identity:key_pair(),
    inbound_handler => inbound_handler()
}.

-type handle() :: pid().

-record(state, {
    config            :: config(),
    station_id        :: binary(),
    daemon_addr       :: <<_:128>>,
    inbound_handler   :: inbound_handler() | undefined
}).

%% =============================================================================
%% Public API
%% =============================================================================

%% @doc Attach to a host station. Performs the handshake and returns
%% an opaque handle on success. The caller may then push envelopes
%% via {@link send/2} and register an inbound handler via
%% {@link set_inbound_handler/2}.
-spec attach(host_endpoint(),
             RealmPubkey :: <<_:256>>,
             macula_identity:key_pair(),
             map()) -> {ok, handle()} | {error, term()}.
attach(HostEndpoint, RealmPubkey, KeyPair, Opts) ->
    Config = #{
        host_endpoint  => HostEndpoint,
        realm_pubkey   => RealmPubkey,
        daemon_keypair => KeyPair,
        inbound_handler => maps:get(inbound_handler, Opts, undefined)
    },
    gen_server:start_link(?MODULE, Config, []).

-spec detach(handle()) -> ok.
detach(Handle) when is_pid(Handle) ->
    catch gen_server:stop(Handle, normal, 5000),
    ok.

%% @doc Send a macula-net envelope over the attach stream.
-spec send(handle(), Envelope :: binary()) -> ok | {error, term()}.
send(Handle, Envelope) when is_pid(Handle), is_binary(Envelope) ->
    gen_server:call(Handle, {send, Envelope}, ?SEND_TIMEOUT).

-spec set_inbound_handler(handle(), inbound_handler()) -> ok.
set_inbound_handler(Handle, Fun) when is_pid(Handle), is_function(Fun, 1) ->
    gen_server:call(Handle, {set_inbound_handler, Fun}).

-spec daemon_address(handle()) -> {ok, <<_:128>>} | {error, term()}.
daemon_address(Handle) when is_pid(Handle) ->
    gen_server:call(Handle, daemon_address).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init(#{host_endpoint   := #{station_pubkey := StationId,
                             host           := Host,
                             port           := Port},
        realm_pubkey    := Realm,
        daemon_keypair  := KeyPair,
        inbound_handler := Handler} = Config) ->
    process_flag(trap_exit, true),
    DaemonPk   = macula_identity:public(KeyPair),
    DaemonAddr = macula_address:derive(Realm, DaemonPk),
    case ensure_connected(StationId, Host, Port) of
        ok ->
            case send_handshake(StationId, DaemonPk, DaemonAddr,
                                Realm, KeyPair) of
                ok ->
                    %% Wire the transport's inbound handler so we can
                    %% deliver received envelopes to the daemon's
                    %% callback.
                    Self = self(),
                    ok = macula_net_transport_quic:set_handler(
                            fun(Cbor) ->
                                gen_server:cast(Self, {inbound, Cbor})
                            end),
                    {ok, #state{config = Config,
                                station_id = StationId,
                                daemon_addr = DaemonAddr,
                                inbound_handler = Handler}};
                {error, _} = E ->
                    {stop, E}
            end;
        {error, _} = E ->
            {stop, E}
    end.

handle_call({send, Envelope}, _From, #state{station_id = SId} = State) ->
    {reply, macula_net_transport_quic:send(SId, Envelope), State};

handle_call({set_inbound_handler, Fun}, _From, State) ->
    {reply, ok, State#state{inbound_handler = Fun}};

handle_call(daemon_address, _From, #state{daemon_addr = Addr} = State) ->
    {reply, {ok, Addr}, State};

handle_call(_Other, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({inbound, _Cbor}, #state{inbound_handler = undefined} = State) ->
    {noreply, State};
handle_cast({inbound, Cbor}, #state{inbound_handler = Fun} = State)
  when is_function(Fun, 1) ->
    safe_invoke(Fun, Cbor),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{station_id = SId}) ->
    catch macula_net_transport_quic:disconnect(SId),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Internals
%% =============================================================================

%% Connect to host (idempotent — already_connected counts as success).
ensure_connected(StationId, Host, Port) ->
    case macula_net_transport_quic:connect(StationId, Host, Port) of
        ok                         -> ok;
        {error, already_connected} -> ok;
        {error, _} = E             -> E
    end.

send_handshake(StationId, DaemonPk, DaemonAddr, Realm, KeyPair) ->
    Now = erlang:system_time(millisecond),
    Delegation = macula_record:sign_host_delegation(
                   macula_record:host_delegation(
                     DaemonPk, StationId, Realm,
                     Now, Now + ?DELEGATION_TTL_MS),
                   KeyPair),
    Hello = macula_cbor_nif:pack(#{
        <<"type">>          => <<"macula_attach_v1">>,
        <<"daemon_pubkey">> => DaemonPk,
        <<"daemon_addr">>   => DaemonAddr,
        <<"delegation">>    => delegation_payload(Delegation)
    }),
    macula_net_transport_quic:send(StationId, Hello).

%% Encode the delegation in the same wire shape host_identity expects
%% to parse (single-letter keys; matches macula_record's internal
%% representation).
delegation_payload(#{daemon_pubkey := DaemonPk,
                      host_pubkey   := HostPk,
                      realm_pubkey  := Realm,
                      not_before_ms := NB,
                      not_after_ms  := NA,
                      daemon_sig    := Sig}) ->
    #{
        <<"d">>  => DaemonPk,
        <<"h">>  => HostPk,
        <<"r">>  => Realm,
        <<"nb">> => NB,
        <<"na">> => NA,
        <<"s">>  => Sig
    }.

safe_invoke(Fun, Arg) ->
    %% Boundary: a user-supplied callback crash must not take down
    %% the gen_server. Same exception logic as macula_net_transport_quic.
    try Fun(Arg)
    catch _:_ -> ok
    end.
